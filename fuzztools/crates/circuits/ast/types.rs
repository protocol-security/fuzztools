//! Implements the Noir IR types

use crate::{
    circuits::{
        ast::{random_string, scope::Scope},
        context::Context,
    },
    math::{bernoulli, random_field_element},
};
use rand::{seq::IndexedRandom, Rng};
use std::collections::VecDeque;

// ────────────────────────────────────────────────────────────────────────────────
// Type definitions
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Field(Field),
    Integer(Integer),
    Boolean(Boolean),
    String(StringType),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Struct(Struct),
    Lambda(Lambda),
    Null,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Field {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Integer {
    pub bits: u8,
    pub signed: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Boolean {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StringType {
    pub size: usize,
    pub hash_number: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Array {
    pub ty: Box<Type>,
    pub size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Slice {
    pub ty: Box<Type>,
    pub size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub inner: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: String,
    pub ty: Box<Type>,
    pub visibility: Visibility,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    PublicCrate,
    Private,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Lambda {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret: Box<Type>,
}

// ────────────────────────────────────────────────────────────────────────────────
// Type Implementations
// ────────────────────────────────────────────────────────────────────────────────

const INTEGER_PARAMS: [(u8, bool); 10] = [
    (1, false),
    (8, false),
    (16, false),
    (32, false),
    (64, false),
    (128, false),
    (8, true),
    (16, true),
    (32, true),
    (64, true),
];

enum WorkItem {
    Generate {
        slot: usize,
        depth: usize,
        allow_slice: bool,
        allow_lambda: bool,
        is_entrypoint: bool,
    },
    FinalizeArray {
        slot: usize,
        inner: usize,
        size: usize,
    },
    FinalizeSlice {
        slot: usize,
        inner: usize,
        size: usize,
    },
    FinalizeTuple {
        slot: usize,
        inners: Vec<usize>,
    },
    FinalizeLambda {
        slot: usize,
        inners: Vec<usize>,
        ret: usize,
    },
}

impl Type {
    // My first approach was to use recursion like `Type::random() -> Array -> Type::random() ->
    // ...` The problem is this made the stack implode when generating deeply nested types
    // (e.g., `[[[Field; 3]; 2]; ...]`)
    //
    // ## The Solution
    // Instead of recursion, we use a work queue on the heap, by storing the type of work to do in a
    // `VecDeque<WorkItem>` and traversing it iteratively, like a to-do list. The generation flow is
    // as follows:
    //
    // - `GenerateType`: Decides what type to create. Primitives go directly into their slot.
    //   Compound types reserve slots for children and push both `GenerateType` (for children) and
    //   `Finalize*` (to assemble)
    // - `Finalize*`: Takes completed children from their slots and assembles the parent type
    //
    // Initial:  slots=[None], work=[Generate(0)]
    // Step 1:   Generate(0) -> Array chosen -> slots=[None, None]
    //           work=[Generate(1), FinalizeArray(0, inner=1)]
    // Step 2:   Generate(1) -> Tuple chosen -> slots=[None, None, None, None]
    //           work=[Generate(2), Generate(3), FinalizeTuple(1), FinalizeArray(0)]
    // Step 3:   Generate(2) -> Field -> slots=[None, None, Some(Field), None]
    // Step 4:   Generate(3) -> bool  -> slots=[None, None, Some(Field), Some(bool)]
    // Step 5:   FinalizeTuple(1)     -> slots=[None, Some(Tuple), ...]
    // Step 6:   FinalizeArray(0)     -> slots=[Some(Array<Tuple>), ...]
    // Done!     Return slots[0]
    //
    // If you do not understand something, just ask claude :D
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        is_entrypoint: bool,
        allow_lambda: bool,
        allow_slice: bool,
    ) -> Self {
        let mut slots: Vec<Option<Type>> = vec![None];
        let mut work: VecDeque<WorkItem> = VecDeque::new();
        work.push_back(WorkItem::Generate {
            slot: 0,
            depth: 0,
            allow_slice,
            is_entrypoint,
            allow_lambda,
        });

        while let Some(item) = work.pop_front() {
            match item {
                WorkItem::Generate { slot, depth, allow_slice, is_entrypoint, allow_lambda } => {
                    Self::process_generate(
                        random,
                        ctx,
                        scope,
                        &mut slots,
                        &mut work,
                        slot,
                        depth,
                        allow_slice,
                        is_entrypoint,
                        allow_lambda,
                    );
                }
                WorkItem::FinalizeArray { slot, inner, size } => {
                    let ty = slots[inner].take().unwrap();
                    slots[slot] = Some(Type::Array(Array { ty: Box::new(ty), size }));
                }
                WorkItem::FinalizeSlice { slot, inner, size } => {
                    let ty = slots[inner].take().unwrap();
                    slots[slot] = Some(Type::Slice(Slice { ty: Box::new(ty), size }));
                }
                WorkItem::FinalizeTuple { slot, inners } => {
                    let ty = inners.iter().map(|&i| slots[i].take().unwrap()).collect();
                    slots[slot] = Some(Type::Tuple(Tuple { inner: ty }));
                }
                WorkItem::FinalizeLambda { slot, inners, ret } => {
                    let args: Vec<_> = inners.iter().map(|&i| slots[i].take().unwrap()).collect();
                    let names: Vec<_> = (0..args.len()).map(|i| format!("param_{}", i)).collect();
                    let params = names
                        .iter()
                        .zip(args.iter())
                        .map(|(name, arg)| (name.clone(), arg.clone()))
                        .collect();
                    let ret = slots[ret].take().unwrap();
                    slots[slot] = Some(Type::Lambda(Lambda {
                        name: format!(
                            "lambda_{}",
                            random_string(random, ctx.max_name_characters_count)
                        ),
                        params,
                        ret: Box::new(ret),
                    }));
                }
            }
        }

        slots[0].take().unwrap()
    }

    fn process_generate(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        slots: &mut Vec<Option<Type>>,
        work: &mut VecDeque<WorkItem>,
        slot: usize,
        depth: usize,
        allow_slice: bool,
        is_entrypoint: bool,
        allow_lambda: bool,
    ) {
        // To avoid infinite recursion, we limit the depth of generated types. If we reach the max
        // depth, the generated type will be `Field`
        if depth > ctx.max_type_depth {
            slots[slot] = Some(Type::Field(Field {}));
            return;
        }

        // Build list of available types
        let valid_structs: Vec<_> = if is_entrypoint {
            // This branch is triggered when creating the types for the main function definition.
            scope
                .structs
                .iter()
                .filter(|s| s.fields.iter().all(|f| f.ty.is_valid_public_input()))
                .collect()
        } else if !allow_slice {
            // Filter out structs that contain slices when slices are not allowed
            scope.structs.iter().filter(|s| !s.contains_slice()).collect()
        } else {
            // This branch is the default case, when creating local variables, compound types,
            // etc...
            scope.structs.iter().collect()
        };

        let mut available = vec!["Field", "Integer", "Boolean", "String", "Array", "Tuple", "Null"];
        if allow_slice {
            available.push("Slice");
        }
        if !valid_structs.is_empty() {
            available.push("Struct");
        }
        if depth == 0 && allow_lambda {
            available.push("Lambda");
        }

        // @todo weighted selection of types ??
        match *available.choose(random).unwrap() {
            "Field" => slots[slot] = Some(Type::Field(Field {})),
            "Integer" => slots[slot] = Some(Type::Integer(Integer::random(random))),
            "Boolean" => slots[slot] = Some(Type::Boolean(Boolean {})),
            "String" => slots[slot] = Some(Type::String(StringType::random(random, ctx))),
            "Array" => {
                let inner = slots.len();

                // Reserve a slot for the inner type
                slots.push(None);

                let size = random.random_range(ctx.min_element_count..ctx.max_element_count);

                // So that we assemble the `Array` once we trigger this work item
                work.push_front(WorkItem::FinalizeArray { slot, inner, size });

                // But first, we need to generate the inner type
                work.push_front(WorkItem::Generate {
                    slot: inner,
                    depth: depth + 1,
                    allow_slice: false,
                    is_entrypoint,
                    allow_lambda: false,
                });
            }
            "Slice" => {
                let inner = slots.len();

                // Reserve a slot for the inner type
                slots.push(None);

                let size = random.random_range(ctx.min_element_count..ctx.max_element_count);

                // So that we assemble the `Slice` once we trigger this work item
                work.push_front(WorkItem::FinalizeSlice { slot, inner, size });

                // But first, we need to generate the inner type
                work.push_front(WorkItem::Generate {
                    slot: inner,
                    depth: depth + 1,
                    allow_slice: false,
                    is_entrypoint: false,
                    allow_lambda: false,
                });
            }
            "Tuple" => {
                let min_count = ctx.min_element_count.max(2); // Tuples need at least 2 elements, otherwise they "downcast" to the inner type
                let count =
                    random.random_range(min_count..ctx.max_element_count.max(min_count + 1));

                let first = slots.len();

                // Reserve slots for the inner types
                let inners: Vec<usize> = (0..count).map(|i| first + i).collect();
                slots.extend((0..count).map(|_| None));

                // So that we assemble the `Tuple` once we trigger this work item
                work.push_front(WorkItem::FinalizeTuple { slot, inners: inners.clone() });

                // But first, we need to generate the inner types
                for &i in &inners {
                    work.push_front(WorkItem::Generate {
                        slot: i,
                        depth: depth + 1,
                        allow_slice: false,
                        is_entrypoint,
                        allow_lambda: false,
                    });
                }
            }
            "Struct" => {
                slots[slot] = Some(Type::Struct((*valid_structs.choose(random).unwrap()).clone()));
            }

            "Lambda" => {
                let count = random.random_range(0..ctx.max_function_parameters_count);

                let first = slots.len();

                // Reserve slots for the parameter types
                let inners: Vec<usize> = (0..count).map(|i| first + i).collect();
                slots.extend((0..count).map(|_| None));

                let ret = slots.len();
                slots.push(None);

                // So that we assemble the `Lambda` once we trigger this work item
                work.push_front(WorkItem::FinalizeLambda { slot, inners: inners.clone(), ret });

                // But first, we need to generate the parameter types @todo check whether lambdas
                // can be inside arrays, lambdas, structs...
                for &i in &inners {
                    work.push_front(WorkItem::Generate {
                        slot: i,
                        depth: depth + 1,
                        allow_slice: true,
                        is_entrypoint: false,
                        allow_lambda: false,
                    });
                }

                // And the return type
                work.push_front(WorkItem::Generate {
                    slot: ret,
                    depth: depth + 1,
                    allow_slice: true,
                    is_entrypoint: false,
                    allow_lambda: false,
                });
            }
            "Null" => slots[slot] = Some(Type::Null),
            _ => unreachable!(),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        match self {
            Type::Field(f) => f.random_value(random, ctx),
            Type::Integer(i) => i.random_value(random, ctx),
            Type::Boolean(b) => b.random_value(random),
            Type::String(s) => s.random_value(random),
            Type::Array(a) => a.random_value(random, ctx),
            Type::Slice(s) => s.random_value(random, ctx),
            Type::Tuple(t) => t.random_value(random, ctx),
            Type::Struct(s) => s.random_value(random, ctx),
            Type::Lambda(l) => l.random_value(random, ctx),
            Type::Null => "()".to_string(),
        }
    }

    pub fn is_valid_public_input(&self) -> bool {
        match self {
            Type::Field(_) | Type::Integer(_) | Type::Boolean(_) => true,
            Type::String(s) => s.size > 0,
            Type::Array(a) => a.size > 0 && a.ty.is_valid_public_input(),
            Type::Tuple(t) => {
                !t.inner.is_empty() && t.inner.iter().all(|ty| ty.is_valid_public_input())
            }
            Type::Struct(s) => s.fields.iter().all(|f| f.ty.is_valid_public_input()),
            Type::Slice(_) | Type::Lambda(_) | Type::Null => false,
        }
    }

    /// Returns true if this type contains a slice anywhere (including nested within
    /// structs/arrays/tuples)
    pub fn contains_slice(&self) -> bool {
        match self {
            Type::Field(_) | Type::Integer(_) | Type::Boolean(_) | Type::String(_) | Type::Null => {
                false
            }
            Type::Slice(_) => true,
            Type::Array(a) => a.ty.contains_slice(),
            Type::Tuple(t) => t.inner.iter().any(|ty| ty.contains_slice()),
            Type::Struct(s) => s.fields.iter().any(|f| f.ty.contains_slice()),
            Type::Lambda(l) => {
                l.params.iter().any(|(_, ty)| ty.contains_slice()) || l.ret.contains_slice()
            }
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Primitive Types
// ────────────────────────────────────────────────────────────────────────────────

impl Field {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let exclude_prime = random.random_bool(ctx.exclude_prime_probability);

        random_field_element(
            "bn254",
            random,
            exclude_prime,
            ctx.boundary_value_probability,
            ctx.small_upper_bound_probability,
            ctx.max_small_upper_bound,
        )
        .to_string()
    }
}

impl Integer {
    pub fn random(random: &mut impl Rng) -> Self {
        let (bits, signed) = *INTEGER_PARAMS.choose(random).unwrap();

        Self { bits, signed }

    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let type_name = format!("{}{}", if self.signed { "i" } else { "u" }, self.bits);

        if self.bits == 1 {
            return format!(
                "({} as {})",
                if random.random_bool(0.5) { "1" } else { "0" },
                type_name
            );
        }

        let (value, neg) = if self.signed {
            let half = 1i128 << (self.bits - 1);
            let (min, max) = (-half, half - 1);

            let v = if bernoulli(ctx.boundary_value_probability, random) {
                *[0, 1, -1, min, max].choose(random).unwrap()
            } else {
                random.random_range(min..=max)
            };

            (v.abs() as u128, v < 0)
        } else {
            let max = if self.bits == 128 { u128::MAX } else { (1u128 << self.bits) - 1 };

            let v = if bernoulli(ctx.boundary_value_probability, random) {
                *[0, 1, max].choose(random).unwrap()
            } else {
                random.random_range(0..=max)
            };

            (v, false)
        };

        // This is to avoid Noir complaining about numbers not fitting in the expected type due to
        // shadow casting @todo overkill?
        if neg {
            format!("((-{}) as {})", value, type_name)
        } else {
            format!("({} as {})", value, type_name)
        }
    }
}

impl Boolean {
    pub fn random_value(&self, random: &mut impl Rng) -> String {
        if random.random_bool(0.5) { "true" } else { "false" }.into()
    }
}

impl StringType {
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        let size = random.random_range(ctx.min_string_size..ctx.max_string_size);
        let hash_number = if random.random_bool(ctx.raw_string_probability) {
            random.random_range(0..ctx.max_string_hashes_count)
        } else {
            0
        };

        Self { size, hash_number }
    }

    pub fn random_value(&self, random: &mut impl Rng) -> String {
        let value = random_string(random, self.size);

        if self.hash_number > 0 {
            let h = "#".repeat(self.hash_number);
            format!("r{h}\"{value}\"{h}")
        } else {
            format!("r\"{value}\"")
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Compound Types
// ────────────────────────────────────────────────────────────────────────────────

impl Array {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = (0..self.size).map(|_| self.ty.random_value(random, ctx)).collect();

        format!("[{}]", elems.join(", "))
    }
}

impl Slice {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let size = random.random_range(0..ctx.max_element_count);
        let elems: Vec<_> = (0..size).map(|_| self.ty.random_value(random, ctx)).collect();

        format!("&[{}]", elems.join(", "))
    }
}

impl Tuple {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = self.inner.iter().map(|ty| ty.random_value(random, ctx)).collect();

        format!("({})", elems.join(", "))
    }
}

impl Struct {
    /// Generate a random struct. `previous_structs` contains structs that can be used as field
    /// types (to avoid circular dependencies, struct N can only contain structs 0..N-1).
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        previous_structs: &[Struct],
        allow_slice: bool,
    ) -> Self {
        let name = format!("Struct_{}", random_string(random, ctx.max_name_characters_count));

        // Create a scope with previous structs available for field types
        let mut scope = Scope::new();
        scope.structs = previous_structs.to_vec();

        let fields = (0..ctx.max_struct_fields_count)
            .map(|i| StructField::random(random, ctx, &scope, format!("field_{}", i), allow_slice))
            .collect();

        Self { name, fields }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let fields: Vec<_> = self
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, f.ty.random_value(random, ctx)))
            .collect();

        format!("{} {{ {} }}", self.name, fields.join(", "))
    }

    /// Returns true if any field contains a slice
    pub fn contains_slice(&self) -> bool {
        self.fields.iter().any(|f| f.ty.contains_slice())
    }
}

impl StructField {
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        name: String,
        allow_slice: bool,
    ) -> Self {
        // Allow lambdas in struct fields
        let ty = Box::new(Type::random(
            random,
            ctx,
            scope,
            false,
            true, // allow_lambda for struct fields
            allow_slice,
        ));
        let visibility = *[Visibility::Public, Visibility::Private, Visibility::PublicCrate]
            .choose(random)
            .unwrap();

        Self { name: name.clone(), ty, visibility }
    }
}

impl Lambda {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let params: Vec<_> =
            self.params.iter().map(|(name, ty)| format!("{}: {}", name, ty)).collect();
        let body = if matches!(*self.ret, Type::Null) {
            String::new()
        } else {
            self.ret.random_value(random, ctx)
        };
        // Format: |param: Type| -> RetType { body }
        format!("|{}| -> {} {{ {} }}", params.join(", "), self.ret, body)
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Field(_) => write!(f, "Field"),
            Type::Integer(i) => write!(f, "{}{}", if i.signed { "i" } else { "u" }, i.bits),
            Type::Boolean(_) => write!(f, "bool"),
            Type::String(s) => write!(f, "str<{}>", s.size),
            Type::Array(a) => write!(f, "[{}; {}]", a.ty, a.size),
            Type::Slice(s) => write!(f, "[{}]", s.ty),
            Type::Tuple(t) => {
                let types: Vec<_> = t.inner.iter().map(ToString::to_string).collect();
                write!(f, "({})", types.join(", "))
            }
            Type::Struct(s) => write!(f, "{}", s.name),
            Type::Lambda(l) => {
                // Type signature format: fn (Type1, Type2) -> RetType
                let param_types: Vec<_> = l.params.iter().map(|(_, ty)| ty.to_string()).collect();
                write!(f, "fn ({}) -> {}", param_types.join(", "), l.ret)
            }
            Type::Null => write!(f, "()"),
        }
    }
}

impl std::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => write!(f, "pub "),
            Visibility::PublicCrate => write!(f, "pub(crate) "),
            Visibility::Private => write!(f, ""),
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Tests
// ────────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::{Write, BufWriter};

    #[test]
    fn test_type_random() {
        let random = &mut rand::rng();
        let ctx = Context {
            max_inputs_count: 5,
            min_element_count: 1,
            max_element_count: 5,
            min_string_size: 1,
            max_string_size: 10,
            max_expression_depth: 3,
            max_type_depth: 3,
            max_structs_count: 5,
            max_struct_fields_count: 10,
            max_globals_count: 5,
            max_functions_count: 5,
            max_function_parameters_count: 5,
            max_function_return_types_count: 5,
            max_main_expressions_count: 25,
            max_block_expressions_count: 5,
            max_string_hashes_count: 5,
            max_name_characters_count: 5,
            max_small_upper_bound: 5,
            integer_signed_probability: 0.33,
            boundary_value_probability: 0.25,
            small_upper_bound_probability: 0.2,
            exclude_prime_probability: 0.1,
            mutable_probability: 0.4,
            raw_string_probability: 0.2,
            type_depth: 0,
            expression_depth: 0,
            new_variable_probability: 0.5,
            max_if_else_branch_count: 4,
        };

        let mut structs: Vec<Struct> = Vec::new();
        for _ in 0..5 {
            let s = Struct::random(random, &ctx, &structs, false); // allow_slice = false
            structs.push(s);
        }

        let mut scope = Scope::with_variables(vec![], vec![]);
        scope.structs = structs;

        let mut file = BufWriter::new(File::create("../a/src/main.nr").expect("Unable to open file"));

        for s in scope.structs.iter() {
            writeln!(
                file,
                "struct {} {{\n{}\n}}\n",
                s.name,
                s.fields
                    .iter()
                    .map(|f| format!("    {}{}: {},", f.visibility, f.name, f.ty))
                    .collect::<Vec<_>>()
                    .join("\n")
            ).unwrap();
        }

        writeln!(file, "fn main() {{\n").unwrap();
        for i in 0..25 {
            let ty = Type::random(random, &ctx, &scope, false, true, true);
            if matches!(ty, Type::Lambda(_)) {
                writeln!(file, "    let v{} = {};", i, ty.random_value(random, &ctx)).unwrap();
            } else {
                writeln!(file, "    let v{}: {} = {};", i, ty, ty.random_value(random, &ctx)).unwrap();
            }
        }
        writeln!(file, "}}").unwrap();
    }
}

