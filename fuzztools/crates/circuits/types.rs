use super::context::Context;
use crate::{
    math::{bernoulli, random_field_element},
    utils::{random_string, RandomChoice},
};
use rand::Rng;
use std::collections::VecDeque;

// ------------------------------------------------------------
// Type Definitions
// ------------------------------------------------------------

#[derive(Clone)]
pub enum Type {
    Field(Field),
    Integer(Integer),
    Boolean(Boolean),
    String(StringType),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Struct(Struct),
    Reference(Reference),
    // @todo lambdas, maybe use them in expressions instead?
}

#[derive(Clone)]
pub struct Field {
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Integer {
    pub bits: u8,
    pub signed: bool,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Boolean {
    pub mutable: bool,
}

#[derive(Clone)]
pub struct StringType {
    pub size: usize,
    pub hashes: usize,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Array {
    pub ty: Box<Type>,
    pub size: usize,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Slice {
    pub ty: Box<Type>,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Tuple {
    pub inner: Vec<Type>,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Box<Type>,
    pub visibility: Visibility,
}

#[derive(Clone, Copy)]
pub enum Visibility {
    Public,
    PublicCrate,
    Private,
}

#[derive(Clone)]
pub struct Reference {
    pub ty: Box<Type>,
}

// ------------------------------------------------------------
// Work Items
// ------------------------------------------------------------

pub enum WorkItem {
    GenerateType { slot_idx: usize, depth: usize },
    FinalizeArray { slot_idx: usize, inner_slot: usize, size: usize, mutable: bool },
    FinalizeSlice { slot_idx: usize, inner_slot: usize, mutable: bool },
    FinalizeTuple { slot_idx: usize, inner_slots: Vec<usize>, mutable: bool },
    FinalizeReference { slot_idx: usize, inner_slot: usize },
}

// ------------------------------------------------------------
// Methods Lists
// ------------------------------------------------------------

const FIELD_METHODS: &[&str] = &[
    "to_le_bits",
    "to_be_bits",
    "to_le_bytes",
    "to_be_bytes",
    "pow_32",
    "assert_max_bit_size",
    "sgn0",
    "lt",
    "wrapping_add",
    "wrapping_sub",
    "wrapping_mul",
];

const INTEGER_METHODS: &[&str] = &["wrapping_add", "wrapping_sub", "wrapping_mul"];

const STRING_METHODS: &[&str] = &["as_bytes"];

const ARRAY_METHODS: &[&str] = &[
    "all",
    "any",
    "as_slice",
    "concat",
    "fold",
    "for_each",
    "for_eachi",
    "len",
    "map",
    "mapi",
    "reduce",
    "sort",
    "sort_via",
    "cmp",
    "eq",
];

const SLICE_METHODS: &[&str] = &[
    "all",
    "any",
    "append",
    "as_array",
    "filter",
    "fold",
    "for_each",
    "for_eachi",
    "insert",
    "join",
    "len",
    "map",
    "mapi",
    "pop_back",
    "pop_front",
    "push_back",
    "push_front",
    "reduce",
    "remove",
    "cmp",
    "eq",
    "sort",
    "sort_via",
    "reverse",
];

const TUPLE_METHODS: &[&str] = &["cmp", "eq"];

const STRUCT_METHODS: &[&str] = &["cmp", "eq"];

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

const VISIBILITIES: [Visibility; 3] =
    [Visibility::Private, Visibility::PublicCrate, Visibility::Public];

// ------------------------------------------------------------
// Type Implementation
// ------------------------------------------------------------

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
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        // Slot 0 will hold our final result; we grow as needed for nested types
        let mut slots: Vec<Option<Type>> = vec![None];
        let mut work: VecDeque<WorkItem> = VecDeque::new();
        work.push_back(WorkItem::GenerateType { slot_idx: 0, depth: 0 });

        while let Some(item) = work.pop_front() {
            match item {
                WorkItem::GenerateType { slot_idx, depth } => {
                    // Base case: max depth reached -> generate primitive to stop nesting
                    if depth > ctx.max_type_depth {
                        slots[slot_idx] = Some(Type::Field(Field::random(random, ctx)));
                        continue;
                    }

                    // Not all structs can be used as public inputs, so we filter them
                    let valid_structs: Vec<&Struct> = if ctx.filter_public_input_structs {
                        structs
                            .iter()
                            .filter(|s| s.fields.iter().all(|f| f.ty.is_valid_public_input()))
                            .collect()
                    } else {
                        structs.iter().collect()
                    };

                    let mut available: Vec<&str> =
                        vec!["Field", "Integer", "Boolean", "String", "Array", "Tuple"];
                    if ctx.allow_slices {
                        available.push("Slice");
                    }
                    if ctx.allow_references {
                        available.push("Reference");
                    }
                    if ctx.allow_structs && !valid_structs.is_empty() {
                        available.push("Struct");
                    }

                    match *random.choice(&available) {
                        // Primitives: no children, fill slot immediately
                        "Field" => {
                            slots[slot_idx] = Some(Type::Field(Field::random(random, ctx)));
                        }
                        "Integer" => {
                            slots[slot_idx] = Some(Type::Integer(Integer::random(random, ctx)));
                        }
                        "Boolean" => {
                            slots[slot_idx] = Some(Type::Boolean(Boolean::random(random, ctx)));
                        }
                        "String" => {
                            slots[slot_idx] = Some(Type::String(StringType::random(random, ctx)));
                        }

                        // Array: has ONE child type
                        // Reserve a slot for inner type, schedule its generation, then finalize
                        "Array" => {
                            let inner_slot = slots.len();
                            slots.push(None); // Reserve slot for inner type

                            let size =
                                random.random_range(ctx.min_element_count..ctx.max_element_count);
                            let mutable = random.random_bool(ctx.mutable_probability);

                            // Generate runs FIRST, Finalize runs AFTER
                            work.push_front(WorkItem::FinalizeArray {
                                slot_idx,
                                inner_slot,
                                size,
                                mutable,
                            });
                            work.push_front(WorkItem::GenerateType {
                                slot_idx: inner_slot,
                                depth: depth + 1,
                            });
                        }

                        // Slice: same pattern as Array
                        "Slice" => {
                            let inner_slot = slots.len();
                            slots.push(None);

                            let mutable = random.random_bool(ctx.mutable_probability);

                            work.push_front(WorkItem::FinalizeSlice {
                                slot_idx,
                                inner_slot,
                                mutable,
                            });
                            work.push_front(WorkItem::GenerateType {
                                slot_idx: inner_slot,
                                depth: depth + 1,
                            });
                        }

                        // Tuple: has N child types (variable count)
                        // Reserve N slots, schedule N generations, then finalize
                        "Tuple" => {
                            let count =
                                random.random_range(ctx.min_element_count..ctx.max_element_count);
                            let mutable = random.random_bool(ctx.mutable_probability);

                            // Reserve contiguous slots for all inner types
                            let first_inner = slots.len();
                            let inner_slots: Vec<usize> =
                                (0..count).map(|i| first_inner + i).collect();
                            for _ in 0..count {
                                slots.push(None);
                            }

                            // Schedule finalization (runs last) then all child generations
                            work.push_front(WorkItem::FinalizeTuple {
                                slot_idx,
                                inner_slots: inner_slots.clone(),
                                mutable,
                            });
                            for &slot in &inner_slots {
                                work.push_front(WorkItem::GenerateType {
                                    slot_idx: slot,
                                    depth: depth + 1,
                                });
                            }
                        }

                        // Struct: pick from existing structs
                        "Struct" => {
                            slots[slot_idx] =
                                Some(Type::Struct((*random.choice(&valid_structs)).clone()));
                        }

                        // Reference: has ONE child type
                        "Reference" => {
                            let inner_slot = slots.len();
                            slots.push(None);

                            work.push_front(WorkItem::FinalizeReference { slot_idx, inner_slot });
                            work.push_front(WorkItem::GenerateType {
                                slot_idx: inner_slot,
                                depth: depth + 1,
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                WorkItem::FinalizeArray { slot_idx, inner_slot, size, mutable } => {
                    let inner = slots[inner_slot].take().expect("inner type should be ready");
                    slots[slot_idx] =
                        Some(Type::Array(Array { ty: Box::new(inner), size, mutable }));
                }

                WorkItem::FinalizeSlice { slot_idx, inner_slot, mutable } => {
                    let inner = slots[inner_slot].take().expect("inner type should be ready");
                    slots[slot_idx] = Some(Type::Slice(Slice { ty: Box::new(inner), mutable }));
                }

                WorkItem::FinalizeTuple { slot_idx, inner_slots, mutable } => {
                    let inner: Vec<Type> = inner_slots
                        .iter()
                        .map(|&s| slots[s].take().expect("inner type should be ready"))
                        .collect();
                    slots[slot_idx] = Some(Type::Tuple(Tuple { inner, mutable }));
                }

                WorkItem::FinalizeReference { slot_idx, inner_slot } => {
                    let inner = slots[inner_slot].take().expect("inner type should be ready");
                    slots[slot_idx] = Some(Type::Reference(Reference { ty: Box::new(inner) }));
                }
            }
        }

        // Slot 0 contains our fully-constructed type
        slots[0].take().unwrap()
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
            Type::Reference(r) => r.random_value(random, ctx),
        }
    }

    pub fn random_method(&self, random: &mut impl Rng) -> Option<&'static str> {
        match self {
            Type::Field(_) => Some(*random.choice(FIELD_METHODS)),
            Type::Integer(_) => Some(*random.choice(INTEGER_METHODS)),
            Type::Boolean(_) => None,
            Type::String(_) => Some(*random.choice(STRING_METHODS)),
            Type::Array(_) => Some(*random.choice(ARRAY_METHODS)),
            Type::Slice(_) => Some(*random.choice(SLICE_METHODS)),
            Type::Tuple(_) => Some(*random.choice(TUPLE_METHODS)),
            Type::Struct(_) => Some(*random.choice(STRUCT_METHODS)),
            Type::Reference(_) => None,
        }
    }

    pub fn is_valid_public_input(&self) -> bool {
        match self {
            Type::Field(_) | Type::Integer(_) | Type::Boolean(_) => true,
            Type::String(s) => s.size > 0,
            Type::Array(a) => a.size > 0 && a.ty.is_valid_public_input(),
            Type::Slice(_) | Type::Reference(_) => false,
            Type::Tuple(t) => {
                !t.inner.is_empty() && t.inner.iter().all(|e| e.is_valid_public_input())
            }
            Type::Struct(s) => s.fields.iter().all(|f| f.ty.is_valid_public_input()),
        }
    }
}

// ------------------------------------------------------------
// Primitive Types
// ------------------------------------------------------------

impl Field {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        Self { mutable: random.random_bool(ctx.mutable_probability) }
    }

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
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        let (bits, signed) = *random.choice(&INTEGER_PARAMS);
        Self { bits, signed, mutable: random.random_bool(ctx.mutable_probability) }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        if self.bits == 1 {
            return if random.random_bool(0.5) { "1" } else { "0" }.into();
        }

        if self.signed {
            let half = 1i128 << (self.bits - 1);
            let (min, max) = (-half, half - 1);

            let value = if bernoulli(ctx.boundary_value_probability, random) {
                *random.choice(&[0, 1, -1, min, max])
            } else {
                random.random_range(min..=max)
            };

            if value < 0 {
                format!("-{}", value.unsigned_abs())
            } else {
                value.to_string()
            }
        } else {
            let max = if self.bits == 128 { u128::MAX } else { (1u128 << self.bits) - 1 };

            let value = if bernoulli(ctx.boundary_value_probability, random) {
                *random.choice(&[0, 1, max])
            } else {
                random.random_range(0..=max)
            };

            value.to_string()
        }
    }
}

impl Boolean {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        Self { mutable: random.random_bool(ctx.mutable_probability) }
    }

    #[inline]
    pub fn random_value(&self, random: &mut impl Rng) -> String {
        if random.random_bool(0.5) { "true" } else { "false" }.into()
    }
}

impl StringType {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        Self {
            size: random.random_range(ctx.min_string_size..ctx.max_string_size),
            hashes: if random.random_bool(ctx.raw_string_probability) {
                random.random_range(0..ctx.max_string_hashes_count)
            } else {
                0
            },
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng) -> String {
        let value = random_string(random, self.size);
        if self.hashes > 0 {
            let hashes = "#".repeat(self.hashes);
            format!("r{hashes}\"{value}\"{hashes}")
        } else {
            format!("r\"{value}\"")
        }
    }
}

// ------------------------------------------------------------
// Compound Types
// ------------------------------------------------------------

impl Array {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner = ctx.inner_type();
        Self {
            ty: Box::new(Type::random(random, &inner, structs)),
            size: random.random_range(ctx.min_element_count..ctx.max_element_count),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = (0..self.size).map(|_| self.ty.random_value(random, ctx)).collect();
        format!("[{}]", elems.join(", "))
    }
}

impl Slice {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner = ctx.inner_type();
        Self {
            ty: Box::new(Type::random(random, &inner, structs)),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let size = random.random_range(0..ctx.max_element_count);
        let elems: Vec<_> = (0..size).map(|_| self.ty.random_value(random, ctx)).collect();
        format!("&[{}]", elems.join(", "))
    }
}

impl Tuple {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner_ctx = ctx.inner_type();
        let size = random.random_range(ctx.min_element_count..ctx.max_element_count);
        Self {
            inner: (0..size).map(|_| Type::random(random, &inner_ctx, structs)).collect(),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = self.inner.iter().map(|e| e.random_value(random, ctx)).collect();
        format!("({})", elems.join(", "))
    }
}

impl Struct {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            fields: (0..ctx.max_struct_fields_count)
                .map(|_| StructField::random(random, &ctx.inner_type(), structs))
                .collect(),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let fields: Vec<_> = self
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, f.ty.random_value(random, ctx)))
            .collect();
        format!("{} {{ {} }}", self.name, fields.join(", "))
    }
}

impl StructField {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            ty: Box::new(Type::random(random, ctx, structs)),
            visibility: *random.choice(&VISIBILITIES),
        }
    }
}

impl Reference {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner = ctx.inner_type();
        Self { ty: Box::new(Type::random(random, &inner, structs)) }
    }

    #[inline]
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        format!("&{}", self.ty.random_value(random, ctx))
    }
}

// ------------------------------------------------------------
// Display
// ------------------------------------------------------------

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Field(_) => f.write_str("Field"),
            Type::Integer(i) => write!(f, "{}{}", if i.signed { "i" } else { "u" }, i.bits),
            Type::Boolean(_) => f.write_str("bool"),
            Type::String(s) => write!(f, "str<{}>", s.size),
            Type::Array(a) => write!(f, "[{}; {}]", a.ty, a.size),
            Type::Slice(s) => write!(f, "[{}]", s.ty),
            Type::Tuple(t) => {
                let types: Vec<_> = t.inner.iter().map(ToString::to_string).collect();
                write!(f, "({})", types.join(", "))
            }
            Type::Struct(s) => f.write_str(&s.name),
            Type::Reference(r) => write!(f, "&{}", r.ty),
        }
    }
}

impl std::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => f.write_str("pub "),
            Visibility::PublicCrate => f.write_str("pub(crate) "),
            Visibility::Private => f.write_str(""),
        }
    }
}
