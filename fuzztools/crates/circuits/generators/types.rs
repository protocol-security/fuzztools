use crate::circuits::{
    ast::types::{
        Array, Integer, Lambda, Slice, StringType, Struct, StructField, Tuple, Type, TypeKind,
        Visibility,
    },
    context::Context,
    scope::Scope,
    utils::biased_weight,
};
use rand::{seq::IndexedRandom, Rng};
use std::collections::VecDeque;

// ────────────────────────────────────────────────────────────────────────────────
// Type generation
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeLocation {
    Main,
    Nested,
    TupleElement,
    TupleNested,
    Default,
}

enum WorkItem {
    Generate { slot: usize, depth: usize, location: TypeLocation },
    FinalizeArray { slot: usize, inner: usize, size: usize },
    FinalizeSlice { slot: usize, inner: usize, size: usize },
    FinalizeTuple { slot: usize, inners: Vec<usize> },
    FinalizeLambda { slot: usize, inners: Vec<usize>, ret: usize },
}

impl Type {
    /// My first approach was to use recursion like `Type::random() -> Array -> Type::random() ->
    /// ...` The problem is this made the stack implode when generating deeply nested types
    /// (e.g., `[[[Field; 3]; 2]; ...]`)
    ///
    /// ## The Solution
    /// Instead of recursion, we use a work queue on the heap, by storing the type of work to do in
    /// a `VecDeque<WorkItem>` and traversing it iteratively, like a to-do list. The generation
    /// flow is as follows:
    ///
    /// - `GenerateType`: Decides what type to create. Primitives go directly into their slot.
    ///   Compound types reserve slots for children and push both `GenerateType` (for children) and
    ///   `Finalize*` (to assemble)
    /// - `Finalize*`: Takes completed children from their slots and assembles the parent type
    ///
    /// - Initial:  slots=[None], work=[Generate(0)]
    /// - Step 1: Generate(0) -> Array chosen -> slots=[None, None] work=[Generate(1),
    ///   FinalizeArray(0, inner=1)]
    /// - Step 2: Generate(1) -> Tuple chosen -> slots=[None, None, None, None] work=[Generate(2),
    ///   Generate(3), FinalizeTuple(1), FinalizeArray(0)]
    /// - Step 3: Generate(2) -> Field -> slots=[None, None, Some(Field), None]
    /// - Step 4: Generate(3) -> bool -> slots=[None, None, Some(Field), Some(bool)]
    /// - Step 5: FinalizeTuple(1) -> slots=[None, Some(Tuple), ...]
    /// - Step 6: FinalizeArray(0) -> slots=[Some(Array<Tuple>), ...]
    ///
    /// - Done! Return slots[0]
    ///
    /// If you do not understand something, just ask claude :D
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        location: TypeLocation,
    ) -> Self {
        let mut slots: Vec<Option<Type>> = vec![None];
        let mut work: VecDeque<WorkItem> = VecDeque::new();
        work.push_back(WorkItem::Generate { slot: 0, depth: 0, location });

        while let Some(item) = work.pop_front() {
            match item {
                WorkItem::Generate { slot, depth, location } => {
                    Self::process_generate(
                        random, ctx, scope, &mut slots, &mut work, slot, depth, location,
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
                    let elements = inners.iter().map(|&i| slots[i].take().unwrap()).collect();
                    slots[slot] = Some(Type::Tuple(Tuple { elements }));
                }
                WorkItem::FinalizeLambda { slot, inners, ret } => {
                    let ret = slots[ret].take().unwrap();
                    let params = inners
                        .into_iter()
                        .enumerate()
                        .map(|(i, slot)| (format!("p{}", i), slots[slot].take().unwrap()))
                        .collect();

                    slots[slot] = Some(Type::Lambda(Lambda { params, ret: Box::new(ret) }));
                }
            }
        }

        slots[0].take().unwrap()
    }

    #[allow(clippy::too_many_arguments)]
    fn process_generate(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        slots: &mut Vec<Option<Type>>,
        work: &mut VecDeque<WorkItem>,
        slot: usize,
        depth: usize,
        location: TypeLocation,
    ) {
        // To avoid infinite recursion, we limit the depth of generated types. If we reach the max
        // depth, the generated type will be `Field`
        if depth >= ctx.max_type_depth {
            slots[slot] = Some(Type::Field);
            return;
        }

        let valid_structs: Vec<_> = scope
            .structs
            .iter()
            .filter(|s| match location {
                TypeLocation::Main => s.fields.iter().all(|f| f.ty.is_valid_public_input()),
                TypeLocation::Nested | TypeLocation::TupleNested => {
                    !s.fields.iter().any(|f| f.ty.has_slice())
                }
                _ => true,
            })
            .collect();

        let (bias, mult) = (
            &scope.type_bias,
            if scope.type_bias.is_empty() { 1 } else { ctx.type_bias_multiplier },
        );
        let w = |k, weight| (k, biased_weight(k, weight, bias, mult));

        let mut options = vec![
            w(TypeKind::Field, ctx.field_weight),
            w(TypeKind::Unsigned, ctx.unsigned_weight),
            w(TypeKind::Signed, ctx.signed_weight),
            w(TypeKind::Boolean, ctx.boolean_weight),
            w(TypeKind::String, ctx.string_weight),
            w(TypeKind::Array, ctx.array_weight),
            w(TypeKind::Tuple, ctx.tuple_weight),
        ];

        if location != TypeLocation::Main {
            options.push(w(TypeKind::Empty, ctx.empty_weight));
        }
        if location.allows_slice() {
            options.push(w(TypeKind::Slice, ctx.slice_weight));
        }
        if location.allows_lambda(scope, ctx) {
            options.push(w(TypeKind::Lambda, ctx.lambda_weight));
        }
        if !valid_structs.is_empty() {
            options.push(w(TypeKind::Struct, ctx.struct_weight));
        }

        match options.choose_weighted(random, |i| i.1).unwrap().0 {
            TypeKind::Field => slots[slot] = Some(Type::Field),
            TypeKind::Boolean => slots[slot] = Some(Type::Boolean),
            TypeKind::Empty => slots[slot] = Some(Type::Empty),
            TypeKind::String => {
                slots[slot] = Some(Type::String(StringType::random(random, ctx, location)))
            }
            TypeKind::Struct => {
                slots[slot] = Some(Type::Struct((*valid_structs.choose(random).unwrap()).clone()))
            }

            kind @ (TypeKind::Unsigned | TypeKind::Signed) => {
                slots[slot] =
                    Some(Type::Integer(Integer::random(random, kind == TypeKind::Signed)));
            }

            kind @ (TypeKind::Array | TypeKind::Slice) => {
                let inner = slots.len();

                // Reserve a slot for the inner type
                slots.push(None);

                let min = if location == TypeLocation::Main { 1 } else { ctx.min_element_count };
                let size = random.random_range(min..=ctx.max_element_count);

                // So that we assemble the `Array`/`Slice` once we trigger this work item
                work.push_front(match kind {
                    TypeKind::Array => WorkItem::FinalizeArray { slot, inner, size },
                    TypeKind::Slice => WorkItem::FinalizeSlice { slot, inner, size },
                    _ => unreachable!(),
                });

                // But first, we need to generate the inner type
                work.push_front(WorkItem::Generate {
                    slot: inner,
                    depth: depth + 1,
                    location: location.into_nested(),
                });
            }

            TypeKind::Tuple => {
                // Tuples need at least 2 elements, otherwise they "downcast" to the inner type
                let min = ctx.min_element_count.max(2);
                let count = random.random_range(min..=ctx.max_element_count.max(min + 1));

                let start = slots.len();

                // Reserve slots for the inner types
                slots.resize(start + count, None);
                let inners: Vec<_> = (start..start + count).collect();

                // So that we assemble the `Tuple` once we trigger this work item
                work.push_front(WorkItem::FinalizeTuple { slot, inners: inners.clone() });

                // But first, we need to generate the inner types
                for i in inners {
                    work.push_front(WorkItem::Generate {
                        slot: i,
                        depth: depth + 1,
                        location: location.into_tuple(),
                    });
                }
            }

            TypeKind::Lambda => {
                let count = random.random_range(0..=ctx.max_function_parameters_count);

                let start = slots.len();

                // Reserve slots for the parameter types and the return type
                // @todo all lambdas are guaranteed to this? coverage?
                slots.resize(start + count + 1, None);
                let inners: Vec<_> = (start..start + count).collect();
                let ret = start + count;

                // So that we assemble the `Lambda` once we trigger this work item
                work.push_front(WorkItem::FinalizeLambda { slot, inners: inners.clone(), ret });

                // But first, we need to generate the parameter types
                for i in inners {
                    work.push_front(WorkItem::Generate {
                        slot: i,
                        depth: depth + 1,
                        location: TypeLocation::Default,
                    });
                }

                // And the return type
                work.push_front(WorkItem::Generate {
                    slot: ret,
                    depth: depth + 1,
                    location: TypeLocation::Default,
                });
            }
        }
    }
}

impl Integer {
    const SIGNED_BITS: [u32; 4] = [8, 16, 32, 64];
    const UNSIGNED_BITS: [u32; 6] = [1, 8, 16, 32, 64, 128];

    pub fn random(random: &mut impl Rng, signed: bool) -> Self {
        let bits = if signed {
            *Self::SIGNED_BITS.choose(random).unwrap()
        } else {
            *Self::UNSIGNED_BITS.choose(random).unwrap()
        };

        Self { bits, signed }
    }
}

impl StringType {
    pub fn random(random: &mut impl Rng, ctx: &Context, location: TypeLocation) -> Self {
        let min_size = if location == TypeLocation::Main { 1 } else { ctx.min_string_size };
        let size = random.random_range(min_size..=ctx.max_string_size);
        let is_raw = random.random_bool(ctx.raw_string_probability);

        Self { size, is_raw }
    }
}

impl Struct {
    /// `previous_structs` contains structs that can be used as field
    /// types (to avoid circular dependencies, struct N can only contain structs 0..N-1).
    pub fn random(random: &mut impl Rng, ctx: &Context, scope: &Scope, name: String) -> Self {
        let size = random.random_range(ctx.min_struct_fields_count..=ctx.max_struct_fields_count);
        let fields = (0..size)
            .map(|i| StructField::random(random, ctx, scope, format!("field_{}", i)))
            .collect();

        Self { name, fields }
    }
}

impl StructField {
    const VISIBILITIES: [Visibility; 3] =
        [Visibility::Public, Visibility::Private, Visibility::PublicCrate];

    pub fn random(random: &mut impl Rng, ctx: &Context, scope: &Scope, name: String) -> Self {
        let visibility = *Self::VISIBILITIES.choose(random).unwrap();
        let ty = Box::new(Type::random(random, ctx, scope, TypeLocation::Default));

        Self { name, ty, visibility }
    }
}

impl TypeLocation {
    /// Transition when entering array/slice element type
    #[inline(always)]
    const fn into_nested(self) -> Self {
        match self {
            Self::Main => Self::Main,
            _ => Self::Nested,
        }
    }

    /// Transition when entering tuple element type
    #[inline(always)]
    const fn into_tuple(self) -> Self {
        match self {
            Self::Main => Self::Main,
            Self::Default => Self::TupleElement,
            Self::Nested => Self::TupleNested,
            other => other, // TupleElement/TupleNested stay the same
        }
    }

    #[inline(always)]
    const fn allows_slice(self) -> bool {
        matches!(self, Self::Default | Self::TupleElement)
    }

    #[inline(always)]
    fn allows_lambda(self, scope: &Scope, ctx: &Context) -> bool {
        self != Self::Main && scope.lambda_depth < ctx.max_type_depth
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builders::CircuitBuilder;
    use std::fs;

    #[test]
    fn test_type_generation_default() {
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        for _ in 0..25 {
            let ty = Type::random(&mut random, &ctx, &scope, TypeLocation::Default);
            println!("{}", ty);
        }
    }

    #[test]
    fn test_type_generation_main() {
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        for _ in 0..25 {
            let ty = Type::random(&mut random, &ctx, &scope, TypeLocation::Main);
            println!("{}", ty);
        }
    }

    #[test]
    fn test_type_generation_complex() {
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        for _ in 0..25 {
            let ty = Type::random(&mut random, &ctx, &scope, TypeLocation::Nested);
            println!("{}", ty);
        }

        println!("\n");

        for _ in 0..25 {
            let ty = Type::random(&mut random, &ctx, &scope, TypeLocation::TupleElement);
            println!("{}", ty);
        }

        println!("\n");

        for _ in 0..25 {
            let ty = Type::random(&mut random, &ctx, &scope, TypeLocation::TupleNested);
            println!("{}", ty);
        }
    }

    #[test]
    fn test_random_value() {
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        for _ in 0..50 {
            let ty = Type::random(&mut random, &ctx, &scope, TypeLocation::Default);
            let value = ty.random_value(&mut random, &ctx, &scope);
            println!("    {}: {}", ty, value);
        }
    }
}
