use crate::circuits::{
    context::Context,
    ir::{Array, Integer, Slice, Struct, StructField, Tuple, Type, TypeKind},
};
use rand::{seq::IndexedRandom, Rng};
use std::collections::VecDeque;

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
    ///   `Finalize*` (to assemble).
    /// - `Finalize*`: Takes completed children from their slots and assembles the parent type.
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
    /// - Done! Return slots[0].
    ///
    /// If you do not understand something, just ask claude :D.
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        available_structs: &[Struct],
        location: TypeLocation,
    ) -> Self {
        let mut slots: Vec<Option<Type>> = vec![None];
        let mut work: VecDeque<WorkItem> = VecDeque::new();
        work.push_back(WorkItem::Generate { slot: 0, depth: 0, location });

        while let Some(item) = work.pop_front() {
            match item {
                WorkItem::Generate { slot, depth, location } => {
                    Self::process_generate(
                        random,
                        ctx,
                        available_structs,
                        &mut slots,
                        &mut work,
                        slot,
                        depth,
                        location,
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
            }
        }

        slots[0].take().unwrap()
    }

    #[allow(clippy::too_many_arguments)]
    fn process_generate(
        random: &mut impl Rng,
        ctx: &Context,
        available_structs: &[Struct],
        slots: &mut Vec<Option<Type>>,
        work: &mut VecDeque<WorkItem>,
        slot: usize,
        depth: usize,
        location: TypeLocation,
    ) {
        if depth >= ctx.max_type_depth {
            slots[slot] = Some(Type::Field);
            return;
        }

        // Filter available structs based on location constraints
        let valid_structs: Vec<_> = available_structs
            .iter()
            .filter(|s| match location {
                TypeLocation::Main => s.fields.iter().all(|f| f.ty.is_valid_public_input()),
                TypeLocation::Nested | TypeLocation::TupleNested => {
                    !s.fields.iter().any(|f| f.ty.has_slice())
                }
                _ => true,
            })
            .collect();

        let mut options = vec![
            (TypeKind::Field, ctx.field_weight),
            (TypeKind::Unsigned, ctx.unsigned_weight),
            (TypeKind::Signed, ctx.signed_weight),
            (TypeKind::Bool, ctx.bool_weight),
            (TypeKind::Array, ctx.array_weight),
            (TypeKind::Tuple, ctx.tuple_weight),
        ];

        if location.allows_slice() {
            options.push((TypeKind::Slice, ctx.slice_weight));
        }

        if !valid_structs.is_empty() {
            options.push((TypeKind::Struct, ctx.struct_weight));
        }

        match options.choose_weighted(random, |i| i.1).unwrap().0 {
            TypeKind::Field => slots[slot] = Some(Type::Field),
            TypeKind::Bool => slots[slot] = Some(Type::Bool),
            TypeKind::Signed => slots[slot] = Some(Type::Integer(Integer::random(random, true))),
            TypeKind::Unsigned => slots[slot] = Some(Type::Integer(Integer::random(random, false))),

            TypeKind::Struct => {
                let chosen = (*valid_structs.choose(random).unwrap()).clone();
                slots[slot] = Some(Type::Struct(chosen));
            }

            kind @ (TypeKind::Array | TypeKind::Slice) => {
                let inner = slots.len();
                slots.push(None);

                let min = if location == TypeLocation::Main { 1 } else { ctx.min_element_count };
                let size = random.random_range(min..=ctx.max_element_count);

                work.push_front(match kind {
                    TypeKind::Array => WorkItem::FinalizeArray { slot, inner, size },
                    TypeKind::Slice => WorkItem::FinalizeSlice { slot, inner, size },
                    _ => unreachable!(),
                });

                work.push_front(WorkItem::Generate {
                    slot: inner,
                    depth: depth + 1,
                    location: location.into_nested(),
                });
            }

            TypeKind::Tuple => {
                let min = ctx.min_element_count.max(2);
                let max = ctx.max_element_count.max(min);
                let count = random.random_range(min..=max);

                let start = slots.len();
                slots.resize(start + count, None);
                let inners: Vec<_> = (start..start + count).collect();

                work.push_front(WorkItem::FinalizeTuple { slot, inners: inners.clone() });

                for i in inners {
                    work.push_front(WorkItem::Generate {
                        slot: i,
                        depth: depth + 1,
                        location: location.into_tuple(),
                    });
                }
            }
        }
    }
}

impl Struct {
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        previous_structs: &[Struct],
        name: String,
    ) -> Self {
        let count = random.random_range(ctx.min_struct_fields_count..=ctx.max_struct_fields_count);
        let fields = (0..count)
            .map(|i| StructField::random(random, ctx, previous_structs, format!("field{i}")))
            .collect();

        Self { name, fields }
    }
}

impl StructField {
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        available_structs: &[Struct],
        name: String,
    ) -> Self {
        let ty = Type::random(random, ctx, available_structs, TypeLocation::Default);
        Self { name, ty }
    }
}

impl TypeLocation {
    #[inline(always)]
    const fn into_nested(self) -> Self {
        match self {
            Self::Main => Self::Main,
            _ => Self::Nested,
        }
    }

    #[inline(always)]
    const fn into_tuple(self) -> Self {
        match self {
            Self::Main => Self::Main,
            Self::Default => Self::TupleElement,
            Self::Nested => Self::TupleNested,
            other => other,
        }
    }

    #[inline(always)]
    const fn allows_slice(self) -> bool {
        matches!(self, Self::Default | Self::TupleElement)
    }
}
