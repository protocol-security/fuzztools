use crate::circuits::{
    ast::{Array, Integer, Slice, Tuple},
    Context, Type, TypeKind,
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

pub(crate) enum WorkItem {
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
    pub fn random(random: &mut impl Rng, ctx: &Context, location: TypeLocation) -> Self {
        let mut slots: Vec<Option<Type>> = vec![None];
        let mut work: VecDeque<WorkItem> = VecDeque::new();
        work.push_back(WorkItem::Generate { slot: 0, depth: 0, location });

        while let Some(item) = work.pop_front() {
            match item {
                WorkItem::Generate { slot, depth, location } => {
                    Self::process_generate(
                        random, ctx, &mut slots, &mut work, slot, depth, location,
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

    fn process_generate(
        random: &mut impl Rng,
        ctx: &Context,
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

        let mut options = vec![
            TypeKind::Field,
            TypeKind::Unsigned,
            TypeKind::Signed,
            TypeKind::Bool,
            TypeKind::Array,
            TypeKind::Tuple,
        ];

        if location.allows_slice() {
            options.push(TypeKind::Slice);
        }

        match options.choose(random).unwrap() {
            TypeKind::Field => slots[slot] = Some(Type::Field),
            TypeKind::Bool => slots[slot] = Some(Type::Bool),
            TypeKind::Signed => {
                const SIGNED_BITS: [u32; 4] = [8, 16, 32, 64];
                let bits = *SIGNED_BITS.choose(random).unwrap();

                slots[slot] = Some(Type::Integer(Integer { bits, signed: true }));
            }
            TypeKind::Unsigned => {
                const UNSIGNED_BITS: [u32; 6] = [1, 8, 16, 32, 64, 128];
                let bits = *UNSIGNED_BITS.choose(random).unwrap();

                slots[slot] = Some(Type::Integer(Integer { bits, signed: false }));
            }

            kind @ (TypeKind::Array | TypeKind::Slice) => {
                let inner = slots.len();

                // Reserve a slot for the inner type
                slots.push(None);

                let min = if location == TypeLocation::Main { 1 } else { ctx.min_element_count };
                let size = random.random_range(min..=ctx.max_element_count); // @todo may trigger cannot sample empty range

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
        }
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
}
