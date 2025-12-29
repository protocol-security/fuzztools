use rand::{seq::IndexedRandom, Rng};

use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::NodeKind,
        operators::Operator,
        types::{Array, Slice, Struct, Tuple, Type, TypeKind},
    },
    context::Context,
    generators::types::TypeLocation,
    scope::Scope,
};
use petgraph::graph::NodeIndex;

impl Forest {
    /// Generates a random tree based on the given `ctx` and `scope`
    pub fn random(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let count = random.random_range(ctx.min_expression_count..ctx.max_expression_count);

        let mut choices = [
            (NodeKind::Literal, 0),
            (NodeKind::Variable, 0),
            (NodeKind::Operator, 0),
            (NodeKind::Index, 0),
            (NodeKind::TupleIndex, 0),
            (NodeKind::FieldAccess, 0),
        ];

        for _ in 0..count {
            let mut n = 0;

            choices[n] = (NodeKind::Literal, ctx.literal_weight);
            n += 1;

            let has_nodes = self.graph.node_count() > 0;
            if has_nodes {
                choices[n] = (NodeKind::Variable, ctx.variable_weight);
                n += 1;
                choices[n] = (NodeKind::Operator, ctx.operator_weight);
                n += 1;
            }

            let has_array = self.types.contains_key(&TypeKind::Array);
            let has_slice = self.types.contains_key(&TypeKind::Slice);
            let has_tuple = self.types.contains_key(&TypeKind::Tuple);
            let has_struct = self.types.contains_key(&TypeKind::Struct);

            if has_array || has_slice {
                choices[n] = (NodeKind::Index, ctx.index_weight);
                n += 1;
            }
            if has_tuple {
                choices[n] = (NodeKind::TupleIndex, ctx.tuple_index_weight);
                n += 1;
            }
            if has_struct {
                choices[n] = (NodeKind::FieldAccess, ctx.field_access_weight);
                n += 1;
            }

            match choices[..n].choose_weighted(random, |c| c.1).unwrap().0 {
                NodeKind::Literal => self.gen_literal(random, ctx, scope),
                NodeKind::Variable => self.gen_variable(random),
                NodeKind::Operator => self.gen_operator(random, ctx),
                NodeKind::Index => self.gen_index(random),
                NodeKind::TupleIndex => self.gen_tuple_index(random),
                NodeKind::FieldAccess => self.gen_field_access(random),
                _ => unreachable!(),
            }
        }
    }

    fn gen_literal(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let ty = Type::random(random, ctx, scope, TypeLocation::Default);
        let idx = self.literal(ty.random_value(random, ctx), ty.clone());
        self.register(idx, NodeKind::Literal, &ty, None);
    }

    fn gen_variable(&mut self, random: &mut impl Rng) {
        // We do this instead of using `node_indices().collect()choose(random)` because the first
        // one is O(n) and the second one is O(1)
        let source = NodeIndex::new(random.random_range(0..self.graph.node_count()));
        let ty = self.ty(source);
        let var_name = self.next_var();
        let idx = self.variable(var_name, ty.clone(), source);
        self.register(idx, NodeKind::Variable, &ty, None);
    }

    fn gen_operator(&mut self, random: &mut impl Rng, ctx: &Context) {
        // We do this instead of using `node_indices().collect()choose(random)` because the first
        // one is O(n) and the second one is O(1)
        let left = NodeIndex::new(random.random_range(0..self.graph.node_count()));
        let left_ty = self.ty(left);
        let kind = left_ty.kind();

        let (bin, un) = match kind {
            TypeKind::Field => (Operator::binary_field(), Operator::unary_field()),
            TypeKind::Signed => {
                (Operator::binary_integer_signed(), Operator::unary_integer_signed())
            }
            TypeKind::Unsigned => {
                (Operator::binary_integer_unsigned(), Operator::unary_integer_unsigned())
            }
            TypeKind::Boolean => (Operator::binary_boolean(), Operator::unary_boolean()),
            _ => return,
        };

        let peers = self.types.get(&kind);
        if random.random_bool(ctx.unary_probability) || peers.map_or(true, |v| v.len() < 2) {
            let op = *un.choose(random).unwrap();
            let idx = self.operator(op, left, None);
            self.register(idx, NodeKind::Operator, &left_ty, Some(op));
        } else {
            let right = *peers.unwrap().choose(random).unwrap();
            let op = *bin.choose(random).unwrap();
            let idx = self.operator(op, left, Some(right));
            self.register(idx, NodeKind::Operator, &left_ty, Some(op));
        }
    }

    fn gen_index(&mut self, random: &mut impl Rng) {
        let parent = *self
            .types
            .get(&TypeKind::Array)
            .or_else(|| self.types.get(&TypeKind::Slice))
            .unwrap()
            .choose(random)
            .unwrap();

        let (inner, size) = match self.ty(parent) {
            Type::Array(Array { ty, size, .. }) | Type::Slice(Slice { ty, size, .. }) => {
                ((*ty).clone(), size)
            }
            _ => unreachable!(),
        };

        let idx = self.index(parent, random.random_range(0..size));
        self.register(idx, NodeKind::Index, &inner, None);
    }

    fn gen_tuple_index(&mut self, random: &mut impl Rng) {
        let parent = *self.types.get(&TypeKind::Tuple).unwrap().choose(random).unwrap();
        let elements = match self.ty(parent) {
            Type::Tuple(Tuple { elements, .. }) => elements,
            _ => unreachable!(),
        };

        let i = random.random_range(0..elements.len());
        let idx = self.tuple_index(parent, i);
        self.register(idx, NodeKind::TupleIndex, &elements[i], None);
    }

    fn gen_field_access(&mut self, random: &mut impl Rng) {
        let parent = *self.types.get(&TypeKind::Struct).unwrap().choose(random).unwrap();
        let fields = match self.ty(parent) {
            Type::Struct(Struct { fields, .. }) => fields,
            _ => unreachable!(),
        };

        let field = fields.choose(random).unwrap();
        let idx = self.field_access(parent, field.name.clone());
        self.register(idx, NodeKind::FieldAccess, &field.ty, None);
    }
}
