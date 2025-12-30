use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::{Node, NodeKind},
        operators::Operator,
        types::{Array, Slice, Struct, Tuple, Type, TypeKind},
    },
    context::Context,
    generators::types::TypeLocation,
    scope::Scope,
};
use petgraph::graph::NodeIndex;
use rand::{seq::IndexedRandom, Rng};
use std::collections::HashSet;

// ────────────────────────────────────────────────────────────────────────────────
// Forest generation
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub fn random(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        // Pre-compute function callables (they don't change during forest generation)
        let functions: Vec<(String, Vec<Type>, Type)> = scope
            .functions
            .iter()
            .map(|f| {
                (
                    f.name.clone(),
                    f.params.iter().map(|(_, ty)| ty.clone()).collect(),
                    f.ret.clone().unwrap_or(Type::Empty),
                )
            })
            .collect();

        for _ in 0..random.random_range(ctx.min_expression_count..ctx.max_expression_count) {
            let has_nodes = self.graph.node_count() > 0;
            let mut choices = vec![(NodeKind::Literal, ctx.literal_weight)];

            if has_nodes {
                choices.push((NodeKind::Variable, ctx.variable_weight));
                choices.push((NodeKind::Operator, ctx.operator_weight));
            }
            if self.type_kinds.contains_key(&TypeKind::Array) ||
                self.type_kinds.contains_key(&TypeKind::Slice)
            {
                choices.push((NodeKind::Index, ctx.index_weight));
            }
            if self.type_kinds.contains_key(&TypeKind::Tuple) {
                choices.push((NodeKind::TupleIndex, ctx.tuple_index_weight));
            }
            if self.type_kinds.contains_key(&TypeKind::Struct) {
                choices.push((NodeKind::FieldAccess, ctx.field_access_weight));
            }
            if has_nodes &&
                (!functions.is_empty() || self.type_kinds.contains_key(&TypeKind::Lambda))
            {
                choices.push((NodeKind::Call, ctx.call_weight));
            }

            match choices.choose_weighted(random, |c| c.1).unwrap().0 {
                NodeKind::Literal => self.gen_literal(random, ctx, scope),
                NodeKind::Variable => self.gen_variable(random),
                NodeKind::Operator => self.gen_operator(random, ctx),
                NodeKind::Index => self.gen_index(random),
                NodeKind::TupleIndex => self.gen_tuple_index(random),
                NodeKind::FieldAccess => self.gen_field_access(random),
                NodeKind::Call => self.gen_call(random, &functions),
                _ => {}
            }
        }

        // This is done to avoid the formatter from cropping the forest
        self.assign_leaf_operators();
    }

    fn assign_leaf_operators(&mut self) {
        let assigned: HashSet<_> = self
            .nodes
            .get(&NodeKind::Variable)
            .into_iter()
            .flatten()
            .filter_map(|&v| self.left(v))
            .collect();

        let unassigned: Vec<_> = [
            NodeKind::Literal,
            NodeKind::Operator,
            NodeKind::Index,
            NodeKind::TupleIndex,
            NodeKind::FieldAccess,
            NodeKind::Call,
        ]
        .into_iter()
        .flat_map(|k| self.nodes.get(&k).into_iter().flatten().copied())
        .filter(|n| !assigned.contains(n))
        .collect();

        for idx in unassigned {
            let ty = self.ty(idx);
            let name = self.next_var();
            self.variable(name, ty.clone(), idx);

            // no need to register the variable as it is a post-processing step
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
        let name = self.next_var();
        let idx = self.variable(name, ty.clone(), source);

        self.register(idx, NodeKind::Variable, &ty, None);
    }

    fn gen_operator(&mut self, random: &mut impl Rng, ctx: &Context) {
        // We do this instead of using `node_indices().collect()choose(random)` because the first
        // one is O(n) and the second one is O(1)
        let left = NodeIndex::new(random.random_range(0..self.graph.node_count()));
        let left_ty = self.ty(left);

        let (bin, un) = match left_ty.kind() {
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

        let peers = self.types.get(&left_ty).map(|v| v.as_slice()).unwrap_or(&[]);
        let use_unary = random.random_bool(ctx.unary_probability) || peers.len() < 2;

        if use_unary {
            let op = *un.choose(random).unwrap();
            let idx = self.operator(op, left_ty.clone(), left, None);

            self.register(idx, NodeKind::Operator, &left_ty, Some(op));
        } else {
            let rhs = *peers.choose(random).unwrap();
            let op = *bin.choose(random).unwrap();
            let ret = if op.is_comparison() { Type::Boolean } else { left_ty };
            let idx = self.operator(op, ret.clone(), left, Some(rhs));

            self.register(idx, NodeKind::Operator, &ret, Some(op));
        }
    }

    fn gen_index(&mut self, random: &mut impl Rng) {
        let parent = *self
            .type_kinds
            .get(&TypeKind::Array)
            .or_else(|| self.type_kinds.get(&TypeKind::Slice))
            .and_then(|v| v.choose(random))
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
        let parent = *self.type_kinds.get(&TypeKind::Tuple).and_then(|v| v.choose(random)).unwrap();
        let elements = match self.ty(parent) {
            Type::Tuple(Tuple { elements, .. }) => elements,
            _ => unreachable!(),
        };

        let i = random.random_range(0..elements.len());
        let idx = self.tuple_index(parent, i);
        self.register(idx, NodeKind::TupleIndex, &elements[i], None);
    }

    fn gen_field_access(&mut self, random: &mut impl Rng) {
        let parent =
            *self.type_kinds.get(&TypeKind::Struct).and_then(|v| v.choose(random)).unwrap();
        let fields = match self.ty(parent) {
            Type::Struct(Struct { fields, .. }) => fields,
            _ => unreachable!(),
        };

        let field = fields.choose(random).unwrap();
        let idx = self.field_access(parent, field.name.clone());
        self.register(idx, NodeKind::FieldAccess, &field.ty, None);
    }

    fn gen_call(&mut self, random: &mut impl Rng, functions: &[(String, Vec<Type>, Type)]) {
        let mut callables: Vec<(String, Vec<Type>, Type)> = Vec::new();

        for &lambda_idx in self.type_kinds.get(&TypeKind::Lambda).into_iter().flatten() {
            let Type::Lambda(lambda) = self.ty(lambda_idx) else { continue };

            let name = self
                .nodes
                .get(&NodeKind::Variable)
                .into_iter()
                .flatten()
                .find(|&&var_idx| self.left(var_idx) == Some(lambda_idx))
                .and_then(|&var_idx| match &self.graph[var_idx] {
                    Node::Variable { name, .. } => Some(name.clone()),
                    _ => None,
                });

            // If the lambda variable is not found, skip it
            if name.is_none() {
                continue
            };

            let params: Vec<Type> = lambda.params.iter().map(|(_, ty)| ty.clone()).collect();
            let ret = (*lambda.ret).clone();
            callables.push((name.unwrap(), params, ret));
        }

        // Extend with pre-computed functions
        callables.extend(functions.iter().cloned());

        let Some((name, params, ret)) = callables.choose(random) else { return };
        let Some(args) = params.iter().map(|ty| self.types.get(ty)?.first().copied()).collect()
        else {
            return
        };

        let idx = self.call(name.clone(), ret.clone(), args);
        self.register(idx, NodeKind::Call, ret, None);
    }
}
