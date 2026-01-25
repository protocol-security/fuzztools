use crate::circuits::{
    ast::{
        forest::{Forest, ForestType},
        nodes::{Node, NodeKind},
        operators::Operator,
        types::{Integer, Type, TypeKind},
    },
    context::Context,
    generators::types::TypeLocation,
    scope::Scope,
};
use petgraph::graph::NodeIndex;
use rand::{
    seq::{IndexedRandom, IteratorRandom},
    Rng,
};

// ────────────────────────────────────────────────────────────────────────────────
// Forest generation
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub fn random(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        set_return_expr: bool,
    ) {
        let (min_expr, max_expr) = match scope.forest_type {
            ForestType::Main => (ctx.min_expression_count, ctx.max_expression_count),
            ForestType::Function => (ctx.min_function_body_size, ctx.max_function_body_size),
            ForestType::Lambda => (ctx.min_lambda_body_size, ctx.max_lambda_body_size),
            ForestType::If => (ctx.min_if_body_size, ctx.max_if_body_size),
            ForestType::For => (ctx.min_for_loop_body_size, ctx.max_for_loop_body_size),
        };

        // Pre-compute function callables (they don't change during forest generation)
        /* @todo enable when appropiate
        let functions: Vec<(String, Lambda)> = scope
            .functions
            .iter()
            .map(|f| {
                (f.name.clone(), Lambda { params: f.params.clone(), ret: Box::new(f.ret.clone()) })
            })
            .collect();
        */

        let mut if_count = 0;
        let mut for_count = 0;
        let mut assert_count = 0;
        for _ in 0..random.random_range(min_expr..=max_expr) {
            let has_nodes = self.graph.node_count() > 0;

            // Build the list of possible statement types
            let mut choices: Vec<(StatementKind, usize)> = vec![];

            // Always allow generating an expression (which creates a variable)
            choices.push((StatementKind::Expression, ctx.operator_weight));

            // Allow function/lambda calls if we have callables
            // @todo impl functions
            if has_nodes && !self.lambdas.is_empty() {
                choices.push((StatementKind::Call, ctx.call_weight));
            }

            // Allow assignments if we have mutable variables
            if self.has_mutable_variables() {
                choices.push((StatementKind::Assignment, ctx.assignment_weight));
            }

            // Only allow for loops at top level (depth 0) - no nesting @todo configurable
            if self.has_integer_types() && self.depth < 1 && for_count < ctx.max_for_count {
                choices.push((StatementKind::ForLoop, ctx.for_loop_weight));
            }

            // Only allow ifs at top level (depth 0) - no nesting @todo configurable
            if self.has_boolean_types() && self.depth < 1 && if_count < ctx.max_if_count {
                choices.push((StatementKind::If, ctx.if_weight));
            }

            // Allow no more than `max_assert_count` asserts
            if has_nodes && assert_count < ctx.max_assert_count {
                choices.push((StatementKind::Assert, ctx.assert_weight));
            }

            match choices.choose_weighted(random, |c| c.1).unwrap().0 {
                StatementKind::Expression => self.gen_expression(random, ctx, scope),
                StatementKind::Call => self.gen_call(random),
                StatementKind::Assignment => self.gen_assignment(random, ctx, scope),
                StatementKind::ForLoop => {
                    self.gen_for_loop(random, ctx, scope);
                    for_count += 1;
                }
                StatementKind::If => {
                    self.gen_if(random, ctx, scope);
                    if_count += 1;
                }
                StatementKind::Assert => {
                    self.gen_assert(random, ctx);
                    assert_count += 1;
                }
            }
        }

        if set_return_expr {
            self.set_return_expression(random, ctx, scope);
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Expression generation
    // ────────────────────────────────────────────────────────────────────────────────

    /// Generate a complete expression tree and assign it to a variable
    /// If `scope.type_bias` is set, generation is biased towards those types
    pub fn gen_expression(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let mut ctx = *ctx;
        if self.lambdas.len() >= ctx.max_lambda_count {
            ctx.lambda_weight = 0;
        }

        let ty = self
            .types
            .keys()
            .filter(|ty| scope.type_bias.is_empty() || scope.type_bias.contains(&ty.kind()))
            .choose(random)
            .cloned()
            .unwrap_or_else(|| Type::random(random, &ctx, scope, TypeLocation::Default));

        let expr_idx = self.build_expr_tree(random, &ctx, scope, &ty, 0);
        let name = self.next_var();
        let mutable = random.random_bool(ctx.mutable_probability);

        self.variable(random, name, ty.clone(), mutable, false, expr_idx);
    }

    /// Build an expression tree of the given type
    fn build_expr_tree(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // We limit the depth of the expression tree to avoid infinite recursion
        if depth >= ctx.max_expr_depth || random.random_bool(ctx.leaf_probability) {
            return self.build_leaf(random, ctx, scope, ty);
        }

        // Collect possible expression forms based on type
        let mut choices: Vec<(ExprKind, usize)> = vec![];
        choices.push((ExprKind::Leaf, ctx.leaf_expr_weight));

        // Can reuse an existing variable of the same type
        let reusable = self.get_reusable_nodes(ty);
        if !reusable.is_empty() {
            choices.push((ExprKind::UseExisting, ctx.use_existing_expr_weight));
        }

        // Operators can only be applied to primitive types.
        if ty.is_primitive() {
            choices.push((ExprKind::UnaryOp, ctx.unary_weight));
            choices.push((ExprKind::BinaryOp, ctx.operator_weight));
        }

        // For accesses, check if we have one `Type` that is/has an element of the expected `ty`
        // Only allow one level of recursion @todo
        if self.has_indexable_of_type(ty) {
            choices.push((ExprKind::Index, ctx.index_weight));
        }

        if self.has_tuples_with_type(ty) {
            choices.push((ExprKind::TupleIndex, ctx.tuple_index_weight));
        }

        if self.has_struct_fields_of_type(ty) {
            choices.push((ExprKind::FieldAccess, ctx.field_access_weight));
        }

        // Cast only for `Field` and `Integer` types.
        if matches!(ty, Type::Field | Type::Integer(_)) {
            choices.push((ExprKind::Cast, ctx.cast_weight));
        }

        match choices.choose_weighted(random, |c| c.1).unwrap().0 {
            ExprKind::Leaf => self.build_leaf(random, ctx, scope, ty),
            ExprKind::UseExisting => *reusable.choose(random).unwrap(),
            ExprKind::UnaryOp => self.build_unary(random, ctx, scope, ty, depth),
            ExprKind::BinaryOp => self.build_binary(random, ctx, scope, ty, depth),
            ExprKind::Index => self.build_index_expr(random, ty),
            ExprKind::TupleIndex => self.build_tuple_index_expr(random, ty),
            ExprKind::FieldAccess => self.build_field_access_expr(random, ty),
            ExprKind::Cast => self.build_cast_expr(random, ctx, scope, ty, depth),
        }
    }

    /// Generate a leaf expression (literal or existing variable)
    fn build_leaf(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
    ) -> NodeIndex {
        // Try to reuse an existing variable/input of the same type (not literals)
        let reusable = self.get_reusable_nodes(ty);
        if !reusable.is_empty() && random.random_bool(ctx.reuse_variable_probability) {
            return *reusable.choose(random).unwrap();
        }

        // Generate a new literal
        let value = ty.random_value(random, ctx, scope);
        self.literal(random, value, ty.clone())
    }

    /// Get nodes of a type that can be reused (only `Node::Variable` and `Node::Input`)
    #[inline(always)]
    fn get_reusable_nodes(&self, ty: &Type) -> Vec<NodeIndex> {
        self.types
            .get(ty)
            .into_iter()
            .flatten()
            .copied()
            .filter(|&idx| matches!(&self.graph[idx], Node::Variable { .. } | Node::Input { .. }))
            .collect()
    }

    /// OP(EXPR)
    fn build_unary(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        let ops = match ty.kind() {
            TypeKind::Field => Operator::unary_field(),
            TypeKind::Signed => Operator::unary_integer_signed(),
            TypeKind::Unsigned => Operator::unary_integer_unsigned(),
            TypeKind::Boolean => Operator::unary_boolean(),
            _ => unreachable!(),
        };

        let op = *ops.choose(random).unwrap();
        let inner = self.build_expr_tree(random, ctx, scope, ty, depth + 1);
        self.operator(random, op, ty.clone(), inner, None)
    }

    /// EXPR OP EXPR
    fn build_binary(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // For boolean type, we might want comparison operators on other types
        if *ty == Type::Boolean && random.random_bool(ctx.comparison_probability) {
            return self.build_comparison(random, ctx, scope, depth);
        }

        let ops = match ty.kind() {
            TypeKind::Field => Operator::binary_field(),
            TypeKind::Signed => Operator::binary_integer_signed(),
            TypeKind::Unsigned => Operator::binary_integer_unsigned(),
            TypeKind::Boolean => Operator::binary_boolean(),
            _ => unreachable!(),
        };

        let op = *ops.choose(random).unwrap();
        let castable: &[TypeKind] = match ty {
            Type::Field => &[TypeKind::Boolean, TypeKind::Unsigned],
            Type::Integer(_) => {
                &[TypeKind::Field, TypeKind::Boolean, TypeKind::Signed, TypeKind::Unsigned]
            }
            _ => &[],
        };

        let castable_kind =
            castable.iter().filter(|k| self.type_kinds.contains_key(k)).choose(random).copied();
        let (left, right) = if random.random_bool(ctx.mixed_types_probability) &&
            let Some(castable_kind) = castable_kind
        {
            // Build a binary expression where one or both of the operands have been casted to
            // the `Operator` type.
            (
                self.build_casted_operand(random, ctx, scope, ty, &castable_kind, depth + 1),
                self.build_casted_operand(random, ctx, scope, ty, &castable_kind, depth + 1),
            )
        } else {
            // Fall back to same-type operands.
            (
                self.build_expr_tree(random, ctx, scope, ty, depth + 1),
                self.build_expr_tree(random, ctx, scope, ty, depth + 1),
            )
        };

        self.operator(random, op, ty.clone(), left, Some(right))
    }

    /// EXPR COMP EXPR
    fn build_comparison(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        depth: usize,
    ) -> NodeIndex {
        // Only use primitive types for comparisons @todo other node kinds/types
        let ty = self
            .types
            .keys()
            .filter(|ty| ty.is_primitive())
            .choose(random)
            .and_then(|ty| self.types.get(ty)?.choose(random).map(|&idx| self.ty(idx)));

        let ty = match ty {
            Some(ty) => ty,
            _ => return self.build_leaf(random, ctx, scope, &Type::Boolean),
        };

        let ops = match ty {
            Type::Field => Operator::field_comparison(),
            Type::Boolean | Type::Integer(_) => Operator::comparison(),
            _ => unreachable!(),
        };

        let op = *ops.choose(random).unwrap();
        let left = self.build_expr_tree(random, ctx, scope, &ty, depth + 1);
        let right = self.build_expr_tree(random, ctx, scope, &ty, depth + 1);

        self.operator(random, op, Type::Boolean, left, Some(right))
    }

    /// CAST(EXPR)
    fn build_casted_operand(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        castable_kind: &TypeKind,
        depth: usize,
    ) -> NodeIndex {
        let types = self.type_kinds.get(castable_kind).unwrap();
        let idx = types.choose(random).unwrap();
        let source_ty = self.ty(*idx);
        let expr = self.build_expr_tree(random, ctx, scope, &source_ty, depth + 1);

        self.cast(random, expr, ty.clone())
    }

    /// EXPR[IDX]
    fn build_index_expr(&mut self, random: &mut impl Rng, ty: &Type) -> NodeIndex {
        let candidates: Vec<_> = self
            .types
            .iter()
            .filter_map(|(container_ty, nodes)| {
                let size = match container_ty {
                    Type::Array(a) if a.ty.as_ref() == ty => a.size,
                    Type::Slice(s) if s.ty.as_ref() == ty => s.size,
                    _ => return None,
                };
                if size != 0 {
                    let candidates = nodes
                        .iter()
                        .filter(|&&i| self.is_reusable_node(i))
                        .map(move |&i| (i, size));

                    Some(candidates)
                } else {
                    None
                }
            })
            .flatten()
            .collect();

        let (idx, size) = *candidates.choose(random).unwrap();
        let value = random.random_range(0..size);

        self.index(random, idx, value, ty)
    }

    /// TUPLE.POS
    fn build_tuple_index_expr(&mut self, random: &mut impl Rng, ty: &Type) -> NodeIndex {
        let candidates: Vec<_> = self
            .types
            .iter()
            .filter_map(|(tuple_ty, nodes)| {
                let positions: Vec<_> = match tuple_ty {
                    Type::Tuple(t) => t
                        .elements
                        .iter()
                        .enumerate()
                        .filter_map(|(i, elem_ty)| (elem_ty == ty).then_some(i))
                        .collect(),
                    _ => return None,
                };

                if !positions.is_empty() {
                    let candidates = nodes
                        .iter()
                        .filter(|&&i| self.is_reusable_node(i))
                        .map(move |&i| (i, positions.clone(), tuple_ty.clone()));
                    Some(candidates)
                } else {
                    None
                }
            })
            .flatten()
            .collect();

        let (idx, positions, tuple_ty) = candidates.choose(random).unwrap();
        let value = *positions.choose(random).unwrap();
        self.tuple_index(random, *idx, value, tuple_ty)
    }

    /// STRUCT.FIELD
    fn build_field_access_expr(&mut self, random: &mut impl Rng, ty: &Type) -> NodeIndex {
        let candidates: Vec<_> = self
            .types
            .iter()
            .filter_map(|(struct_ty, nodes)| {
                let fields: Vec<_> = match struct_ty {
                    Type::Struct(s) => s
                        .fields
                        .iter()
                        .filter(|&f| f.ty.as_ref() == ty)
                        .map(|f| f.name.clone())
                        .collect(),
                    _ => return None,
                };

                if !fields.is_empty() {
                    let candidates = nodes
                        .iter()
                        .filter(|&&i| self.is_reusable_node(i))
                        .map(move |&i| (i, fields.clone(), struct_ty.clone()));
                    Some(candidates)
                } else {
                    None
                }
            })
            .flatten()
            .collect();

        let (idx, fields, struct_ty) = candidates.choose(random).unwrap();
        let name = fields.choose(random).unwrap().clone();
        self.field_access(random, *idx, name, struct_ty)
    }

    /// Build a cast expression (Field ← unsigned/bool, Integer ← int/Field/bool)
    fn build_cast_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        let source_ty = match ty {
            Type::Field if random.random_bool(0.5) => Type::Boolean,
            Type::Field => Type::Integer(Integer::random(random, false)),
            Type::Integer(_) if random.random_bool(0.5) => Type::Field,
            Type::Integer(_) => {
                let signed = random.random_bool(0.5);
                Type::Integer(Integer::random(random, signed))
            }
            _ => unreachable!(),
        };

        let source_idx = self.build_expr_tree(random, ctx, scope, &source_ty, depth + 1);
        self.cast(random, source_idx, ty.clone())
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Call generation
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn gen_call(&mut self, random: &mut impl Rng) {
        let (name, lambda, idx) = self.lambdas.choose(random).unwrap().clone();

        // Collect arguments for each parameter type, skipping if any is missing
        let args: Vec<_> = lambda
            .params
            .iter()
            .filter_map(|(_, ty)| self.types.get(ty)?.choose(random).copied())
            .collect();

        if args.len() == lambda.params.len() {
            let call_idx = self.call(random, name, *lambda.ret.clone(), args, idx);

            let var_name = self.next_var();
            self.variable(random, var_name, *lambda.ret, false, false, call_idx);
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Assignment generation
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn gen_assignment(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let (_, &idx) = self.mutables.iter().choose(random).unwrap();
        let ty = self.ty(idx);
        let value_idx = self.build_expr_tree(random, ctx, scope, &ty, ctx.max_expr_depth);

        let op = if random.random_bool(ctx.compound_assignment_probability) {
            self.get_compound_operators(&ty).and_then(|ops| ops.choose(random).copied())
        } else {
            None
        };

        // Prevent self-assignment without operator (a = a), but allow compound (a op= a)
        if op.is_none() && idx == value_idx {
            return;
        }

        self.assignment(random, idx, value_idx, op, &ty);
    }

    #[inline(always)]
    const fn get_compound_operators(&self, ty: &Type) -> Option<&'static [Operator]> {
        match ty.kind() {
            TypeKind::Field => Some(Operator::compound_field()),
            TypeKind::Signed | TypeKind::Unsigned => Some(Operator::compound_integer()),
            TypeKind::Boolean => Some(Operator::compound_boolean()),
            _ => None,
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Control flow generation
    // ────────────────────────────────────────────────────────────────────────────────

    /// Create a nested body forest with outer variables as inputs
    fn create_nested_body(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> Forest {
        let mut body = Forest::default();
        body.depth = self.depth + 1;
        body.var_counter = self.var_counter;

        // Register outer variables as inputs in the nested body
        for &idx in self.node_kinds.get(&NodeKind::Variable).into_iter().flatten() {
            if let Node::Variable { name, ty, mutable, .. } = &self.graph[idx] {
                let input_idx = body.input(random, name.clone(), ty.clone());

                if *mutable {
                    body.mutables.insert(name.clone(), input_idx);
                }
            }
        }

        // Register inputs too
        for (name, ty, _) in &scope.inputs {
            body.input(random, name.clone(), ty.clone());
        }

        body.random(random, ctx, scope, false);
        body
    }

    pub fn gen_for_loop(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let (a, b) = (random.random_range(-5..5), random.random_range(-5..5));
        let (start, end) = (a.min(b), a.max(b) + 1);

        let ty = Type::Integer(Integer { bits: 32, signed: true });
        let scope = &Scope {
            inputs: [scope.inputs.clone(), vec![("tmp".into(), ty.clone(), false)]].concat(),
            forest_type: ForestType::For,
            ..scope.clone()
        };

        let body = self.create_nested_body(random, ctx, scope);
        let suffix = ty.to_string();

        let idx = self.graph.add_node(Node::ForLoop {
            var: "tmp".into(),
            ty,
            start: format!("{start}{suffix}"),
            end: format!("{end}{suffix}"),
            body: Box::new(body),
        });

        self.node_kinds.entry(NodeKind::ForLoop).or_default().push(idx);
    }

    pub fn gen_if(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let &condition = self.type_kinds[&TypeKind::Boolean].choose(random).unwrap();
        let mut conditions = self.type_kinds[&TypeKind::Boolean].clone();
        let scope = &Scope { forest_type: ForestType::If, ..scope.clone() };

        let then_body = Box::new(self.create_nested_body(random, ctx, scope));

        let else_ifs: Vec<_> = (0..random
            .random_range(ctx.min_else_if_count..=ctx.max_else_if_count))
            .map_while(|_| {
                let cond = conditions.pop()?;
                Some((cond, Box::new(self.create_nested_body(random, ctx, scope))))
            })
            .collect();

        let else_body = random
            .random_bool(ctx.else_probability)
            .then(|| Box::new(self.create_nested_body(random, ctx, scope)));

        let idx = self.graph.add_node(Node::If { condition, then_body, else_ifs, else_body });
        self.node_kinds.entry(NodeKind::If).or_default().push(idx);
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Assert generation
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn gen_assert(&mut self, random: &mut impl Rng, ctx: &Context) {
        let Some(&condition) =
            self.type_kinds.get(&TypeKind::Boolean).and_then(|c| c.choose(random))
        else {
            return;
        };
        let message = random
            .random_bool(ctx.assert_message_probability)
            .then(|| "\"assertion failed\"".into());
        let idx = self.graph.add_node(Node::Assert { condition, message });

        self.node_kinds.entry(NodeKind::Assert).or_default().push(idx);
    }

    #[inline(always)]
    fn next_var(&mut self) -> String {
        let n = self.var_counter;
        self.var_counter += 1;
        format!("v{n}")
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Helper types
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy)]
enum StatementKind {
    Expression,
    Call,
    Assignment,
    ForLoop,
    If,
    Assert,
}

#[derive(Debug, Clone, Copy)]
enum ExprKind {
    Leaf,
    UseExisting,
    UnaryOp,
    BinaryOp,
    Index,
    TupleIndex,
    FieldAccess,
    Cast,
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::builders::CircuitBuilder;

    use super::*;

    #[test]
    fn test_random_forest() {
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();

        for (name, ty, _) in &scope.inputs {
            forest.input(&mut random, name.clone(), ty.clone());
        }

        forest.random(&mut random, &ctx, &scope, true);

        println!("{}", forest.format_with_indent("    "));

        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_random_forest.dot"));
    }
}
