use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::{Node, NodeKind},
        operators::Operator,
        types::{Array, Integer, Lambda, Slice, Tuple, Type, TypeKind},
    },
    context::Context,
    generators::types::TypeLocation,
    scope::Scope,
};
use petgraph::graph::NodeIndex;
use rand::{seq::IndexedRandom, Rng};

// ────────────────────────────────────────────────────────────────────────────────
// Forest generation
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub fn random(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        self.random_with_bounds(
            random,
            ctx,
            scope,
            ctx.min_expression_count,
            ctx.max_expression_count,
            false,
        );

        self.set_return_expression(random, ctx, scope);
    }

    pub fn random_with_bounds(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        min_expr: usize,
        max_expr: usize,
        skip_idle_vars: bool,
    ) {
        self.skip_idle_vars = skip_idle_vars;
        // Pre-compute function callables (they don't change during forest generation)
        let functions: Vec<(String, Lambda)> = scope
            .functions
            .iter()
            .map(|f| {
                (f.name.clone(), Lambda { params: f.params.clone(), ret: Box::new(f.ret.clone()) })
            })
            .collect();

        let mut for_count = 0;
        let mut assert_count = 0;
        let mut if_count = 0;
        for _ in 0..random.random_range(min_expr..=max_expr) {
            let has_nodes = self.graph.node_count() > 0;

            // Build the list of possible statement types
            let mut choices: Vec<(StatementKind, usize)> = vec![];

            // Always allow generating an expression (which creates a variable)
            choices.push((StatementKind::Expression, ctx.operator_weight * 2));

            // Allow function/lambda calls if we have callables
            if has_nodes && (!functions.is_empty() || !self.lambdas.is_empty()) {
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
            // Only allow ifs at top level (depth 0) - no nesting
            if self.has_boolean_types() && self.depth < 1 && if_count < ctx.max_if_count {
                choices.push((StatementKind::If, ctx.if_weight));
            }
            // Allow no more than `max_assert_count` asserts
            if has_nodes &&
                (self.has_boolean_types() || self.has_comparison_operators()) &&
                assert_count < ctx.max_assert_count
            {
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
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Expression generation
    // ────────────────────────────────────────────────────────────────────────────────

    /// Generate a complete expression tree and assign it to a variable
    /// If `scope.type_bias` is set, generation is biased towards those types
    pub fn gen_expression(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let ctx = if self.lambdas.len() < ctx.max_lambda_count {
            *ctx
        } else {
            let mut ctx = *ctx;
            ctx.lambda_weight = 0;
            ctx
        };

        // If we have bias and existing nodes of biased types, sometimes pick one directly
        let ty = if scope.type_bias.is_empty() {
            Type::random(random, &ctx, scope, TypeLocation::Default)
        } else {
            // Try to pick an existing biased type we already have nodes for
            let biased_types: Vec<_> = self
                .types
                .keys()
                .filter(|t| scope.type_bias.contains(&t.kind()))
                .cloned()
                .collect();

            if let Some(ty) = biased_types.choose(random) {
                ty.clone()
            } else {
                Type::random(random, &ctx, scope, TypeLocation::Default)
            }
        };

        let expr_idx = self.build_expr_tree(random, &ctx, scope, &ty, 0);

        let name = self.next_var();
        let var_idx = self.variable(
            name,
            ty.clone(),
            random.random_bool(ctx.mutable_probability),
            false,
            expr_idx,
        );
        self.register(random, var_idx, NodeKind::Variable, &ty, None);
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

        // Can reuse an existing variable of the same type (higher weight to encourage reuse)
        let reusable = self.get_reusable_nodes(ty);
        if !reusable.is_empty() {
            choices.push((ExprKind::UseExisting, ctx.use_existing_expr_weight));
        }

        // Operators for primitive types
        if self.is_primitive(ty) {
            choices.push((
                ExprKind::UnaryOp,
                (ctx.operator_weight as f64 * ctx.unary_probability) as usize,
            ));
            choices.push((ExprKind::BinaryOp, ctx.operator_weight));
        }

        // Index/TupleIndex/FieldAccess/Cast for primitive types without zero-sized components
        if self.can_build_complex_expr(ty) {
            choices.push((ExprKind::Index, ctx.index_weight));
            choices.push((ExprKind::TupleIndex, ctx.tuple_index_weight));
            choices.push((ExprKind::FieldAccess, ctx.field_access_weight));
        }

        // Cast only for Field/Integer targets (Field ← unsigned/bool, Integer ← int/Field/bool)
        if matches!(ty, Type::Field | Type::Integer(_)) {
            choices.push((ExprKind::Cast, ctx.cast_weight));
        }

        match choices.choose_weighted(random, |c| c.1).unwrap().0 {
            ExprKind::Leaf => self.build_leaf(random, ctx, scope, ty),
            ExprKind::UseExisting => *reusable.choose(random).unwrap(),
            ExprKind::UnaryOp => self.build_unary(random, ctx, scope, ty, depth),
            ExprKind::BinaryOp => self.build_binary(random, ctx, scope, ty, depth),
            ExprKind::Index => self.build_index_expr(random, ctx, scope, ty, depth),
            ExprKind::TupleIndex => self.build_tuple_index_expr(random, ctx, scope, ty, depth),
            ExprKind::FieldAccess => self.build_field_access_expr(random, ctx, scope, ty, depth),
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

        // Generate a new literal (don't register in types - literals are not for reuse)
        self.literal(ty.random_value(random, ctx, scope, true), ty.clone())
    }

    /// Get nodes of a type that can be reused (only Variable and Input nodes)
    fn get_reusable_nodes(&self, ty: &Type) -> Vec<NodeIndex> {
        self.types
            .get(ty)
            .map(|indices| {
                indices
                    .iter()
                    .copied()
                    .filter(|&idx| {
                        matches!(&self.graph[idx], Node::Variable { .. } | Node::Input { .. })
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Build a unary expression: OP(EXPR)
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
            _ => return self.build_leaf(random, ctx, scope, ty),
        };

        let op = *ops.choose(random).unwrap();
        let inner = self.build_expr_tree(random, ctx, scope, ty, depth + 1);
        let idx = self.operator(op, ty.clone(), inner, None);
        self.register(random, idx, NodeKind::Operator, ty, Some(op));
        idx
    }

    /// Build a binary expression: EXPR OP EXPR
    /// Supports mixing compatible numeric types via casts
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
            _ => return self.build_leaf(random, ctx, scope, ty),
        };

        // Filter out comparison operators (they return bool, not the operand type)
        let non_cmp_ops: Vec<_> = ops.iter().filter(|op| !op.is_comparison()).copied().collect();
        if non_cmp_ops.is_empty() {
            return self.build_leaf(random, ctx, scope, ty);
        }

        let op = *non_cmp_ops.choose(random).unwrap();

        // Try to use mixed types with casts
        if depth > 0 && random.random_bool(ctx.mixed_types_probability) {
            if let Some((left, right)) =
                self.build_mixed_operands(random, ctx, scope, ty, depth + 1)
            {
                let idx = self.operator(op, ty.clone(), left, Some(right));
                self.register(random, idx, NodeKind::Operator, ty, Some(op));
                return idx;
            }
        }

        // Fall back to same-type operands
        let left = self.build_expr_tree(random, ctx, scope, ty, depth + 1);
        let right = self.build_expr_tree(random, ctx, scope, ty, depth + 1);
        let idx = self.operator(op, ty.clone(), left, Some(right));
        self.register(random, idx, NodeKind::Operator, ty, Some(op));
        idx
    }

    /// Build operands of potentially different types and cast them to the target type
    fn build_mixed_operands(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        target_ty: &Type,
        depth: usize,
    ) -> Option<(NodeIndex, NodeIndex)> {
        let castable_kinds = self.get_type_kinds_castable_to(target_ty);
        if castable_kinds.is_empty() {
            return None;
        }

        let left =
            self.build_operand_maybe_cast(random, ctx, scope, target_ty, &castable_kinds, depth);
        let right =
            self.build_operand_maybe_cast(random, ctx, scope, target_ty, &castable_kinds, depth);
        Some((left, right))
    }

    fn build_operand_maybe_cast(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        target_ty: &Type,
        castable_kinds: &[TypeKind],
        depth: usize,
    ) -> NodeIndex {
        if let Some(kind) = castable_kinds.choose(random) {
            // Pick a random existing type of this kind
            let ty = self
                .type_kinds
                .get(kind)
                .and_then(|indices| indices.choose(random))
                .map(|&idx| self.ty(idx))
                .unwrap_or_else(|| target_ty.clone());

            let expr = self.build_expr_tree(random, ctx, scope, &ty, depth);
            if ty == *target_ty {
                expr
            } else {
                let cast_idx = self.cast(expr, target_ty.clone());
                self.register(random, cast_idx, NodeKind::Cast, target_ty, None);
                cast_idx
            }
        } else {
            self.build_expr_tree(random, ctx, scope, target_ty, depth)
        }
    }

    /// - Field ← unsigned integers, bool
    /// - Integer ← other integers, Field, bool
    /// - Bool cannot be cast to
    fn get_type_kinds_castable_to(&self, target: &Type) -> Vec<TypeKind> {
        let mut result = vec![];

        match target {
            Type::Field => {
                // Field ← unsigned integers, bool
                result.push(TypeKind::Boolean);
                result.push(TypeKind::Unsigned);
            }
            Type::Integer(_) => {
                // Integer ← other integers, Field, bool
                result.push(TypeKind::Field);
                result.push(TypeKind::Boolean);
                result.push(TypeKind::Signed);
                result.push(TypeKind::Unsigned);
            }
            _ => {}
        }

        // Keep only types we have nodes for
        result.retain(|kind| self.type_kinds.contains_key(kind));
        result
    }

    /// Build a comparison expression that returns bool
    fn build_comparison(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        depth: usize,
    ) -> NodeIndex {
        // Choose a comparable type
        let cmp_types = [TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned];
        let kind = *cmp_types.choose(random).unwrap();

        let operand_ty = match kind {
            TypeKind::Field => Type::Field,
            TypeKind::Signed => Type::Integer(Integer::random(random, true)),
            TypeKind::Unsigned => Type::Integer(Integer::random(random, false)),
            _ => return self.build_leaf(random, ctx, scope, &Type::Boolean),
        };

        // Get comparison operators
        let cmp_ops: Vec<_> =
            Operator::binary_field().iter().filter(|op| op.is_comparison()).copied().collect();
        let op = *cmp_ops.choose(random).unwrap();

        let left = self.build_expr_tree(random, ctx, scope, &operand_ty, depth + 1);
        let right = self.build_expr_tree(random, ctx, scope, &operand_ty, depth + 1);

        let idx = self.operator(op, Type::Boolean, left, Some(right));
        self.register(random, idx, NodeKind::Operator, &Type::Boolean, Some(op));
        idx
    }

    /// Build an index expression that produces the target type
    fn build_index_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // Try to find an existing array/slice variable with this element type
        let existing: Vec<_> = self
            .types
            .iter()
            .filter_map(|(t, indices)| match t {
                Type::Array(arr) if arr.ty.as_ref() == ty && arr.size > 0 => {
                    Some((t.clone(), arr.size, indices))
                }
                Type::Slice(slice) if slice.ty.as_ref() == ty && slice.size > 0 => {
                    Some((t.clone(), slice.size, indices))
                }
                _ => None,
            })
            .flat_map(|(t, size, indices)| {
                indices
                    .iter()
                    .filter(|&&idx| {
                        matches!(
                            self.graph.node_weight(idx),
                            Some(Node::Variable { .. } | Node::Input { .. })
                        )
                    })
                    .map(move |&idx| (t.clone(), size, idx))
            })
            .collect();

        // Either use existing or create on-the-fly
        let (arr_idx, size) = if !existing.is_empty() && random.random_bool(0.5) {
            let (_, size, idx) = existing.choose(random).unwrap();
            (*idx, *size)
        } else {
            // Create on-the-fly
            let size = random.random_range(ctx.min_element_count..=ctx.max_element_count).max(1);
            let arr_ty = if random.random_bool(ctx.array_vs_slice_probability) {
                Type::Array(Array { ty: Box::new(ty.clone()), size })
            } else {
                Type::Slice(Slice { ty: Box::new(ty.clone()), size })
            };
            let idx = self.build_expr_tree(random, ctx, scope, &arr_ty, depth + 1);
            (idx, size)
        };

        let index_val = random.random_range(0..size);
        let idx = self.index(arr_idx, index_val);
        self.register(random, idx, NodeKind::Index, ty, None);
        idx
    }

    /// Build a tuple index expression that produces the target type
    fn build_tuple_index_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // Try to find an existing tuple variable with an element of the target type
        let existing: Vec<_> = self
            .types
            .iter()
            .filter_map(|(t, indices)| {
                if let Type::Tuple(tuple) = t {
                    // Find positions where element matches target type
                    let positions: Vec<_> = tuple
                        .elements
                        .iter()
                        .enumerate()
                        .filter(|(_, elem)| *elem == ty)
                        .map(|(i, _)| i)
                        .collect();

                    (!positions.is_empty()).then(|| (t.clone(), positions, indices))
                } else {
                    None
                }
            })
            .flat_map(|(t, positions, indices)| {
                indices
                    .iter()
                    .filter(|&&idx| {
                        matches!(
                            self.graph.node_weight(idx),
                            Some(Node::Variable { .. } | Node::Input { .. })
                        )
                    })
                    .map(move |&idx| (t.clone(), positions.clone(), idx))
            })
            .collect();

        // Either use existing or create on-the-fly
        let (tuple_idx, target_pos) = if !existing.is_empty() && random.random_bool(0.5) {
            let (_, positions, idx) = existing.choose(random).unwrap();
            let pos = *positions.choose(random).unwrap();
            (*idx, pos)
        } else {
            // Create on-the-fly
            // Tuples need at least 2 elements (1-element is just parenthesized expression)
            let mut expr_ctx = *ctx;
            expr_ctx.min_element_count = expr_ctx.min_element_count.max(2);
            let elements_count =
                random.random_range(expr_ctx.min_element_count..expr_ctx.max_element_count.max(3));

            let target_pos = random.random_range(0..elements_count);

            let elements: Vec<Type> = (0..elements_count)
                .map(|i| {
                    if i == target_pos {
                        ty.clone()
                    } else {
                        Type::random(random, &expr_ctx, scope, TypeLocation::Default)
                    }
                })
                .collect();

            let tuple_ty = Type::Tuple(Tuple { elements });
            let idx = self.build_expr_tree(random, ctx, scope, &tuple_ty, depth + 1);
            (idx, target_pos)
        };

        let idx = self.tuple_index(tuple_idx, target_pos);
        self.register(random, idx, NodeKind::TupleIndex, ty, None);
        idx
    }

    /// Build a field access expression that produces the target type
    fn build_field_access_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // Try to find an existing struct variable with a field of the target type
        let existing: Vec<_> = self
            .types
            .iter()
            .filter_map(|(t, indices)| {
                if let Type::Struct(s) = t {
                    // Find fields that match target type
                    let fields: Vec<_> = s
                        .fields
                        .iter()
                        .filter(|f| f.ty.as_ref() == ty)
                        .map(|f| f.name.clone())
                        .collect();

                    (!fields.is_empty()).then(|| (t.clone(), fields, indices))
                } else {
                    None
                }
            })
            .flat_map(|(t, fields, indices)| {
                indices
                    .iter()
                    .filter(|&&idx| {
                        matches!(
                            self.graph.node_weight(idx),
                            Some(Node::Variable { .. } | Node::Input { .. })
                        )
                    })
                    .map(move |&idx| (t.clone(), fields.clone(), idx))
            })
            .collect();

        // Either use existing or create on-the-fly
        if !existing.is_empty() && random.random_bool(0.5) {
            let (_, fields, struct_idx) = existing.choose(random).unwrap();
            let field_name = fields.choose(random).unwrap().clone();

            let idx = self.field_access(*struct_idx, field_name);
            self.register(random, idx, NodeKind::FieldAccess, ty, None);
            return idx;
        }

        // Create on-the-fly: find a struct definition with the target field type
        for struct_def in &scope.structs {
            for field in &struct_def.fields {
                if &*field.ty == ty {
                    let struct_ty = Type::Struct(struct_def.clone());
                    let struct_idx =
                        self.build_expr_tree(random, ctx, scope, &struct_ty, depth + 1);

                    let idx = self.field_access(struct_idx, field.name.clone());
                    self.register(random, idx, NodeKind::FieldAccess, ty, None);
                    return idx;
                }
            }
        }

        // Fallback to leaf
        self.build_leaf(random, ctx, scope, ty)
    }

    /// Build a cast expression that produces the target type
    /// Rules:
    /// - Field ← unsigned integers, bool
    /// - Integer ← other integers, Field, bool
    /// - Bool cannot be cast to
    fn build_cast_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // Find a source type that can cast to the target
        let source_ty = match ty {
            Type::Field => {
                // Field ← unsigned or bool
                if random.random_bool(0.5) {
                    Type::Boolean
                } else {
                    Type::Integer(Integer::random(random, false))
                }
            }
            Type::Integer(_) => {
                // Integer ← other integers, Field, or bool
                match random.random_range(0..3) {
                    0 => Type::Field,
                    1 => Type::Boolean,
                    _ => {
                        let signed = random.random_bool(0.5);
                        let source_int = Integer::random(random, signed);
                        Type::Integer(source_int)
                    }
                }
            }
            _ => return self.build_leaf(random, ctx, scope, ty),
        };

        let source_idx = self.build_expr_tree(random, ctx, scope, &source_ty, depth + 1);
        let idx = self.cast(source_idx, ty.clone());
        self.register(random, idx, NodeKind::Cast, ty, None);
        idx
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Helper methods for expression building
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    const fn is_primitive(&self, ty: &Type) -> bool {
        matches!(
            ty.kind(),
            TypeKind::Field | TypeKind::Signed | TypeKind::Unsigned | TypeKind::Boolean
        )
    }

    #[inline(always)]
    fn can_build_complex_expr(&self, ty: &Type) -> bool {
        !ty.has_zero_sized() && self.is_primitive(ty)
    }

    #[inline(always)]
    fn has_type_kind(&self, kind: TypeKind) -> bool {
        self.type_kinds.contains_key(&kind)
    }

    #[inline(always)]
    pub fn has_mutable_variables(&self) -> bool {
        !self.mutable_refs.is_empty() ||
            self.nodes.get(&NodeKind::Variable).is_some_and(|vars| {
                vars.iter()
                    .any(|&idx| matches!(&self.graph[idx], Node::Variable { mutable: true, .. }))
            })
    }

    #[inline(always)]
    pub fn has_integer_types(&self) -> bool {
        self.has_type_kind(TypeKind::Signed) || self.has_type_kind(TypeKind::Unsigned)
    }

    #[inline(always)]
    pub fn has_boolean_types(&self) -> bool {
        self.has_type_kind(TypeKind::Boolean)
    }

    #[inline(always)]
    pub fn has_comparison_operators(&self) -> bool {
        self.operators.keys().any(|op| op.is_comparison())
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Call generation
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn gen_call(&mut self, random: &mut impl Rng) {
        if self.lambdas.is_empty() {
            return;
        }

        let (name, lambda, idx) = self.lambdas.choose(random).unwrap().clone();

        // Collect arguments for each parameter type, skipping if any is missing
        let mut args = Vec::with_capacity(lambda.params.len());
        for (_, ty) in &lambda.params {
            let Some(types) = self.types.get(ty) else { return };
            let Some(&idx) = types.choose(random) else { return };
            args.push(idx);
        }

        let call_idx = self.call(name, *lambda.ret.clone(), args, idx);
        self.register(random, call_idx, NodeKind::Call, &lambda.ret.clone(), None);

        let var_name = self.next_var();
        let var_idx = self.variable(var_name, *lambda.ret.clone(), false, false, call_idx);
        self.register(random, var_idx, NodeKind::Variable, &lambda.ret, None);
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Assignment generation
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn gen_assignment(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let candidates: Vec<_> =
            self.mutable_refs
                .values()
                .copied()
                .chain(self.nodes.get(&NodeKind::Variable).into_iter().flatten().copied().filter(
                    |&idx| matches!(&self.graph[idx], Node::Variable { mutable: true, .. }),
                ))
                .collect();

        if candidates.is_empty() {
            return;
        }

        let source_idx = *candidates.choose(random).unwrap();
        let source_ty = self.ty(source_idx);

        // Build an expression with operations instead of just picking existing variables
        let value_idx = self.build_expr_tree(random, ctx, scope, &source_ty, ctx.max_expr_depth);

        let compound_op = random
            .random_bool(ctx.compound_assignment_probability)
            .then(|| {
                self.get_compound_operators(&source_ty).and_then(|ops| ops.choose(random).copied())
            })
            .flatten();

        // Prevent self-assignment without operator (a = a), but allow compound (a op= a)
        if compound_op.is_none() && source_idx == value_idx {
            return;
        }

        let assign_idx = self.assignment(source_idx, value_idx, compound_op);
        self.register(random, assign_idx, NodeKind::Assignment, &source_ty, None); // @todo should
                                                                                   // this be compound_op
                                                                                   // instead of
                                                                                   // None?
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
    fn create_nested_body(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        min_size: usize,
        max_size: usize,
    ) -> Forest {
        let mut body = Forest::default();
        body.depth = self.depth + 1;
        body.var_counter = self.var_counter;

        // Register outer variables as inputs in the nested body
        for &idx in self.nodes.get(&NodeKind::Variable).into_iter().flatten() {
            if let Node::Variable { name, ty, mutable, .. } = &self.graph[idx] {
                let input_idx = body.input(name.clone(), ty.clone());
                body.register(random, input_idx, NodeKind::Input, ty, None);

                if *mutable {
                    body.mutable_refs.insert(name.clone(), input_idx);
                }
            }
        }

        body.random_with_bounds(random, ctx, scope, min_size, max_size, self.skip_idle_vars);

        body
    }

    pub fn gen_for_loop(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        // @todo use different integer types
        let a = random.random_range(-5..5);
        let b = random.random_range(-5..5);

        // Ensure valid range: if equal, make at least one iteration
        let (start, end) = if a < b { (a, b) } else { (b, if a == b { a + 1 } else { a }) };
        let ty = Type::Integer(Integer { bits: 32, signed: true });
        let suffix = ty.to_string();

        let mut scope = scope.clone();
        scope.inputs.push(("tmp".into(), ty.clone(), false));

        // @todo add bias here?

        let body = self.create_nested_body(
            random,
            ctx,
            &scope,
            ctx.min_for_loop_body_size,
            ctx.max_for_loop_body_size,
        );

        let idx = self.graph.add_node(Node::ForLoop {
            var: "tmp".into(),
            ty,
            start: format!("{start}{suffix}"),
            end: format!("{end}{suffix}"),
            body: Box::new(body),
        });
        self.nodes.entry(NodeKind::ForLoop).or_default().push(idx);
        self.nested_forests.push(idx);
    }

    pub fn gen_if(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let mut conditions = self.collect_boolean_nodes();

        if !conditions.is_empty() {
            let condition = *conditions.choose(random).unwrap();

            // We do not remove `condition` yet as a way to increase coverage where it is possible
            // to have two if branches with the same condition

            let then_body = Box::new(self.create_nested_body(
                random,
                ctx,
                scope,
                ctx.min_if_body_size,
                ctx.max_if_body_size,
            ));

            let else_if_count = random.random_range(ctx.min_else_if_count..=ctx.max_else_if_count);
            let mut else_ifs: Vec<_> = vec![];

            for _ in 0..else_if_count {
                // Stop if we run out of conditions
                if conditions.is_empty() {
                    break;
                }
                // To avoid using `choose` and then finding the idx, we do a little trick here to
                // achive O(1) instead O(N)
                let idx = random.random_range(0..conditions.len());
                let condition = conditions[idx];
                conditions.swap_remove(idx);

                let body = self.create_nested_body(
                    random,
                    ctx,
                    scope,
                    ctx.min_if_body_size,
                    ctx.max_if_body_size,
                );

                else_ifs.push((condition, Box::new(body)));
            }

            let else_body = random.random_bool(ctx.else_probability).then(|| {
                Box::new(self.create_nested_body(
                    random,
                    ctx,
                    scope,
                    ctx.min_if_body_size,
                    ctx.max_if_body_size,
                ))
            });

            let idx = self.graph.add_node(Node::If { condition, then_body, else_ifs, else_body });
            self.nodes.entry(NodeKind::If).or_default().push(idx);
            self.nested_forests.push(idx);
        }
    }

    pub fn gen_assert(&mut self, random: &mut impl Rng, ctx: &Context) {
        let conditions = self.collect_boolean_nodes();

        if !conditions.is_empty() {
            let condition = *conditions.choose(random).unwrap();

            // Optionally generate a message
            let message = random
                .random_bool(ctx.assert_message_probability)
                .then(|| "\"assertion failed\"".to_string()); // @audit should we use a random message?

            let idx = self.graph.add_node(Node::Assert { condition, message });
            self.nodes.entry(NodeKind::Assert).or_default().push(idx);
        }
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
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(&mut random, idx, NodeKind::Input, ty, None);
        }

        forest.random(&mut random, &ctx, &scope);

        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_random_forest.dot"));
    }
}
