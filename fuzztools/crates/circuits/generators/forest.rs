use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::{Node, NodeKind},
        operators::Operator,
        types::{Array, Integer, Slice, Tuple, Type, TypeKind},
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
    pub fn random(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        self.random_with_bounds(
            random,
            ctx,
            scope,
            ctx.min_expression_count,
            ctx.max_expression_count,
            false,
        );
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
            if has_nodes &&
                (!functions.is_empty() || self.type_kinds.contains_key(&TypeKind::Lambda))
            {
                choices.push((StatementKind::Call, ctx.call_weight));
            }
            // Allow assignments if we have mutable variables
            if self.has_mutable_variables() {
                choices.push((StatementKind::Assignment, ctx.assignment_weight));
            }
            // Only allow for loops with one level of nesting and no more than `max_for_count`
            if self.has_integer_types() && self.depth < 2 && for_count < ctx.max_for_count {
                choices.push((StatementKind::ForLoop, ctx.for_loop_weight));
            }
            // Only allow ifs with one level of nesting and no more than `max_if_count`
            if self.has_boolean_types() && self.depth < 2 && if_count < ctx.max_if_count {
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
                StatementKind::Call => self.gen_call(random, &functions),
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
    pub fn gen_expression(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let ctx = if self.type_kinds.get(&TypeKind::Lambda).map(|l| l.len()).unwrap_or_default() <
            ctx.max_lambda_count
        {
            *ctx
        } else {
            let mut ctx = *ctx;
            ctx.lambda_weight = 0;
            ctx
        };

        let ty = Type::random(random, &ctx, scope, TypeLocation::Default);
        let expr_idx = self.build_expr_tree(random, &ctx, scope, &ty, ctx.max_expr_depth);

        let name = self.next_var();
        let var_idx = self.variable(
            name,
            ty.clone(),
            random.random_bool(ctx.mutable_probability),
            false,
            expr_idx,
        );
        self.register(var_idx, NodeKind::Variable, &ty, None);
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
        // At depth 0 or with leaf probability, generate a leaf
        if depth == 0 || random.random_bool(ctx.leaf_probability) {
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
        match ty.kind() {
            TypeKind::Field | TypeKind::Signed | TypeKind::Unsigned | TypeKind::Boolean => {
                choices.push((
                    ExprKind::UnaryOp,
                    (ctx.operator_weight as f64 * ctx.unary_probability) as usize,
                ));
                choices.push((ExprKind::BinaryOp, ctx.operator_weight));
            }
            _ => {}
        }

        // Index/TupleIndex/FieldAccess/Cast for primitive types without zero-sized components
        if self.can_build_complex_expr(ty) {
            choices.push((ExprKind::Index, ctx.index_weight));
            choices.push((ExprKind::TupleIndex, ctx.tuple_index_weight));
            choices.push((ExprKind::FieldAccess, ctx.field_access_weight));
        }

        // Cast only for Field/Integer targets (Bool → Field/Integer, Field → Integer)
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
        self.literal(ty.random_value(random, ctx, scope, &self.exprs), ty.clone())
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
        let inner = self.build_expr_tree(random, ctx, scope, ty, depth - 1);
        let idx = self.operator(op, ty.clone(), inner, None);
        self.register(idx, NodeKind::Operator, ty, Some(op));
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
                self.build_mixed_operands(random, ctx, scope, ty, depth - 1)
            {
                let idx = self.operator(op, ty.clone(), left, Some(right));
                self.register(idx, NodeKind::Operator, ty, Some(op));
                return idx;
            }
        }

        // Fall back to same-type operands
        let left = self.build_expr_tree(random, ctx, scope, ty, depth - 1);
        let right = self.build_expr_tree(random, ctx, scope, ty, depth - 1);
        let idx = self.operator(op, ty.clone(), left, Some(right));
        self.register(idx, NodeKind::Operator, ty, Some(op));
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
        let castable = self.get_types_castable_to(target_ty);
        if castable.is_empty() {
            return None;
        }

        let left = self.build_operand_maybe_cast(random, ctx, scope, target_ty, &castable, depth);
        let right = self.build_operand_maybe_cast(random, ctx, scope, target_ty, &castable, depth);
        Some((left, right))
    }

    #[inline(always)]
    fn build_operand_maybe_cast(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        target_ty: &Type,
        castable: &[Type],
        depth: usize,
    ) -> NodeIndex {
        let ty = if random.random_bool(0.5) {
            target_ty.clone()
        } else {
            castable.choose(random).cloned().unwrap_or_else(|| target_ty.clone())
        };
        let expr = self.build_expr_tree(random, ctx, scope, &ty, depth);
        if ty == *target_ty {
            expr
        } else {
            let cast_idx = self.cast(expr, target_ty.clone());
            self.register(cast_idx, NodeKind::Cast, target_ty, None);
            cast_idx
        }
    }

    /// Get types that can be safely cast to the target type
    /// Rules: Field → any Integer, Bool → Field/Integer
    fn get_types_castable_to(&self, target: &Type) -> Vec<Type> {
        let mut result = Vec::new();

        match target {
            Type::Field => {
                // Bool can be cast to Field
                result.push(Type::Boolean);
            }
            Type::Integer(_) => {
                // Field and Bool can be cast to any Integer
                result.push(Type::Field);
                result.push(Type::Boolean);
            }
            _ => {}
        }

        // Keep only types we have nodes for
        result.retain(|ty| self.types.contains_key(ty));
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
        let Some(&kind) = cmp_types.choose(random) else {
            return self.build_leaf(random, ctx, scope, &Type::Boolean);
        };

        let operand_ty = match kind {
            TypeKind::Field => Type::Field,
            TypeKind::Signed => Type::Integer(Integer::random(random, true)),
            TypeKind::Unsigned => Type::Integer(Integer::random(random, false)),
            _ => return self.build_leaf(random, ctx, scope, &Type::Boolean),
        };

        // Get comparison operators (from binary field ops, filtered)
        let cmp_ops: Vec<_> =
            Operator::binary_field().iter().filter(|op| op.is_comparison()).copied().collect();
        let Some(&op) = cmp_ops.choose(random) else {
            return self.build_leaf(random, ctx, scope, &Type::Boolean);
        };

        let left = self.build_expr_tree(random, ctx, scope, &operand_ty, depth - 1);
        let right = self.build_expr_tree(random, ctx, scope, &operand_ty, depth - 1);
        let idx = self.operator(op, Type::Boolean, left, Some(right));
        self.register(idx, NodeKind::Operator, &Type::Boolean, Some(op));
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
        // Find or create an array/slice with this element type
        let size = random.random_range(ctx.min_element_count..ctx.max_element_count).max(1);
        let arr_ty = if random.random_bool(ctx.array_vs_slice_probability) {
            Type::Array(Array { ty: Box::new(ty.clone()), size })
        } else {
            Type::Slice(Slice { ty: Box::new(ty.clone()), size })
        };

        let arr_idx = self.build_expr_tree(random, ctx, scope, &arr_ty, depth - 1);
        let index_val = if size > 0 { random.random_range(0..size) } else { 0 };
        let idx = self.index(arr_idx, index_val);
        self.register(idx, NodeKind::Index, ty, None);
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
        // Create a tuple with at least one element of the target type
        let target_pos = random.random_range(0..3);

        // For on-the-fly literals in expressions, don't allow zero-sized arrays/slices
        // as they cause type inference failures in Noir
        let mut expr_ctx = *ctx;
        expr_ctx.min_element_count = expr_ctx.min_element_count.max(1);

        let elements: Vec<Type> = (0..3)
            .map(|i| {
                if i == target_pos {
                    ty.clone()
                } else {
                    Type::random(random, &expr_ctx, scope, TypeLocation::Default)
                }
            })
            .collect();

        let tuple_ty = Type::Tuple(Tuple { elements });
        let tuple_idx = self.build_expr_tree(random, ctx, scope, &tuple_ty, depth - 1);
        let idx = self.tuple_index(tuple_idx, target_pos);
        self.register(idx, NodeKind::TupleIndex, ty, None);
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
        // Find a struct with a field of the target type
        for struct_def in &scope.structs {
            for field in &struct_def.fields {
                if &*field.ty == ty {
                    let struct_ty = Type::Struct(struct_def.clone());
                    let struct_idx =
                        self.build_expr_tree(random, ctx, scope, &struct_ty, depth - 1);
                    let idx = self.field_access(struct_idx, field.name.clone());
                    self.register(idx, NodeKind::FieldAccess, ty, None);
                    return idx;
                }
            }
        }

        // Fallback to leaf
        self.build_leaf(random, ctx, scope, ty)
    }

    /// Build a cast expression that produces the target type
    /// Rules: Field → any Integer, Bool → Field/Integer
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
            Type::Field => Type::Boolean,
            Type::Integer(_) => {
                if random.random_bool(ctx.cast_source_field_probability) {
                    Type::Field
                } else {
                    Type::Boolean
                }
            }
            _ => return self.build_leaf(random, ctx, scope, ty),
        };

        let source_idx = self.build_expr_tree(random, ctx, scope, &source_ty, depth - 1);
        let idx = self.cast(source_idx, ty.clone());
        self.register(idx, NodeKind::Cast, ty, None);
        idx
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Helper methods for expression building
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    fn is_primitive(&self, ty: &Type) -> bool {
        matches!(
            ty.kind(),
            TypeKind::Field | TypeKind::Signed | TypeKind::Unsigned | TypeKind::Boolean
        )
    }

    #[inline(always)]
    fn can_build_complex_expr(&self, ty: &Type) -> bool {
        !ty.has_zero_sized() && self.is_primitive(ty)
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Call generation
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn gen_call(&mut self, random: &mut impl Rng, functions: &[(String, Vec<Type>, Type)]) {
        // Build iterator over all callables (lambdas + functions)
        let lambdas =
            self.nodes.get(&NodeKind::Variable).into_iter().flatten().filter_map(|&idx| {
                let Node::Variable { name, .. } = &self.graph[idx] else { return None };
                let Type::Lambda(l) = self.ty(self.left(idx)?) else { return None };
                Some((
                    name.clone(),
                    l.params.iter().map(|(_, t)| t.clone()).collect(),
                    (*l.ret).clone(),
                ))
            });

        // Pick a random callable we have args for
        let Some((name, params, ret)) = lambdas.chain(functions.iter().cloned()).choose(random)
        else {
            return;
        };
        let Some(args) = params.iter().map(|ty| self.types.get(ty)?.first().copied()).collect()
        else {
            return;
        };

        let call_idx = self.call(name, ret.clone(), args);
        self.register(call_idx, NodeKind::Call, &ret, None);

        let var_name = self.next_var();
        let var_idx = self.variable(var_name, ret.clone(), false, false, call_idx);
        self.register(var_idx, NodeKind::Variable, &ret, None);
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Assignment generation
    // ────────────────────────────────────────────────────────────────────────────────

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

    pub fn find_root_variable(&self, idx: NodeIndex) -> Option<NodeIndex> {
        let mut current = idx;
        loop {
            match &self.graph[current] {
                Node::Variable { .. } => return Some(current),
                Node::Input { name, .. } => {
                    if self.mutable_refs.contains_key(name) {
                        return Some(current);
                    }
                    return None;
                }
                Node::Index { .. } | Node::TupleIndex { .. } | Node::FieldAccess { .. } => {
                    current = self.left(current)?;
                }
                _ => return None,
            }
        }
    }

    pub fn is_rooted_in_mutable(&self, idx: NodeIndex) -> bool {
        self.find_root_variable(idx)
            .map(|root| match &self.graph[root] {
                Node::Variable { mutable: true, .. } => true,
                Node::Input { name, .. } => self.mutable_refs.contains_key(name),
                _ => false,
            })
            .unwrap_or(false)
    }

    pub fn gen_assignment(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        // Prefer mutable refs, otherwise pick from local mutable variables
        let source_idx = if !self.mutable_refs.is_empty() &&
            random.random_bool(ctx.prefer_mutable_refs_probability)
        {
            *self.mutable_refs.values().choose(random).unwrap()
        } else {
            let local_muts =
                self.nodes.get(&NodeKind::Variable).into_iter().flatten().copied().filter(|&idx| {
                    matches!(&self.graph[idx], Node::Variable { mutable: true, .. })
                });
            let Some(idx) = self.mutable_refs.values().copied().chain(local_muts).choose(random)
            else {
                return;
            };
            idx
        };

        let source_ty = self.ty(source_idx);

        // Build an expression with operations instead of just picking existing variables
        let value_idx = self.build_expr_tree(random, ctx, scope, &source_ty, ctx.max_expr_depth);

        let compound_op = random
            .random_bool(ctx.compound_assignment_probability)
            .then(|| {
                self.get_compound_operators(&source_ty).and_then(|ops| ops.choose(random).copied())
            })
            .flatten();

        let assign_idx = self.assignment(source_idx, value_idx, compound_op);
        self.register(assign_idx, NodeKind::Assignment, &source_ty, None);
    }

    pub fn get_compound_operators(&self, ty: &Type) -> Option<&'static [Operator]> {
        match ty {
            Type::Field => Some(Operator::compound_field()),
            Type::Integer(i) if i.signed => Some(Operator::compound_integer_signed()),
            Type::Integer(_) => Some(Operator::compound_integer_unsigned()),
            Type::Boolean => Some(Operator::compound_boolean()),
            _ => None,
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Control flow generation
    // ────────────────────────────────────────────────────────────────────────────────

    /// Create a nested body forest with outer variables as inputs
    fn create_nested_body(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        min_size: usize,
        max_size: usize,
    ) -> Forest {
        let mut body =
            Forest { var_counter: self.var_counter, depth: self.depth + 1, ..Default::default() };

        // Register outer variables as inputs in the nested body
        for &idx in self.nodes.get(&NodeKind::Variable).into_iter().flatten() {
            let Node::Variable { name, ty, mutable, .. } = &self.graph[idx] else { continue };
            let input_idx = body.input(name.clone(), ty.clone());
            body.register(input_idx, NodeKind::Input, ty, None);
            if *mutable {
                body.mutable_refs.insert(name.clone(), input_idx);
            }
        }

        let body_ctx = if body.mutable_refs.is_empty() {
            *ctx
        } else {
            Context {
                assignment_weight: ctx.assignment_weight * 10,
                operator_weight: ctx.operator_weight / 2,
                ..*ctx
            }
        };

        body.random_with_bounds(random, &body_ctx, scope, min_size, max_size, self.skip_idle_vars);
        self.var_counter = body.var_counter;
        body
    }

    pub fn gen_for_loop(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let (bits, signed) = (
            *[8u8, 16, 32, 64].choose(random).unwrap(),
            random.random_bool(ctx.signed_loop_probability),
        );
        let loop_ty = Type::Integer(Integer { bits, signed });
        let ty_suffix = if signed { format!("i{bits}") } else { format!("u{bits}") };

        let start: i64 =
            if signed { random.random_range(-5..=5) } else { random.random_range(0..=5) };
        let end = start + random.random_range(1i64..=10);

        let mut body = self.create_nested_body(
            random,
            ctx,
            scope,
            ctx.min_for_loop_body_size,
            ctx.max_for_loop_body_size,
        );
        let loop_var_idx = body.input("tmp".into(), loop_ty.clone());
        body.register(loop_var_idx, NodeKind::Input, &loop_ty, None);

        let idx = self.graph.add_node(Node::ForLoop {
            var: "tmp".into(),
            ty: loop_ty,
            start: format!("{start}{ty_suffix}"),
            end: format!("{end}{ty_suffix}"),
            body: Box::new(body),
        });
        self.nodes.entry(NodeKind::ForLoop).or_default().push(idx);
    }

    pub fn gen_if(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let Some(&condition) =
            self.type_kinds.get(&TypeKind::Boolean).and_then(|n| n.choose(random))
        else {
            return
        };

        let then_body = Box::new(self.create_nested_body(
            random,
            ctx,
            scope,
            ctx.min_if_body_size,
            ctx.max_if_body_size,
        ));

        let else_if_count = random.random_range(ctx.min_else_if_count..=ctx.max_else_if_count);
        let else_ifs: Vec<_> = (0..else_if_count)
            .filter_map(|_| {
                let cond = *self.type_kinds.get(&TypeKind::Boolean)?.choose(random)?;
                Some((
                    cond,
                    Box::new(self.create_nested_body(
                        random,
                        ctx,
                        scope,
                        ctx.min_if_body_size,
                        ctx.max_if_body_size,
                    )),
                ))
            })
            .collect();

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
    }

    pub fn gen_assert(&mut self, random: &mut impl Rng, ctx: &Context) {
        // Collect all valid boolean conditions:
        // 1. Boolean type nodes directly
        // 2. Comparison operator results (which are boolean)
        let mut conditions: Vec<NodeIndex> = Vec::new();

        // Add boolean type nodes
        if let Some(bool_nodes) = self.type_kinds.get(&TypeKind::Boolean) {
            conditions.extend(bool_nodes.iter().copied());
        }

        // Add comparison operator results
        for (op, indices) in &self.operators {
            if op.is_comparison() {
                conditions.extend(indices.iter().copied());
            }
        }

        let Some(&condition) = conditions.choose(random) else { return };

        // Optionally generate a message
        let message = if random.random_bool(ctx.assert_message_probability) {
            Some("\"assertion failed\"".to_string())
        } else {
            None
        };

        let idx = self.graph.add_node(Node::Assert { condition, message });
        self.nodes.entry(NodeKind::Assert).or_default().push(idx);
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
    use crate::builders::CircuitBuilder;

    use super::*;

    #[test]
    pub fn test_random_forest() {
        let ctx = Context::default();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        forest.random(&mut random, &ctx, &scope);

        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_random_forest.dot"));
    }
}
