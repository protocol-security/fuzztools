use super::types::TypeLocation;
use crate::circuits::{
    context::Context,
    ir::{Forest, Node, Operator, Struct, Type, TypeKind},
};
use petgraph::graph::NodeIndex;
use rand::{
    seq::{IndexedRandom, IteratorRandom},
    Rng,
};

#[derive(Debug, Clone, Copy)]
pub enum StatementKind {
    Expression,
    Assignment,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Leaf,
    UnaryOp,
    BinaryOp,
    Index,
    TupleIndex,
    StructField,
    Cast,
}

pub enum AssignementKind {
    Direct,
    Index(usize, Type),
    TupleIndex(usize, Type),
    StructField(String, Type),
}

impl Forest {
    pub fn random(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        available_structs: &[Struct],
        ret: Option<&Type>,
    ) {
        let expr_count = random.random_range(ctx.min_expression_count..ctx.max_expression_count);

        for _ in 0..expr_count {
            let mut choices = vec![];

            // Always allow new variables
            choices.push((StatementKind::Expression, ctx.expression_weight));

            // Allow assignements if we have mutable variables or assignements nodes
            if !self.mutables.is_empty() {
                choices.push((StatementKind::Assignment, ctx.assignment_weight));
            }

            match choices.choose_weighted(random, |i| i.1).unwrap().0 {
                StatementKind::Expression => self.gen_expression(random, ctx, available_structs),
                StatementKind::Assignment => self.gen_assignement(random, ctx),
            }
        }

        // Handle return expression if specified
        if let Some(ret_ty) = ret {
            self.return_expr = Some(self.find_return_expr(random, ctx, ret_ty));
        }
    }

    /// Finds or creates an expression that 1) has the same type or 2) castable type or 3) new
    /// literal
    fn find_return_expr(&mut self, random: &mut impl Rng, ctx: &Context, ret_ty: &Type) -> String {
        // 1) Look for a variable/assignment with the exact type
        if let Some(&idx) = self.get_reusables(ret_ty).choose(random) {
            if let Some(name) = self.get_node_name(idx) {
                return name;
            }
        }

        // 2) Look for a variable that can be cast to ret_ty (only for numeric types)
        if ret_ty.is_numeric() {
            let castable_kinds = match ret_ty.kind() {
                TypeKind::Field => vec![TypeKind::Bool, TypeKind::Unsigned],
                TypeKind::Signed | TypeKind::Unsigned => {
                    vec![TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned, TypeKind::Bool]
                }
                _ => vec![],
            };

            for kind in castable_kinds {
                if let Some(indices) = self.kinds.get(&kind) {
                    // Filter to only reusable nodes (variables/inputs/assignments)
                    let reusable: Vec<_> =
                        indices.iter().filter(|&&idx| self.get_node_name(idx).is_some()).collect();

                    if let Some(&&idx) = reusable.choose(random) {
                        if let Some(name) = self.get_node_name(idx) {
                            return format!("({name} as {ret_ty})");
                        }
                    }
                }
            }
        }

        // 3) Create a literal with random value
        ret_ty.random_value(random, ctx)
    }

    /// Gets the variable name for a node if it's a named node (Input, Variable, or Assignment)
    fn get_node_name(&self, idx: NodeIndex) -> Option<String> {
        match &self.inner[idx] {
            Node::Input { name, .. } |
            Node::Variable { name, .. } |
            Node::Assignment { name, .. } => Some(name.clone()),
            _ => None,
        }
    }

    // let mut? VAR: TYPE = EXPR
    fn gen_expression(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        available_structs: &[Struct],
    ) {
        let ty = Type::random(random, ctx, available_structs, TypeLocation::Default);
        let idx = self.build_expr(random, ctx, &ty, 0);
        let mutable = random.random_bool(ctx.mutable_probability);

        let name = format!("v{}", self.var_count);
        self.var_count += 1;

        self.variable(name, &ty, mutable, idx);
    }

    // EXPR OP?= EXPR
    fn gen_assignement(&mut self, random: &mut impl Rng, ctx: &Context) {
        let (&variable, _) = self.mutables.iter().choose(random).unwrap();
        let ty = self.ty(variable);

        let mut choices = vec![AssignementKind::Direct];

        // This is done to support assignements to inner values within `Type::Array`, `Type::Slice`,
        // `Type::Tuple`, and `Type::Struct`
        match &ty {
            Type::Array(a) if a.size > 0 => {
                let pos = random.random_range(0..a.size);
                choices.push(AssignementKind::Index(pos, a.ty.as_ref().clone()));
            }
            Type::Slice(s) if s.size > 0 => {
                let pos = random.random_range(0..s.size);
                choices.push(AssignementKind::Index(pos, s.ty.as_ref().clone()));
            }
            Type::Tuple(t) if !t.elements.is_empty() => {
                let pos = random.random_range(0..t.elements.len());
                choices.push(AssignementKind::TupleIndex(pos, t.elements[pos].clone()));
            }
            Type::Struct(s) if !s.fields.is_empty() => {
                let pos = random.random_range(0..s.fields.len());
                let field = &s.fields[pos];
                choices.push(AssignementKind::StructField(field.name.clone(), field.ty.clone()));
            }
            _ => {}
        }

        let kind = choices.choose(random).unwrap();
        let target_ty = match kind {
            AssignementKind::Direct => &ty,
            AssignementKind::Index(_, elem_ty) |
            AssignementKind::TupleIndex(_, elem_ty) |
            AssignementKind::StructField(_, elem_ty) => elem_ty,
        };

        let idx = self.build_expr(random, ctx, target_ty, 0);
        let op = if target_ty.is_primitive() &&
            random.random_bool(ctx.compound_assignment_probability)
        {
            Operator::binary_ops_for_kind(target_ty.kind())
                .and_then(|ops| ops.choose(random).copied())
        } else {
            None
        };

        match kind {
            AssignementKind::Direct => {
                self.assignement(op, &ty, idx, variable);
            }
            AssignementKind::Index(pos, elem_ty) => {
                self.indexed_assignment(op, *pos, elem_ty, idx, variable);
            }
            AssignementKind::TupleIndex(pos, elem_ty) => {
                self.tuple_field_assignment(op, *pos, elem_ty, idx, variable);
            }
            AssignementKind::StructField(field_name, elem_ty) => {
                self.struct_field_assignment(op, field_name.clone(), elem_ty, idx, variable);
            }
        }
    }

    fn build_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // We limit the depth of the expression to avoid infinite recursion
        if depth >= ctx.max_expr_depth || random.random_bool(ctx.leaf_probability) {
            return self.build_leaf(random, ctx, ty);
        }

        let mut choices = vec![];

        // Always allow new leafs
        choices.push((ExprKind::Leaf, ctx.leaf_expr_weight));

        // Operators can only be applied to primitive types
        if ty.is_primitive() {
            choices.push((ExprKind::UnaryOp, ctx.unary_weight));
            choices.push((ExprKind::BinaryOp, ctx.expression_weight));
        }

        // For accesses, check if we have one indexable `Type` that is/has an element of the
        // expected `ty`
        if !self.get_indexables(ty).is_empty() {
            choices.push((ExprKind::Index, ctx.index_weight));
        }

        if !self.get_tuple_indexables(ty).is_empty() {
            choices.push((ExprKind::TupleIndex, ctx.tuple_index_weight));
        }

        if !self.get_struct_indexables(ty).is_empty() {
            choices.push((ExprKind::StructField, ctx.field_access_weight));
        }

        // Only allow casts to numeric types, as others are not supported
        if ty.is_numeric() {
            choices.push((ExprKind::Cast, ctx.cast_weight));
        }

        match choices.choose_weighted(random, |c| c.1).unwrap().0.clone() {
            ExprKind::Leaf => self.build_leaf(random, ctx, ty),
            ExprKind::UnaryOp => self.build_unary(random, ctx, ty, depth),
            ExprKind::BinaryOp => self.build_binary(random, ctx, ty, depth),
            ExprKind::Index => self.build_index(random, ty),
            ExprKind::TupleIndex => self.build_tuple_index(random, ty),
            ExprKind::StructField => self.build_struct_field(random, ty),
            ExprKind::Cast => self.build_cast_expr(random, ctx, ty, depth),
        }
    }

    fn build_leaf(&mut self, random: &mut impl Rng, ctx: &Context, ty: &Type) -> NodeIndex {
        if random.random_bool(ctx.reuse_variable_probability) {
            // Try reusing existing values
            if let Some(&idx) = self.get_reusables(ty).choose(random) {
                return idx;
            }

            if !self.get_indexables(ty).is_empty() {
                return self.build_index(random, ty);
            }

            if !self.get_tuple_indexables(ty).is_empty() {
                return self.build_tuple_index(random, ty);
            }

            if !self.get_struct_indexables(ty).is_empty() {
                return self.build_struct_field(random, ty);
            }
        }

        let value = ty.random_value(random, ctx);
        self.literal(value, ty)
    }

    // OP(EXPR)
    fn build_unary(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        let op = Operator::unary_ops_for_kind(ty.kind()).choose(random).copied().unwrap();
        let inner = self.build_expr(random, ctx, ty, depth + 1);

        self.operator(op, ty, inner, None)
    }

    // EXPR OP EXPR
    fn build_binary(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        // For `Type::Bool`, we are not restricted to create operands of the same type, we can use
        // numeric types with comparison operators
        if *ty == Type::Bool && random.random_bool(ctx.comparison_probability) {
            return self.build_comparison(random, ctx, depth);
        }

        let op = Operator::binary_ops_for_kind(ty.kind()).unwrap().choose(random).copied().unwrap();
        let (left, right) = self.build_binary_operands(random, ctx, ty, depth);

        self.operator(op, ty, left, Some(right))
    }

    fn build_comparison(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        depth: usize,
    ) -> NodeIndex {
        let ty = self.types.keys().filter(|ty| ty.is_primitive()).choose(random).cloned();

        let Some(ty) = ty else {
            return self.build_leaf(random, ctx, &Type::Bool);
        };

        let op = Operator::comparison_ops_for_kind(ty.kind()).choose(random).copied().unwrap();
        let left = self.build_expr(random, ctx, &ty, depth + 1);
        let right = self.build_expr(random, ctx, &ty, depth + 1);

        self.operator(op, &Type::Bool, left, Some(right))
    }

    fn build_binary_operands(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        ty: &Type,
        depth: usize,
    ) -> (NodeIndex, NodeIndex) {
        // Try mixed-type operands with casting
        if random.random_bool(ctx.mixed_types_probability) {
            if let Some(kind) = self.find_castable_kind(random, ty) {
                if let Some(left) = self.build_casted_operand(random, ctx, ty, &kind, depth + 1) {
                    // Sometimes, we may be able to create two casted operands at the start of this
                    // function, but after calling `Forest::unregister` on the first
                    // `build_casted_operand`, it may consume all remaining types, so we default to
                    // the usual `build_expr` if that happens
                    let right = self
                        .build_casted_operand(random, ctx, ty, &kind, depth + 1)
                        .unwrap_or_else(|| self.build_expr(random, ctx, ty, depth + 1));

                    return (left, right);
                }
            }
        }

        // Same-type operands
        (self.build_expr(random, ctx, ty, depth + 1), self.build_expr(random, ctx, ty, depth + 1))
    }

    fn find_castable_kind(&self, random: &mut impl Rng, ty: &Type) -> Option<TypeKind> {
        const FIELD_CASTABLE: &[TypeKind] = &[TypeKind::Bool, TypeKind::Unsigned];
        const INTEGER_CASTABLE: &[TypeKind] =
            &[TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned, TypeKind::Bool];

        let castables = match ty.kind() {
            TypeKind::Field => FIELD_CASTABLE,
            TypeKind::Signed | TypeKind::Unsigned => INTEGER_CASTABLE,
            _ => return None,
        };

        castables
            .iter()
            .copied()
            .filter(|k| self.kinds.get(k).is_some_and(|v| !v.is_empty()))
            .choose(random)
    }

    fn build_casted_operand(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        ty: &Type,
        source_kind: &TypeKind,
        depth: usize,
    ) -> Option<NodeIndex> {
        let idx = self.kinds.get(source_kind)?.choose(random)?;
        let source_ty = self.ty(*idx);
        let expr = self.build_expr(random, ctx, &source_ty, depth + 1);

        Some(self.cast(ty, expr))
    }

    // EXPR[IDX]
    fn build_index(&mut self, random: &mut impl Rng, ty: &Type) -> NodeIndex {
        let candidates = self.get_indexables(ty);
        let (idx, size) = *candidates.choose(random).unwrap();
        let value = random.random_range(0..size);

        self.index(value, ty, idx)
    }

    // EXPR.IDX
    fn build_tuple_index(&mut self, random: &mut impl Rng, ty: &Type) -> NodeIndex {
        let candidates = self.get_tuple_indexables(ty);
        let (idx, positions) = candidates.choose(random).unwrap();
        let inner_ty = self.ty(*idx);

        self.tuple_index(*positions, &inner_ty, *idx)
    }

    // EXPR.FIELD_NAME
    fn build_struct_field(&mut self, random: &mut impl Rng, ty: &Type) -> NodeIndex {
        let candidates = self.get_struct_indexables(ty);
        let (idx, field_name) = candidates.choose(random).unwrap();

        self.struct_field(field_name.clone(), ty, *idx)
    }

    // EXPR as TYPE
    fn build_cast_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        let source_ty = Type::random_castable_source(random, ty);
        let source = self.build_expr(random, ctx, &source_ty, depth + 1);

        self.cast(ty, source)
    }
}
