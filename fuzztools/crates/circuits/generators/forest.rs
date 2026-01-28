use super::types::TypeLocation;
use crate::circuits::{
    ast::{Forest, Integer, Operator, Type, TypeKind},
    context::Context,
};
use petgraph::graph::NodeIndex;
use rand::{
    seq::{IndexedRandom, IteratorRandom},
    Rng,
};

#[derive(Debug, Clone, Copy)]
enum StatementKind {
    Expression,
    Assignment,
}

#[derive(Debug, Clone, Copy)]
enum ExprKind {
    Leaf,
    UnaryOp,
    BinaryOp,
    Index,
    TupleIndex,
    Cast,
}

enum AssignementKind {
    Direct,
    Index(usize, Type),
    TupleIndex(usize, Type)
}

impl Forest {
    pub fn random(&mut self, random: &mut impl Rng, ctx: &Context) {
        let expr_count = random.random_range(ctx.min_expression_count..ctx.max_expression_count);

        for _ in 0..expr_count {
            let mut choices = vec![];
            choices.push(StatementKind::Expression);

            // Allow assignements if we have mutable variables
            if !self.mutables.is_empty() {
                choices.push(StatementKind::Assignment);
            }

            match choices.choose(random).unwrap() {
                StatementKind::Expression => self.gen_expression(random, ctx),
                StatementKind::Assignment => self.gen_assignement(random, ctx),
            }
        }
    }

    // let mut? VAR: TYPE = EXPR
    fn gen_expression(&mut self, random: &mut impl Rng, ctx: &Context) {
        let ty = Type::random(random, ctx, TypeLocation::Default);
        let idx = self.build_expr(random, ctx, &ty, 0);
        let mutable = random.random_bool(ctx.mutable_probability);

        let name = format!("v{}", self.var_count);
        self.var_count += 1;

        self.variable(name, &ty, mutable, idx);
    }

    // EXPR OP?= EXPR
    fn gen_assignement(&mut self, random: &mut impl Rng, ctx: &Context) {
        let (&variable, _) = self.mutables.iter().choose(random).unwrap();
        let ty = self.inner[variable].ty();

        let mut choices = vec![AssignementKind::Direct];
        match &ty {
            Type::Array(a) if a.size > 0 => {
                let idx = random.random_range(0..a.size);
                choices.push(AssignementKind::Index(idx, a.ty.as_ref().clone()));
            }
            Type::Slice(s) if s.size > 0 => {
                let idx = random.random_range(0..s.size);
                choices.push(AssignementKind::Index(idx, s.ty.as_ref().clone()));
            }
            Type::Tuple(t) if !t.elements.is_empty() => {
                let pos = random.random_range(0..t.elements.len());
                choices.push(AssignementKind::TupleIndex(pos, t.elements[pos].clone()));
            }
            _ => {}
        }

        match choices.choose(random).unwrap() {
            AssignementKind::Direct => {
                let value = self.build_expr(random, ctx, &ty, 0);

                let op = if random.random_bool(ctx.compound_assignment_probability) {
                    self.binary_ops_for_kind(ty.kind()).and_then(|ops| ops.choose(random).copied())
                } else {
                    None
                };

                self.assignement(op, &ty, value, variable);
            }
            AssignementKind::Index(idx, elem_ty) => {
                let value = self.build_expr(random, ctx, &elem_ty, 0);

                let op = if elem_ty.is_primitive() && random.random_bool(ctx.compound_assignment_probability) {
                    self.binary_ops_for_kind(elem_ty.kind()).and_then(|ops| ops.choose(random).copied())
                } else {
                    None
                };

                self.indexed_assignment(op, *idx, &elem_ty, value, variable);
            }
            AssignementKind::TupleIndex(pos, elem_ty) => {
                let value = self.build_expr(random, ctx, &elem_ty, 0);

                let op = if elem_ty.is_primitive() && random.random_bool(ctx.compound_assignment_probability) {
                    self.binary_ops_for_kind(elem_ty.kind()).and_then(|ops| ops.choose(random).copied())
                } else {
                    None
                };

                self.tuple_field_assignment(op, *pos, &elem_ty, value, variable);
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
        // We limit the depth of the expression tree to avoid infinite recursion
        if depth >= ctx.max_expr_depth || random.random_bool(ctx.leaf_probability) {
            return self.build_leaf(random, ctx, ty);
        }

        let mut choices = vec![];
        choices.push((ExprKind::Leaf, ctx.leaf_expr_weight));

        // Operators can only be applied to primitive types.
        if ty.is_primitive() {
            choices.push((ExprKind::UnaryOp, ctx.unary_weight));
            choices.push((ExprKind::BinaryOp, ctx.expression_weight));
        }

        // For accesses, check if we have one `Type` that is/has an element of the expected `ty`
        if !self.get_indexables(ty).is_empty() {
            choices.push((ExprKind::Index, ctx.index_weight));
        }

        if !self.get_tuple_indexables(ty).is_empty() {
            choices.push((ExprKind::TupleIndex, ctx.tuple_index_weight));
        }

        // Only allow casts to numeric types, as others are not supported.
        if ty.is_numeric() {
            choices.push((ExprKind::Cast, ctx.cast_weight));
        }

        match choices.choose_weighted(random, |c| c.1).unwrap().0 {
            ExprKind::Leaf => self.build_leaf(random, ctx, ty),
            ExprKind::UnaryOp => self.build_unary(random, ctx, ty, depth),
            ExprKind::BinaryOp => self.build_binary(random, ctx, ty, depth),
            ExprKind::Index => self.build_index(random, ty),
            ExprKind::TupleIndex => self.build_tuple_index(random, ty),
            ExprKind::Cast => self.build_cast_expr(random, ctx, ty, depth),
        }
    }

    fn build_leaf(&mut self, random: &mut impl Rng, ctx: &Context, ty: &Type) -> NodeIndex {
        let reusables = self.get_reusables(ty);
        let indexables = self.get_indexables(ty);
        let tuple_indexables = self.get_tuple_indexables(ty);

        let r = reusables.len();
        let i = indexables.len();
        let t = tuple_indexables.len();
        let total = r + i + t;

        if total > 0 && random.random_bool(ctx.reuse_variable_probability) {
            let choice = random.random_range(0..total);

            if choice < r {
                return *reusables.choose(random).unwrap();
            } else if choice < r + i {
                return self.build_index(random, ty);
            }

            return self.build_tuple_index(random, ty);
        }

        // Generate a new literal
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
        let op = self.unary_ops_for_kind(ty.kind()).choose(random).copied().unwrap();
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
        // For boolean type, we might want comparison operators on other types
        if *ty == Type::Bool && random.random_bool(ctx.comparison_probability) {
            return self.build_comparison(random, ctx, depth);
        }

        let op = self.binary_ops_for_kind(ty.kind()).unwrap().choose(random).copied().unwrap();
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

        let op = self.comparison_ops_for_kind(ty.kind()).choose(random).copied().unwrap();
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
            let castable = self.find_castable_kind(random, ty);

            if let Some(kind) = castable {
                let left = self.build_casted_operand(random, ctx, ty, &kind, depth + 1);

                if let Some(left) = left {
                    let right = self
                        .build_casted_operand(random, ctx, ty, &kind, depth + 1)
                        .unwrap_or_else(|| self.build_expr(random, ctx, ty, depth + 1));

                    return (left, right);
                }
            }
        }

        // Same-type operands
        let left = self.build_expr(random, ctx, ty, depth + 1);
        let right = self.build_expr(random, ctx, ty, depth + 1);

        (left, right)
    }

    fn find_castable_kind(&self, random: &mut impl Rng, ty: &Type) -> Option<TypeKind> {
        let castables = match ty.kind() {
            TypeKind::Field => vec![TypeKind::Bool, TypeKind::Unsigned],
            TypeKind::Signed | TypeKind::Unsigned => {
                vec![TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned, TypeKind::Bool]
            }
            _ => return None,
        };

        castables
            .into_iter()
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
        let source_ty = self.inner[*idx].ty();
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
        let inner_ty = self.inner[*idx].ty();

        self.tuple_index(*positions, &inner_ty, *idx)
    }

    // EXPR as TYPE
    fn build_cast_expr(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        ty: &Type,
        depth: usize,
    ) -> NodeIndex {
        let source_ty = self.random_castable_source(random, ty);
        let source = self.build_expr(random, ctx, &source_ty, depth + 1);

        self.cast(ty, source)
    }

    const fn unary_ops_for_kind(&self, source_kind: TypeKind) -> &'static [Operator] {
        match source_kind {
            TypeKind::Field => Operator::unary_field(),
            TypeKind::Signed => Operator::unary_signed_integer(),
            TypeKind::Unsigned => Operator::unary_unsigned_integer(),
            TypeKind::Bool => Operator::unary_boolean(),
            _ => unreachable!(),
        }
    }

    const fn binary_ops_for_kind(&self, source_kind: TypeKind) -> Option<&'static [Operator]> {
        match source_kind {
            TypeKind::Field => Some(Operator::binary_field()),
            TypeKind::Signed => Some(Operator::binary_signed_integer()),
            TypeKind::Unsigned => Some(Operator::binary_unsigned_integer()),
            TypeKind::Bool => Some(Operator::binary_boolean()),
            _ => None,
        }
    }

    const fn comparison_ops_for_kind(&self, source_kind: TypeKind) -> &'static [Operator] {
        match source_kind {
            TypeKind::Field => Operator::field_comparison(),
            _ => Operator::comparison(),
        }
    }

    fn random_castable_source(&self, random: &mut impl Rng, target: &Type) -> Type {
        const UNSIGNED_BITS: [u32; 6] = [1, 8, 16, 32, 64, 128];
        const SIGNED_BITS: [u32; 4] = [8, 16, 32, 64];

        match target {
            Type::Field => {
                if random.random_bool(0.5) {
                    Type::Bool
                } else {
                    let bits = *UNSIGNED_BITS.choose(random).unwrap();
                    Type::Integer(Integer { bits, signed: false })
                }
            }
            Type::Integer(_) => {
                if random.random_bool(0.5) {
                    Type::Field
                } else {
                    let signed = random.random_bool(0.5);
                    let bits = if signed {
                        *SIGNED_BITS.choose(random).unwrap()
                    } else {
                        *UNSIGNED_BITS.choose(random).unwrap()
                    };
                    Type::Integer(Integer { bits, signed })
                }
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn test_random_forest() {
        let ctx =
            &serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let mut forest = Forest::default();
        forest.random(&mut random, ctx);

        println!("{}", forest.format(""));

        forest
            .save_as_dot(&std::env::current_dir().unwrap().join("test_random_forest.dot"))
            .unwrap();
    }
}
