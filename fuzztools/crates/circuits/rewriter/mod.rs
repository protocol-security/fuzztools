pub mod rules;

use crate::circuits::ast::{
    operators::Operator,
    tree::{Expr, ExprGraph},
    types::*,
};
use petgraph::graph::NodeIndex;
use rand::{seq::IndexedRandom, Rng};
use rules::{Rule, RuleKind, EQUIVALENCE_RULES};

/// Rewriter that applies transformation rules to an ExprGraph
pub struct Rewriter<'a> {
    rules: &'a [Rule],
}

impl<'a> Rewriter<'a> {
    /// Create a new rewriter with custom rules
    pub fn new(rules: &'a [Rule]) -> Self {
        Self { rules }
    }

    /// Create rewriter with all equivalence rules
    pub fn equivalence() -> Rewriter<'static> {
        Rewriter { rules: EQUIVALENCE_RULES }
    }

    /// Create rewriter with obfuscation rules (random injection)
    pub fn obfuscation() -> Rewriter<'static> {
        Rewriter {
            rules: &[
                rules::INJECT_ADD_SUB,
                rules::INJECT_SUB_ADD,
                rules::INJECT_MUL_DIV,
                rules::INJECT_XOR_XOR,
                rules::DOUBLE_NEG,
                rules::DOUBLE_NOT,
            ],
        }
    }

    /// Create rewriter with simplification rules
    pub fn simplification() -> Rewriter<'static> {
        Rewriter {
            rules: &[
                rules::DOUBLE_NEG,
                rules::DOUBLE_NOT,
                rules::ADD_ZERO,
                rules::MUL_ONE,
                rules::MUL_ZERO,
                rules::SELF_SUB,
                rules::SELF_XOR,
                rules::IDEM_AND,
                rules::IDEM_OR,
            ],
        }
    }

    /// Apply multiple random rules
    pub fn rewrite(
        &self,
        graph: &mut ExprGraph,
        random: &mut impl Rng,
        iterations: usize,
    ) -> usize {
        let mut applied = 0;
        for _ in 0..iterations {
            if self.apply_random(graph, random) {
                applied += 1;
            }
        }
        applied
    }

    /// Try to apply a random applicable rule to the graph
    /// Returns true if a rule was successfully applied
    pub fn apply_random(&self, graph: &mut ExprGraph, random: &mut impl Rng) -> bool {
        // Collect all (node, rule) pairs that can be applied
        let mut applicable: Vec<(NodeIndex, &Rule)> = Vec::new();

        for node in graph.inner.node_indices() {
            for rule in self.rules {
                if self.can_apply(graph, node, rule) {
                    applicable.push((node, rule));
                }
            }
        }

        if applicable.is_empty() {
            return false;
        }

        // Pick a random applicable rule and apply it
        let &(node, rule) = applicable.choose(random).unwrap();
        self.apply(graph, node, rule, random)
    }

    /// Apply a rule to a specific node (bidirectional - detects which direction)
    pub fn apply(
        &self,
        graph: &mut ExprGraph,
        node: NodeIndex,
        rule: &Rule,
        random: &mut impl Rng,
    ) -> bool {
        match &rule.kind {
            RuleKind::SwapOperands { .. } => {
                graph.swap_operands(node);
                true
            }

            RuleKind::Associate { .. } => {
                // Bidirectional: ((a op b) op c) <=> (a op (b op c))
                let Expr::Binary { op, .. } = graph.inner[node].clone() else { return false };

                let left = graph.left(node);
                let right = graph.right(node);

                let left_matches = left
                    .map(
                        |l| matches!(&graph.inner[l], Expr::Binary { op: l_op, .. } if *l_op == op),
                    )
                    .unwrap_or(false);

                let right_matches = right
                    .map(
                        |r| matches!(&graph.inner[r], Expr::Binary { op: r_op, .. } if *r_op == op),
                    )
                    .unwrap_or(false);

                if left_matches {
                    // ((a op b) op c) -> (a op (b op c))
                    let left_node = left.unwrap();
                    let right_c = right.unwrap();
                    let left_a = graph.left(left_node).unwrap();
                    let left_b = graph.right(left_node).unwrap();
                    let ty = graph.ty(node).unwrap();

                    let new_right = graph.binop(op, left_b, right_c, ty.clone());
                    graph.replace_operand(node, 0, left_a);
                    graph.replace_operand(node, 1, new_right);
                    true
                } else if right_matches {
                    // (a op (b op c)) -> ((a op b) op c)
                    let left_a = left.unwrap();
                    let right_node = right.unwrap();
                    let right_b = graph.left(right_node).unwrap();
                    let right_c = graph.right(right_node).unwrap();
                    let ty = graph.ty(node).unwrap();

                    let new_left = graph.binop(op, left_a, right_b, ty.clone());
                    graph.replace_operand(node, 0, new_left);
                    graph.replace_operand(node, 1, right_c);
                    true
                } else {
                    false
                }
            }

            RuleKind::Distribute { outer, inner } => {
                // Bidirectional: (a inner b) outer c <=> (a outer c) inner (b outer c)
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    if op == outer {
                        // Check for distribute pattern: left child is inner op
                        if let Some(left) = graph.left(node) {
                            if let Expr::Binary { op: left_op, .. } = &graph.inner[left] {
                                if left_op == inner {
                                    // Distribute
                                    let right_c = graph.right(node).unwrap();
                                    let left_a = graph.left(left).unwrap();
                                    let left_b = graph.right(left).unwrap();
                                    let ty = graph.ty(node).unwrap();

                                    let new_left = graph.binop(*outer, left_a, right_c, ty.clone());
                                    let new_right =
                                        graph.binop(*outer, left_b, right_c, ty.clone());

                                    graph.set_op(node, *inner);
                                    graph.replace_operand(node, 0, new_left);
                                    graph.replace_operand(node, 1, new_right);
                                    return true;
                                }
                            }
                        }
                    } else if op == inner {
                        // Check for factor pattern: both children are outer op with same right
                        // operand
                        let left = graph.left(node);
                        let right = graph.right(node);
                        if let (Some(l), Some(r)) = (left, right) {
                            if let (Expr::Binary { op: l_op, .. }, Expr::Binary { op: r_op, .. }) =
                                (&graph.inner[l], &graph.inner[r])
                            {
                                if l_op == outer && r_op == outer {
                                    let l_right = graph.right(l);
                                    let r_right = graph.right(r);
                                    if l_right == r_right && l_right.is_some() {
                                        // Factor
                                        let left_a = graph.left(l).unwrap();
                                        let right_b = graph.left(r).unwrap();
                                        let common_c = l_right.unwrap();
                                        let ty = graph.ty(node).unwrap();

                                        let new_left =
                                            graph.binop(*inner, left_a, right_b, ty.clone());
                                        graph.set_op(node, *outer);
                                        graph.replace_operand(node, 0, new_left);
                                        graph.replace_operand(node, 1, common_c);
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
                false
            }

            RuleKind::Identity { op, identity_right } => {
                // Bidirectional: a <=> (a op identity)
                // Try to remove identity first
                if let Expr::Binary { op: node_op, .. } = &graph.inner[node] {
                    if node_op == op {
                        let check_node =
                            if *identity_right { graph.right(node) } else { graph.left(node) };
                        if let Some(n) = check_node {
                            if self.is_identity_for(graph, n, *op) {
                                let keep = if *identity_right {
                                    graph.left(node)
                                } else {
                                    graph.right(node)
                                };
                                if let Some(k) = keep {
                                    let ty = graph.ty(k).unwrap();
                                    let name = graph.next_var();
                                    graph.var(name, ty, k);
                                    return true;
                                }
                            }
                        }
                    }
                }
                // Add identity
                if graph.ty(node).map(|t| self.is_numeric_or_bool(&t)).unwrap_or(false) {
                    let ty = graph.ty(node).unwrap();
                    let identity = self.make_identity_lit(graph, &ty, *op);
                    if *identity_right {
                        graph.binop(*op, node, identity, ty);
                    } else {
                        graph.binop(*op, identity, node, ty);
                    }
                    return true;
                }
                false
            }

            RuleKind::Absorb { op } => {
                let Some(ty) = graph.ty(node) else { return false };
                let absorb = self.make_absorbing_lit(graph, &ty, *op);
                let name = graph.next_var();
                graph.var(name, ty, absorb);
                true
            }

            RuleKind::SelfInverse { op } => {
                let Some(ty) = graph.ty(node) else { return false };
                let result = match op {
                    Operator::Sub | Operator::Xor | Operator::Mod => self.make_zero_lit(graph, &ty),
                    Operator::Div => self.make_one_lit(graph, &ty),
                    _ => return false,
                };
                let name = graph.next_var();
                graph.var(name, ty, result);
                true
            }

            RuleKind::Idempotent { op } => {
                // Bidirectional: a <=> (a op a) for idempotent ops
                // Try to contract first
                if let Expr::Binary { op: node_op, .. } = &graph.inner[node] {
                    if node_op == op &&
                        graph.left(node) == graph.right(node) &&
                        graph.left(node).is_some()
                    {
                        let operand = graph.left(node).unwrap();
                        let ty = graph.ty(operand).unwrap();
                        let name = graph.next_var();
                        graph.var(name, ty, operand);
                        return true;
                    }
                }
                // Expand (for boolean types)
                if graph.ty(node).map(|t| matches!(t, Type::Boolean(_))).unwrap_or(false) {
                    let ty = graph.ty(node).unwrap();
                    graph.binop(*op, node, node, ty);
                    return true;
                }
                false
            }

            RuleKind::DoubleUnary { op } => {
                // Bidirectional: a <=> op(op(a))
                // Try to remove double unary first
                if let Expr::Unary { op: outer_op, .. } = &graph.inner[node] {
                    if outer_op == op {
                        if let Some(inner) = graph.left(node) {
                            if let Expr::Unary { op: inner_op, .. } = &graph.inner[inner] {
                                if inner_op == op {
                                    let x = graph.left(inner).unwrap();
                                    let ty = graph.ty(x).unwrap();
                                    let name = graph.next_var();
                                    graph.var(name, ty, x);
                                    return true;
                                }
                            }
                        }
                    }
                }
                // Add double unary
                if graph.ty(node).is_some() {
                    let ty = graph.ty(node).unwrap();
                    let inner = graph.unary(*op, node, ty.clone());
                    graph.unary(*op, inner, ty);
                    return true;
                }
                false
            }

            RuleKind::AddNegSub => {
                // Bidirectional: (a - b) <=> (a + (-b))
                if let Expr::Binary { op: Operator::Sub, .. } = &graph.inner[node] {
                    let right = graph.right(node).unwrap();
                    let ty = graph.ty(node).unwrap();
                    let neg_right = graph.unary(Operator::Neg, right, ty.clone());
                    graph.set_op(node, Operator::Add);
                    graph.replace_operand(node, 1, neg_right);
                    return true;
                }
                if let Expr::Binary { op: Operator::Add, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if let Expr::Unary { op: Operator::Neg, .. } = &graph.inner[right] {
                            let b = graph.left(right).unwrap();
                            graph.set_op(node, Operator::Sub);
                            graph.replace_operand(node, 1, b);
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::NegZeroSub => {
                // Bidirectional: (-a) <=> (0 - a)
                if let Expr::Unary { op: Operator::Neg, .. } = &graph.inner[node] {
                    let operand = graph.left(node).unwrap();
                    let ty = graph.ty(node).unwrap();
                    let zero = self.make_zero_lit(graph, &ty);
                    graph.binop(Operator::Sub, zero, operand, ty);
                    return true;
                }
                if let Expr::Binary { op: Operator::Sub, .. } = &graph.inner[node] {
                    if let Some(left) = graph.left(node) {
                        if self.is_zero(graph, left) {
                            let right = graph.right(node).unwrap();
                            let ty = graph.ty(node).unwrap();
                            graph.unary(Operator::Neg, right, ty);
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::FlipComparison => {
                // (a < b) <=> (b > a), etc.
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    let flipped = match op {
                        Operator::Less => Operator::Greater,
                        Operator::Greater => Operator::Less,
                        Operator::LessOrEqual => Operator::GreaterOrEqual,
                        Operator::GreaterOrEqual => Operator::LessOrEqual,
                        _ => return false,
                    };
                    graph.set_op(node, flipped);
                    graph.swap_operands(node);
                    return true;
                }
                false
            }

            RuleKind::NegateComparison => {
                // Bidirectional: (a < b) <=> !(a >= b)
                // Try to remove negation first
                if let Expr::Unary { op: Operator::Not, .. } = &graph.inner[node] {
                    if let Some(inner) = graph.left(node) {
                        if let Expr::Binary { op, .. } = &graph.inner[inner] {
                            let negated = match op {
                                Operator::Less => Operator::GreaterOrEqual,
                                Operator::Greater => Operator::LessOrEqual,
                                Operator::LessOrEqual => Operator::Greater,
                                Operator::GreaterOrEqual => Operator::Less,
                                Operator::Equal => Operator::NotEqual,
                                Operator::NotEqual => Operator::Equal,
                                _ => return false,
                            };
                            graph.set_op(inner, negated);
                            let ty = graph.ty(inner).unwrap();
                            let name = graph.next_var();
                            graph.var(name, ty, inner);
                            return true;
                        }
                    }
                }
                // Add negation
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    let negated = match op {
                        Operator::Less => Operator::GreaterOrEqual,
                        Operator::Greater => Operator::LessOrEqual,
                        Operator::LessOrEqual => Operator::Greater,
                        Operator::GreaterOrEqual => Operator::Less,
                        Operator::Equal => Operator::NotEqual,
                        Operator::NotEqual => Operator::Equal,
                        _ => return false,
                    };
                    let ty = graph.ty(node).unwrap();
                    graph.set_op(node, negated);
                    graph.unary(Operator::Not, node, ty);
                    return true;
                }
                false
            }

            RuleKind::DeMorgan => {
                // Bidirectional: !(a & b) <=> (!a | !b), !(a | b) <=> (!a & !b)
                // Try expand first
                if let Expr::Unary { op: Operator::Not, .. } = &graph.inner[node] {
                    if let Some(inner) = graph.left(node) {
                        if let Expr::Binary { op, .. } = &graph.inner[inner] {
                            let to_op = match op {
                                Operator::And => Operator::Or,
                                Operator::Or => Operator::And,
                                _ => return false,
                            };
                            let a = graph.left(inner).unwrap();
                            let b = graph.right(inner).unwrap();
                            let ty = graph.ty(node).unwrap();

                            let not_a = graph.unary(Operator::Not, a, ty.clone());
                            let not_b = graph.unary(Operator::Not, b, ty.clone());
                            graph.binop(to_op, not_a, not_b, ty);
                            return true;
                        }
                    }
                }
                // Try contract
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    let to_op = match op {
                        Operator::And => Operator::Or,
                        Operator::Or => Operator::And,
                        _ => return false,
                    };
                    let left_is_not = graph
                        .left(node)
                        .map(|n| matches!(&graph.inner[n], Expr::Unary { op: Operator::Not, .. }))
                        .unwrap_or(false);
                    let right_is_not = graph
                        .right(node)
                        .map(|n| matches!(&graph.inner[n], Expr::Unary { op: Operator::Not, .. }))
                        .unwrap_or(false);

                    if left_is_not && right_is_not {
                        let left_not = graph.left(node).unwrap();
                        let right_not = graph.right(node).unwrap();
                        let a = graph.left(left_not).unwrap();
                        let b = graph.left(right_not).unwrap();
                        let ty = graph.ty(node).unwrap();

                        let inner = graph.binop(to_op, a, b, ty.clone());
                        graph.unary(Operator::Not, inner, ty);
                        return true;
                    }
                }
                false
            }

            RuleKind::ComplementXor => {
                // Bidirectional: (!a) <=> (a ^ 1)
                if let Expr::Unary { op: Operator::Not, .. } = &graph.inner[node] {
                    let operand = graph.left(node).unwrap();
                    let ty = graph.ty(node).unwrap();
                    let one = self.make_one_lit(graph, &ty);
                    graph.binop(Operator::Xor, operand, one, ty);
                    return true;
                }
                if let Expr::Binary { op: Operator::Xor, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if self.is_one(graph, right) {
                            let left = graph.left(node).unwrap();
                            let ty = graph.ty(node).unwrap();
                            graph.unary(Operator::Not, left, ty);
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::InjectRandomAddSub => {
                let Some(ty) = graph.ty(node) else { return false };
                let r = self.make_random_lit(graph, &ty, random);
                let add = graph.binop(Operator::Add, node, r, ty.clone());
                graph.binop(Operator::Sub, add, r, ty);
                true
            }

            RuleKind::InjectRandomSubAdd => {
                let Some(ty) = graph.ty(node) else { return false };
                let r = self.make_random_lit(graph, &ty, random);
                let sub = graph.binop(Operator::Sub, node, r, ty.clone());
                graph.binop(Operator::Add, sub, r, ty);
                true
            }

            RuleKind::InjectRandomMulDiv => {
                let Some(ty) = graph.ty(node) else { return false };
                let r = self.make_nonzero_lit(graph, &ty, random);
                let mul = graph.binop(Operator::Mul, node, r, ty.clone());
                graph.binop(Operator::Div, mul, r, ty);
                true
            }

            RuleKind::InjectRandomXorXor => {
                let Some(ty) = graph.ty(node) else { return false };
                let r = self.make_random_lit(graph, &ty, random);
                let xor1 = graph.binop(Operator::Xor, node, r, ty.clone());
                graph.binop(Operator::Xor, xor1, r, ty);
                true
            }

            RuleKind::DoubleMulTwo => {
                // Bidirectional: (a + a) <=> (a * 2)
                if let Expr::Binary { op: Operator::Add, .. } = &graph.inner[node] {
                    if graph.left(node) == graph.right(node) && graph.left(node).is_some() {
                        let ty = graph.ty(node).unwrap();
                        let two = self.make_two_lit(graph, &ty);
                        graph.set_op(node, Operator::Mul);
                        graph.replace_operand(node, 1, two);
                        return true;
                    }
                }
                if let Expr::Binary { op: Operator::Mul, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if self.is_two(graph, right) {
                            let left = graph.left(node).unwrap();
                            graph.set_op(node, Operator::Add);
                            graph.replace_operand(node, 1, left);
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::MulNegOneNeg => {
                // Bidirectional: (a * -1) <=> (-a)
                if let Expr::Binary { op: Operator::Mul, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if self.is_neg_one(graph, right) {
                            let left = graph.left(node).unwrap();
                            let ty = graph.ty(node).unwrap();
                            graph.unary(Operator::Neg, left, ty);
                            return true;
                        }
                    }
                }
                if let Expr::Unary { op: Operator::Neg, .. } = &graph.inner[node] {
                    let operand = graph.left(node).unwrap();
                    let ty = graph.ty(node).unwrap();
                    let neg_one = self.make_neg_one_lit(graph, &ty);
                    graph.binop(Operator::Mul, operand, neg_one, ty);
                    return true;
                }
                false
            }
        }
    }

    /// Check if a rule can be applied to a specific node (either direction for bidirectional rules)
    pub fn can_apply(&self, graph: &ExprGraph, node: NodeIndex, rule: &Rule) -> bool {
        match &rule.kind {
            RuleKind::SwapOperands { ops } => {
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    if !ops.contains(op) {
                        return false;
                    }
                    // Check operator is valid for the node's type
                    if let Some(ty) = graph.ty(node) {
                        return self.is_binary_op_valid_for_type(*op, &ty);
                    }
                }
                false
            }

            RuleKind::Associate { ops } => {
                // Bidirectional: can apply if left OR right child has same operator
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    if !ops.contains(op) {
                        return false;
                    }
                    // Check operator is valid for the node's type
                    if let Some(ty) = graph.ty(node) {
                        if !self.is_binary_op_valid_for_type(*op, &ty) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                    let left_matches = graph
                        .left(node)
                        .map(|l| matches!(&graph.inner[l], Expr::Binary { op: l_op, .. } if l_op == op))
                        .unwrap_or(false);
                    let right_matches = graph
                        .right(node)
                        .map(|r| matches!(&graph.inner[r], Expr::Binary { op: r_op, .. } if r_op == op))
                        .unwrap_or(false);
                    return left_matches || right_matches;
                }
                false
            }

            RuleKind::Distribute { outer, inner } => {
                // Bidirectional: distribute or factor
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    // Check operators are valid for the node's type
                    if let Some(ty) = graph.ty(node) {
                        if !self.is_binary_op_valid_for_type(*outer, &ty) ||
                            !self.is_binary_op_valid_for_type(*inner, &ty)
                        {
                            return false;
                        }
                    } else {
                        return false;
                    }
                    // Check distribute pattern
                    if op == outer {
                        if let Some(left) = graph.left(node) {
                            if let Expr::Binary { op: left_op, .. } = &graph.inner[left] {
                                if left_op == inner {
                                    return true;
                                }
                            }
                        }
                    }
                    // Check factor pattern
                    if op == inner {
                        let left = graph.left(node);
                        let right = graph.right(node);
                        if let (Some(l), Some(r)) = (left, right) {
                            if let (Expr::Binary { op: l_op, .. }, Expr::Binary { op: r_op, .. }) =
                                (&graph.inner[l], &graph.inner[r])
                            {
                                if l_op == outer && r_op == outer {
                                    let l_right = graph.right(l);
                                    let r_right = graph.right(r);
                                    if l_right == r_right && l_right.is_some() {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
                false
            }

            RuleKind::Identity { op, identity_right } => {
                // Bidirectional: can remove identity or add identity
                // Can remove if pattern matches
                if let Expr::Binary { op: node_op, .. } = &graph.inner[node] {
                    if node_op == op {
                        let check_node =
                            if *identity_right { graph.right(node) } else { graph.left(node) };
                        if let Some(n) = check_node {
                            if self.is_identity_for(graph, n, *op) {
                                return true;
                            }
                        }
                    }
                }
                // Can add identity only if operator is valid for the type
                if let Some(ty) = graph.ty(node) {
                    return self.is_binary_op_valid_for_type(*op, &ty);
                }
                false
            }

            RuleKind::Absorb { op } => {
                if let Expr::Binary { op: node_op, .. } = &graph.inner[node] {
                    if node_op != op {
                        return false;
                    }
                    if let Some(left) = graph.left(node) {
                        if self.is_absorbing_for(graph, left, *op) {
                            return true;
                        }
                    }
                    if let Some(right) = graph.right(node) {
                        if self.is_absorbing_for(graph, right, *op) {
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::SelfInverse { op } => {
                if let Expr::Binary { op: node_op, .. } = &graph.inner[node] {
                    if node_op != op {
                        return false;
                    }
                    graph.left(node) == graph.right(node) && graph.left(node).is_some()
                } else {
                    false
                }
            }

            RuleKind::Idempotent { op } => {
                // Bidirectional: can contract (a op a) or expand (for bools)
                if let Expr::Binary { op: node_op, .. } = &graph.inner[node] {
                    if node_op == op &&
                        graph.left(node) == graph.right(node) &&
                        graph.left(node).is_some()
                    {
                        return true;
                    }
                }
                // Can expand for boolean types
                graph.ty(node).map(|t| matches!(t, Type::Boolean(_))).unwrap_or(false)
            }

            RuleKind::DoubleUnary { op } => {
                // Bidirectional: can remove op(op(a)) or add double unary
                // First check if we can remove double unary
                if let Expr::Unary { op: outer_op, .. } = &graph.inner[node] {
                    if outer_op == op {
                        if let Some(inner) = graph.left(node) {
                            if let Expr::Unary { op: inner_op, .. } = &graph.inner[inner] {
                                if inner_op == op {
                                    return true;
                                }
                            }
                        }
                    }
                }
                // Can add double unary only if the unary op is valid for the type
                if let Some(ty) = graph.ty(node) {
                    return self.is_unary_op_valid_for_type(*op, &ty);
                }
                false
            }

            RuleKind::AddNegSub => {
                // Bidirectional: (a - b) <=> (a + (-b))
                // Requires Sub, Add, and Neg to be valid for the type
                if let Some(ty) = graph.ty(node) {
                    // Sub and Add need to be valid
                    if !self.is_binary_op_valid_for_type(Operator::Sub, &ty)
                        || !self.is_binary_op_valid_for_type(Operator::Add, &ty)
                    {
                        return false;
                    }
                    // Neg needs to be valid (only for signed integers)
                    if !self.supports_negation(&ty) {
                        return false;
                    }
                } else {
                    return false;
                }

                if matches!(&graph.inner[node], Expr::Binary { op: Operator::Sub, .. }) {
                    return true;
                }
                if let Expr::Binary { op: Operator::Add, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if matches!(&graph.inner[right], Expr::Unary { op: Operator::Neg, .. }) {
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::NegZeroSub => {
                // Bidirectional: (-a) <=> (0 - a)
                // Requires Neg and Sub to be valid for the type
                if let Some(ty) = graph.ty(node) {
                    if !self.supports_negation(&ty) {
                        return false;
                    }
                    if !self.is_binary_op_valid_for_type(Operator::Sub, &ty) {
                        return false;
                    }
                } else {
                    return false;
                }

                if matches!(&graph.inner[node], Expr::Unary { op: Operator::Neg, .. }) {
                    return true;
                }
                if let Expr::Binary { op: Operator::Sub, .. } = &graph.inner[node] {
                    if let Some(left) = graph.left(node) {
                        if self.is_zero(graph, left) {
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::FlipComparison => {
                // Only integers support ordered comparisons (<, <=, >, >=)
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    if matches!(
                        op,
                        Operator::Less
                            | Operator::Greater
                            | Operator::LessOrEqual
                            | Operator::GreaterOrEqual
                    ) {
                        // Check that ordered comparisons are valid for this type
                        if let Some(ty) = graph.ty(node) {
                            return self.supports_ordered_comparison(&ty);
                        }
                    }
                }
                false
            }

            RuleKind::NegateComparison => {
                // Bidirectional: can add or remove negation
                // Check type supports the comparison operators being used
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    if let Some(ty) = graph.ty(node) {
                        // Ordered comparisons only for integers
                        if matches!(
                            op,
                            Operator::Less
                                | Operator::Greater
                                | Operator::LessOrEqual
                                | Operator::GreaterOrEqual
                        ) {
                            if self.supports_ordered_comparison(&ty) {
                                return true;
                            }
                        }
                        // Equality comparisons for all types
                        if matches!(op, Operator::Equal | Operator::NotEqual) {
                            return true;
                        }
                    }
                }
                if let Expr::Unary { op: Operator::Not, .. } = &graph.inner[node] {
                    if let Some(inner) = graph.left(node) {
                        if let Expr::Binary { op, .. } = &graph.inner[inner] {
                            if let Some(ty) = graph.ty(inner) {
                                if matches!(
                                    op,
                                    Operator::Less
                                        | Operator::Greater
                                        | Operator::LessOrEqual
                                        | Operator::GreaterOrEqual
                                ) {
                                    if self.supports_ordered_comparison(&ty) {
                                        return true;
                                    }
                                }
                                if matches!(op, Operator::Equal | Operator::NotEqual) {
                                    return true;
                                }
                            }
                        }
                    }
                }
                false
            }

            RuleKind::DeMorgan => {
                // Bidirectional: expand !(a & b) or contract (!a | !b)
                // And/Or valid for Boolean and Integer types
                // Not valid for Boolean, Integer (unsigned), Field
                if let Expr::Unary { op: Operator::Not, .. } = &graph.inner[node] {
                    if let Some(inner) = graph.left(node) {
                        if let Expr::Binary { op, .. } = &graph.inner[inner] {
                            if matches!(op, Operator::And | Operator::Or) {
                                if let Some(ty) = graph.ty(node) {
                                    if self.supports_bitwise(&ty)
                                        && self.is_unary_op_valid_for_type(Operator::Not, &ty)
                                    {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
                if let Expr::Binary { op, .. } = &graph.inner[node] {
                    if matches!(op, Operator::And | Operator::Or) {
                        if let Some(ty) = graph.ty(node) {
                            if !self.supports_bitwise(&ty)
                                || !self.is_unary_op_valid_for_type(Operator::Not, &ty)
                            {
                                return false;
                            }
                        } else {
                            return false;
                        }
                        let left_is_not = graph
                            .left(node)
                            .map(|n| {
                                matches!(&graph.inner[n], Expr::Unary { op: Operator::Not, .. })
                            })
                            .unwrap_or(false);
                        let right_is_not = graph
                            .right(node)
                            .map(|n| {
                                matches!(&graph.inner[n], Expr::Unary { op: Operator::Not, .. })
                            })
                            .unwrap_or(false);
                        if left_is_not && right_is_not {
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::ComplementXor => {
                // Bidirectional: (!a) <=> (a ^ 1)
                // Not valid for Boolean, Integer (unsigned), Field
                // Xor valid for Boolean, Integer
                if let Some(ty) = graph.ty(node) {
                    if !self.is_unary_op_valid_for_type(Operator::Not, &ty)
                        || !self.is_binary_op_valid_for_type(Operator::Xor, &ty)
                    {
                        return false;
                    }
                } else {
                    return false;
                }

                if matches!(&graph.inner[node], Expr::Unary { op: Operator::Not, .. }) {
                    return true;
                }
                if let Expr::Binary { op: Operator::Xor, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if self.is_one(graph, right) {
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::InjectRandomAddSub => {
                // a -> ((a + r) - r) - requires Add and Sub
                if let Some(ty) = graph.ty(node) {
                    self.is_binary_op_valid_for_type(Operator::Add, &ty)
                        && self.is_binary_op_valid_for_type(Operator::Sub, &ty)
                } else {
                    false
                }
            }

            RuleKind::InjectRandomSubAdd => {
                // a -> ((a - r) + r) - requires Sub and Add
                if let Some(ty) = graph.ty(node) {
                    self.is_binary_op_valid_for_type(Operator::Sub, &ty)
                        && self.is_binary_op_valid_for_type(Operator::Add, &ty)
                } else {
                    false
                }
            }

            RuleKind::InjectRandomMulDiv => {
                // a -> ((a * r) / r) - requires Mul and Div
                if let Some(ty) = graph.ty(node) {
                    self.is_binary_op_valid_for_type(Operator::Mul, &ty)
                        && self.is_binary_op_valid_for_type(Operator::Div, &ty)
                } else {
                    false
                }
            }

            RuleKind::InjectRandomXorXor => {
                // a -> ((a ^ r) ^ r) - requires Xor
                if let Some(ty) = graph.ty(node) {
                    self.is_binary_op_valid_for_type(Operator::Xor, &ty)
                } else {
                    false
                }
            }

            RuleKind::DoubleMulTwo => {
                // Bidirectional: (a + a) <=> (a * 2) - requires Add and Mul
                if let Some(ty) = graph.ty(node) {
                    if !self.is_binary_op_valid_for_type(Operator::Add, &ty)
                        || !self.is_binary_op_valid_for_type(Operator::Mul, &ty)
                    {
                        return false;
                    }
                } else {
                    return false;
                }

                if let Expr::Binary { op: Operator::Add, .. } = &graph.inner[node] {
                    if graph.left(node) == graph.right(node) && graph.left(node).is_some() {
                        return true;
                    }
                }
                if let Expr::Binary { op: Operator::Mul, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if self.is_two(graph, right) {
                            return true;
                        }
                    }
                }
                false
            }

            RuleKind::MulNegOneNeg => {
                // Bidirectional: (a * -1) <=> (-a) - requires Mul and Neg
                if let Some(ty) = graph.ty(node) {
                    if !self.is_binary_op_valid_for_type(Operator::Mul, &ty)
                        || !self.supports_negation(&ty)
                    {
                        return false;
                    }
                } else {
                    return false;
                }

                if let Expr::Binary { op: Operator::Mul, .. } = &graph.inner[node] {
                    if let Some(right) = graph.right(node) {
                        if self.is_neg_one(graph, right) {
                            return true;
                        }
                    }
                }
                if matches!(&graph.inner[node], Expr::Unary { op: Operator::Neg, .. }) {
                    return true;
                }
                false
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // HELPER METHODS
    // ═══════════════════════════════════════════════════════════════════════════

    fn is_numeric_or_bool(&self, ty: &Type) -> bool {
        matches!(ty, Type::Field(_) | Type::Integer(_) | Type::Boolean(_))
    }

    fn is_zero(&self, graph: &ExprGraph, node: NodeIndex) -> bool {
        // For now, just check if it's a Field literal (simplified)
        matches!(&graph.inner[node], Expr::Literal(Type::Field(_)))
    }

    fn is_one(&self, graph: &ExprGraph, node: NodeIndex) -> bool {
        matches!(&graph.inner[node], Expr::Literal(Type::Field(_) | Type::Integer(_)))
    }

    fn is_two(&self, _graph: &ExprGraph, _node: NodeIndex) -> bool {
        false // Would need value tracking
    }

    fn is_neg_one(&self, _graph: &ExprGraph, _node: NodeIndex) -> bool {
        false // Would need value tracking
    }

    fn is_identity_for(&self, graph: &ExprGraph, node: NodeIndex, op: Operator) -> bool {
        match op {
            Operator::Add |
            Operator::Sub |
            Operator::Xor |
            Operator::Or |
            Operator::Shl |
            Operator::Shr => self.is_zero(graph, node),
            Operator::Mul | Operator::Div => self.is_one(graph, node),
            Operator::And => matches!(&graph.inner[node], Expr::Literal(Type::Boolean(_))),
            _ => false,
        }
    }

    fn is_absorbing_for(&self, graph: &ExprGraph, node: NodeIndex, op: Operator) -> bool {
        match op {
            Operator::Mul => self.is_zero(graph, node),
            Operator::And => matches!(&graph.inner[node], Expr::Literal(Type::Boolean(_))), /* false */
            Operator::Or => matches!(&graph.inner[node], Expr::Literal(Type::Boolean(_))),  // true
            _ => false,
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // TYPE-AWARE OPERATOR VALIDATION
    // Based on operators.rs type constraints
    // ═══════════════════════════════════════════════════════════════════════════

    /// Check if a binary operator is valid for a given type
    fn is_binary_op_valid_for_type(&self, op: Operator, ty: &Type) -> bool {
        match ty {
            Type::Field(_) => {
                // Field: arithmetic (+, -, *, /) and equality (==, !=)
                matches!(
                    op,
                    Operator::Add |
                        Operator::Sub |
                        Operator::Mul |
                        Operator::Div |
                        Operator::Equal |
                        Operator::NotEqual
                )
            }
            Type::Integer(int_ty) => {
                // Integer: arithmetic (+, -, *, /, %), comparisons, bitwise, shifts
                let is_arithmetic = matches!(
                    op,
                    Operator::Add | Operator::Sub | Operator::Mul | Operator::Div | Operator::Mod
                );
                let is_comparison = matches!(
                    op,
                    Operator::Less |
                        Operator::LessOrEqual |
                        Operator::Greater |
                        Operator::GreaterOrEqual |
                        Operator::Equal |
                        Operator::NotEqual
                );
                let is_bitwise = matches!(
                    op,
                    Operator::And | Operator::Or | Operator::Xor | Operator::Shl | Operator::Shr
                );
                is_arithmetic || is_comparison || is_bitwise
            }
            Type::Boolean(_) => {
                // Boolean: And, Or, Xor only
                matches!(op, Operator::And | Operator::Or | Operator::Xor)
            }
            _ => false,
        }
    }

    /// Check if a unary operator is valid for a given type
    fn is_unary_op_valid_for_type(&self, op: Operator, ty: &Type) -> bool {
        match ty {
            Type::Field(_) => {
                // Field: Not only (no Neg for unsigned field)
                matches!(op, Operator::Not)
            }
            Type::Integer(int_ty) => {
                // Signed integers: Neg and Not
                // Unsigned integers: Not only
                if int_ty.signed {
                    matches!(op, Operator::Neg | Operator::Not)
                } else {
                    matches!(op, Operator::Not)
                }
            }
            Type::Boolean(_) => {
                // Boolean: Not only
                matches!(op, Operator::Not)
            }
            _ => false,
        }
    }

    /// Check if comparison operators (other than == !=) are valid for a type
    fn supports_ordered_comparison(&self, ty: &Type) -> bool {
        // Only integers support <, <=, >, >=
        matches!(ty, Type::Integer(_))
    }

    /// Check if the type supports arithmetic operations
    fn supports_arithmetic(&self, ty: &Type) -> bool {
        matches!(ty, Type::Field(_) | Type::Integer(_))
    }

    /// Check if the type supports bitwise operations
    fn supports_bitwise(&self, ty: &Type) -> bool {
        matches!(ty, Type::Integer(_) | Type::Boolean(_))
    }

    /// Check if the type supports shift operations
    fn supports_shift(&self, ty: &Type) -> bool {
        matches!(ty, Type::Integer(_))
    }

    /// Check if the type supports negation (unary minus)
    fn supports_negation(&self, ty: &Type) -> bool {
        match ty {
            Type::Integer(int_ty) => int_ty.signed,
            _ => false,
        }
    }

    /// Check if the type supports modulo operation
    fn supports_modulo(&self, ty: &Type) -> bool {
        matches!(ty, Type::Integer(_))
    }

    /// Check if the type supports division
    fn supports_division(&self, ty: &Type) -> bool {
        matches!(ty, Type::Field(_) | Type::Integer(_))
    }

    fn make_zero_lit(&self, graph: &mut ExprGraph, ty: &Type) -> NodeIndex {
        graph.lit(ty.clone())
    }

    fn make_one_lit(&self, graph: &mut ExprGraph, ty: &Type) -> NodeIndex {
        graph.lit(ty.clone())
    }

    fn make_two_lit(&self, graph: &mut ExprGraph, ty: &Type) -> NodeIndex {
        graph.lit(ty.clone())
    }

    fn make_neg_one_lit(&self, graph: &mut ExprGraph, ty: &Type) -> NodeIndex {
        graph.lit(ty.clone())
    }

    fn make_identity_lit(&self, graph: &mut ExprGraph, ty: &Type, _op: Operator) -> NodeIndex {
        graph.lit(ty.clone())
    }

    fn make_absorbing_lit(&self, graph: &mut ExprGraph, ty: &Type, _op: Operator) -> NodeIndex {
        graph.lit(ty.clone())
    }

    fn make_random_lit(
        &self,
        graph: &mut ExprGraph,
        ty: &Type,
        _random: &mut impl Rng,
    ) -> NodeIndex {
        graph.lit(ty.clone())
    }

    fn make_nonzero_lit(
        &self,
        graph: &mut ExprGraph,
        ty: &Type,
        _random: &mut impl Rng,
    ) -> NodeIndex {
        graph.lit(ty.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swap_operands() {
        let mut graph = ExprGraph::new();
        let a = graph.lit(Type::Field(Field {}));
        let b_node = graph.lit(Type::Field(Field {}));
        let b = graph.var("custom_var".to_string(), Type::Field(Field {}), b_node);
        let add = graph.binop(Operator::Add, a, b, Type::Field(Field {}));

        let rewriter = Rewriter::equivalence();
        assert!(rewriter.can_apply(&graph, add, &rules::COMM_ADD));

        assert_eq!(graph.left(add), Some(a));
        assert_eq!(graph.right(add), Some(b));

        graph.save_dot("swap_operands_before.dot");

        let random = &mut rand::rng();
        rewriter.apply(&mut graph, add, &rules::COMM_ADD, random);

        assert_eq!(graph.left(add), Some(b));
        assert_eq!(graph.right(add), Some(a));

        graph.save_dot("swap_operands_after.dot");
    }

    #[test]
    fn test_rewrite_random() {
        let mut graph = ExprGraph::new();
        let a = graph.lit(Type::Field(Field {}));
        let b = graph.lit(Type::Field(Field {}));
        let c = graph.lit(Type::Field(Field {}));
        let _ab = graph.binop(Operator::Add, a, b, Type::Field(Field {}));
        let _bc = graph.binop(Operator::Mul, b, c, Type::Field(Field {}));

        graph.save_dot("rewrite_random_before.dot");

        let rewriter = Rewriter::equivalence();
        let random = &mut rand::rng();

        let applied = rewriter.rewrite(&mut graph, random, 1);
        println!("Applied {} rules", applied);

        graph.save_dot("rewrite_random_after.dot");
    }

    #[test]
    fn test_rewrite_expression_tree() {
        let random = &mut rand::rng();
        let mut graph = ExprGraph::new();

        // Build expression: ((a + b) * c) - (d & e)
        //
        //          Sub
        //         /   \
        //       Mul   And
        //      /  \   / \
        //    Add   c d   e
        //   /  \
        //  a    b

        let field_ty = Type::Field(Field {});
        let bool_ty = Type::Boolean(Boolean {});

        // Literals
        let a = graph.lit(field_ty.clone());
        let b = graph.lit(field_ty.clone());
        let c = graph.lit(field_ty.clone());
        let d = graph.lit(bool_ty.clone());
        let e = graph.lit(bool_ty.clone());

        // Build tree bottom-up
        let add_ab = graph.binop(Operator::Add, a, b, field_ty.clone());
        let mul = graph.binop(Operator::Mul, add_ab, c, field_ty.clone());
        let and_de = graph.binop(Operator::And, d, e, bool_ty.clone());
        let _sub = graph.binop(Operator::Sub, mul, and_de, field_ty.clone());

        // Assign some to variables
        let v0 = graph.next_var();
        graph.var(v0, field_ty.clone(), add_ab);

        println!("=== Before rewriting ===");
        println!("Nodes: {}, Edges: {}", graph.inner.node_count(), graph.inner.edge_count());
        graph.save_dot("rewrite_before.dot");

        // Apply random rules
        let rewriter = Rewriter::equivalence();
        let mut total_applied = 0;

        for i in 0..20 {
            if rewriter.apply_random(&mut graph, random) {
                total_applied += 1;
                println!(
                    "Iteration {}: applied a rule (total nodes: {})",
                    i,
                    graph.inner.node_count()
                );
            }
        }

        println!("\n=== After rewriting ===");
        println!("Applied {} rules total", total_applied);
        println!("Nodes: {}, Edges: {}", graph.inner.node_count(), graph.inner.edge_count());
        graph.save_dot("rewrite_after.dot");

        assert!(total_applied > 0, "Should have applied at least one rule");
    }

    #[test]
    fn test_specific_rules() {
        let random = &mut rand::rng();
        let mut graph = ExprGraph::new();
        let field_ty = Type::Field(Field {});

        // Test associativity: ((a + b) + c)
        let a = graph.lit(field_ty.clone());
        let b = graph.lit(field_ty.clone());
        let c = graph.lit(field_ty.clone());
        let ab = graph.binop(Operator::Add, a, b, field_ty.clone());
        let abc = graph.binop(Operator::Add, ab, c, field_ty.clone());

        graph.save_dot("specific_rules_before.dot");

        let rewriter = Rewriter::new(&[rules::ASSOC_ADD]);

        // Should be able to apply associativity (bidirectional)
        assert!(rewriter.can_apply(&graph, abc, &rules::ASSOC_ADD));

        // Apply it
        let applied = rewriter.apply(&mut graph, abc, &rules::ASSOC_ADD, random);
        assert!(applied, "Should have applied ASSOC_ADD");

        // Now the structure should be (a + (b + c))
        // The right child of abc should now be a binop
        if let Some(right) = graph.right(abc) {
            assert!(matches!(graph.inner[right], Expr::Binary { op: Operator::Add, .. }));
        }

        println!("Associativity test passed!");

        graph.save_dot("specific_rules_after.dot");
    }

    #[test]
    fn test_obfuscation_rules() {
        let random = &mut rand::rng();
        let mut graph = ExprGraph::new();
        let field_ty = Type::Field(Field {});

        // Simple expression: a
        let a = graph.lit(field_ty.clone());

        println!("=== Before obfuscation ===");
        println!("Nodes: {}", graph.inner.node_count());
        graph.save_dot("obfuscation_before.dot");

        // Apply obfuscation rules
        let rewriter = Rewriter::obfuscation();
        let applied = rewriter.rewrite(&mut graph, random, 5);

        println!("\n=== After obfuscation ===");
        println!("Applied {} obfuscation rules", applied);
        println!("Nodes: {}", graph.inner.node_count());

        // Obfuscation should increase node count
        assert!(graph.inner.node_count() > 1, "Obfuscation should add nodes");

        graph.save_dot("obfuscation_after.dot");

        // Verify the original node still exists
        assert!(graph.inner.node_indices().any(|n| n == a));
    }

    #[test]
    fn test_simplification_rules() {
        let random = &mut rand::rng();
        let mut graph = ExprGraph::new();
        let field_ty = Type::Field(Field {});

        // Build: !!a (double negation)
        let a = graph.lit(field_ty.clone());
        let not_a = graph.unary(Operator::Not, a, field_ty.clone());
        let not_not_a = graph.unary(Operator::Not, not_a, field_ty.clone());

        println!("=== Before simplification ===");
        println!("Nodes: {}", graph.inner.node_count());

        graph.save_dot("simplification_before.dot");

        let rewriter = Rewriter::simplification();

        // Check that DOUBLE_NOT can be applied (bidirectional - will remove double not)
        assert!(rewriter.can_apply(&graph, not_not_a, &rules::DOUBLE_NOT));

        // Apply simplification
        let applied = rewriter.apply(&mut graph, not_not_a, &rules::DOUBLE_NOT, random);
        assert!(applied);

        println!("\n=== After simplification ===");
        println!("Nodes: {}", graph.inner.node_count());

        graph.save_dot("simplification_after.dot");
    }
}
