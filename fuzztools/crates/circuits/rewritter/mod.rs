mod rules;

use crate::circuits::{
    ast::{Node, Operator},
    Forest, Type, TypeKind,
};
use petgraph::graph::NodeIndex;
use rand::Rng;
use std::collections::HashMap;
use strum::IntoEnumIterator;

use rules::Rule;

// ────────────────────────────────────────────────────────────────────────────────
// Rewriter
// ────────────────────────────────────────────────────────────────────────────────

pub struct Rewriter;

impl Rewriter {
    /// Scan `forest` and collect all applicable `(rule, idx)` pairs
    pub fn collect_applicable(forest: &Forest) -> HashMap<Rule, Vec<NodeIndex>> {
        let mut map: HashMap<Rule, Vec<NodeIndex>> = HashMap::new();

        for idx in forest.inner.node_indices() {
            for rule in Rule::iter() {
                if Self::can_apply(forest, idx, rule) {
                    map.entry(rule).or_default().push(idx);
                }
            }
        }

        map
    }

    pub fn can_apply(forest: &Forest, idx: NodeIndex, rule: Rule) -> bool {
        match rule {
            // Structural
            Rule::SwapOperands => can_swap_operands(forest, idx),
            Rule::Associate => can_associate(forest, idx),
            Rule::AssociateSub => can_associate_sub(forest, idx),
            Rule::AssociateDiv => can_associate_div(forest, idx),
            Rule::DivCommute => can_div_commute(forest, idx),
            Rule::DistributeMulAdd => can_distribute_mul_add(forest, idx),
            Rule::DistributeMulSub => can_distribute_mul_sub(forest, idx),
            Rule::DistributeAndOr => can_distribute_and_or(forest, idx),
            Rule::DistributeOrAnd => can_distribute_or_and(forest, idx),

            // Identity
            Rule::IdentityAdd => can_identity(forest, idx, Operator::Add, "0"),
            Rule::IdentitySub => can_identity_sub(forest, idx),
            Rule::IdentityMul => can_identity(forest, idx, Operator::Mul, "1"),
            Rule::IdentityDiv => can_identity_right(forest, idx, Operator::Div, "1"),
            Rule::IdentityXor => can_identity(forest, idx, Operator::Xor, "0"),
            Rule::IdentityOr => can_identity(forest, idx, Operator::Or, "0"),
            Rule::IdentityAnd => can_identity_and(forest, idx),
            Rule::IdentityShl => can_identity_right(forest, idx, Operator::Shl, "0"),
            Rule::IdentityShr => can_identity_right(forest, idx, Operator::Shr, "0"),

            // Absorb
            Rule::AbsorbMul => can_absorb(forest, idx, Operator::Mul, "0"),
            Rule::AbsorbAnd => can_absorb(forest, idx, Operator::And, "0"),
            Rule::AbsorbOr => can_absorb_or(forest, idx),

            // Self-inverse
            Rule::SelfInverseSub => can_self_inverse(forest, idx, Operator::Sub),
            Rule::SelfInverseXor => can_self_inverse(forest, idx, Operator::Xor),
            Rule::SelfInverseDiv => can_self_inverse(forest, idx, Operator::Div),

            // Idempotent
            Rule::IdempotentAnd => can_idempotent(forest, idx, Operator::And),
            Rule::IdempotentOr => can_idempotent(forest, idx, Operator::Or),

            // Unary
            Rule::DoubleNeg => can_double_unary(forest, idx, Operator::Neg),
            Rule::DoubleNot => can_double_unary(forest, idx, Operator::Not),
            Rule::AddNegSub => can_add_neg_sub(forest, idx),
            Rule::NegZeroSub => can_neg_zero_sub(forest, idx),

            // Comparison
            Rule::FlipComparison => can_flip_comparison(forest, idx),
            Rule::NegateComparison => can_negate_comparison(forest, idx),
            Rule::ExpandComparison => can_expand_comparison(forest, idx),

            // Boolean logic
            Rule::DeMorgan => can_de_morgan(forest, idx),
            Rule::ComplementXor => can_complement_xor(forest, idx),
            Rule::XorToAndOr => can_xor_to_and_or(forest, idx),

            // Modulo
            Rule::ModOne => can_mod_one(forest, idx),
            Rule::AndToMod => can_and_to_mod(forest, idx),

            // Shift
            Rule::ShiftZero => can_shift_zero(forest, idx),

            // Obfuscation (always applicable to matching types)
            Rule::InjectAddSub => can_inject_add_sub(forest, idx),
            Rule::InjectSubAdd => can_inject_add_sub(forest, idx),
            Rule::InjectMulDiv => can_inject_mul_div(forest, idx),
            Rule::InjectXorXor => can_inject_xor(forest, idx),
            Rule::InjectDivDiv => can_inject_div_div(forest, idx),
            Rule::InjectOrZero => can_inject_or_zero(forest, idx),
            Rule::InjectAndSelf => can_inject_and_self(forest, idx),

            // Simplification
            Rule::DoubleMulTwo => can_double_mul_two(forest, idx),
            Rule::MulNegOneNeg => can_mul_neg_one(forest, idx),
        }
    }

    pub fn apply(
        forest: &mut Forest,
        idx: NodeIndex,
        rule: Rule,
        rng: &mut impl Rng,
    ) {
        match rule {
            // Structural
            Rule::SwapOperands => do_swap_operands(forest, idx),
            Rule::Associate => do_associate(forest, idx),
            Rule::AssociateSub => do_associate_sub(forest, idx),
            Rule::AssociateDiv => do_associate_div(forest, idx),
            Rule::DivCommute => do_div_commute(forest, idx),
            Rule::DistributeMulAdd => do_distribute_mul_add(forest, idx),
            Rule::DistributeMulSub => do_distribute_mul_sub(forest, idx),
            Rule::DistributeAndOr => do_distribute_and_or(forest, idx),
            Rule::DistributeOrAnd => do_distribute_or_and(forest, idx),

            // Identity
            Rule::IdentityAdd => do_identity(forest, idx, Operator::Add, "0"),
            Rule::IdentitySub => do_identity_sub(forest, idx),
            Rule::IdentityMul => do_identity(forest, idx, Operator::Mul, "1"),
            Rule::IdentityDiv => do_identity_right(forest, idx, Operator::Div, "1"),
            Rule::IdentityXor => do_identity(forest, idx, Operator::Xor, "0"),
            Rule::IdentityOr => do_identity(forest, idx, Operator::Or, "0"),
            Rule::IdentityAnd => do_identity_and(forest, idx),
            Rule::IdentityShl => do_identity_right(forest, idx, Operator::Shl, "0"),
            Rule::IdentityShr => do_identity_right(forest, idx, Operator::Shr, "0"),

            // Absorb
            Rule::AbsorbMul => do_absorb(forest, idx, "0"),
            Rule::AbsorbAnd => do_absorb(forest, idx, "0"),
            Rule::AbsorbOr => do_absorb(forest, idx, "true"),

            // Self-inverse
            Rule::SelfInverseSub => do_self_inverse(forest, idx, "0"),
            Rule::SelfInverseXor => do_self_inverse(forest, idx, "0"),
            Rule::SelfInverseDiv => do_self_inverse(forest, idx, "1"),

            // Idempotent
            Rule::IdempotentAnd => do_idempotent(forest, idx),
            Rule::IdempotentOr => do_idempotent(forest, idx),

            // Unary
            Rule::DoubleNeg => do_double_unary(forest, idx),
            Rule::DoubleNot => do_double_unary(forest, idx),
            Rule::AddNegSub => do_add_neg_sub(forest, idx),
            Rule::NegZeroSub => do_neg_zero_sub(forest, idx),

            // Comparison
            Rule::FlipComparison => do_flip_comparison(forest, idx),
            Rule::NegateComparison => do_negate_comparison(forest, idx),
            Rule::ExpandComparison => do_expand_comparison(forest, idx),

            // Boolean logic
            Rule::DeMorgan => do_de_morgan(forest, idx),
            Rule::ComplementXor => do_complement_xor(forest, idx),
            Rule::XorToAndOr => do_xor_to_and_or(forest, idx),

            // Modulo
            Rule::ModOne => do_mod_one(forest, idx),
            Rule::AndToMod => do_and_to_mod(forest, idx),

            // Shift
            Rule::ShiftZero => do_shift_zero(forest, idx),

            // Obfuscation
            Rule::InjectAddSub => do_inject_add_sub(forest, idx, rng),
            Rule::InjectSubAdd => do_inject_sub_add(forest, idx, rng),
            Rule::InjectMulDiv => do_inject_mul_div(forest, idx, rng),
            Rule::InjectXorXor => do_inject_xor_xor(forest, idx, rng),
            Rule::InjectDivDiv => do_inject_div_div(forest, idx, rng),
            Rule::InjectOrZero => do_inject_or_zero(forest, idx),
            Rule::InjectAndSelf => do_inject_and_self(forest, idx),

            // Simplification
            Rule::DoubleMulTwo => do_double_mul_two(forest, idx),
            Rule::MulNegOneNeg => do_mul_neg_one(forest, idx),
        };
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Helper: Node inspection
// ════════════════════════════════════════════════════════════════════════════════

fn get_op(forest: &Forest, idx: NodeIndex) -> Option<Operator> {
    match &forest.inner[idx] {
        Node::Operator { op, .. } => Some(*op),
        _ => None,
    }
}

fn get_ty(forest: &Forest, idx: NodeIndex) -> Type {
    forest.inner[idx].ty()
}

fn is_literal(forest: &Forest, idx: NodeIndex, value: &str) -> bool {
    matches!(&forest.inner[idx], Node::Literal { value: v, .. } if v == value)
}

fn is_binary(forest: &Forest, idx: NodeIndex) -> bool {
    forest.right(idx).is_some()
}

fn same_node(forest: &Forest, a: NodeIndex, b: NodeIndex) -> bool {
    // Check if two nodes represent the same value (same variable/input name or same literal)
    match (&forest.inner[a], &forest.inner[b]) {
        (Node::Input { name: n1, .. }, Node::Input { name: n2, .. }) => n1 == n2,
        (Node::Variable { name: n1, .. }, Node::Variable { name: n2, .. }) => n1 == n2,
        (Node::Literal { value: v1, .. }, Node::Literal { value: v2, .. }) => v1 == v2,
        _ => a == b,
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: SwapOperands
// (a op b) <-> (b op a)
// ════════════════════════════════════════════════════════════════════════════════

fn can_swap_operands(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };
    is_binary(forest, idx) && op.is_commutative()
}

fn do_swap_operands(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    forest.swap_edges(idx);
    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: Associate
// ((a op b) op c) <-> (a op (b op c))
// ════════════════════════════════════════════════════════════════════════════════

fn can_associate(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };
    if !op.is_associative() {
        return false;
    }

    // Check left child has same op (left-associative form)
    let left_match = forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(op));
    // Check right child has same op (right-associative form)
    let right_match = forest.right(idx).is_some_and(|r| get_op(forest, r) == Some(op));

    left_match || right_match
}

fn do_associate(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let ty = get_ty(forest, idx);
    let left = forest.left(idx)?;
    let right = forest.right(idx)?;

    // Detect form and transform
    if get_op(forest, left) == Some(op) {
        // Left-associative: ((a op b) op c) -> (a op (b op c))
        let a = forest.left(left)?;
        let b = forest.right(left)?;
        let c = right;

        let new_right = forest.operator(op, &ty, b, Some(c));
        forest.replace_binary(idx, a, new_right);
    } else {
        // Right-associative: (a op (b op c)) -> ((a op b) op c)
        let a = left;
        let b = forest.left(right)?;
        let c = forest.right(right)?;

        let new_left = forest.operator(op, &ty, a, Some(b));
        forest.replace_binary(idx, new_left, c);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: AssociateSub
// ((a - b) - c) <-> (a - (b + c))
// ════════════════════════════════════════════════════════════════════════════════

fn can_associate_sub(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(Operator::Sub) = get_op(forest, idx) else { return false };
    let left = forest.left(idx);
    let right = forest.right(idx);

    // Form 1: ((a - b) - c) - left child is Sub
    let form1 = left.is_some_and(|l| get_op(forest, l) == Some(Operator::Sub));
    // Form 2: (a - (b + c)) - right child is Add
    let form2 = right.is_some_and(|r| get_op(forest, r) == Some(Operator::Add));

    form1 || form2
}

fn do_associate_sub(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let left = forest.left(idx)?;
    let right = forest.right(idx)?;

    if get_op(forest, left) == Some(Operator::Sub) {
        // ((a - b) - c) -> (a - (b + c))
        let a = forest.left(left)?;
        let b = forest.right(left)?;
        let c = right;

        let sum = forest.operator(Operator::Add, &ty, b, Some(c));
        forest.replace_binary(idx, a, sum);
    } else {
        // (a - (b + c)) -> ((a - b) - c)
        let a = left;
        let b = forest.left(right)?;
        let c = forest.right(right)?;

        let diff = forest.operator(Operator::Sub, &ty, a, Some(b));
        forest.replace_binary(idx, diff, c);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: AssociateDiv
// ((a / b) * c) <-> ((a * c) / b)
// ════════════════════════════════════════════════════════════════════════════════

fn can_associate_div(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };
    let left = forest.left(idx);

    // Form 1: ((a / b) * c) - Mul at root, Div on left
    if op == Operator::Mul {
        return left.is_some_and(|l| get_op(forest, l) == Some(Operator::Div));
    }

    // Form 2: ((a * c) / b) - Div at root, Mul on left
    if op == Operator::Div {
        return left.is_some_and(|l| get_op(forest, l) == Some(Operator::Mul));
    }

    false
}

fn do_associate_div(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;
    let left = forest.left(idx)?;
    let right = forest.right(idx)?;
    let a = forest.left(left)?;

    if op == Operator::Mul {
        // ((a / b) * c) -> ((a * c) / b)
        let b = forest.right(left)?;
        let c = right;

        let mul = forest.operator(Operator::Mul, &ty, a, Some(c));
        forest.replace_binary(idx, mul, b);
        forest.set_op(idx, Operator::Div);
    } else {
        // ((a * c) / b) -> ((a / b) * c)
        let a = forest.left(left)?;
        let c = forest.right(left)?;
        let b = right;

        let div = forest.operator(Operator::Div, &ty, a, Some(b));
        forest.replace_binary(idx, div, c);
        forest.set_op(idx, Operator::Mul);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: DivCommute
// (a / b) <-> ((1 / b) * a)
// ════════════════════════════════════════════════════════════════════════════════

fn can_div_commute(forest: &Forest, idx: NodeIndex) -> bool {
    let op = get_op(forest, idx);

    // Form 1: (a / b)
    if op == Some(Operator::Div) {
        return true;
    }

    // Form 2: ((1 / b) * a) where left is (1 / b)
    if op == Some(Operator::Mul) {
        if let Some(left) = forest.left(idx) {
            if get_op(forest, left) == Some(Operator::Div) {
                if let Some(ll) = forest.left(left) {
                    return is_literal(forest, ll, "1");
                }
            }
        }
    }

    false
}

fn do_div_commute(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;

    if op == Operator::Div {
        // (a / b) -> ((1 / b) * a)
        let a = forest.left(idx)?;
        let b = forest.right(idx)?;

        let one = forest.literal("1".to_string(), &ty);
        let inv = forest.operator(Operator::Div, &ty, one, Some(b));
        forest.replace_binary(idx, inv, a);
        forest.set_op(idx, Operator::Mul);
    } else {
        // ((1 / b) * a) -> (a / b)
        let left = forest.left(idx)?;
        let a = forest.right(idx)?;
        let b = forest.right(left)?;

        forest.replace_binary(idx, a, b);
        forest.set_op(idx, Operator::Div);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: DistributeMulAdd
// (a + b) * c <-> (a * c) + (b * c)
// ════════════════════════════════════════════════════════════════════════════════

fn can_distribute_mul_add(forest: &Forest, idx: NodeIndex) -> bool {
    let op = get_op(forest, idx);

    // Form 1: (a + b) * c - Mul with left child being Add
    if op == Some(Operator::Mul) {
        return forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::Add));
    }

    // Form 2: (a * c) + (b * c) - Add with both children being Mul
    if op == Some(Operator::Add) {
        let left_mul = forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::Mul));
        let right_mul = forest.right(idx).is_some_and(|r| get_op(forest, r) == Some(Operator::Mul));
        return left_mul && right_mul;
    }

    false
}

fn do_distribute_mul_add(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;
    let left = forest.left(idx)?;

    if op == Operator::Mul {
        // (a + b) * c -> (a * c) + (b * c)
        let c = forest.right(idx)?;
        let a = forest.left(left)?;
        let b = forest.right(left)?;

        let ac = forest.operator(Operator::Mul, &ty, a, Some(c));
        let bc = forest.operator(Operator::Mul, &ty, b, Some(c));
        forest.replace_binary(idx, ac, bc);
        forest.set_op(idx, Operator::Add);
    } else {
        // (a * c) + (b * c) -> (a + b) * c (factor out common term)
        // This assumes right operands are the same
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = forest.left(left)?;
        let c = forest.right(left)?;
        let b = forest.left(right)?;

        let sum = forest.operator(Operator::Add, &ty, a, Some(b));
        forest.replace_binary(idx, sum, c);
        forest.set_op(idx, Operator::Mul);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: DistributeMulSub
// (a - b) * c <-> (a * c) - (b * c)
// ════════════════════════════════════════════════════════════════════════════════

fn can_distribute_mul_sub(forest: &Forest, idx: NodeIndex) -> bool {
    let op = get_op(forest, idx);

    if op == Some(Operator::Mul) {
        return forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::Sub));
    }

    if op == Some(Operator::Sub) {
        let left_mul = forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::Mul));
        let right_mul = forest.right(idx).is_some_and(|r| get_op(forest, r) == Some(Operator::Mul));
        return left_mul && right_mul;
    }

    false
}

fn do_distribute_mul_sub(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;
    let left = forest.left(idx)?;

    if op == Operator::Mul {
        let c = forest.right(idx)?;
        let a = forest.left(left)?;
        let b = forest.right(left)?;

        let ac = forest.operator(Operator::Mul, &ty, a, Some(c));
        let bc = forest.operator(Operator::Mul, &ty, b, Some(c));
        forest.replace_binary(idx, ac, bc);
        forest.set_op(idx, Operator::Sub);
    } else {
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = forest.left(left)?;
        let c = forest.right(left)?;
        let b = forest.left(right)?;

        let diff = forest.operator(Operator::Sub, &ty, a, Some(b));
        forest.replace_binary(idx, diff, c);
        forest.set_op(idx, Operator::Mul);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: DistributeAndOr
// (a | b) & c <-> (a & c) | (b & c)
// ════════════════════════════════════════════════════════════════════════════════

fn can_distribute_and_or(forest: &Forest, idx: NodeIndex) -> bool {
    let op = get_op(forest, idx);

    if op == Some(Operator::And) {
        return forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::Or));
    }

    if op == Some(Operator::Or) {
        let left_and = forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::And));
        let right_and = forest.right(idx).is_some_and(|r| get_op(forest, r) == Some(Operator::And));
        return left_and && right_and;
    }

    false
}

fn do_distribute_and_or(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;
    let left = forest.left(idx)?;

    if op == Operator::And {
        let c = forest.right(idx)?;
        let a = forest.left(left)?;
        let b = forest.right(left)?;

        let ac = forest.operator(Operator::And, &ty, a, Some(c));
        let bc = forest.operator(Operator::And, &ty, b, Some(c));
        forest.replace_binary(idx, ac, bc);
        forest.set_op(idx, Operator::Or);
    } else {
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = forest.left(left)?;
        let c = forest.right(left)?;
        let b = forest.left(right)?;

        let or_node = forest.operator(Operator::Or, &ty, a, Some(b));
        forest.replace_binary(idx, or_node, c);
        forest.set_op(idx, Operator::And);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Structural: DistributeOrAnd
// (a & b) | c <-> (a | c) & (b | c)
// ════════════════════════════════════════════════════════════════════════════════

fn can_distribute_or_and(forest: &Forest, idx: NodeIndex) -> bool {
    let op = get_op(forest, idx);

    if op == Some(Operator::Or) {
        return forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::And));
    }

    if op == Some(Operator::And) {
        let left_or = forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::Or));
        let right_or = forest.right(idx).is_some_and(|r| get_op(forest, r) == Some(Operator::Or));
        return left_or && right_or;
    }

    false
}

fn do_distribute_or_and(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;
    let left = forest.left(idx)?;

    if op == Operator::Or {
        let c = forest.right(idx)?;
        let a = forest.left(left)?;
        let b = forest.right(left)?;

        let ac = forest.operator(Operator::Or, &ty, a, Some(c));
        let bc = forest.operator(Operator::Or, &ty, b, Some(c));
        forest.replace_binary(idx, ac, bc);
        forest.set_op(idx, Operator::And);
    } else {
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = forest.left(left)?;
        let c = forest.right(left)?;
        let b = forest.left(right)?;

        let and_node = forest.operator(Operator::And, &ty, a, Some(b));
        forest.replace_binary(idx, and_node, c);
        forest.set_op(idx, Operator::Or);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Identity rules: (a op identity) <-> a
// ════════════════════════════════════════════════════════════════════════════════

fn can_identity(forest: &Forest, idx: NodeIndex, op: Operator, identity: &str) -> bool {
    if get_op(forest, idx) != Some(op) {
        return false;
    }

    let left = forest.left(idx);
    let right = forest.right(idx);

    left.is_some_and(|l| is_literal(forest, l, identity)) ||
        right.is_some_and(|r| is_literal(forest, r, identity))
}

fn do_identity(
    forest: &Forest,
    idx: NodeIndex,
    _op: Operator,
    identity: &str,
) -> Option<NodeIndex> {
    let left = forest.left(idx)?;
    let right = forest.right(idx)?;

    // Simplify: return the non-identity operand
    if is_literal(forest, left, identity) {
        Some(right)
    } else {
        Some(left)
    }
}

fn can_identity_sub(forest: &Forest, idx: NodeIndex) -> bool {
    if get_op(forest, idx) != Some(Operator::Sub) {
        return false;
    }
    forest.right(idx).is_some_and(|r| is_literal(forest, r, "0"))
}

fn do_identity_sub(forest: &Forest, idx: NodeIndex) -> Option<NodeIndex> {
    forest.left(idx)
}

fn can_identity_right(forest: &Forest, idx: NodeIndex, op: Operator, identity: &str) -> bool {
    if get_op(forest, idx) != Some(op) {
        return false;
    }
    forest.right(idx).is_some_and(|r| is_literal(forest, r, identity))
}

fn do_identity_right(
    forest: &Forest,
    idx: NodeIndex,
    _op: Operator,
    _identity: &str,
) -> Option<NodeIndex> {
    forest.left(idx)
}

fn can_identity_and(forest: &Forest, idx: NodeIndex) -> bool {
    if get_op(forest, idx) != Some(Operator::And) {
        return false;
    }

    let ty = get_ty(forest, idx);
    if ty != Type::Bool {
        return false;
    }

    let left = forest.left(idx);
    let right = forest.right(idx);

    left.is_some_and(|l| is_literal(forest, l, "true")) ||
        right.is_some_and(|r| is_literal(forest, r, "true"))
}

fn do_identity_and(forest: &Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let left = forest.left(idx)?;
    let right = forest.right(idx)?;

    if is_literal(forest, left, "true") {
        Some(right)
    } else {
        Some(left)
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Absorb rules: (a op absorb) <-> absorb
// ════════════════════════════════════════════════════════════════════════════════

fn can_absorb(forest: &Forest, idx: NodeIndex, op: Operator, absorb: &str) -> bool {
    if get_op(forest, idx) != Some(op) {
        return false;
    }

    let left = forest.left(idx);
    let right = forest.right(idx);

    left.is_some_and(|l| is_literal(forest, l, absorb)) ||
        right.is_some_and(|r| is_literal(forest, r, absorb))
}

fn do_absorb(forest: &mut Forest, idx: NodeIndex, value: &str) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    Some(forest.literal(value.to_string(), &ty))
}

fn can_absorb_or(forest: &Forest, idx: NodeIndex) -> bool {
    if get_op(forest, idx) != Some(Operator::Or) {
        return false;
    }

    let ty = get_ty(forest, idx);
    if ty != Type::Bool {
        return false;
    }

    let left = forest.left(idx);
    let right = forest.right(idx);

    left.is_some_and(|l| is_literal(forest, l, "true")) ||
        right.is_some_and(|r| is_literal(forest, r, "true"))
}

// ════════════════════════════════════════════════════════════════════════════════
// Self-inverse: (a op a) <-> result
// ════════════════════════════════════════════════════════════════════════════════

fn can_self_inverse(forest: &Forest, idx: NodeIndex, op: Operator) -> bool {
    if get_op(forest, idx) != Some(op) {
        return false;
    }

    let left = forest.left(idx);
    let right = forest.right(idx);

    match (left, right) {
        (Some(l), Some(r)) => same_node(forest, l, r),
        _ => false,
    }
}

fn do_self_inverse(forest: &mut Forest, idx: NodeIndex, result: &str) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    Some(forest.literal(result.to_string(), &ty))
}

// ════════════════════════════════════════════════════════════════════════════════
// Idempotent: (a op a) <-> a
// ════════════════════════════════════════════════════════════════════════════════

fn can_idempotent(forest: &Forest, idx: NodeIndex, op: Operator) -> bool {
    if get_op(forest, idx) != Some(op) {
        return false;
    }

    let left = forest.left(idx);
    let right = forest.right(idx);

    match (left, right) {
        (Some(l), Some(r)) => same_node(forest, l, r),
        _ => false,
    }
}

fn do_idempotent(forest: &Forest, idx: NodeIndex) -> Option<NodeIndex> {
    forest.left(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Unary: DoubleNeg/DoubleNot
// --a <-> a, !!a <-> a
// ════════════════════════════════════════════════════════════════════════════════

fn can_double_unary(forest: &Forest, idx: NodeIndex, op: Operator) -> bool {
    if get_op(forest, idx) != Some(op) {
        return false;
    }

    // Must be unary
    if is_binary(forest, idx) {
        return false;
    }

    // Inner must be same unary op
    forest
        .left(idx)
        .is_some_and(|inner| get_op(forest, inner) == Some(op) && !is_binary(forest, inner))
}

fn do_double_unary(forest: &Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let inner = forest.left(idx)?;
    forest.left(inner)
}

// ════════════════════════════════════════════════════════════════════════════════
// Unary: AddNegSub
// (a - b) <-> (a + (-b))
// ════════════════════════════════════════════════════════════════════════════════

fn can_add_neg_sub(forest: &Forest, idx: NodeIndex) -> bool {
    let op = get_op(forest, idx);

    // Form 1: (a - b)
    if op == Some(Operator::Sub) && is_binary(forest, idx) {
        return true;
    }

    // Form 2: (a + (-b)) where right is unary Neg
    if op == Some(Operator::Add) {
        return forest
            .right(idx)
            .is_some_and(|r| get_op(forest, r) == Some(Operator::Neg) && !is_binary(forest, r));
    }

    false
}

fn do_add_neg_sub(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;
    let a = forest.left(idx)?;

    if op == Operator::Sub {
        // (a - b) -> (a + (-b))
        let b = forest.right(idx)?;

        let neg_b = forest.operator(Operator::Neg, &ty, b, None);
        forest.replace_binary(idx, a, neg_b);
        forest.set_op(idx, Operator::Add);
    } else {
        // (a + (-b)) -> (a - b)
        let a = forest.left(idx)?;
        let neg_b = forest.right(idx)?;
        let b = forest.left(neg_b)?;

        forest.replace_binary(idx, a, b);
        forest.set_op(idx, Operator::Sub);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Unary: NegZeroSub
// (-a) <-> (0 - a)
// ════════════════════════════════════════════════════════════════════════════════

fn can_neg_zero_sub(forest: &Forest, idx: NodeIndex) -> bool {
    let op = get_op(forest, idx);

    // Form 1: unary (-a)
    if op == Some(Operator::Neg) && !is_binary(forest, idx) {
        return true;
    }

    // Form 2: (0 - a)
    if op == Some(Operator::Sub) {
        return forest.left(idx).is_some_and(|l| is_literal(forest, l, "0"));
    }

    false
}

fn do_neg_zero_sub(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let op = get_op(forest, idx)?;

    if op == Operator::Neg {
        // (-a) -> (0 - a)
        let a = forest.left(idx)?;
        let zero = forest.literal("0".to_string(), &ty);

        forest.replace_binary(idx, zero, a);
        forest.set_op(idx, Operator::Sub);
    } else {
        // (0 - a) -> (-a)
        let a = forest.right(idx)?;

        forest.replace_unary(idx, a);
        forest.set_op(idx, Operator::Neg);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Comparison: FlipComparison
// (a < b) <-> (b > a)
// ════════════════════════════════════════════════════════════════════════════════

fn can_flip_comparison(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };
    op.is_comparison() && is_binary(forest, idx)
}

fn do_flip_comparison(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let flipped = op.flip_comparison()?;

    forest.swap_edges(idx);
    forest.set_op(idx, flipped);

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Comparison: NegateComparison
// (a < b) <-> !(a >= b)
// ════════════════════════════════════════════════════════════════════════════════

fn can_negate_comparison(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    // Form 1: comparison
    if op.is_comparison() {
        return true;
    }

    // Form 2: !(comparison)
    if op == Operator::Not && !is_binary(forest, idx) {
        return forest
            .left(idx)
            .is_some_and(|inner| get_op(forest, inner).is_some_and(|iop| iop.is_comparison()));
    }

    false
}

fn do_negate_comparison(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;

    if op.is_comparison() {
        // (a < b) -> !(a >= b)
        let negated = op.negate_comparison()?;
        forest.set_op(idx, negated);

        // Create NOT wrapper manually to avoid consuming idx through add_edge
        let ty = Type::Bool;
        let not_node = forest.inner.add_node(Node::Operator { op: Operator::Not, ty: ty.clone() });

        // Register the NOT node in types/kinds
        forest.types.entry(ty.clone()).or_default().push(not_node);
        forest.kinds.entry(ty.kind()).or_default().push(not_node);
        forest.operators.entry(Operator::Not).or_default().push(not_node);

        // Add edge directly without triggering single-use removal
        forest.inner.add_edge(not_node, idx, 0);

        Some(not_node)
    } else {
        // !(a >= b) -> (a < b)
        let inner = forest.left(idx)?;
        let inner_op = get_op(forest, inner)?;
        let negated = inner_op.negate_comparison()?;

        forest.set_op(inner, negated);

        // Remove the NOT wrapper
        let ty = Type::Bool;
        if let Some(v) = forest.types.get_mut(&ty) { v.retain(|&x| x != idx) }
        if let Some(v) = forest.kinds.get_mut(&ty.kind()) { v.retain(|&x| x != idx) }
        if let Some(v) = forest.operators.get_mut(&Operator::Not) { v.retain(|&x| x != idx) }
        forest.inner.remove_node(idx);

        Some(inner)
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Comparison: ExpandComparison
// (a <= b) <-> ((a < b) || (a == b))
// ════════════════════════════════════════════════════════════════════════════════

fn can_expand_comparison(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    // Form 1: <= or >=
    if op == Operator::LessOrEqual || op == Operator::GreaterOrEqual {
        return true;
    }

    // Form 2: ((a < b) || (a == b)) or ((a > b) || (a == b))
    if op == Operator::Or {
        let left = forest.left(idx);
        let right = forest.right(idx);

        let left_cmp = left.and_then(|l| get_op(forest, l));
        let right_eq = right.and_then(|r| get_op(forest, r)) == Some(Operator::Equal);

        return right_eq &&
            (left_cmp == Some(Operator::Less) || left_cmp == Some(Operator::Greater));
    }

    false
}

fn do_expand_comparison(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;

    if op == Operator::LessOrEqual || op == Operator::GreaterOrEqual {
        // (a <= b) -> ((a < b) || (a == b))
        let a = forest.left(idx)?;
        let b = forest.right(idx)?;
        let ty = Type::Bool;

        let strict_op =
            if op == Operator::LessOrEqual { Operator::Less } else { Operator::Greater };
        let strict = forest.operator(strict_op, &ty, a, Some(b));
        let eq = forest.operator(Operator::Equal, &ty, a, Some(b));

        forest.replace_binary(idx, strict, eq);
        forest.set_op(idx, Operator::Or);
    } else {
        // ((a < b) || (a == b)) -> (a <= b)
        let left = forest.left(idx)?;
        let left_op = get_op(forest, left)?;
        let a = forest.left(left)?;
        let b = forest.right(left)?;

        let combined = if left_op == Operator::Less {
            Operator::LessOrEqual
        } else {
            Operator::GreaterOrEqual
        };
        forest.replace_binary(idx, a, b);
        forest.set_op(idx, combined);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Boolean: DeMorgan
// !(a && b) <-> (!a || !b), !(a || b) <-> (!a && !b)
// ════════════════════════════════════════════════════════════════════════════════

fn can_de_morgan(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    // Form 1: !(a && b) or !(a || b)
    if op == Operator::Not && !is_binary(forest, idx) {
        return forest.left(idx).is_some_and(|inner| {
            let iop = get_op(forest, inner);
            iop == Some(Operator::And) || iop == Some(Operator::Or)
        });
    }

    // Form 2: (!a || !b) or (!a && !b)
    if op == Operator::And || op == Operator::Or {
        let left_not = forest
            .left(idx)
            .is_some_and(|l| get_op(forest, l) == Some(Operator::Not) && !is_binary(forest, l));
        let right_not = forest
            .right(idx)
            .is_some_and(|r| get_op(forest, r) == Some(Operator::Not) && !is_binary(forest, r));
        return left_not && right_not;
    }

    false
}

fn do_de_morgan(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let ty = Type::Bool;

    if op == Operator::Not {
        // !(a && b) -> (!a || !b)
        let inner = forest.left(idx)?;
        let inner_op = get_op(forest, inner)?;
        let a = forest.left(inner)?;
        let b = forest.right(inner)?;

        let not_a = forest.operator(Operator::Not, &ty, a, None);
        let not_b = forest.operator(Operator::Not, &ty, b, None);

        let new_op = if inner_op == Operator::And { Operator::Or } else { Operator::And };
        forest.replace_binary(idx, not_a, not_b);
        forest.set_op(idx, new_op);
    } else {
        // (!a || !b) -> !(a && b)
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = forest.left(left)?;
        let b = forest.left(right)?;

        let new_op = if op == Operator::Or { Operator::And } else { Operator::Or };
        let inner = forest.operator(new_op, &ty, a, Some(b));

        forest.replace_unary(idx, inner);
        forest.set_op(idx, Operator::Not);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Boolean: ComplementXor
// !a <-> (a ^ true)
// ════════════════════════════════════════════════════════════════════════════════

fn can_complement_xor(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };
    let ty = get_ty(forest, idx);

    if ty != Type::Bool {
        return false;
    }

    // Form 1: !a
    if op == Operator::Not && !is_binary(forest, idx) {
        return true;
    }

    // Form 2: (a ^ true)
    if op == Operator::Xor {
        let left = forest.left(idx);
        let right = forest.right(idx);
        return left.is_some_and(|l| is_literal(forest, l, "true")) ||
            right.is_some_and(|r| is_literal(forest, r, "true"));
    }

    false
}

fn do_complement_xor(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let ty = Type::Bool;

    if op == Operator::Not {
        // !a -> (a ^ true)
        let a = forest.left(idx)?;
        let t = forest.literal("true".to_string(), &ty);

        forest.replace_binary(idx, a, t);
        forest.set_op(idx, Operator::Xor);
    } else {
        // (a ^ true) -> !a
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;

        let a = if is_literal(forest, left, "true") { right } else { left };
        forest.replace_unary(idx, a);
        forest.set_op(idx, Operator::Not);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Boolean: XorToAndOr
// (a ^ b) <-> ((!a & b) | (a & !b))
// ════════════════════════════════════════════════════════════════════════════════

fn can_xor_to_and_or(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    // Form 1: (a ^ b)
    if op == Operator::Xor && is_binary(forest, idx) {
        return true;
    }

    // Form 2: (X | Y) where X = (!a & b), Y = (a & !b)
    // This is complex to detect, so we just check basic structure
    if op == Operator::Or {
        let left_and = forest.left(idx).is_some_and(|l| get_op(forest, l) == Some(Operator::And));
        let right_and = forest.right(idx).is_some_and(|r| get_op(forest, r) == Some(Operator::And));
        return left_and && right_and;
    }

    false
}

fn do_xor_to_and_or(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let ty = Type::Bool;

    if op == Operator::Xor {
        // (a ^ b) -> ((!a & b) | (a & !b))
        let a = forest.left(idx)?;
        let b = forest.right(idx)?;

        let not_a = forest.operator(Operator::Not, &ty, a, None);
        let not_b = forest.operator(Operator::Not, &ty, b, None);
        let left = forest.operator(Operator::And, &ty, not_a, Some(b));
        let right = forest.operator(Operator::And, &ty, a, Some(not_b));

        forest.replace_binary(idx, left, right);
        forest.set_op(idx, Operator::Or);
    } else {
        // ((!a & b) | (a & !b)) -> (a ^ b)
        let left = forest.left(idx)?; // (!a & b)
        let right = forest.right(idx)?; // (a & !b)

        // Extract from right side: (a & !b) -> a is left child
        let a = forest.left(right)?;
        // Extract from left side: (!a & b) -> b is right child
        let b = forest.right(left)?;

        forest.replace_binary(idx, a, b);
        forest.set_op(idx, Operator::Xor);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Modulo: ModOne
// (a % 1) <-> 0
// ════════════════════════════════════════════════════════════════════════════════

fn can_mod_one(forest: &Forest, idx: NodeIndex) -> bool {
    if get_op(forest, idx) != Some(Operator::Mod) {
        return false;
    }
    forest.right(idx).is_some_and(|r| is_literal(forest, r, "1"))
}

fn do_mod_one(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    Some(forest.literal("0".to_string(), &ty))
}

// ════════════════════════════════════════════════════════════════════════════════
// Modulo: AndToMod
// (a & 1) <-> (a % 2)
// ════════════════════════════════════════════════════════════════════════════════

fn can_and_to_mod(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    // Form 1: (a & 1)
    if op == Operator::And {
        let left = forest.left(idx);
        let right = forest.right(idx);
        return left.is_some_and(|l| is_literal(forest, l, "1")) ||
            right.is_some_and(|r| is_literal(forest, r, "1"));
    }

    // Form 2: (a % 2)
    if op == Operator::Mod {
        return forest.right(idx).is_some_and(|r| is_literal(forest, r, "2"));
    }

    false
}

fn do_and_to_mod(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let ty = get_ty(forest, idx);

    if op == Operator::And {
        // (a & 1) -> (a % 2)
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = if is_literal(forest, left, "1") { right } else { left };
        let two = forest.literal("2".to_string(), &ty);

        forest.replace_binary(idx, a, two);
        forest.set_op(idx, Operator::Mod);
    } else {
        // (a % 2) -> (a & 1)
        let a = forest.left(idx)?;
        let one = forest.literal("1".to_string(), &ty);

        forest.replace_binary(idx, a, one);
        forest.set_op(idx, Operator::And);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Shift: ShiftZero
// (a << 0) <-> a, (a >> 0) <-> a
// ════════════════════════════════════════════════════════════════════════════════

fn can_shift_zero(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    if op != Operator::Shl && op != Operator::Shr {
        return false;
    }

    forest.right(idx).is_some_and(|r| is_literal(forest, r, "0"))
}

fn do_shift_zero(forest: &Forest, idx: NodeIndex) -> Option<NodeIndex> {
    forest.left(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Obfuscation: InjectAddSub
// a <-> ((a + r) - r)
// ════════════════════════════════════════════════════════════════════════════════

fn can_inject_add_sub(forest: &Forest, idx: NodeIndex) -> bool {
    let ty = get_ty(forest, idx);
    ty == Type::Field // Only for Field types (overflow-safe)
}

fn do_inject_add_sub(forest: &mut Forest, idx: NodeIndex, rng: &mut impl Rng) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let r = rng.random::<u64>();
    let r_node = forest.literal(r.to_string(), &ty);

    let add = forest.operator(Operator::Add, &ty, idx, Some(r_node));
    let r_node2 = forest.literal(r.to_string(), &ty);
    Some(forest.operator(Operator::Sub, &ty, add, Some(r_node2)))
}

// ════════════════════════════════════════════════════════════════════════════════
// Obfuscation: InjectSubAdd
// a <-> ((a - r) + r)
// ════════════════════════════════════════════════════════════════════════════════

fn do_inject_sub_add(forest: &mut Forest, idx: NodeIndex, rng: &mut impl Rng) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let r = rng.random::<u64>();
    let r_node = forest.literal(r.to_string(), &ty);

    let sub = forest.operator(Operator::Sub, &ty, idx, Some(r_node));
    let r_node2 = forest.literal(r.to_string(), &ty);
    Some(forest.operator(Operator::Add, &ty, sub, Some(r_node2)))
}

// ════════════════════════════════════════════════════════════════════════════════
// Obfuscation: InjectMulDiv
// a <-> ((a * r) / r)
// ════════════════════════════════════════════════════════════════════════════════

fn can_inject_mul_div(forest: &Forest, idx: NodeIndex) -> bool {
    let ty = get_ty(forest, idx);
    ty == Type::Field
}

fn do_inject_mul_div(forest: &mut Forest, idx: NodeIndex, rng: &mut impl Rng) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let r = rng.random_range(1u64..u64::MAX); // Non-zero
    let r_node = forest.literal(r.to_string(), &ty);

    let mul = forest.operator(Operator::Mul, &ty, idx, Some(r_node));
    let r_node2 = forest.literal(r.to_string(), &ty);
    Some(forest.operator(Operator::Div, &ty, mul, Some(r_node2)))
}

// ════════════════════════════════════════════════════════════════════════════════
// Obfuscation: InjectXorXor
// a <-> ((a ^ r) ^ r)
// ════════════════════════════════════════════════════════════════════════════════

fn can_inject_xor(forest: &Forest, idx: NodeIndex) -> bool {
    let ty = get_ty(forest, idx);
    ty.kind() == TypeKind::Unsigned || ty.kind() == TypeKind::Signed || ty == Type::Bool
}

fn do_inject_xor_xor(forest: &mut Forest, idx: NodeIndex, rng: &mut impl Rng) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let r = rng.random::<u64>();
    let r_lit = if ty == Type::Bool {
        if r % 2 == 0 { "false" } else { "true" }.to_string()
    } else {
        r.to_string()
    };

    let r_node = forest.literal(r_lit.clone(), &ty);
    let xor1 = forest.operator(Operator::Xor, &ty, idx, Some(r_node));
    let r_node2 = forest.literal(r_lit, &ty);
    Some(forest.operator(Operator::Xor, &ty, xor1, Some(r_node2)))
}

// ════════════════════════════════════════════════════════════════════════════════
// Obfuscation: InjectDivDiv
// 1 <-> (r / r)
// ════════════════════════════════════════════════════════════════════════════════

fn can_inject_div_div(forest: &Forest, idx: NodeIndex) -> bool {
    is_literal(forest, idx, "1")
}

fn do_inject_div_div(forest: &mut Forest, idx: NodeIndex, rng: &mut impl Rng) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let r = rng.random_range(1u64..u64::MAX);
    let r1 = forest.literal(r.to_string(), &ty);
    let r2 = forest.literal(r.to_string(), &ty);

    let result = forest.operator(Operator::Div, &ty, r1, Some(r2));

    // The original node (idx) is now orphaned - remove it
    forest.remove_if_orphan(idx);

    Some(result)
}

// ════════════════════════════════════════════════════════════════════════════════
// Obfuscation: InjectOrZero
// a <-> (a | 0)
// ════════════════════════════════════════════════════════════════════════════════

fn can_inject_or_zero(forest: &Forest, idx: NodeIndex) -> bool {
    let ty = get_ty(forest, idx);
    ty.kind() == TypeKind::Unsigned || ty.kind() == TypeKind::Signed
}

fn do_inject_or_zero(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    let zero = forest.literal("0".to_string(), &ty);
    Some(forest.operator(Operator::Or, &ty, idx, Some(zero)))
}

// ════════════════════════════════════════════════════════════════════════════════
// Obfuscation: InjectAndSelf
// a <-> (a & a)
// ════════════════════════════════════════════════════════════════════════════════

fn can_inject_and_self(forest: &Forest, idx: NodeIndex) -> bool {
    let ty = get_ty(forest, idx);
    ty.kind() == TypeKind::Unsigned || ty.kind() == TypeKind::Signed
}

fn do_inject_and_self(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let ty = get_ty(forest, idx);
    Some(forest.operator(Operator::And, &ty, idx, Some(idx)))
}

// ════════════════════════════════════════════════════════════════════════════════
// Simplification: DoubleMulTwo
// (a + a) <-> (a * 2)
// ════════════════════════════════════════════════════════════════════════════════

fn can_double_mul_two(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    // Form 1: (a + a)
    if op == Operator::Add {
        let left = forest.left(idx);
        let right = forest.right(idx);
        if let (Some(l), Some(r)) = (left, right) {
            return same_node(forest, l, r);
        }
    }

    // Form 2: (a * 2)
    if op == Operator::Mul {
        let left = forest.left(idx);
        let right = forest.right(idx);
        return left.is_some_and(|l| is_literal(forest, l, "2")) ||
            right.is_some_and(|r| is_literal(forest, r, "2"));
    }

    false
}

fn do_double_mul_two(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let ty = get_ty(forest, idx);

    if op == Operator::Add {
        // (a + a) -> (a * 2)
        let a = forest.left(idx)?;
        let two = forest.literal("2".to_string(), &ty);

        forest.replace_binary(idx, a, two);
        forest.set_op(idx, Operator::Mul);
    } else {
        // (a * 2) -> (a + a)
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = if is_literal(forest, left, "2") { right } else { left };

        forest.replace_binary(idx, a, a);
        forest.set_op(idx, Operator::Add);
    }

    Some(idx)
}

// ════════════════════════════════════════════════════════════════════════════════
// Simplification: MulNegOneNeg
// (a * -1) <-> (-a)
// ════════════════════════════════════════════════════════════════════════════════

fn can_mul_neg_one(forest: &Forest, idx: NodeIndex) -> bool {
    let Some(op) = get_op(forest, idx) else { return false };

    // Form 1: (a * -1)
    if op == Operator::Mul {
        let left = forest.left(idx);
        let right = forest.right(idx);
        return left.is_some_and(|l| is_literal(forest, l, "-1")) ||
            right.is_some_and(|r| is_literal(forest, r, "-1"));
    }

    // Form 2: (-a)
    if op == Operator::Neg && !is_binary(forest, idx) {
        return true;
    }

    false
}

fn do_mul_neg_one(forest: &mut Forest, idx: NodeIndex) -> Option<NodeIndex> {
    let op = get_op(forest, idx)?;
    let ty = get_ty(forest, idx);

    if op == Operator::Mul {
        // (a * -1) -> (-a)
        let left = forest.left(idx)?;
        let right = forest.right(idx)?;
        let a = if is_literal(forest, left, "-1") { right } else { left };

        forest.replace_unary(idx, a);
        forest.set_op(idx, Operator::Neg);
    } else {
        // (-a) -> (a * -1)
        let a = forest.left(idx)?;
        let neg_one = forest.literal("-1".to_string(), &ty);

        forest.replace_binary(idx, a, neg_one);
        forest.set_op(idx, Operator::Mul);
    }

    Some(idx)
}
