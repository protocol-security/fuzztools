use crate::circuits::{
    ast::{forest::Forest, nodes::Node, operators::Operator, types::*},
    context::Context,
    scope::Scope,
};
use petgraph::{graph::NodeIndex, visit::EdgeRef, Direction};
use rand::Rng;
use rules::{Rule, RuleKind, RULES};
use std::collections::HashMap;
use Operator::{
    Add, And, Div, Equal, Greater, GreaterOrEqual, Less, LessOrEqual, Mod, Mul, Neg, Not, NotEqual,
    Or, Shl, Shr, Sub, Xor,
};

pub mod rules;

pub struct Rewriter {
    rules: &'static [Rule],
    op_rules: HashMap<Operator, Vec<usize>>,
    type_rules: HashMap<TypeKind, Vec<usize>>,
}

// ────────────────────────────────────────────────────────────────────────────────
// Rewriter implementation
// ────────────────────────────────────────────────────────────────────────────────

impl Default for Rewriter {
    /// Build rule indices once at construction time
    fn default() -> Self {
        let mut op_rules: HashMap<Operator, Vec<usize>> = HashMap::new();
        let mut type_rules: HashMap<TypeKind, Vec<usize>> = HashMap::new();

        for (i, rule) in RULES.iter().enumerate() {
            for op in operators_for_rule(&rule.kind) {
                op_rules.entry(op).or_default().push(i);
            }
            for tk in types_for_inject_rule(&rule.kind) {
                type_rules.entry(tk).or_default().push(i);
            }
        }

        Self { rules: RULES, op_rules, type_rules }
    }
}

impl Rewriter {
    /// Randomly select a rule, then apply it to ALL matching instances in the forest
    pub fn apply_random(
        &self,
        random: &mut impl Rng,
        forest: &mut Forest,
        ctx: &Context,
        scope: &Scope,
    ) {
        // Collect all (node, rule_index) pairs that match
        let mut matches: Vec<_> = vec![];

        // 1. Operator-specific rules
        for (&op, nodes) in &forest.operators {
            if let Some(rules) = self.op_rules.get(&op) {
                for &n in nodes {
                    if is_assignment_lhs(forest, n) {
                        continue;
                    }
                    let (left, right) = (forest.left(n), forest.right(n));
                    for &i in rules {
                        if matches_rule(forest, Some(op), left, right, &self.rules[i].kind) {
                            matches.push((n, i));
                        }
                    }
                }
            }
        }

        // 2. Type-based inject rules (for terminal nodes like literals/variables)
        for (&ty_kind, nodes) in &forest.type_kinds {
            if let Some(rules) = self.type_rules.get(&ty_kind) {
                for &n in nodes {
                    if op_of(forest, n).is_some() {
                        continue;
                    }
                    if is_assignment_lhs(forest, n) {
                        continue;
                    }
                    if forest.graph.edges_directed(n, Direction::Incoming).next().is_none() {
                        continue;
                    }
                    for &i in rules {
                        if matches_rule(forest, None, Some(n), None, &self.rules[i].kind) {
                            matches.push((n, i));
                        }
                    }
                }
            }
        }

        if matches.is_empty() {
            return;
        }

        // Randomly select which rule to apply
        let selected_rule_idx = matches[random.random_range(0..matches.len())].1;

        // Collect all nodes that match this specific rule
        let nodes_for_rule: Vec<NodeIndex> = matches
            .iter()
            .filter(|(_, rule_idx)| *rule_idx == selected_rule_idx)
            .map(|(n, _)| *n)
            .collect();

        // Apply to all matching nodes (check node still exists - graph may change during rewrites)
        for node in nodes_for_rule {
            if forest.graph.contains_node(node) {
                self.apply(random, forest, node, &self.rules[selected_rule_idx].kind, ctx, scope);
            }
        }
    }

    fn apply(
        &self,
        random: &mut impl Rng,
        forest: &mut Forest,
        n: NodeIndex,
        kind: &RuleKind,
        ctx: &Context,
        scope: &Scope,
    ) {
        match kind {
            // Structural
            RuleKind::SwapOperands { .. } => forest.swap_operands(n),
            RuleKind::Associate { .. } => do_associate(forest, n),
            RuleKind::AssociateSub => do_associate_sub(forest, n),
            RuleKind::AssociateDiv => do_associate_div(forest, n),
            RuleKind::DivCommute => do_div_commute(forest, n),
            RuleKind::Distribute { outer, inner } => do_distribute(forest, n, *outer, *inner),

            // Identity / Absorb
            RuleKind::Identity { op, identity_right } => {
                do_identity(forest, n, *op, *identity_right)
            }
            RuleKind::Absorb { op } => do_absorb(forest, n, *op),
            RuleKind::SelfInverse { op } => do_self_inverse(forest, n, *op),
            RuleKind::Idempotent { op } => do_idempotent(forest, n, *op),

            // Unary
            RuleKind::DoubleUnary { op } => do_double_unary(forest, n, *op),
            RuleKind::AddNegSub => do_add_neg_sub(forest, n),
            RuleKind::NegZeroSub => do_neg_zero_sub(forest, n),

            // Comparison
            RuleKind::FlipComparison => do_flip_comparison(forest, n),
            RuleKind::NegateComparison => do_negate_comparison(forest, n),
            RuleKind::ExpandComparison => do_expand_comparison(forest, n),

            // Boolean logic
            RuleKind::DeMorgan => do_demorgan(forest, n),
            RuleKind::ComplementXor => do_complement_xor(forest, n),
            RuleKind::XorToAndOr => do_xor_to_and_or(forest, n),

            // Modulo
            RuleKind::ModOne => do_mod_one(forest, n),
            RuleKind::AndToMod => do_and_to_mod(forest, n),

            // Shift
            RuleKind::ShiftZero => do_shift_zero(forest, n),

            // Obfuscation
            RuleKind::InjectAddSub => {
                do_inject(random, forest, n, Operator::Add, Operator::Sub, ctx, scope)
            }
            RuleKind::InjectSubAdd => {
                do_inject(random, forest, n, Operator::Sub, Operator::Add, ctx, scope)
            }
            RuleKind::InjectMulDiv => do_inject_nonzero(random, forest, n, ctx, scope),
            RuleKind::InjectXorXor => {
                do_inject(random, forest, n, Operator::Xor, Operator::Xor, ctx, scope)
            }
            RuleKind::InjectDivDiv => do_inject_div_div(random, forest, n, ctx, scope),
            RuleKind::InjectOrZero => do_inject_or_zero(forest, n),
            RuleKind::InjectAndSelf => do_inject_and_self(forest, n),

            // Simplification
            RuleKind::DoubleMulTwo => do_double_mul_two(forest, n),
            RuleKind::MulNegOneNeg => do_mul_neg_one_neg(forest, n),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Rule matching
// ═══════════════════════════════════════════════════════════════════════════════

fn matches_rule(
    f: &Forest,
    op: Option<Operator>,
    left: Option<NodeIndex>,
    right: Option<NodeIndex>,
    kind: &RuleKind,
) -> bool {
    let is_binary = left.is_some() && right.is_some();
    let is_unary = left.is_some() && right.is_none();
    let left_op = left.and_then(|l| op_of(f, l));
    let right_op = right.and_then(|r| op_of(f, r));

    match (kind, op) {
        // ─────────────────────────────────────────────────────────────────────────
        // Structural rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::SwapOperands { ops }, Some(o)) => ops.contains(&o) && is_binary,

        (RuleKind::Associate { ops }, Some(o)) => {
            ops.contains(&o) && is_binary && (left_op == Some(o) || right_op == Some(o))
        }

        // ((a - b) - c) or (a - (b + c))
        (RuleKind::AssociateSub, Some(Operator::Sub)) => {
            is_binary && (left_op == Some(Operator::Sub) || right_op == Some(Operator::Add))
        }

        // ((a / b) * c) ↔ (a * (c / b)) - only valid for Field (not integer division!)
        (RuleKind::AssociateDiv, Some(Operator::Mul)) => {
            is_binary &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Field)) &&
                (left_op == Some(Operator::Div) || right_op == Some(Operator::Div))
        }

        // (a / b) → ((1 / b) * a) - only valid for Field (not integer division!)
        (RuleKind::DivCommute, Some(Operator::Div)) => {
            is_binary && left.is_some_and(|l| matches!(f.ty(l), Type::Field))
        }

        (RuleKind::Distribute { outer, inner }, Some(o)) => {
            (o == *outer && left_op == Some(*inner)) ||
                (o == *inner && left_op == Some(*outer) && right_op == Some(*outer))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Identity/Absorb rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::Identity { op: rule_op, identity_right }, Some(o))
            if o == *rule_op && is_binary =>
        {
            // a & 1 = a is only correct for booleans (for integers, a & 1 keeps only LSB)
            // a | 0 = a is correct for all types
            if o == Operator::And && !left.is_some_and(|l| matches!(f.ty(l), Type::Boolean)) {
                return false;
            }
            let check = if *identity_right { right } else { left };
            check.is_some_and(|c| is_identity(f, c, o))
        }
        (RuleKind::Identity { op, .. }, _) => {
            // For And injection (a -> a & 1), only apply to booleans
            // For Or injection (a -> a | 0), apply to all integer types
            if *op == Operator::And {
                left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
            } else {
                left.is_some_and(|l| is_numeric_or_bool(f, l))
            }
        }

        (RuleKind::Absorb { op: rule_op }, Some(o)) => {
            // a & 0 = 0 is correct for all types
            // a | 1 = 1 is only correct for booleans (for integers, a | 1 sets only LSB)
            if o == Operator::Or && !left.is_some_and(|l| matches!(f.ty(l), Type::Boolean)) {
                return false;
            }
            o == *rule_op &&
                is_binary &&
                (left.is_some_and(|l| is_absorbing(f, l, o)) ||
                    right.is_some_and(|r| is_absorbing(f, r, o)))
        }

        (RuleKind::SelfInverse { op: rule_op }, Some(o)) => {
            if o != *rule_op || !is_binary || left != right {
                return false;
            }
            // For Div, a / a = 1 only if a != 0. Only apply to known non-zero literals.
            if o == Operator::Div {
                left.is_some_and(|l| {
                    matches!(&f.graph[l], Node::Literal { value, .. }
                        if !value.starts_with('0') && !value.starts_with("-0") && value != "false")
                })
            } else {
                true
            }
        }

        (RuleKind::Idempotent { op: rule_op }, Some(o)) => {
            (o == *rule_op && is_binary && left == right) ||
                left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Unary rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::DoubleUnary { op: rule_op }, Some(o)) => {
            (o == *rule_op &&
                is_unary &&
                left.is_some_and(|i| op_of(f, i) == Some(o) && f.right(i).is_none())) ||
                can_apply_unary(f, left, *rule_op)
        }
        (RuleKind::DoubleUnary { op }, None) => can_apply_unary(f, left, *op),

        (RuleKind::AddNegSub, Some(Operator::Sub)) => {
            is_binary && left.is_some_and(|l| is_signed_type(f, l))
        }
        (RuleKind::AddNegSub, Some(Operator::Add)) => {
            right_op == Some(Operator::Neg) && left.is_some_and(|l| is_signed_type(f, l))
        }

        (RuleKind::NegZeroSub, Some(Operator::Neg)) => {
            is_unary && left.is_some_and(|l| is_signed_type(f, l))
        }
        (RuleKind::NegZeroSub, Some(Operator::Sub)) => {
            left.is_some_and(|l| is_lit(f, l, "0")) && right.is_some_and(|r| is_signed_type(f, r))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Comparison rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::FlipComparison, Some(o)) => matches!(
            o,
            Operator::Less | Operator::Greater | Operator::LessOrEqual | Operator::GreaterOrEqual
        ),

        (RuleKind::NegateComparison, Some(o)) => {
            o.is_comparison() || (o == Operator::Not && left_op.is_some_and(|x| x.is_comparison()))
        }

        // (a <= b) ↔ ((a < b) || (a == b))
        (RuleKind::ExpandComparison, Some(o)) => {
            matches!(o, Operator::LessOrEqual | Operator::GreaterOrEqual) ||
                (o == Operator::Or &&
                    left_op.is_some_and(|x| matches!(x, Operator::Less | Operator::Greater)) &&
                    right_op == Some(Operator::Equal))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Boolean rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::DeMorgan, Some(Operator::Not)) => {
            left_op.is_some_and(|x| matches!(x, Operator::And | Operator::Or))
        }
        (RuleKind::DeMorgan, Some(Operator::And | Operator::Or)) => {
            left_op == Some(Operator::Not) && right_op == Some(Operator::Not)
        }

        // !a ↔ a ^ true only works for booleans (for integers, !a is bitwise NOT ≠ a ^ 1)
        (RuleKind::ComplementXor, Some(Operator::Not)) => {
            is_unary && left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }
        (RuleKind::ComplementXor, Some(Operator::Xor)) => {
            right.is_some_and(|r| is_one(f, r)) &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }

        // (a ^ b) ↔ ((!a & b) | (a & !b))
        (RuleKind::XorToAndOr, Some(Operator::Xor)) => {
            is_binary && left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }
        (RuleKind::XorToAndOr, Some(Operator::Or)) => {
            // Match pattern: ((!a & b) | (a & !b))
            left_op == Some(Operator::And) && right_op == Some(Operator::And) && {
                let ll_op = left.and_then(|l| f.left(l)).and_then(|ll| op_of(f, ll));
                let rl_op = right.and_then(|r| f.right(r)).and_then(|rr| op_of(f, rr));
                ll_op == Some(Operator::Not) && rl_op == Some(Operator::Not)
            }
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Modulo rules
        // ─────────────────────────────────────────────────────────────────────────
        // a % 1 → 0
        (RuleKind::ModOne, Some(Operator::Mod)) => right.is_some_and(|r| is_one(f, r)),
        // 0 → r % 1 (injection)
        (RuleKind::ModOne, _) => {
            left.is_some_and(|l| is_zero(f, l) && matches!(f.ty(l), Type::Integer(_)))
        }

        // a & 1 ↔ a % 2
        (RuleKind::AndToMod, Some(Operator::And)) => {
            right.is_some_and(|r| is_one(f, r)) &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_)))
        }
        (RuleKind::AndToMod, Some(Operator::Mod)) => {
            right.is_some_and(|r| is_two(f, r)) &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_)))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Shift rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::ShiftZero, Some(Operator::Shl | Operator::Shr)) => {
            right.is_some_and(|r| is_zero(f, r))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Simplification rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::DoubleMulTwo, Some(Operator::Add)) => left == right,
        (RuleKind::DoubleMulTwo, Some(Operator::Mul)) => right.is_some_and(|r| is_two(f, r)),

        (RuleKind::MulNegOneNeg, Some(Operator::Mul)) => {
            right.is_some_and(|r| is_neg_one(f, r)) && left.is_some_and(|l| is_signed_type(f, l))
        }
        (RuleKind::MulNegOneNeg, Some(Operator::Neg)) => {
            is_unary && left.is_some_and(|l| is_signed_type(f, l))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Injection rules (obfuscation) - only for Field to avoid integer overflow issues
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv, _) => {
            left.is_some_and(|l| matches!(f.ty(l), Type::Field))
        }

        (RuleKind::InjectXorXor, _) => {
            left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_) | Type::Boolean))
        }

        // 1 → r / r
        (RuleKind::InjectDivDiv, _) => {
            left.is_some_and(|l| is_one(f, l) && matches!(f.ty(l), Type::Field | Type::Integer(_)))
        }

        // a → a | 0 (for integers)
        (RuleKind::InjectOrZero, _) => left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_))),

        // a → a & a (for integers)
        (RuleKind::InjectAndSelf, _) => left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_))),

        _ => false,
    }
}

/// Get all operators that a rule can target (for building the index)
fn operators_for_rule(kind: &RuleKind) -> Vec<Operator> {
    match kind {
        RuleKind::SwapOperands { ops } | RuleKind::Associate { ops } => ops.to_vec(),
        RuleKind::AssociateSub => vec![Sub],
        RuleKind::AssociateDiv => vec![Mul],
        RuleKind::DivCommute => vec![Div],
        RuleKind::Distribute { outer, inner } => vec![*outer, *inner],
        RuleKind::Identity { op, .. } |
        RuleKind::Absorb { op } |
        RuleKind::SelfInverse { op } |
        RuleKind::Idempotent { op } |
        RuleKind::DoubleUnary { op } => vec![*op],
        RuleKind::AddNegSub | RuleKind::NegZeroSub => vec![Add, Sub, Neg],
        RuleKind::FlipComparison => vec![Less, Greater, LessOrEqual, GreaterOrEqual],
        RuleKind::NegateComparison => {
            vec![Less, Greater, LessOrEqual, GreaterOrEqual, Equal, NotEqual, Not]
        }
        RuleKind::ExpandComparison => vec![LessOrEqual, GreaterOrEqual, Or],
        RuleKind::DeMorgan => vec![Not, And, Or],
        RuleKind::ComplementXor => vec![Not, Xor],
        RuleKind::XorToAndOr => vec![Xor, Or],
        RuleKind::ModOne => vec![Mod],
        RuleKind::AndToMod => vec![And, Mod],
        RuleKind::ShiftZero => vec![Shl, Shr],
        RuleKind::DoubleMulTwo => vec![Add, Mul],
        RuleKind::MulNegOneNeg => vec![Mul, Neg],
        RuleKind::InjectAddSub |
        RuleKind::InjectSubAdd |
        RuleKind::InjectMulDiv |
        RuleKind::InjectXorXor |
        RuleKind::InjectDivDiv |
        RuleKind::InjectOrZero |
        RuleKind::InjectAndSelf => vec![],
    }
}

/// Get all type kinds that an inject rule can target (for building the index)
fn types_for_inject_rule(kind: &RuleKind) -> Vec<TypeKind> {
    match kind {
        // AddSub/SubAdd/MulDiv only for Field to avoid integer overflow issues
        RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv => {
            vec![TypeKind::Field]
        }
        RuleKind::InjectXorXor => vec![TypeKind::Signed, TypeKind::Unsigned, TypeKind::Boolean],
        RuleKind::InjectDivDiv => vec![TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned],
        RuleKind::InjectOrZero | RuleKind::InjectAndSelf => {
            vec![TypeKind::Signed, TypeKind::Unsigned]
        }
        RuleKind::ModOne => vec![TypeKind::Signed, TypeKind::Unsigned],
        _ => vec![],
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Rule implementations
// ═══════════════════════════════════════════════════════════════════════════════

fn do_associate(f: &mut Forest, n: NodeIndex) {
    let op = match op_of(f, n) {
        Some(op) => op,
        None => return,
    };
    let ret = ret_of(f, n);
    let (left, right) = (f.left(n), f.right(n));

    // (a op b) op c -> a op (b op c)
    if let Some(l) = left.filter(|&l| op_of(f, l) == Some(op)) {
        let (a, b, c) = (f.left(l).unwrap(), f.right(l).unwrap(), right.unwrap());
        let new_right = f.operator(op, ret, b, Some(c));
        f.replace_operand(n, 0, a);
        f.replace_operand(n, 1, new_right);
        return;
    }
    // a op (b op c) -> (a op b) op c
    if let Some(r) = right.filter(|&r| op_of(f, r) == Some(op)) {
        let (a, b, c) = (left.unwrap(), f.left(r).unwrap(), f.right(r).unwrap());
        let new_left = f.operator(op, ret, a, Some(b));
        f.replace_operand(n, 0, new_left);
        f.replace_operand(n, 1, c);
    }
}

/// ((a - b) - c) ↔ (a - (b + c))
fn do_associate_sub(f: &mut Forest, n: NodeIndex) {
    if op_of(f, n) != Some(Operator::Sub) {
        return;
    }
    let ret = ret_of(f, n);
    let (left, right) = (f.left(n), f.right(n));

    // (a - b) - c -> a - (b + c)
    if let Some(l) = left.filter(|&l| op_of(f, l) == Some(Operator::Sub)) {
        let (a, b, c) = (f.left(l).unwrap(), f.right(l).unwrap(), right.unwrap());
        let new_right = f.operator(Operator::Add, ret, b, Some(c));
        f.replace_operand(n, 0, a);
        f.replace_operand(n, 1, new_right);
        return;
    }
    // a - (b + c) -> (a - b) - c
    if let Some(r) = right.filter(|&r| op_of(f, r) == Some(Operator::Add)) {
        let (a, b, c) = (left.unwrap(), f.left(r).unwrap(), f.right(r).unwrap());
        let new_left = f.operator(Operator::Sub, ret, a, Some(b));
        f.replace_operand(n, 0, new_left);
        f.replace_operand(n, 1, c);
    }
}

/// ((a / b) * c) ↔ (a * (c / b))
fn do_associate_div(f: &mut Forest, n: NodeIndex) {
    if op_of(f, n) != Some(Operator::Mul) {
        return;
    }
    let ret = ret_of(f, n);
    let (left, right) = (f.left(n), f.right(n));

    // (a / b) * c -> a * (c / b)
    if let Some(l) = left.filter(|&l| op_of(f, l) == Some(Operator::Div)) {
        let (a, b, c) = (f.left(l).unwrap(), f.right(l).unwrap(), right.unwrap());
        let new_right = f.operator(Operator::Div, ret, c, Some(b));
        f.replace_operand(n, 0, a);
        f.replace_operand(n, 1, new_right);
        return;
    }
    // a * (c / b) -> (a / b) * c
    if let Some(r) = right.filter(|&r| op_of(f, r) == Some(Operator::Div)) {
        let (a, c, b) = (left.unwrap(), f.left(r).unwrap(), f.right(r).unwrap());
        let new_left = f.operator(Operator::Div, ret, a, Some(b));
        f.replace_operand(n, 0, new_left);
        f.replace_operand(n, 1, c);
    }
}

/// (a / b) → ((1 / b) * a)
fn do_div_commute(f: &mut Forest, n: NodeIndex) {
    if op_of(f, n) != Some(Operator::Div) {
        return;
    }
    let (a, b) = match (f.left(n), f.right(n)) {
        (Some(a), Some(b)) => (a, b),
        _ => return,
    };

    let ty = ret_of(f, n);
    let one = make_one(f, &ty);
    let one_div_b = f.operator(Operator::Div, ty.clone(), one, Some(b));
    set_op(f, n, Operator::Mul);
    f.replace_operand(n, 1, a);
    f.replace_operand(n, 0, one_div_b);
}

fn do_distribute(f: &mut Forest, n: NodeIndex, outer: Operator, inner: Operator) {
    if op_of(f, n) != Some(outer) {
        return;
    }
    let left = match f.left(n).filter(|&l| op_of(f, l) == Some(inner)) {
        Some(l) => l,
        None => return,
    };

    let ret = ret_of(f, n);
    let (a, b, c) = (f.left(left).unwrap(), f.right(left).unwrap(), f.right(n).unwrap());
    let new_left = f.operator(outer, ret.clone(), a, Some(c));
    let new_right = f.operator(outer, ret, b, Some(c));
    set_op(f, n, inner);
    f.replace_operand(n, 0, new_left);
    f.replace_operand(n, 1, new_right);
}

fn do_identity(f: &mut Forest, n: NodeIndex, op: Operator, identity_right: bool) {
    if op_of(f, n) == Some(op) {
        let (keep, check) =
            if identity_right { (f.left(n), f.right(n)) } else { (f.right(n), f.left(n)) };
        if check.is_some_and(|c| is_identity(f, c, op)) {
            if let Some(k) = keep {
                redirect(f, n, k);
                return;
            }
        }
    }
    // Inject: x -> x op identity
    let ty = f.ty(n);
    let edges = f.incoming_edges(n);
    let id = make_identity(f, &ty, op);
    let (lhs, rhs) = if identity_right { (n, id) } else { (id, n) };
    let new = f.operator(op, ty, lhs, Some(rhs));
    f.redirect_edges(n, new, &edges);
}

fn do_absorb(f: &mut Forest, n: NodeIndex, op: Operator) {
    let ty = f.ty(n);
    let absorb = make_absorbing(f, &ty, op);
    redirect(f, n, absorb);
}

fn do_self_inverse(f: &mut Forest, n: NodeIndex, op: Operator) {
    let ty = f.ty(n);
    let lit = match op {
        Operator::Sub | Operator::Xor => make_zero(f, &ty),
        Operator::Div => make_one(f, &ty),
        _ => return,
    };
    redirect(f, n, lit);
}

fn do_idempotent(f: &mut Forest, n: NodeIndex, op: Operator) {
    if op_of(f, n) == Some(op) && f.left(n) == f.right(n) {
        if let Some(operand) = f.left(n) {
            redirect(f, n, operand);
            return;
        }
    }
    if matches!(f.ty(n), Type::Boolean) {
        let edges = f.incoming_edges(n);
        let new = f.operator(op, Type::Boolean, n, Some(n));
        f.redirect_edges(n, new, &edges);
    }
}

fn do_double_unary(f: &mut Forest, n: NodeIndex, op: Operator) {
    if op_of(f, n) == Some(op) && f.right(n).is_none() {
        if let Some(inner) = f.left(n).filter(|&i| op_of(f, i) == Some(op) && f.right(i).is_none())
        {
            if let Some(x) = f.left(inner) {
                redirect(f, n, x);
                return;
            }
        }
    }
    let ty = f.ty(n);
    let edges = f.incoming_edges(n);
    let inner = f.operator(op, ty.clone(), n, None);
    let outer = f.operator(op, ty, inner, None);
    f.redirect_edges(n, outer, &edges);
}

fn do_add_neg_sub(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Sub) => {
            let right = f.right(n).unwrap();
            let neg = f.operator(Operator::Neg, f.ty(right), right, None);
            set_op(f, n, Operator::Add);
            f.replace_operand(n, 1, neg);
        }
        Some(Operator::Add) if f.right(n).is_some_and(|r| op_of(f, r) == Some(Operator::Neg)) => {
            let b = f.left(f.right(n).unwrap()).unwrap();
            set_op(f, n, Operator::Sub);
            f.replace_operand(n, 1, b);
        }
        _ => {}
    }
}

fn do_neg_zero_sub(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Neg) => {
            // -x → 0 - x
            let operand = f.left(n).unwrap();
            let ty = ret_of(f, n);
            let zero = f.literal("0".into(), ty);
            set_op(f, n, Operator::Sub);
            f.add_operand(n, 1, operand);
            f.replace_operand(n, 0, zero);
        }
        Some(Operator::Sub) if f.left(n).is_some_and(|l| is_lit(f, l, "0")) => {
            // 0 - x → -x
            let right = f.right(n).unwrap();
            set_op(f, n, Operator::Neg);
            f.add_operand(n, 0, right);
            f.remove_operand(n, 1);
            // Remove old pos 0 (the duplicate edge)
            if let Some(edge) = f
                .graph
                .edges_directed(n, Direction::Outgoing)
                .find(|e| *e.weight() == 0 && e.target() != right)
            {
                let old = edge.target();
                let id = edge.id();
                f.graph.remove_edge(id);
                f.remove_if_orphan(old);
            }
        }
        _ => {}
    }
}

fn do_flip_comparison(f: &mut Forest, n: NodeIndex) {
    let flipped = match op_of(f, n) {
        Some(Operator::Less) => Operator::Greater,
        Some(Operator::Greater) => Operator::Less,
        Some(Operator::LessOrEqual) => Operator::GreaterOrEqual,
        Some(Operator::GreaterOrEqual) => Operator::LessOrEqual,
        _ => return,
    };
    set_op(f, n, flipped);
    f.swap_operands(n);
}

fn do_negate_comparison(f: &mut Forest, n: NodeIndex) {
    // not(a cmp b) -> a neg_cmp b
    if op_of(f, n) == Some(Operator::Not) {
        if let Some(inner) = f.left(n) {
            if let Some(negated) = op_of(f, inner).and_then(negate_cmp) {
                set_op(f, inner, negated);
                redirect(f, n, inner);
                return;
            }
        }
    }
    // a cmp b -> not(a neg_cmp b)
    if let Some(negated) = op_of(f, n).and_then(negate_cmp) {
        let edges = f.incoming_edges(n);
        set_op(f, n, negated);
        let not_node = f.operator(Operator::Not, Type::Boolean, n, None);
        f.redirect_edges(n, not_node, &edges);
    }
}

/// (a <= b) ↔ ((a < b) || (a == b))
fn do_expand_comparison(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::LessOrEqual) => {
            // a <= b -> (a < b) || (a == b)
            let (a, b) = (f.left(n).unwrap(), f.right(n).unwrap());
            let edges = f.incoming_edges(n);
            let less = f.operator(Operator::Less, Type::Boolean, a, Some(b));
            let eq = f.operator(Operator::Equal, Type::Boolean, a, Some(b));
            let new = f.operator(Operator::Or, Type::Boolean, less, Some(eq));
            f.redirect_edges(n, new, &edges);
        }
        Some(Operator::GreaterOrEqual) => {
            // a >= b -> (a > b) || (a == b)
            let (a, b) = (f.left(n).unwrap(), f.right(n).unwrap());
            let edges = f.incoming_edges(n);
            let greater = f.operator(Operator::Greater, Type::Boolean, a, Some(b));
            let eq = f.operator(Operator::Equal, Type::Boolean, a, Some(b));
            let new = f.operator(Operator::Or, Type::Boolean, greater, Some(eq));
            f.redirect_edges(n, new, &edges);
        }
        Some(Operator::Or) => {
            // (a < b) || (a == b) -> a <= b
            let (left, right) = (f.left(n).unwrap(), f.right(n).unwrap());
            let left_op = op_of(f, left);
            if left_op == Some(Operator::Less) && op_of(f, right) == Some(Operator::Equal) {
                let a = f.left(left).unwrap();
                let b = f.right(left).unwrap();
                let edges = f.incoming_edges(n);
                let new = f.operator(Operator::LessOrEqual, Type::Boolean, a, Some(b));
                f.redirect_edges(n, new, &edges);
            } else if left_op == Some(Operator::Greater) && op_of(f, right) == Some(Operator::Equal)
            {
                let a = f.left(left).unwrap();
                let b = f.right(left).unwrap();
                let edges = f.incoming_edges(n);
                let new = f.operator(Operator::GreaterOrEqual, Type::Boolean, a, Some(b));
                f.redirect_edges(n, new, &edges);
            }
        }
        _ => {}
    }
}

fn do_demorgan(f: &mut Forest, n: NodeIndex) {
    let op = op_of(f, n);

    // not(a and/or b) -> not(a) or/and not(b)
    if op == Some(Operator::Not) {
        if let Some(inner) = f.left(n) {
            let dual = match op_of(f, inner) {
                Some(Operator::And) => Operator::Or,
                Some(Operator::Or) => Operator::And,
                _ => return,
            };
            let edges = f.incoming_edges(n);
            let (a, b) = (f.left(inner).unwrap(), f.right(inner).unwrap());
            let not_a = f.operator(Operator::Not, Type::Boolean, a, None);
            let not_b = f.operator(Operator::Not, Type::Boolean, b, None);
            let new = f.operator(dual, Type::Boolean, not_a, Some(not_b));
            f.redirect_edges(n, new, &edges);
            return;
        }
    }

    // not(a) and/or not(b) -> not(a or/and b)
    if matches!(op, Some(Operator::And | Operator::Or)) {
        let (left, right) = (f.left(n), f.right(n));
        if left.is_some_and(|l| op_of(f, l) == Some(Operator::Not)) &&
            right.is_some_and(|r| op_of(f, r) == Some(Operator::Not))
        {
            let edges = f.incoming_edges(n);
            let dual = if op == Some(Operator::And) { Operator::Or } else { Operator::And };
            let (a, b) = (f.left(left.unwrap()).unwrap(), f.left(right.unwrap()).unwrap());
            let inner = f.operator(dual, Type::Boolean, a, Some(b));
            let new = f.operator(Operator::Not, Type::Boolean, inner, None);
            f.redirect_edges(n, new, &edges);
        }
    }
}

fn do_complement_xor(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Not) => {
            let ty = ret_of(f, n);
            let one = make_one(f, &ty);
            set_op(f, n, Operator::Xor);
            f.add_operand(n, 1, one);
        }
        Some(Operator::Xor) if f.right(n).is_some_and(|r| is_one(f, r)) => {
            set_op(f, n, Operator::Not);
            f.remove_operand(n, 1);
        }
        _ => {}
    }
}

/// (a ^ b) ↔ ((!a & b) | (a & !b))
fn do_xor_to_and_or(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Xor) => {
            // a ^ b -> (!a & b) | (a & !b)
            let (a, b) = (f.left(n).unwrap(), f.right(n).unwrap());
            let edges = f.incoming_edges(n);
            let not_a = f.operator(Operator::Not, Type::Boolean, a, None);
            let not_b = f.operator(Operator::Not, Type::Boolean, b, None);
            let left_and = f.operator(Operator::And, Type::Boolean, not_a, Some(b));
            let right_and = f.operator(Operator::And, Type::Boolean, a, Some(not_b));
            let new = f.operator(Operator::Or, Type::Boolean, left_and, Some(right_and));
            f.redirect_edges(n, new, &edges);
        }
        Some(Operator::Or) => {
            // (!a & b) | (a & !b) -> a ^ b
            let (left, right) = (f.left(n).unwrap(), f.right(n).unwrap());
            if op_of(f, left) == Some(Operator::And) && op_of(f, right) == Some(Operator::And) {
                let ll = f.left(left).unwrap();
                let lr = f.right(left).unwrap();
                let rl = f.left(right).unwrap();
                let rr = f.right(right).unwrap();
                // Check pattern: (!a & b) | (a & !b)
                if op_of(f, ll) == Some(Operator::Not) && op_of(f, rr) == Some(Operator::Not) {
                    let a_from_left = f.left(ll).unwrap();
                    let a_from_right = rl;
                    let b_from_left = lr;
                    let b_from_right = f.left(rr).unwrap();
                    if a_from_left == a_from_right && b_from_left == b_from_right {
                        let edges = f.incoming_edges(n);
                        let new = f.operator(
                            Operator::Xor,
                            Type::Boolean,
                            a_from_left,
                            Some(b_from_left),
                        );
                        f.redirect_edges(n, new, &edges);
                    }
                }
            }
        }
        _ => {}
    }
}

/// a % 1 ↔ 0
fn do_mod_one(f: &mut Forest, n: NodeIndex) {
    if op_of(f, n) == Some(Operator::Mod) && f.right(n).is_some_and(|r| is_one(f, r)) {
        // a % 1 -> 0
        let ty = f.ty(n);
        let zero = make_zero(f, &ty);
        redirect(f, n, zero);
    } else if is_zero(f, n) {
        // 0 -> r % 1 (inject)
        let ty = f.ty(n);
        if matches!(ty, Type::Integer(_)) {
            let edges = f.incoming_edges(n);
            let one = make_one(f, &ty);
            let new = f.operator(Operator::Mod, ty, n, Some(one));
            f.redirect_edges(n, new, &edges);
        }
    }
}

/// a & 1 ↔ a % 2
fn do_and_to_mod(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::And) if f.right(n).is_some_and(|r| is_one(f, r)) => {
            // a & 1 -> a % 2
            let ty = ret_of(f, n);
            let two = make_two(f, &ty);
            set_op(f, n, Operator::Mod);
            f.replace_operand(n, 1, two);
        }
        Some(Operator::Mod) if f.right(n).is_some_and(|r| is_two(f, r)) => {
            // a % 2 -> a & 1
            let ty = ret_of(f, n);
            let one = make_one(f, &ty);
            set_op(f, n, Operator::And);
            f.replace_operand(n, 1, one);
        }
        _ => {}
    }
}

/// (a << 0) → a, (a >> 0) → a
fn do_shift_zero(f: &mut Forest, n: NodeIndex) {
    if matches!(op_of(f, n), Some(Operator::Shl | Operator::Shr)) {
        if f.right(n).is_some_and(|r| is_zero(f, r)) {
            if let Some(left) = f.left(n) {
                redirect(f, n, left);
            }
        }
    }
}

fn do_inject(
    random: &mut impl Rng,
    f: &mut Forest,
    n: NodeIndex,
    op1: Operator,
    op2: Operator,
    ctx: &Context,
    scope: &Scope,
) {
    let ty = f.ty(n);
    let edges = f.incoming_edges(n);
    let r = f.literal(ty.random_value(random, ctx, scope), ty.clone());
    let first = f.operator(op1, ty.clone(), n, Some(r));
    let second = f.operator(op2, ty, first, Some(r));
    f.redirect_edges(n, second, &edges);
}

fn do_inject_nonzero(
    random: &mut impl Rng,
    f: &mut Forest,
    n: NodeIndex,
    ctx: &Context,
    scope: &Scope,
) {
    let ty = f.ty(n);
    let value = ty.random_value(random, ctx, scope);
    // Skip if value is zero (can't divide by zero)
    if value.starts_with('0') || value.starts_with("-0") || value == "false" {
        return;
    }
    let edges = f.incoming_edges(n);
    let r = f.literal(value, ty.clone());
    let first = f.operator(Operator::Mul, ty.clone(), n, Some(r));
    let second = f.operator(Operator::Div, ty, first, Some(r));
    f.redirect_edges(n, second, &edges);
}

/// 1 → r / r
fn do_inject_div_div(
    random: &mut impl Rng,
    f: &mut Forest,
    n: NodeIndex,
    ctx: &Context,
    scope: &Scope,
) {
    if !is_one(f, n) {
        return;
    }
    let ty = f.ty(n);
    let value = ty.random_value(random, ctx, scope);
    // Skip if value is zero (can't divide by zero)
    if value.starts_with('0') || value.starts_with("-0") || value == "false" {
        return;
    }
    let edges = f.incoming_edges(n);
    let r = f.literal(value, ty.clone());
    let new = f.operator(Operator::Div, ty, r, Some(r));
    f.redirect_edges(n, new, &edges);
}

/// a → a | 0
fn do_inject_or_zero(f: &mut Forest, n: NodeIndex) {
    let ty = f.ty(n);
    if !matches!(ty, Type::Integer(_)) {
        return;
    }
    let edges = f.incoming_edges(n);
    let zero = make_zero(f, &ty);
    let new = f.operator(Operator::Or, ty, n, Some(zero));
    f.redirect_edges(n, new, &edges);
}

/// a → a & a
fn do_inject_and_self(f: &mut Forest, n: NodeIndex) {
    let ty = f.ty(n);
    if !matches!(ty, Type::Integer(_)) {
        return;
    }
    let edges = f.incoming_edges(n);
    let new = f.operator(Operator::And, ty, n, Some(n));
    f.redirect_edges(n, new, &edges);
}

fn do_double_mul_two(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Add) if f.left(n) == f.right(n) => {
            let ty = f.ty(n);
            let two = make_two(f, &ty);
            set_op(f, n, Operator::Mul);
            f.replace_operand(n, 1, two);
        }
        Some(Operator::Mul) if f.right(n).is_some_and(|r| is_two(f, r)) => {
            let left = f.left(n).unwrap();
            set_op(f, n, Operator::Add);
            f.replace_operand(n, 1, left);
        }
        _ => {}
    }
}

fn do_mul_neg_one_neg(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Mul) if f.right(n).is_some_and(|r| is_neg_one(f, r)) => {
            set_op(f, n, Operator::Neg);
            f.remove_operand(n, 1);
        }
        Some(Operator::Neg) => {
            let ty = ret_of(f, n);
            let neg_one = make_neg_one(f, &ty);
            set_op(f, n, Operator::Mul);
            f.add_operand(n, 1, neg_one);
        }
        _ => {}
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════════

#[inline(always)]
fn op_of(f: &Forest, n: NodeIndex) -> Option<Operator> {
    match &f.graph[n] {
        Node::Operator { op, .. } => Some(*op),
        _ => None,
    }
}

#[inline(always)]
fn ret_of(f: &Forest, n: NodeIndex) -> Type {
    match &f.graph[n] {
        Node::Operator { ret, .. } => ret.clone(),
        _ => f.ty(n),
    }
}

#[inline(always)]
fn set_op(f: &mut Forest, n: NodeIndex, new_op: Operator) {
    if let Node::Operator { op, .. } = &mut f.graph[n] {
        *op = new_op;
    }
}

#[inline(always)]
fn redirect(f: &mut Forest, from: NodeIndex, to: NodeIndex) {
    let edges = f.incoming_edges(from);
    f.redirect_edges(from, to, &edges);
}

// ─────────────────────────────────────────────────────────────────────────────
// Literal predicates
// ─────────────────────────────────────────────────────────────────────────────

#[inline(always)]
fn is_lit(f: &Forest, n: NodeIndex, val: &str) -> bool {
    matches!(&f.graph[n], Node::Literal { value, .. } if value == val)
}

/// Check if node is a literal with numeric value (handles bool/field/int)
#[inline(always)]
fn is_numeric_literal(f: &Forest, n: NodeIndex, num: i8) -> bool {
    match &f.graph[n] {
        Node::Literal { value, ty } => match (num, ty) {
            (0, Type::Boolean) => value == "false",
            (1, Type::Boolean) => value == "true",
            (0, _) => {
                value.starts_with("0i") || value.starts_with("0u") || value.starts_with("0Field")
            }
            (1, _) => {
                value.starts_with("1i") || value.starts_with("1u") || value.starts_with("1Field")
            }
            (-1, _) => value == "-1" || value.starts_with("-1i") || value.starts_with("-1Field"),
            (2, _) => {
                value.starts_with("2i") || value.starts_with("2u") || value.starts_with("2Field")
            }
            _ => false,
        },
        _ => false,
    }
}

#[inline(always)]
fn is_zero(f: &Forest, n: NodeIndex) -> bool {
    is_numeric_literal(f, n, 0)
}

#[inline(always)]
fn is_one(f: &Forest, n: NodeIndex) -> bool {
    is_numeric_literal(f, n, 1)
}

#[inline(always)]
fn is_neg_one(f: &Forest, n: NodeIndex) -> bool {
    is_numeric_literal(f, n, -1)
}

#[inline(always)]
fn is_two(f: &Forest, n: NodeIndex) -> bool {
    is_numeric_literal(f, n, 2)
}

// ─────────────────────────────────────────────────────────────────────────────
// Structural predicates
// ─────────────────────────────────────────────────────────────────────────────

/// Check if node is the left-hand side of an assignment (position 0 of Assignment node)
/// Such nodes must NOT be transformed as they must remain valid lvalues
#[inline(always)]
fn is_assignment_lhs(f: &Forest, n: NodeIndex) -> bool {
    f.graph
        .edges_directed(n, Direction::Incoming)
        .any(|e| *e.weight() == 0 && matches!(f.graph[e.source()], Node::Assignment { .. }))
}

// ─────────────────────────────────────────────────────────────────────────────
// Type predicates
// ─────────────────────────────────────────────────────────────────────────────

#[inline(always)]
fn is_signed_type(f: &Forest, n: NodeIndex) -> bool {
    matches!(f.ty(n), Type::Field | Type::Integer(Integer { signed: true, .. }))
}

#[inline(always)]
fn is_numeric_or_bool(f: &Forest, n: NodeIndex) -> bool {
    matches!(f.ty(n), Type::Field | Type::Integer(_) | Type::Boolean)
}

fn can_apply_unary(f: &Forest, node: Option<NodeIndex>, op: Operator) -> bool {
    node.is_some_and(|n| match f.ty(n) {
        Type::Field => op == Operator::Neg,
        Type::Integer(Integer { signed, .. }) => {
            op == Operator::Not || (op == Operator::Neg && signed)
        }
        Type::Boolean => op == Operator::Not,
        _ => false,
    })
}

// ─────────────────────────────────────────────────────────────────────────────
// Operator properties
// ─────────────────────────────────────────────────────────────────────────────

#[inline(always)]
fn is_identity(f: &Forest, n: NodeIndex, op: Operator) -> bool {
    match op {
        Operator::Add |
        Operator::Sub |
        Operator::Xor |
        Operator::Or |
        Operator::Shl |
        Operator::Shr => is_zero(f, n),
        Operator::Mul | Operator::Div | Operator::And => is_one(f, n),
        _ => false,
    }
}

#[inline(always)]
fn is_absorbing(f: &Forest, n: NodeIndex, op: Operator) -> bool {
    match op {
        Operator::Mul | Operator::And => is_zero(f, n),
        Operator::Or => is_one(f, n),
        _ => false,
    }
}

#[inline(always)]
const fn negate_cmp(op: Operator) -> Option<Operator> {
    match op {
        Less => Some(GreaterOrEqual),
        Greater => Some(LessOrEqual),
        LessOrEqual => Some(Greater),
        GreaterOrEqual => Some(Less),
        Equal => Some(NotEqual),
        NotEqual => Some(Equal),
        _ => None,
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Literal creation
// ─────────────────────────────────────────────────────────────────────────────

/// Create a literal node with the given numeric value
fn make_literal(f: &mut Forest, ty: &Type, num: i8) -> NodeIndex {
    let val = match (num, ty) {
        (0, Type::Boolean) => "false".into(),
        (1, Type::Boolean) => "true".into(),
        (-1, Type::Field) => "-1Field".into(),
        (-1, Type::Integer(i)) => format!("-1i{}", i.bits),
        (2, Type::Field) => "2Field".into(),
        (2, Type::Integer(i)) => format!("2{}{}", if i.signed { "i" } else { "u" }, i.bits),
        (n, _) => format!("{n}{ty}"),
    };
    f.literal(val, ty.clone())
}

#[inline(always)]
fn make_zero(f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(f, ty, 0)
}

#[inline(always)]
fn make_one(f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(f, ty, 1)
}

#[inline(always)]
fn make_neg_one(f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(f, ty, -1)
}

#[inline(always)]
fn make_two(f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(f, ty, 2)
}

#[inline(always)]
fn make_identity(f: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    if matches!(op, Operator::Mul | Operator::Div | Operator::And) {
        make_one(f, ty)
    } else {
        make_zero(f, ty)
    }
}

#[inline(always)]
fn make_absorbing(f: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    if op == Operator::Or {
        make_one(f, ty)
    } else {
        make_zero(f, ty)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{builders::CircuitBuilder, circuits::ast::nodes::NodeKind};

    use super::*;

    /// ANSI color codes for terminal output
    const RED: &str = "\x1b[31m";
    const GREEN: &str = "\x1b[32m";
    const RESET: &str = "\x1b[0m";

    /// Find common prefix length between two strings
    fn common_prefix_len(a: &str, b: &str) -> usize {
        a.chars().zip(b.chars()).take_while(|(ca, cb)| ca == cb).count()
    }

    /// Find common suffix length between two strings (after prefix)
    fn common_suffix_len(a: &str, b: &str, prefix_len: usize) -> usize {
        let a_remaining: Vec<char> = a.chars().skip(prefix_len).collect();
        let b_remaining: Vec<char> = b.chars().skip(prefix_len).collect();
        a_remaining
            .iter()
            .rev()
            .zip(b_remaining.iter().rev())
            .take_while(|(ca, cb)| ca == cb)
            .count()
    }

    /// Highlight only the differing part of a line
    fn highlight_diff(line: &str, other: &str, color: &str) -> String {
        let prefix_len = common_prefix_len(line, other);
        let suffix_len = common_suffix_len(line, other, prefix_len);

        let line_chars: Vec<char> = line.chars().collect();
        let diff_end = line_chars.len().saturating_sub(suffix_len);

        if prefix_len >= diff_end {
            return line.to_string();
        }

        let prefix: String = line_chars[..prefix_len].iter().collect();
        let diff: String = line_chars[prefix_len..diff_end].iter().collect();
        let suffix: String = line_chars[diff_end..].iter().collect();

        format!("{prefix}{color}{diff}{RESET}{suffix}")
    }

    /// Compare two strings line by line and print differences with colors
    fn print_diff(before: &str, after: &str) {
        let before_lines: Vec<&str> = before.lines().collect();
        let after_lines: Vec<&str> = after.lines().collect();

        println!("\n{}", "=".repeat(80));
        println!("CIRCUIT COMPARISON (red = removed, green = added)");
        println!("{}\n", "=".repeat(80));

        let max_lines = before_lines.len().max(after_lines.len());

        for i in 0..max_lines {
            let before_line = before_lines.get(i).copied().unwrap_or("");
            let after_line = after_lines.get(i).copied().unwrap_or("");

            if before_line == after_line {
                println!("  {before_line}");
            } else {
                if !before_line.is_empty() {
                    let highlighted = highlight_diff(before_line, after_line, RED);
                    println!("- {highlighted}");
                }
                if !after_line.is_empty() {
                    let highlighted = highlight_diff(after_line, before_line, GREEN);
                    println!("+ {highlighted}");
                }
            }
        }

        println!("\n{}", "=".repeat(80));
    }

    #[test]
    fn test_rewriter() {
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rewriter = Rewriter::default();

        for (name, ty, _) in &scope.inputs {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(&mut random, idx, NodeKind::Input, ty, None);
        }
        for (name, ty, _) in &scope.globals {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(&mut random, idx, NodeKind::Input, ty, None);
        }

        forest.random(&mut random, &ctx, &scope);

        // Clone forest before rewriting to compare
        let forest_before = forest.clone();

        // Format circuit before rewriting (with fixed return)
        let before = builder.format_circuit(&scope, &forest_before);

        // Apply rewriter
        rewriter.apply_random(&mut random, &mut forest, &ctx, &scope);

        // Format circuit after rewriting (with same fixed return)
        let after = builder.format_circuit(&scope, &forest);

        // Print diff with colors
        print_diff(&before, &after);
    }
}
