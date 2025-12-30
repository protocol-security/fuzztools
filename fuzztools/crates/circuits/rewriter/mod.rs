use crate::circuits::ast::{forest::Forest, nodes::Node, operators::Operator, types::*};
use petgraph::graph::NodeIndex;
use rand::Rng;
use rules::{Rule, RuleKind, RULES};
use std::collections::HashMap;

pub mod rules;

pub struct Rewriter {
    rules: &'static [Rule],
    op_rules: HashMap<Operator, Vec<usize>>,
    type_rules: HashMap<TypeKind, Vec<usize>>,
}

// ────────────────────────────────────────────────────────────────────────────────
// Rewriter implementation
// ────────────────────────────────────────────────────────────────────────────────

// @todo this was heavily done by claude, redo manually

impl Rewriter {
    /// Build rule indices once at construction time
    pub fn new() -> Self {
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

    /// O(n), is the best i can do for now @todo
    pub fn apply_random(&self, random: &mut impl Rng, forest: &mut Forest) {
        let mut choice: Option<(NodeIndex, usize)> = None;
        let mut count = 0u32;

        // 1. Operator-specific rules
        for (&op, nodes) in &forest.operators {
            let Some(rules) = self.op_rules.get(&op) else { continue };
            for &n in nodes {
                let (left, right) = (forest.left(n), forest.right(n));
                for &i in rules {
                    if matches_rule(forest, Some(op), left, right, &self.rules[i].kind) {
                        count += 1;
                        if random.random_ratio(1, count) {
                            choice = Some((n, i));
                        }
                    }
                }
            }
        }

        // 2. Type-based inject rules
        for (&ty_kind, nodes) in &forest.type_kinds {
            let Some(rules) = self.type_rules.get(&ty_kind) else { continue };
            for &n in nodes {
                if op_of(forest, n).is_some() {
                    continue
                }
                let (left, right) = (forest.left(n), forest.right(n));
                for &i in rules {
                    if matches_rule(forest, None, left, right, &self.rules[i].kind) {
                        count += 1;
                        if random.random_ratio(1, count) {
                            choice = Some((n, i));
                        }
                    }
                }
            }
        }

        if let Some((node, idx)) = choice {
            self.apply(random, forest, node, &self.rules[idx].kind);
        }
    }

    fn apply(&self, random: &mut impl Rng, forest: &mut Forest, n: NodeIndex, kind: &RuleKind) {
        match kind {
            RuleKind::SwapOperands { .. } => forest.swap_operands(n),
            RuleKind::Associate { .. } => do_associate(forest, n),
            RuleKind::Distribute { outer, inner } => do_distribute(forest, n, *outer, *inner),
            RuleKind::Identity { op, identity_right } => {
                do_identity(forest, n, *op, *identity_right)
            }
            RuleKind::Absorb { op } => do_absorb(forest, n, *op),
            RuleKind::SelfInverse { op } => do_self_inverse(forest, n, *op),
            RuleKind::Idempotent { op } => do_idempotent(forest, n, *op),
            RuleKind::DoubleUnary { op } => do_double_unary(forest, n, *op),
            RuleKind::AddNegSub => do_add_neg_sub(forest, n),
            RuleKind::NegZeroSub => do_neg_zero_sub(forest, n),
            RuleKind::FlipComparison => do_flip_comparison(forest, n),
            RuleKind::NegateComparison => do_negate_comparison(forest, n),
            RuleKind::DeMorgan => do_demorgan(forest, n),
            RuleKind::ComplementXor => do_complement_xor(forest, n),
            RuleKind::InjectAddSub => do_inject(random, forest, n, Operator::Add, Operator::Sub),
            RuleKind::InjectSubAdd => do_inject(random, forest, n, Operator::Sub, Operator::Add),
            RuleKind::InjectMulDiv => do_inject_nonzero(random, forest, n),
            RuleKind::InjectXorXor => do_inject(random, forest, n, Operator::Xor, Operator::Xor),
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

    match (kind, op) {
        (RuleKind::SwapOperands { ops }, Some(o)) => ops.contains(&o) && is_binary,

        (RuleKind::Associate { ops }, Some(o)) => {
            ops.contains(&o) &&
                is_binary &&
                (left.is_some_and(|l| op_of(f, l) == Some(o)) ||
                    right.is_some_and(|r| op_of(f, r) == Some(o)))
        }

        (RuleKind::Distribute { outer, inner }, Some(o)) => {
            (o == *outer && left.is_some_and(|l| op_of(f, l) == Some(*inner))) ||
                (o == *inner &&
                    left.is_some_and(|l| op_of(f, l) == Some(*outer)) &&
                    right.is_some_and(|r| op_of(f, r) == Some(*outer)))
        }

        (RuleKind::Identity { op: rule_op, identity_right }, Some(o))
            if o == *rule_op && is_binary =>
        {
            let check = if *identity_right { right } else { left };
            check.is_some_and(|c| is_identity(f, c, o))
        }

        (RuleKind::Identity { .. }, _) => left.is_some_and(|l| is_numeric_or_bool(f, l)),

        (RuleKind::Absorb { op: rule_op }, Some(o)) => {
            o == *rule_op &&
                is_binary &&
                (left.is_some_and(|l| is_absorbing(f, l, o)) ||
                    right.is_some_and(|r| is_absorbing(f, r, o)))
        }

        (RuleKind::SelfInverse { op: rule_op }, Some(o)) => {
            o == *rule_op && is_binary && left == right
        }

        (RuleKind::Idempotent { op: rule_op }, Some(o)) => {
            (o == *rule_op && is_binary && left == right) ||
                left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }

        (RuleKind::DoubleUnary { op: rule_op }, Some(o)) => {
            (o == *rule_op &&
                is_unary &&
                left.is_some_and(|i| op_of(f, i) == Some(o) && f.right(i).is_none())) ||
                can_apply_unary(f, left, *rule_op)
        }

        (RuleKind::DoubleUnary { op }, None) => can_apply_unary(f, left, *op),

        (RuleKind::AddNegSub, Some(Operator::Sub)) => is_binary,
        (RuleKind::AddNegSub, Some(Operator::Add)) => {
            right.is_some_and(|r| op_of(f, r) == Some(Operator::Neg))
        }

        (RuleKind::NegZeroSub, Some(Operator::Neg)) => is_unary,
        (RuleKind::NegZeroSub, Some(Operator::Sub)) => left.is_some_and(|l| is_lit(f, l, "0")),

        (RuleKind::FlipComparison, Some(o)) => matches!(
            o,
            Operator::Less | Operator::Greater | Operator::LessOrEqual | Operator::GreaterOrEqual
        ),

        (RuleKind::NegateComparison, Some(o)) => {
            o.is_comparison() ||
                (o == Operator::Not &&
                    left.and_then(|i| op_of(f, i)).is_some_and(|x| x.is_comparison()))
        }

        (RuleKind::DeMorgan, Some(Operator::Not)) => left
            .and_then(|i| op_of(f, i))
            .is_some_and(|x| matches!(x, Operator::And | Operator::Or)),
        (RuleKind::DeMorgan, Some(Operator::And | Operator::Or)) => {
            left.is_some_and(|l| op_of(f, l) == Some(Operator::Not)) &&
                right.is_some_and(|r| op_of(f, r) == Some(Operator::Not))
        }

        (RuleKind::ComplementXor, Some(Operator::Not)) => is_unary,
        (RuleKind::ComplementXor, Some(Operator::Xor)) => right.is_some_and(|r| is_one(f, r)),

        (RuleKind::DoubleMulTwo, Some(Operator::Add)) => left == right,
        (RuleKind::DoubleMulTwo, Some(Operator::Mul)) => right.is_some_and(|r| is_lit(f, r, "2")),

        (RuleKind::MulNegOneNeg, Some(Operator::Mul)) => right.is_some_and(|r| is_lit(f, r, "-1")),
        (RuleKind::MulNegOneNeg, Some(Operator::Neg)) => is_unary,

        (RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv, _) => {
            left.is_some_and(|l| matches!(f.ty(l), Type::Field | Type::Integer(_)))
        }

        (RuleKind::InjectXorXor, _) => {
            left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_) | Type::Boolean))
        }

        _ => false,
    }
}

/// Get all operators that a rule can target (for building the index)
fn operators_for_rule(kind: &RuleKind) -> Vec<Operator> {
    match kind {
        RuleKind::SwapOperands { ops } | RuleKind::Associate { ops } => ops.to_vec(),
        RuleKind::Distribute { outer, inner } => vec![*outer, *inner],
        RuleKind::Identity { op, .. } |
        RuleKind::Absorb { op } |
        RuleKind::SelfInverse { op } |
        RuleKind::Idempotent { op } |
        RuleKind::DoubleUnary { op } => vec![*op],
        RuleKind::AddNegSub => vec![Operator::Add, Operator::Sub],
        RuleKind::NegZeroSub => vec![Operator::Neg, Operator::Sub],
        RuleKind::FlipComparison => {
            vec![Operator::Less, Operator::Greater, Operator::LessOrEqual, Operator::GreaterOrEqual]
        }
        RuleKind::NegateComparison => vec![
            Operator::Less,
            Operator::Greater,
            Operator::LessOrEqual,
            Operator::GreaterOrEqual,
            Operator::Equal,
            Operator::NotEqual,
            Operator::Not,
        ],
        RuleKind::DeMorgan => vec![Operator::Not, Operator::And, Operator::Or],
        RuleKind::ComplementXor => vec![Operator::Not, Operator::Xor],
        RuleKind::DoubleMulTwo => vec![Operator::Add, Operator::Mul],
        RuleKind::MulNegOneNeg => vec![Operator::Mul, Operator::Neg],
        RuleKind::InjectAddSub |
        RuleKind::InjectSubAdd |
        RuleKind::InjectMulDiv |
        RuleKind::InjectXorXor => vec![],
    }
}

/// Get all type kinds that an inject rule can target (for building the index)
fn types_for_inject_rule(kind: &RuleKind) -> Vec<TypeKind> {
    match kind {
        RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv => {
            vec![TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned]
        }
        RuleKind::InjectXorXor => vec![TypeKind::Signed, TypeKind::Unsigned, TypeKind::Boolean],
        _ => vec![],
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Rule implementations
// ═══════════════════════════════════════════════════════════════════════════════

fn do_associate(f: &mut Forest, n: NodeIndex) {
    let Some(op) = op_of(f, n) else { return };
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

fn do_distribute(f: &mut Forest, n: NodeIndex, outer: Operator, inner: Operator) {
    if op_of(f, n) != Some(outer) {
        return
    }
    let Some(left) = f.left(n).filter(|&l| op_of(f, l) == Some(inner)) else { return };

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
    let edges = f.incoming_edges(n); // Capture BEFORE creating new nodes
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
    let val = match op {
        Operator::Sub | Operator::Xor => "0",
        Operator::Div => "1",
        _ => return,
    };
    let lit = f.literal(val.into(), ty);
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
        let edges = f.incoming_edges(n); // Capture BEFORE creating new nodes
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
    let edges = f.incoming_edges(n); // Capture BEFORE creating new nodes
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
            let operand = f.left(n).unwrap();
            let ty = ret_of(f, n);
            let zero = f.literal("0".into(), ty);
            set_op(f, n, Operator::Sub);
            f.replace_operand(n, 0, zero);
            f.add_operand(n, 1, operand);
        }
        Some(Operator::Sub) if f.left(n).is_some_and(|l| is_lit(f, l, "0")) => {
            let right = f.right(n).unwrap();
            set_op(f, n, Operator::Neg);
            f.remove_operand(n, 0);
            f.remove_operand(n, 1);
            f.add_operand(n, 0, right);
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
            let edges = f.incoming_edges(n); // Capture BEFORE creating new nodes
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
            let edges = f.incoming_edges(n); // Capture BEFORE creating new nodes
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

fn do_inject(random: &mut impl Rng, f: &mut Forest, n: NodeIndex, op1: Operator, op2: Operator) {
    let ty = f.ty(n);
    let edges = f.incoming_edges(n); // Capture BEFORE creating new nodes
    let r = f.literal(random.random::<u32>().to_string(), ty.clone());
    let first = f.operator(op1, ty.clone(), n, Some(r));
    let second = f.operator(op2, ty, first, Some(r));
    f.redirect_edges(n, second, &edges);
}

fn do_inject_nonzero(random: &mut impl Rng, f: &mut Forest, n: NodeIndex) {
    let ty = f.ty(n);
    let edges = f.incoming_edges(n); // Capture BEFORE creating new nodes
    let r = f.literal(random.random::<u32>().saturating_add(1).to_string(), ty.clone());
    let first = f.operator(Operator::Mul, ty.clone(), n, Some(r));
    let second = f.operator(Operator::Div, ty, first, Some(r));
    f.redirect_edges(n, second, &edges);
}

fn do_double_mul_two(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Add) if f.left(n) == f.right(n) => {
            let two = f.literal("2".into(), f.ty(n));
            set_op(f, n, Operator::Mul);
            f.replace_operand(n, 1, two);
        }
        Some(Operator::Mul) if f.right(n).is_some_and(|r| is_lit(f, r, "2")) => {
            let left = f.left(n).unwrap();
            set_op(f, n, Operator::Add);
            f.replace_operand(n, 1, left);
        }
        _ => {}
    }
}

fn do_mul_neg_one_neg(f: &mut Forest, n: NodeIndex) {
    match op_of(f, n) {
        Some(Operator::Mul) if f.right(n).is_some_and(|r| is_lit(f, r, "-1")) => {
            set_op(f, n, Operator::Neg);
            f.remove_operand(n, 1);
        }
        Some(Operator::Neg) => {
            let ty = ret_of(f, n);
            let neg_one = f.literal("-1".into(), ty);
            set_op(f, n, Operator::Mul);
            f.add_operand(n, 1, neg_one);
        }
        _ => {}
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════════

#[inline]
fn op_of(f: &Forest, n: NodeIndex) -> Option<Operator> {
    match &f.graph[n] {
        Node::Operator { op, .. } => Some(*op),
        _ => None,
    }
}

#[inline]
fn ret_of(f: &Forest, n: NodeIndex) -> Type {
    match &f.graph[n] {
        Node::Operator { ret, .. } => ret.clone(),
        _ => f.ty(n),
    }
}

#[inline]
fn set_op(f: &mut Forest, n: NodeIndex, new_op: Operator) {
    if let Node::Operator { op, .. } = &mut f.graph[n] {
        *op = new_op;
    }
}

#[inline]
fn redirect(f: &mut Forest, from: NodeIndex, to: NodeIndex) {
    let edges = f.incoming_edges(from);
    f.redirect_edges(from, to, &edges);
}

#[inline]
fn is_lit(f: &Forest, n: NodeIndex, val: &str) -> bool {
    matches!(&f.graph[n], Node::Literal { value, .. } if value == val)
}

#[inline]
fn is_one(f: &Forest, n: NodeIndex) -> bool {
    matches!(&f.graph[n], Node::Literal { value, .. } if value == "1" || value == "true")
}

#[inline]
fn is_identity(f: &Forest, n: NodeIndex, op: Operator) -> bool {
    match op {
        Operator::Add | Operator::Sub | Operator::Xor | Operator::Or => is_lit(f, n, "0"),
        Operator::Mul | Operator::Div | Operator::And => is_one(f, n),
        _ => false,
    }
}

#[inline]
fn is_absorbing(f: &Forest, n: NodeIndex, op: Operator) -> bool {
    match op {
        Operator::Mul | Operator::And => is_lit(f, n, "0"),
        Operator::Or => is_one(f, n),
        _ => false,
    }
}

#[inline]
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

fn negate_cmp(op: Operator) -> Option<Operator> {
    match op {
        Operator::Less => Some(Operator::GreaterOrEqual),
        Operator::Greater => Some(Operator::LessOrEqual),
        Operator::LessOrEqual => Some(Operator::Greater),
        Operator::GreaterOrEqual => Some(Operator::Less),
        Operator::Equal => Some(Operator::NotEqual),
        Operator::NotEqual => Some(Operator::Equal),
        _ => None,
    }
}

#[inline]
fn make_one(f: &mut Forest, ty: &Type) -> NodeIndex {
    f.literal(if matches!(ty, Type::Boolean) { "true" } else { "1" }.into(), ty.clone())
}

#[inline]
fn make_identity(f: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    match op {
        Operator::Mul | Operator::Div => make_one(f, ty),
        _ => f.literal("0".into(), ty.clone()),
    }
}

#[inline]
fn make_absorbing(f: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    match op {
        Operator::Or => make_one(f, ty),
        _ => f.literal("0".into(), ty.clone()),
    }
}
