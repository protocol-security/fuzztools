pub mod rules;

use crate::circuits::ast::{
    forest::Forest,
    nodes::Node,
    operators::Operator,
    types::*,
};
use petgraph::graph::NodeIndex;
use rand::Rng;
use rules::{Rule, RuleKind, EQUIVALENCE_RULES, OBFUSCATION_RULES, SIMPLIFICATION_RULES};

pub struct Rewriter<'a> {
    rules: &'a [Rule],
}

impl<'a> Rewriter<'a> {
    #[inline]
    pub fn new(rules: &'a [Rule]) -> Self { Self { rules } }

    #[inline]
    pub fn equivalence() -> Rewriter<'static> { Rewriter { rules: EQUIVALENCE_RULES } }

    #[inline]
    pub fn obfuscation() -> Rewriter<'static> { Rewriter { rules: OBFUSCATION_RULES } }

    #[inline]
    pub fn simplification() -> Rewriter<'static> { Rewriter { rules: SIMPLIFICATION_RULES } }

    /// Apply one random applicable rule using reservoir sampling - O(n*m) time, O(1) space
    pub fn apply_random(&self, random: &mut impl Rng, forest: &mut Forest) {
        let mut choice: Option<(NodeIndex, usize)> = None;
        let mut count = 0u32;

        for n in forest.graph.node_indices() {
            let ctx = Cache::new(forest, n);

            for (i, rule) in self.rules.iter().enumerate() {
                if ctx.matches(forest, rule) {
                    count += 1;
                    if random.random_ratio(1, count) {
                        choice = Some((n, i));
                    }
                }
            }
        }

        if let Some((node, idx)) = choice {
            self.apply(random, forest, node, &self.rules[idx]);
        }
    }

    fn apply(&self, random: &mut impl Rng, forest: &mut Forest, node: NodeIndex, rule: &Rule) {
        match &rule.kind {
            RuleKind::SwapOperands { .. } => forest.swap_operands(node),
            RuleKind::Associate { .. } => self.do_associate(forest, node),
            RuleKind::Distribute { outer, inner } => self.do_distribute(forest, node, *outer, *inner),
            RuleKind::Identity { op, identity_right } => self.do_identity(forest, node, *op, *identity_right),
            RuleKind::Absorb { op } => self.do_absorb(forest, node, *op),
            RuleKind::SelfInverse { op } => self.do_self_inverse(forest, node, *op),
            RuleKind::Idempotent { op } => self.do_idempotent(forest, node, *op),
            RuleKind::DoubleUnary { op } => self.do_double_unary(forest, node, *op),
            RuleKind::AddNegSub => self.do_add_neg_sub(forest, node),
            RuleKind::NegZeroSub => self.do_neg_zero_sub(forest, node),
            RuleKind::FlipComparison => self.do_flip_comparison(forest, node),
            RuleKind::NegateComparison => self.do_negate_comparison(forest, node),
            RuleKind::DeMorgan => self.do_demorgan(forest, node),
            RuleKind::ComplementXor => self.do_complement_xor(forest, node),
            RuleKind::InjectAddSub => self.do_inject(random, forest, node, Operator::Add, Operator::Sub),
            RuleKind::InjectSubAdd => self.do_inject(random, forest, node, Operator::Sub, Operator::Add),
            RuleKind::InjectMulDiv => self.do_inject_nonzero(random, forest, node),
            RuleKind::InjectXorXor => self.do_inject(random, forest, node, Operator::Xor, Operator::Xor),
            RuleKind::DoubleMulTwo => self.do_double_mul_two(forest, node),
            RuleKind::MulNegOneNeg => self.do_mul_neg_one_neg(forest, node),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════════
    // RULE IMPLEMENTATION
    // ═══════════════════════════════════════════════════════════════════════════════

    fn do_associate(&self, forest: &mut Forest, node: NodeIndex) {
        let Some(op) = op_of(forest, node) else { return };
        let (left, right) = (forest.left(node), forest.right(node));

        // Try left associativity: (a op b) op c -> a op (b op c)
        if let Some(l) = left.filter(|&l| op_of(forest, l) == Some(op)) {
            let (a, b, c) = (forest.left(l).unwrap(), forest.right(l).unwrap(), right.unwrap());
            let new_right = forest.operator(op, b, Some(c));
            forest.replace_operand(node, 0, a);
            forest.replace_operand(node, 1, new_right);
            return;
        }

        // Try right associativity: a op (b op c) -> (a op b) op c
        if let Some(r) = right.filter(|&r| op_of(forest, r) == Some(op)) {
            let (a, b, c) = (left.unwrap(), forest.left(r).unwrap(), forest.right(r).unwrap());
            let new_left = forest.operator(op, a, Some(b));
            forest.replace_operand(node, 0, new_left);
            forest.replace_operand(node, 1, c);
        }
    }

    fn do_distribute(&self, forest: &mut Forest, node: NodeIndex, outer: Operator, inner: Operator) {
        let Some(op) = op_of(forest, node) else { return };
        if op != outer { return }

        let Some(left) = forest.left(node).filter(|&l| op_of(forest, l) == Some(inner)) else { return };
        let (a, b, c) = (forest.left(left).unwrap(), forest.right(left).unwrap(), forest.right(node).unwrap());

        let new_left = forest.operator(outer, a, Some(c));
        let new_right = forest.operator(outer, b, Some(c));
        set_op(forest, node, inner);
        forest.replace_operand(node, 0, new_left);
        forest.replace_operand(node, 1, new_right);
    }

    fn do_identity(&self, forest: &mut Forest, node: NodeIndex, op: Operator, identity_right: bool) {
        if op_of(forest, node) == Some(op) {
            let (keep, check) = if identity_right {
                (forest.left(node), forest.right(node))
            } else {
                (forest.right(node), forest.left(node))
            };

            if check.map(|n| is_identity(forest, n, op)).unwrap_or(false) {
                if let Some(k) = keep {
                    let ty = forest.ty(k);
                    let name = forest.next_var();
                    forest.variable(name, ty, k);
                    return;
                }
            }
        }

        // Inject identity
        let ty = forest.ty(node);
        let id = make_identity(forest, &ty, op);
        if identity_right {
            forest.operator(op, node, Some(id));
        } else {
            forest.operator(op, id, Some(node));
        }
    }

    fn do_absorb(&self, forest: &mut Forest, node: NodeIndex, op: Operator) {
        let ty = forest.ty(node);
        let absorb = make_absorbing(forest, &ty, op);
        let name = forest.next_var();
        forest.variable(name, ty, absorb);
    }

    fn do_self_inverse(&self, forest: &mut Forest, node: NodeIndex, op: Operator) {
        let ty = forest.ty(node);
        let result = match op {
            Operator::Sub | Operator::Xor => make_lit(forest, &ty, "0"),
            Operator::Div => make_lit(forest, &ty, "1"),
            _ => return,
        };
        let name = forest.next_var();
        forest.variable(name, ty, result);
    }

    fn do_idempotent(&self, forest: &mut Forest, node: NodeIndex, op: Operator) {
        if op_of(forest, node) == Some(op) && forest.left(node) == forest.right(node) {
            if let Some(operand) = forest.left(node) {
                let ty = forest.ty(operand);
                let name = forest.next_var();
                forest.variable(name, ty, operand);
                return;
            }
        }
        // Inject: x -> x op x
        if matches!(forest.ty(node), Type::Boolean) {
            forest.operator(op, node, Some(node));
        }
    }

    fn do_double_unary(&self, forest: &mut Forest, node: NodeIndex, op: Operator) {
        // Simplify: op(op(x)) -> x
        if op_of(forest, node) == Some(op) && is_unary(forest, node) {
            if let Some(inner) = forest.left(node).filter(|&i| op_of(forest, i) == Some(op) && is_unary(forest, i)) {
                if let Some(x) = forest.left(inner) {
                    let ty = forest.ty(x);
                    let name = forest.next_var();
                    forest.variable(name, ty, x);
                    return;
                }
            }
        }
        // Inject: x -> op(op(x))
        let inner = forest.operator(op, node, None);
        forest.operator(op, inner, None);
    }

    fn do_add_neg_sub(&self, forest: &mut Forest, node: NodeIndex) {
        match op_of(forest, node) {
            Some(Operator::Sub) => {
                let right = forest.right(node).unwrap();
                let neg_right = forest.operator(Operator::Neg, right, None);
                set_op(forest, node, Operator::Add);
                forest.replace_operand(node, 1, neg_right);
            }
            Some(Operator::Add) => {
                if let Some(r) = forest.right(node).filter(|&r| op_of(forest, r) == Some(Operator::Neg)) {
                    let b = forest.left(r).unwrap();
                    set_op(forest, node, Operator::Sub);
                    forest.replace_operand(node, 1, b);
                }
            }
            _ => {}
        }
    }

    fn do_neg_zero_sub(&self, forest: &mut Forest, node: NodeIndex) {
        match op_of(forest, node) {
            Some(Operator::Neg) => {
                let operand = forest.left(node).unwrap();
                let ty = forest.ty(node);
                let zero = make_lit(forest, &ty, "0");
                forest.operator(Operator::Sub, zero, Some(operand));
            }
            Some(Operator::Sub) if forest.left(node).map(|l| is_lit(forest, l, "0")).unwrap_or(false) => {
                let right = forest.right(node).unwrap();
                forest.operator(Operator::Neg, right, None);
            }
            _ => {}
        }
    }

    fn do_flip_comparison(&self, forest: &mut Forest, node: NodeIndex) {
        let flipped = match op_of(forest, node) {
            Some(Operator::Less) => Operator::Greater,
            Some(Operator::Greater) => Operator::Less,
            Some(Operator::LessOrEqual) => Operator::GreaterOrEqual,
            Some(Operator::GreaterOrEqual) => Operator::LessOrEqual,
            _ => return,
        };
        set_op(forest, node, flipped);
        forest.swap_operands(node);
    }

    fn do_negate_comparison(&self, forest: &mut Forest, node: NodeIndex) {
        let op = op_of(forest, node);

        // Simplify: not(a cmp b) -> a neg_cmp b
        if op == Some(Operator::Not) {
            if let Some(inner) = forest.left(node) {
                if let Some(negated) = op_of(forest, inner).and_then(negate_cmp) {
                    set_op(forest, inner, negated);
                    let ty = forest.ty(inner);
                    let name = forest.next_var();
                    forest.variable(name, ty, inner);
                    return;
                }
            }
        }

        // Inject: a cmp b -> not(a neg_cmp b)
        if let Some(negated) = op.and_then(negate_cmp) {
            set_op(forest, node, negated);
            forest.operator(Operator::Not, node, None);
        }
    }

    fn do_demorgan(&self, forest: &mut Forest, node: NodeIndex) {
        let op = op_of(forest, node);

        // not(a and/or b) -> not(a) or/and not(b)
        if op == Some(Operator::Not) {
            if let Some(inner) = forest.left(node) {
                let dual = match op_of(forest, inner) {
                    Some(Operator::And) => Operator::Or,
                    Some(Operator::Or) => Operator::And,
                    _ => return,
                };
                let (a, b) = (forest.left(inner).unwrap(), forest.right(inner).unwrap());
                let not_a = forest.operator(Operator::Not, a, None);
                let not_b = forest.operator(Operator::Not, b, None);
                forest.operator(dual, not_a, Some(not_b));
                return;
            }
        }

        // not(a) and/or not(b) -> not(a or/and b)
        if matches!(op, Some(Operator::And | Operator::Or)) {
            let (left, right) = (forest.left(node), forest.right(node));
            if left.map(|l| op_of(forest, l) == Some(Operator::Not)).unwrap_or(false)
                && right.map(|r| op_of(forest, r) == Some(Operator::Not)).unwrap_or(false)
            {
                let dual = if op == Some(Operator::And) { Operator::Or } else { Operator::And };
                let (a, b) = (forest.left(left.unwrap()).unwrap(), forest.left(right.unwrap()).unwrap());
                let inner = forest.operator(dual, a, Some(b));
                forest.operator(Operator::Not, inner, None);
            }
        }
    }

    fn do_complement_xor(&self, forest: &mut Forest, node: NodeIndex) {
        match op_of(forest, node) {
            Some(Operator::Not) => {
                let operand = forest.left(node).unwrap();
                let ty = forest.ty(node);
                let one = make_one(forest, &ty);
                forest.operator(Operator::Xor, operand, Some(one));
            }
            Some(Operator::Xor) if forest.right(node).map(|r| is_one(forest, r)).unwrap_or(false) => {
                let left = forest.left(node).unwrap();
                forest.operator(Operator::Not, left, None);
            }
            _ => {}
        }
    }

    fn do_inject(&self, random: &mut impl Rng, forest: &mut Forest, node: NodeIndex, op1: Operator, op2: Operator) {
        let ty = forest.ty(node);
        let r = forest.literal(random.random::<u32>().to_string(), ty);
        let first = forest.operator(op1, node, Some(r));
        forest.operator(op2, first, Some(r));
    }

    fn do_inject_nonzero(&self, random: &mut impl Rng, forest: &mut Forest, node: NodeIndex) {
        let ty = forest.ty(node);
        let r = forest.literal(random.random::<u32>().saturating_add(1).to_string(), ty);
        let first = forest.operator(Operator::Mul, node, Some(r));
        forest.operator(Operator::Div, first, Some(r));
    }

    fn do_double_mul_two(&self, forest: &mut Forest, node: NodeIndex) {
        match op_of(forest, node) {
            Some(Operator::Add) if forest.left(node) == forest.right(node) => {
                let ty = forest.ty(node);
                let two = make_lit(forest, &ty, "2");
                set_op(forest, node, Operator::Mul);
                forest.replace_operand(node, 1, two);
            }
            Some(Operator::Mul) if forest.right(node).map(|r| is_lit(forest, r, "2")).unwrap_or(false) => {
                let left = forest.left(node).unwrap();
                set_op(forest, node, Operator::Add);
                forest.replace_operand(node, 1, left);
            }
            _ => {}
        }
    }

    fn do_mul_neg_one_neg(&self, forest: &mut Forest, node: NodeIndex) {
        match op_of(forest, node) {
            Some(Operator::Mul) if forest.right(node).map(|r| is_lit(forest, r, "-1")).unwrap_or(false) => {
                let left = forest.left(node).unwrap();
                forest.operator(Operator::Neg, left, None);
            }
            Some(Operator::Neg) => {
                let operand = forest.left(node).unwrap();
                let ty = forest.ty(node);
                let neg_one = make_lit(forest, &ty, "-1");
                forest.operator(Operator::Mul, operand, Some(neg_one));
            }
            _ => {}
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Cached node context for fast rule matching
// ═══════════════════════════════════════════════════════════════════════════════

struct Cache {
    op: Option<Operator>,
    left: Option<NodeIndex>,
    right: Option<NodeIndex>,
    is_binary: bool,
    is_unary: bool,
}

impl Cache {
    #[inline]
    fn new(forest: &Forest, n: NodeIndex) -> Self {
        let op = op_of(forest, n);
        let left = forest.left(n);
        let right = forest.right(n);
        Self { op, left, right, is_binary: left.is_some() && right.is_some(), is_unary: left.is_some() && right.is_none() }
    }

    fn matches(&self, forest: &Forest, rule: &Rule) -> bool {
        match (&rule.kind, self.op) {
            (RuleKind::SwapOperands { ops }, Some(o)) => ops.contains(&o) && self.is_binary,

            (RuleKind::Associate { ops }, Some(o)) => {
                ops.contains(&o) && self.is_binary &&
                (self.left.map(|l| op_of(forest, l) == Some(o)).unwrap_or(false) ||
                 self.right.map(|r| op_of(forest, r) == Some(o)).unwrap_or(false))
            }

            (RuleKind::Distribute { outer, inner }, Some(o)) => {
                (o == *outer && self.left.map(|l| op_of(forest, l) == Some(*inner)).unwrap_or(false)) ||
                (o == *inner && self.left.map(|l| op_of(forest, l) == Some(*outer)).unwrap_or(false)
                             && self.right.map(|r| op_of(forest, r) == Some(*outer)).unwrap_or(false))
            }

            (RuleKind::Identity { op: rule_op, identity_right }, Some(o)) => {
                if o == *rule_op && self.is_binary {
                    let check = if *identity_right { self.right } else { self.left };
                    check.map(|n| is_identity(forest, n, o)).unwrap_or(false)
                } else {
                    matches!(forest.ty(self.left.unwrap_or(NodeIndex::new(0))), Type::Field | Type::Integer(_) | Type::Boolean)
                }
            }

            (RuleKind::Absorb { op: rule_op }, Some(o)) => {
                o == *rule_op && self.is_binary &&
                (self.left.map(|l| is_absorbing(forest, l, o)).unwrap_or(false) ||
                 self.right.map(|r| is_absorbing(forest, r, o)).unwrap_or(false))
            }

            (RuleKind::SelfInverse { op: rule_op }, Some(o)) => o == *rule_op && self.is_binary && self.left == self.right,

            (RuleKind::Idempotent { op: rule_op }, Some(o)) => {
                (o == *rule_op && self.is_binary && self.left == self.right) ||
                self.left.map(|l| matches!(forest.ty(l), Type::Boolean)).unwrap_or(false)
            }

            (RuleKind::DoubleUnary { op: rule_op }, Some(o)) => {
                (o == *rule_op && self.is_unary &&
                 self.left.map(|i| op_of(forest, i) == Some(o) && is_unary(forest, i)).unwrap_or(false)) ||
                is_unary_valid(forest, self.left, *rule_op)
            }

            (RuleKind::DoubleUnary { op }, None) => is_unary_valid(forest, self.left, *op),

            (RuleKind::AddNegSub, Some(o)) => {
                (o == Operator::Sub && self.is_binary) ||
                (o == Operator::Add && self.right.map(|r| op_of(forest, r) == Some(Operator::Neg)).unwrap_or(false))
            }

            (RuleKind::NegZeroSub, Some(o)) => {
                (o == Operator::Neg && self.is_unary) ||
                (o == Operator::Sub && self.left.map(|l| is_lit(forest, l, "0")).unwrap_or(false))
            }

            (RuleKind::FlipComparison, Some(o)) => matches!(o, Operator::Less | Operator::Greater | Operator::LessOrEqual | Operator::GreaterOrEqual),

            (RuleKind::NegateComparison, Some(o)) => {
                o.is_comparison() ||
                (o == Operator::Not && self.left.and_then(|i| op_of(forest, i)).map(|x| x.is_comparison()).unwrap_or(false))
            }

            (RuleKind::DeMorgan, Some(o)) => {
                (o == Operator::Not && self.left.and_then(|i| op_of(forest, i)).map(|x| matches!(x, Operator::And | Operator::Or)).unwrap_or(false)) ||
                (matches!(o, Operator::And | Operator::Or) &&
                 self.left.map(|l| op_of(forest, l) == Some(Operator::Not)).unwrap_or(false) &&
                 self.right.map(|r| op_of(forest, r) == Some(Operator::Not)).unwrap_or(false))
            }

            (RuleKind::ComplementXor, Some(o)) => {
                (o == Operator::Not && self.is_unary) ||
                (o == Operator::Xor && self.right.map(|r| is_one(forest, r)).unwrap_or(false))
            }

            (RuleKind::DoubleMulTwo, Some(o)) => {
                (o == Operator::Add && self.left == self.right) ||
                (o == Operator::Mul && self.right.map(|r| is_lit(forest, r, "2")).unwrap_or(false))
            }

            (RuleKind::MulNegOneNeg, Some(o)) => {
                (o == Operator::Mul && self.right.map(|r| is_lit(forest, r, "-1")).unwrap_or(false)) ||
                (o == Operator::Neg && self.is_unary)
            }

            (RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv, _) => {
                self.left.map(|l| matches!(forest.ty(l), Type::Field | Type::Integer(_))).unwrap_or(false)
            }

            (RuleKind::InjectXorXor, _) => {
                self.left.map(|l| matches!(forest.ty(l), Type::Integer(_) | Type::Boolean)).unwrap_or(false)
            }

            _ => false,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Unified helpers (avoid duplication)
// ═══════════════════════════════════════════════════════════════════════════════

#[inline]
fn op_of(forest: &Forest, n: NodeIndex) -> Option<Operator> {
    match &forest.graph[n] { Node::Operator { op } => Some(*op), _ => None }
}

#[inline]
fn set_op(forest: &mut Forest, n: NodeIndex, new_op: Operator) {
    if let Node::Operator { op } = &mut forest.graph[n] { *op = new_op; }
}

#[inline]
fn is_unary(forest: &Forest, n: NodeIndex) -> bool {
    forest.left(n).is_some() && forest.right(n).is_none()
}

#[inline]
fn is_lit(forest: &Forest, n: NodeIndex, val: &str) -> bool {
    matches!(&forest.graph[n], Node::Literal { value, .. } if value == val)
}

#[inline]
fn is_one(forest: &Forest, n: NodeIndex) -> bool {
    matches!(&forest.graph[n], Node::Literal { value, .. } if value == "1" || value == "true")
}

#[inline]
fn is_identity(forest: &Forest, n: NodeIndex, op: Operator) -> bool {
    match op {
        Operator::Add | Operator::Sub | Operator::Xor | Operator::Or => is_lit(forest, n, "0"),
        Operator::Mul | Operator::Div | Operator::And => is_one(forest, n),
        _ => false,
    }
}

#[inline]
fn is_absorbing(forest: &Forest, n: NodeIndex, op: Operator) -> bool {
    match op {
        Operator::Mul | Operator::And => is_lit(forest, n, "0"),
        Operator::Or => is_one(forest, n),
        _ => false,
    }
}

fn is_unary_valid(forest: &Forest, node: Option<NodeIndex>, op: Operator) -> bool {
    let Some(n) = node else { return false };
    match forest.ty(n) {
        Type::Field => op == Operator::Neg,
        Type::Integer(Integer { signed, .. }) => op == Operator::Not || (op == Operator::Neg && signed),
        Type::Boolean => op == Operator::Not,
        _ => false,
    }
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
fn make_lit(forest: &mut Forest, ty: &Type, val: &str) -> NodeIndex {
    forest.literal(val.into(), ty.clone())
}

#[inline]
fn make_one(forest: &mut Forest, ty: &Type) -> NodeIndex {
    forest.literal(if matches!(ty, Type::Boolean) { "true" } else { "1" }.into(), ty.clone())
}

#[inline]
fn make_identity(forest: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    match op { Operator::Mul | Operator::Div => make_one(forest, ty), _ => make_lit(forest, ty, "0") }
}

#[inline]
fn make_absorbing(forest: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    match op { Operator::Or => make_one(forest, ty), _ => make_lit(forest, ty, "0") }
}
