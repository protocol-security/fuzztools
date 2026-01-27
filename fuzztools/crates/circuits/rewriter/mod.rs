use crate::circuits::{
    ast::{forest::Forest, nodes::Node, operators::Operator, types::*},
    context::Context,
    scope::Scope,
};
use petgraph::{graph::NodeIndex, visit::EdgeRef, Direction};
use rand::{seq::IteratorRandom, Rng};
use rules::{Rule, RuleKind, RULES};
use std::collections::{HashMap, HashSet};
use Operator::{
    Add, And, Div, Equal, Greater, GreaterOrEqual, Less, LessOrEqual, Mod, Mul, Neg, Not, NotEqual,
    Or, Shl, Shr, Sub, Xor,
};

mod rules;
mod tests;

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
    /// Get rule names in the order defined in RULES (for ordered display)
    pub fn rule_names_ordered() -> Vec<String> {
        RULES.iter().map(|r| r.kind.name().to_string()).collect()
    }

    /// Randomly select a rule, then apply it to a matching node in the forest
    /// Returns the rule name if a rule was applied, None otherwise
    pub fn apply_random(
        &self,
        random: &mut impl Rng,
        forest: &mut Forest,
        ctx: &Context,
        scope: &Scope,
    ) -> Option<&'static str> {
        // Collect nodes that are If/Assert conditions (these can't be redirected safely)
        let condition_nodes = collect_condition_nodes(forest);

        // Collect all nodes that match a rule
        let mut matches = HashMap::<usize, Vec<NodeIndex>>::default();

        // 1. Operator-specific rules
        for (&op, nodes) in &forest.operators {
            if let Some(rules) = self.op_rules.get(&op) {
                for &idx in nodes {
                    // Skip removed nodes (can happen during rewriting)
                    if !forest.graph.contains_node(idx) {
                        continue;
                    }
                    // Skip non-operator nodes (e.g., Assignment nodes with operators like +=)
                    if !matches!(forest.graph[idx], Node::Operator { .. }) {
                        continue;
                    }

                    let (left, right) = (forest.left(idx), forest.right(idx));
                    for &i in rules {
                        if condition_nodes.contains(&idx) {
                            continue;
                        }
                        if matches_rule(forest, Some(op), left, right, &self.rules[i].kind, idx) {
                            matches.entry(i).or_default().push(idx);
                        }
                    }
                }
            }
        }

        // 2. Type-based inject rules (for terminal nodes like literals/variables)
        // Skip inject rules if the forest is already large to prevent exponential blowup
        if forest.graph.node_count() < ctx.max_forest_size {
            for (&ty_kind, nodes) in &forest.type_kinds {
                if let Some(rules) = self.type_rules.get(&ty_kind) {
                    for &idx in nodes {
                        // Skip removed nodes (can happen during rewriting)
                        if !forest.graph.contains_node(idx) {
                            continue;
                        }
                        // Skip condition nodes - they're stored as direct NodeIndex, not edges
                        if condition_nodes.contains(&idx) {
                            continue;
                        }
                        if op_of(forest, idx).is_some() {
                            continue;
                        }
                        if is_assignment_lhs(forest, idx) {
                            continue;
                        }
                        if forest.graph.edges_directed(idx, Direction::Incoming).next().is_none() {
                            continue;
                        }
                        for &i in rules {
                            if matches_rule(forest, None, Some(idx), None, &self.rules[i].kind, idx)
                            {
                                matches.entry(i).or_default().push(idx);
                            }
                        }
                    }
                }
            }
        }

        // If we found matches in the main forest, apply ONE rule and return
        if !matches.is_empty() {
            // Randomly select which rule to apply
            let selected_rule_idx = matches.keys().choose(random).unwrap();
            let selected_rule = self.rules[*selected_rule_idx].clone().kind;
            let rule_name = selected_rule.name();

            // Get all nodes that match this specific rule
            let nodes_for_rule = matches.get(selected_rule_idx).unwrap();

            // Randomly select ONE node to apply the rule to (prevents exponential blowup)
            let selected_node = *nodes_for_rule.iter().choose(random).unwrap();

            // Apply the rule to the selected node
            if forest.graph.contains_node(selected_node) {
                self.apply(forest, selected_node, &selected_rule, ctx, scope, &condition_nodes);
                return Some(rule_name);
            }
        }

        // @todo apply to nested forests too
        None
    }

    fn apply(
        &self,
        forest: &mut Forest,
        idx: NodeIndex,
        kind: &RuleKind,
        ctx: &Context,
        scope: &Scope,
        condition_nodes: &HashSet<NodeIndex>,
    ) {
        if condition_nodes.contains(&idx) {
            return;
        }

        match kind {
            // Structural
            RuleKind::SwapOperands => forest.swap_operands(idx),
            RuleKind::Associate => do_associate(forest, idx),
            RuleKind::AssociateSub => do_associate_sub(forest, idx),
            RuleKind::AssociateDiv => do_associate_div(forest, idx),
            RuleKind::DivCommute => do_div_commute(forest, idx),
            RuleKind::DistributeMulAdd => do_distribute(forest, idx, Mul, Add),
            RuleKind::DistributeMulSub => do_distribute(forest, idx, Mul, Sub),
            RuleKind::DistributeAndOr => do_distribute(forest, idx, And, Or),
            RuleKind::DistributeOrAnd => do_distribute(forest, idx, Or, And),

            // Identity
            RuleKind::IdentityAdd => {
                do_identity(forest, idx, Add, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentitySub => {
                do_identity(forest, idx, Sub, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityMul => {
                do_identity(forest, idx, Mul, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityDiv => {
                do_identity(forest, idx, Div, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityXor => {
                do_identity(forest, idx, Xor, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityOr => {
                do_identity(forest, idx, Or, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityAnd => {
                do_identity(forest, idx, And, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityShl => {
                do_identity(forest, idx, Shl, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityShr => {
                do_identity(forest, idx, Shr, &forest.ty(forest.left(idx).unwrap()))
            }

            // Absorb
            RuleKind::AbsorbMul => do_absorb(forest, idx, Mul, ctx, scope),
            RuleKind::AbsorbAnd => do_absorb(forest, idx, And, ctx, scope),
            RuleKind::AbsorbOr => do_absorb(forest, idx, Or, ctx, scope),

            // SelfInverse
            RuleKind::SelfInverseSub => do_self_inverse(forest, idx, Sub, ctx, scope),
            RuleKind::SelfInverseXor => do_self_inverse(forest, idx, Xor, ctx, scope),
            RuleKind::SelfInverseDiv => do_self_inverse(forest, idx, Div, ctx, scope),

            // Idempotent
            RuleKind::IdempotentAnd => do_idempotent(forest, idx, And),
            RuleKind::IdempotentOr => do_idempotent(forest, idx, Or),

            // Unary
            RuleKind::DoubleNeg => do_double_unary(forest, idx, Neg),
            RuleKind::DoubleNot => do_double_unary(forest, idx, Not),
            RuleKind::AddNegSub => do_add_neg_sub(forest, idx),
            RuleKind::NegZeroSub => do_neg_zero_sub(forest, idx),

            // Comparison
            RuleKind::FlipComparison => do_flip_comparison(forest, idx),
            RuleKind::NegateComparison => do_negate_comparison(forest, idx, condition_nodes),
            RuleKind::ExpandComparison => do_expand_comparison(forest, idx),

            // Boolean logic
            RuleKind::DeMorgan => do_demorgan(forest, idx),
            RuleKind::ComplementXor => do_complement_xor(forest, idx),
            RuleKind::XorToAndOr => do_xor_to_and_or(forest, idx),

            // Modulo
            RuleKind::ModOne => do_mod_one(forest, idx),
            RuleKind::AndToMod => do_and_to_mod(forest, idx),

            // Shift
            RuleKind::ShiftZero => do_shift_zero(forest, idx),

            // Obfuscation
            RuleKind::InjectAddSub => do_inject(forest, idx, Add, Sub, ctx, scope),
            RuleKind::InjectSubAdd => do_inject(forest, idx, Sub, Add, ctx, scope),
            RuleKind::InjectMulDiv => do_inject_nonzero(forest, idx, ctx, scope),
            RuleKind::InjectXorXor => do_inject(forest, idx, Xor, Xor, ctx, scope),
            RuleKind::InjectDivDiv => do_inject_div_div(forest, idx, ctx, scope),
            RuleKind::InjectOrZero => do_inject_or_zero(forest, idx),
            RuleKind::InjectAndSelf => do_inject_and_self(forest, idx),

            // Simplification
            RuleKind::DoubleMulTwo => do_double_mul_two(forest, idx),
            RuleKind::MulNegOneNeg => do_mul_neg_one_neg(forest, idx),
        }

        forest.remove_if_orphaned(idx);
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Rule matching
// ═══════════════════════════════════════════════════════════════════════════════

fn matches_rule(
    forest: &Forest,
    op: Option<Operator>,
    left: Option<NodeIndex>,
    right: Option<NodeIndex>,
    kind: &RuleKind,
    idx: NodeIndex,
) -> bool {
    let is_binary = left.is_some() && right.is_some();
    let is_unary = left.is_some() && right.is_none();
    let left_op = left.and_then(|l| op_of(forest, l));
    let right_op = right.and_then(|r| op_of(forest, r));

    match (kind, op) {
        // ─────────────────────────────────────────────────────────────────────────
        // Structural rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::SwapOperands, Some(o)) if is_binary => {
            matches!(o, Add | Mul | And | Or | Xor | Equal | NotEqual)
        }

        (RuleKind::Associate, Some(o)) if is_binary => {
            matches!(o, Add | Mul | And | Or | Xor) && (left_op == Some(o) || right_op == Some(o))
        }

        // ((a - b) - c) <-> (a - (b + c))
        (RuleKind::AssociateSub, Some(Sub)) if is_binary => {
            left_op == Some(Sub) || right_op == Some(Add)
        }

        // ((a / b) * c) <-> (a * (c / b)) - only valid for Field as integers round down
        (RuleKind::AssociateDiv, Some(Mul)) if is_binary => {
            matches!(forest.ty(left.unwrap()), Type::Field) &&
                (left_op == Some(Div) || right_op == Some(Div))
        }

        // (a / b) -> ((1 / b) * a) - only valid for Field as integers round down
        (RuleKind::DivCommute, Some(Div)) if is_binary => {
            matches!(forest.ty(left.unwrap()), Type::Field)
        }

        // ((1 / b) * a) -> (a / b) - only valid for Field as integers round down
        (RuleKind::DivCommute, Some(Mul)) if is_binary => {
            let l = left.unwrap();
            matches!(forest.ty(l), Type::Field) &&
                left_op == Some(Div) &&
                forest.left(l).is_some_and(|ll| is_one(forest, ll))
        }

        // (a * (b + c)) <-> ((a * b) + (a * c))
        // Forward: Mul with Add/Sub child
        (RuleKind::DistributeMulAdd, Some(Mul)) if is_binary => {
            right_op == Some(Add) || left_op == Some(Add)
        }
        (RuleKind::DistributeMulSub, Some(Mul)) if is_binary => {
            right_op == Some(Sub) || left_op == Some(Sub)
        }
        // Reverse: Add/Sub with two Mul children
        (RuleKind::DistributeMulAdd, Some(Add)) if is_binary => {
            left_op == Some(Mul) && right_op == Some(Mul)
        }
        (RuleKind::DistributeMulSub, Some(Sub)) if is_binary => {
            left_op == Some(Mul) && right_op == Some(Mul)
        }
        // And/Or distribution
        (RuleKind::DistributeAndOr, Some(And)) if is_binary => {
            right_op == Some(Or) || left_op == Some(Or)
        }
        (RuleKind::DistributeAndOr, Some(Or)) if is_binary => {
            left_op == Some(And) && right_op == Some(And)
        }
        (RuleKind::DistributeOrAnd, Some(Or)) if is_binary => {
            right_op == Some(And) || left_op == Some(And)
        }
        (RuleKind::DistributeOrAnd, Some(And)) if is_binary => {
            left_op == Some(Or) && right_op == Some(Or)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Identity rules
        // ─────────────────────────────────────────────────────────────────────────
        // Simplification case: a op identity -> a
        (RuleKind::IdentityAdd, Some(Add)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            ty.is_numeric() &&
                (is_identity(forest, l, Add, &ty) || is_identity(forest, r, Add, &ty))
        }
        (RuleKind::IdentitySub, Some(Sub)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            ty.is_numeric() && is_identity(forest, r, Sub, &ty)
        }
        (RuleKind::IdentityMul, Some(Mul)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            ty.is_numeric() &&
                (is_identity(forest, l, Mul, &ty) || is_identity(forest, r, Mul, &ty))
        }
        (RuleKind::IdentityDiv, Some(Div)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            ty.is_numeric() && is_identity(forest, r, Div, &ty)
        }
        (RuleKind::IdentityXor, Some(Xor)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_identity(forest, l, Xor, &forest.ty(l)) || is_identity(forest, r, Xor, &forest.ty(r))
        }
        (RuleKind::IdentityOr, Some(Or)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_identity(forest, l, Or, &forest.ty(l)) || is_identity(forest, r, Or, &forest.ty(r))
        }
        (RuleKind::IdentityAnd, Some(And)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            ty.is_bool() && (is_identity(forest, l, And, &ty) || is_identity(forest, r, And, &ty))
        }
        (RuleKind::IdentityShl, Some(Shl)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            ty.is_integer() && is_identity(forest, r, Shl, &ty)
        }
        (RuleKind::IdentityShr, Some(Shr)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            ty.is_integer() && is_identity(forest, r, Shr, &ty)
        }

        // Injection case: a -> a op identity
        (RuleKind::IdentityAnd, None) => left.is_some_and(|l| forest.ty(l).is_bool()),
        (RuleKind::IdentityOr | RuleKind::IdentityXor, None) => {
            left.is_some_and(|l| forest.ty(l).is_primitive())
        }
        (
            RuleKind::IdentityAdd |
            RuleKind::IdentitySub |
            RuleKind::IdentityMul |
            RuleKind::IdentityDiv,
            None,
        ) => left.is_some_and(|l| forest.ty(l).is_numeric()),
        (RuleKind::IdentityShl | RuleKind::IdentityShr, None) => {
            left.is_some_and(|l| forest.ty(l).is_integer())
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Absorb rules
        // ─────────────────────────────────────────────────────────────────────────
        // Simplification case: a op absorbing -> absorbing
        (RuleKind::AbsorbMul, Some(Mul)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_absorbing(forest, l, Mul, &forest.ty(l)) ||
                is_absorbing(forest, r, Mul, &forest.ty(r))
        }
        (RuleKind::AbsorbAnd, Some(And)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_absorbing(forest, l, And, &forest.ty(l)) ||
                is_absorbing(forest, r, And, &forest.ty(r))
        }
        (RuleKind::AbsorbOr, Some(Or)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_absorbing(forest, l, Or, &forest.ty(l)) || is_absorbing(forest, r, Or, &forest.ty(r))
        }

        // Injection case: absorbing -> a op absorbing
        (RuleKind::AbsorbMul, None) => {
            left.is_some_and(|l| is_absorbing(forest, l, Mul, &forest.ty(l)))
        }
        (RuleKind::AbsorbAnd, None) => {
            left.is_some_and(|l| is_absorbing(forest, l, And, &forest.ty(l)))
        }
        (RuleKind::AbsorbOr, None) => {
            left.is_some_and(|l| is_absorbing(forest, l, Or, &forest.ty(l)))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // SelfInverse rules: a op a = identity
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::SelfInverseSub, Some(Sub)) if is_binary && left == right => {
            forest.ty(left.unwrap()).is_numeric()
        }
        (RuleKind::SelfInverseXor, Some(Xor)) if is_binary && left == right => {
            let ty = forest.ty(left.unwrap());
            ty.is_integer() || ty.is_bool()
        }
        (RuleKind::SelfInverseDiv, Some(Div)) if is_binary && left == right => {
            let l = left.unwrap();
            forest.ty(l).is_numeric() && !is_zero(forest, l)
        }

        // Injection case: identity -> a op a
        (RuleKind::SelfInverseSub, None) => {
            left.is_some_and(|l| is_identity(forest, l, Sub, &forest.ty(l)))
        }
        (RuleKind::SelfInverseXor, None) => {
            left.is_some_and(|l| is_identity(forest, l, Xor, &forest.ty(l)))
        }
        (RuleKind::SelfInverseDiv, None) => {
            left.is_some_and(|l| is_identity(forest, l, Div, &forest.ty(l)))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Idempotent rules: a op a -> a
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::IdempotentAnd, Some(And)) if is_binary && left == right => {
            let ty = forest.ty(left.unwrap());
            ty.is_integer() || ty.is_bool()
        }
        (RuleKind::IdempotentOr, Some(Or)) if is_binary && left == right => {
            let ty = forest.ty(left.unwrap());
            ty.is_integer() || ty.is_bool()
        }

        // Injection: a -> a op a
        (RuleKind::IdempotentAnd, None) => left.is_some_and(|l| is_idempotent(And, &forest.ty(l))),
        (RuleKind::IdempotentOr, None) => left.is_some_and(|l| is_idempotent(Or, &forest.ty(l))),

        // ─────────────────────────────────────────────────────────────────────────
        // Unary rules
        // ─────────────────────────────────────────────────────────────────────────
        // Absorbing op(op(a)) --> a
        (RuleKind::DoubleNeg, Some(Neg)) if is_unary => {
            let l = left.unwrap();
            op_of(forest, l) == Some(Neg) && forest.right(l).is_none()
        }
        (RuleKind::DoubleNot, Some(Not)) if is_unary => {
            let l = left.unwrap();
            op_of(forest, l) == Some(Not) && forest.right(l).is_none()
        }

        // Injection: a -> op(op(a))
        (RuleKind::DoubleNeg, None) => left.is_some_and(|l| forest.ty(l).is_numeric()),
        (RuleKind::DoubleNot, None) => left.is_some_and(|l| {
            let ty = forest.ty(l);
            ty.is_bool() || ty.is_signed()
        }),

        // (a - b) -> (a + (-b))
        (RuleKind::AddNegSub, Some(Sub)) if is_binary => forest.ty(left.unwrap()).is_signed(),

        // (a + (-b)) -> (a - b) @todo ((-b) + a)
        (RuleKind::AddNegSub, Some(Add)) if is_binary => {
            matches!(right_op, Some(Neg))
        }

        // (-a) -> (0 - a)
        (RuleKind::NegZeroSub, Some(Neg)) if is_unary => true,

        // (0 - a) -> (-a) @todo ((-a) + 0) and ((-a) - 0) and equivalent
        (RuleKind::NegZeroSub, Some(Sub)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            forest.ty(r).is_signed() && is_zero(forest, l)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Comparison rules
        // ─────────────────────────────────────────────────────────────────────────
        // a cmp b -> b flip(cmp) a
        (RuleKind::FlipComparison, Some(o)) if is_binary => o.is_comparison(),

        // a cmp b -> !(a !cmp b) @todo should do the reverse
        (RuleKind::NegateComparison, Some(o)) if is_binary => o.is_comparison(),

        // (a cmpeq b) -> ((a cmp b) || (a == b))
        (RuleKind::ExpandComparison, Some(LessOrEqual | GreaterOrEqual)) if is_binary => true,

        // ((a cmp b) || (a == b)) -> (a cmpeq b)
        (RuleKind::ExpandComparison, Some(Or)) if is_binary => {
            (matches!(left_op, Some(Less | Greater)) && right_op == Some(Equal)) ||
                (left_op == Some(Equal) && matches!(right_op, Some(Less | Greater)))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Boolean rules
        // ─────────────────────────────────────────────────────────────────────────
        // !(a op b) -> (!a !op !b)
        (RuleKind::DeMorgan, Some(Not)) if is_unary => {
            matches!(left_op, Some(And | Or))
        }

        //(!a !op !b) -> !(a op b)
        (RuleKind::DeMorgan, Some(And | Or)) if is_binary => {
            left_op == Some(Not) && right_op == Some(Not)
        }

        // !a -> a ^ true (only for boolean NOT, not bitwise NOT on integers)
        (RuleKind::ComplementXor, Some(Not)) if is_unary => {
            forest.ty(left.unwrap()).is_bool() && ret_of(forest, idx).is_bool()
        }

        // a ^ true -> !a
        (RuleKind::ComplementXor, Some(Xor)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            forest.ty(l).is_bool() && (is_one(forest, r) || is_one(forest, l))
        }

        // (a ^ b) -> ((!a & b) | (a & !b))
        // Exclude cases where an operand is a literal (handled by ComplementXor)
        (RuleKind::XorToAndOr, Some(Xor)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            forest.ty(l).is_bool() &&
                !is_one(forest, l) &&
                !is_one(forest, r) &&
                !is_zero(forest, l) &&
                !is_zero(forest, r)
        }
        // ((!a & b) | (a & !b)) -> (a ^ b)
        (RuleKind::XorToAndOr, Some(Or)) if is_binary => {
            left_op == Some(And) && right_op == Some(And)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Modulo rules
        // ─────────────────────────────────────────────────────────────────────────
        // a % 1 -> 0
        (RuleKind::ModOne, Some(Mod)) if is_binary => is_one(forest, right.unwrap()),

        // 0 -> a % 1
        (RuleKind::ModOne, None) if is_unary => forest.ty(left.unwrap()).is_integer(),

        // a & 1 -> a % 2
        (RuleKind::AndToMod, Some(And)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            // `u1` can't hold `2`
            is_one(forest, r) && matches!(ty, Type::Integer(i) if i.bits > 1)
        }

        // a % 2 -> a & 1
        (RuleKind::AndToMod, Some(Mod)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_two(forest, r) && forest.ty(l).is_integer()
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Shift rules
        // ─────────────────────────────────────────────────────────────────────────
        // a << 0 -> a, a >> 0 -> a
        (RuleKind::ShiftZero, Some(Shl | Shr)) if is_binary => is_zero(forest, right.unwrap()),

        // ─────────────────────────────────────────────────────────────────────────
        // Simplification rules
        // ─────────────────────────────────────────────────────────────────────────
        // a + a -> a * 2
        (RuleKind::DoubleMulTwo, Some(Add)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            // `u1` can't hold `2`
            l == r && (matches!(ty, Type::Field) || matches!(ty, Type::Integer(i) if i.bits > 1))
        }

        // a * 2 -> a + a
        (RuleKind::DoubleMulTwo, Some(Mul)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_two(forest, r) && forest.ty(l).is_numeric()
        }

        // (a * (-1)) -> (-a) @todo ((-1) * a)
        (RuleKind::MulNegOneNeg, Some(Mul)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            let ty = forest.ty(l);
            is_neg_one(forest, r) && (ty.is_signed() || matches!(ty, Type::Field))
        }

        // (-a) -> (a * (-1)) @todo ((-1) * a)
        (RuleKind::MulNegOneNeg, Some(Neg)) if is_unary => {
            let ty = forest.ty(left.unwrap());
            ty.is_signed() || matches!(ty, Type::Field)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Injection rules (obfuscation) - only when there's no existing operator
        // ─────────────────────────────────────────────────────────────────────────
        // a -> ((a - n) + n) @todo subadd
        (RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv, None) => {
            let ty = forest.ty(left.unwrap());
            matches!(ty, Type::Field) || ty.is_signed()
        }

        // a -> ((a ^ n) ^ n) // a -> a & a
        (RuleKind::InjectXorXor | RuleKind::InjectAndSelf, None) => {
            let ty = forest.ty(left.unwrap());
            ty.is_integer() || ty.is_bool()
        }

        // 1 -> r / r
        (RuleKind::InjectDivDiv, None) => {
            let l = left.unwrap();
            is_one(forest, l) && forest.ty(l).is_numeric()
        }

        // a -> a | 0
        (RuleKind::InjectOrZero, None) => forest.ty(left.unwrap()).is_integer(),

        _ => false,
    }
}

/// Get all operators that a rule can target (for building the index)
fn operators_for_rule(kind: &RuleKind) -> Vec<Operator> {
    match kind {
        RuleKind::SwapOperands => vec![Add, Mul, And, Or, Xor, Equal, NotEqual],
        RuleKind::Associate => vec![Add, Mul, And, Or, Xor],
        RuleKind::AssociateSub | RuleKind::IdentitySub | RuleKind::SelfInverseSub => vec![Sub],
        RuleKind::AssociateDiv | RuleKind::IdentityMul | RuleKind::AbsorbMul => vec![Mul],
        RuleKind::DivCommute => vec![Div, Mul],
        RuleKind::DistributeMulAdd => vec![Mul, Add],
        RuleKind::DistributeMulSub => vec![Mul, Sub],
        RuleKind::DistributeAndOr => vec![And, Or],
        RuleKind::DistributeOrAnd => vec![Or, And],
        RuleKind::IdentityAdd => vec![Add],
        RuleKind::IdentityDiv | RuleKind::SelfInverseDiv => vec![Div],
        RuleKind::IdentityXor => vec![Xor],
        RuleKind::IdentityOr => vec![Or],
        RuleKind::IdentityAnd => vec![And],
        RuleKind::IdentityShl => vec![Shl],
        RuleKind::IdentityShr => vec![Shr],
        RuleKind::AbsorbAnd => vec![And],
        RuleKind::AbsorbOr => vec![Or],
        RuleKind::SelfInverseXor => vec![Xor],
        RuleKind::IdempotentAnd => vec![And],
        RuleKind::IdempotentOr => vec![Or],
        RuleKind::DoubleNeg => vec![Neg],
        RuleKind::DoubleNot => vec![Not],
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
        // Absorb injection: 0 -> a * 0, 0 -> a & 0, true -> a | true
        RuleKind::AbsorbMul => vec![TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned],
        RuleKind::AbsorbAnd => vec![TypeKind::Boolean, TypeKind::Signed, TypeKind::Unsigned],
        RuleKind::AbsorbOr => vec![TypeKind::Boolean],
        // SelfInverse injection: 0 -> a - a, 0 -> a ^ a, 1 -> a / a
        RuleKind::SelfInverseSub => vec![TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned],
        RuleKind::SelfInverseXor => vec![TypeKind::Boolean, TypeKind::Signed, TypeKind::Unsigned],
        RuleKind::SelfInverseDiv => vec![TypeKind::Field],
        _ => vec![],
    }
}

/// Collect all nodes that are used as If/Assert conditions.
/// These nodes are stored as direct `NodeIndex` fields (not edges), so `redirect` won't update
/// them.
fn collect_condition_nodes(forest: &Forest) -> HashSet<NodeIndex> {
    let mut conditions = HashSet::new();
    for idx in forest.graph.node_indices() {
        match &forest.graph[idx] {
            Node::If { condition, else_ifs, .. } => {
                conditions.insert(*condition);
                for (cond, _) in else_ifs {
                    conditions.insert(*cond);
                }
            }
            Node::Assert { condition, .. } => {
                conditions.insert(*condition);
            }
            _ => {}
        }
    }
    conditions
}

// ═══════════════════════════════════════════════════════════════════════════════
// Rule implementations
// ═══════════════════════════════════════════════════════════════════════════════

fn do_associate(f: &mut Forest, idx: NodeIndex) {
    let left = f.left(idx).unwrap();
    let right = f.right(idx).unwrap();

    // (a op b) idx c -> a idx (b op c)
    if op_of(f, left).is_some() {
        let a = f.left(left).unwrap();
        let b = f.right(left).unwrap();

        // Remove old edges
        f.graph.remove_edge(f.left_edge(left));
        f.graph.remove_edge(f.right_edge(left));
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));

        // Add new edges a -> idx, op -> idx
        f.graph.add_edge(idx, a, 0);
        f.graph.add_edge(idx, left, 1);

        // left now becomes (b op c)
        f.graph.add_edge(left, b, 0);
        f.graph.add_edge(left, right, 1);
    }
    // a op (b op c) -> (a op b) op c
    else if op_of(f, right).is_some() {
        let b = f.left(right).unwrap();
        let c = f.right(right).unwrap();

        // Remove old edges
        f.graph.remove_edge(f.left_edge(right));
        f.graph.remove_edge(f.right_edge(right));
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));

        // Add new edges: op -> idx, c -> idx
        f.graph.add_edge(idx, right, 0);
        f.graph.add_edge(idx, c, 1);
        // right now becomes (a op b)
        f.graph.add_edge(right, left, 0);
        f.graph.add_edge(right, b, 1);
    }
}

fn do_associate_sub(f: &mut Forest, idx: NodeIndex) {
    let left = f.left(idx).unwrap();
    let right = f.right(idx).unwrap();

    // (a - b) idx c -> a idx (b + c)
    if op_of(f, left) == Some(Operator::Sub) {
        let a = f.left(left).unwrap();
        let b = f.right(left).unwrap();

        // Remove old edges
        f.graph.remove_edge(f.left_edge(left));
        f.graph.remove_edge(f.right_edge(left));
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));

        // Change left from Sub to Add
        f.set_operator(left, Operator::Add);

        // Add new edges: a -> idx, + -> idx
        f.graph.add_edge(idx, a, 0);
        f.graph.add_edge(idx, left, 1);

        // left now becomes (b + c)
        f.graph.add_edge(left, b, 0);
        f.graph.add_edge(left, right, 1);
    }
    // a idx (b + c) -> (a - b) idx c
    else if op_of(f, right) == Some(Operator::Add) {
        let b = f.left(right).unwrap();
        let c = f.right(right).unwrap();

        // Remove old edges
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));
        f.graph.remove_edge(f.left_edge(right));
        f.graph.remove_edge(f.right_edge(right));

        // Change right from Add to Sub
        f.set_operator(right, Operator::Sub);

        // Add new edges: - -> idx, c -> idx
        f.graph.add_edge(idx, right, 0);
        f.graph.add_edge(idx, c, 1);

        // right now becomes (a - b)
        f.graph.add_edge(right, left, 0);
        f.graph.add_edge(right, b, 1);
    }
}

fn do_associate_div(f: &mut Forest, idx: NodeIndex) {
    let left = f.left(idx).unwrap();
    let right = f.right(idx).unwrap();

    // (a / b) idx c -> a idx (c / b)
    if op_of(f, left) == Some(Operator::Div) {
        let a = f.left(left).unwrap();
        let b = f.right(left).unwrap();

        // Remove old edges
        f.graph.remove_edge(f.left_edge(left));
        f.graph.remove_edge(f.right_edge(left));
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));

        // Add new edges: a -> idx, / -> idx
        f.graph.add_edge(idx, a, 0);
        f.graph.add_edge(idx, left, 1);

        // left now becomes (c / b)
        f.graph.add_edge(left, right, 0);
        f.graph.add_edge(left, b, 1);
    }
    // a idx (c / b) -> (a / b) idx c
    else if op_of(f, right) == Some(Operator::Div) {
        let c = f.left(right).unwrap();
        let b = f.right(right).unwrap();

        // Remove old edges
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));
        f.graph.remove_edge(f.left_edge(right));
        f.graph.remove_edge(f.right_edge(right));

        // Add new edges: / -> idx, c -> idx
        f.graph.add_edge(idx, right, 0);
        f.graph.add_edge(idx, c, 1);

        // right now becomes (a / b)
        f.graph.add_edge(right, left, 0);
        f.graph.add_edge(right, b, 1);
    }
}

fn do_div_commute(f: &mut Forest, idx: NodeIndex) {
    // (a / b) -> ((1 / b) * a)
    if op_of(f, idx) == Some(Operator::Div) {
        let a = f.left(idx).unwrap();
        let b = f.right(idx).unwrap();

        // Remove old edges
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));

        // Create constant 1 and (1 / b) node
        let one = f.literal("1Field".into(), Type::Field);
        let one_div_b = f.operator(Operator::Div, Type::Field, one, Some(b));

        // Change idx from Div to Mul
        f.set_operator(idx, Operator::Mul);

        // Add new edges: (1/b) -> idx, a -> idx
        f.graph.add_edge(idx, one_div_b, 0);
        f.graph.add_edge(idx, a, 1);
    }
    // ((1 / b) * a) -> (a / b)
    else if op_of(f, idx) == Some(Operator::Mul) {
        let left = f.left(idx).unwrap();
        if op_of(f, left) == Some(Operator::Div) && is_one(f, f.left(left).unwrap()) {
            let b = f.right(left).unwrap();
            let a = f.right(idx).unwrap();

            // Remove old edges
            f.graph.remove_edge(f.left_edge(idx));
            f.graph.remove_edge(f.right_edge(idx));

            // Change idx from Mul to Div
            f.set_operator(idx, Operator::Div);

            // Add new edges: a -> idx, b -> idx
            f.graph.add_edge(idx, a, 0);
            f.graph.add_edge(idx, b, 1);
        }
    }
}

fn do_distribute(f: &mut Forest, idx: NodeIndex, outer: Operator, inner: Operator) {
    let left = f.left(idx).unwrap();
    let right = f.right(idx).unwrap();

    // (a inner b) outer c -> (a outer c) inner (b outer c)
    if op_of(f, idx) == Some(outer) && op_of(f, left) == Some(inner) {
        let a = f.left(left).unwrap();
        let b = f.right(left).unwrap();
        let c = right;

        // Remove old edges
        f.graph.remove_edge(f.left_edge(left));
        f.graph.remove_edge(f.right_edge(left));
        f.graph.remove_edge(f.left_edge(idx));
        f.graph.remove_edge(f.right_edge(idx));

        // Create new node for (b outer c)
        let b_outer_c = f.operator(outer, f.ty(idx), b, Some(c));

        // Change idx from outer to inner
        f.set_operator(idx, inner);

        // Change left from inner to outer
        f.set_operator(left, outer);

        // Add new edges: (a outer c) -> idx, (b outer c) -> idx
        f.graph.add_edge(idx, left, 0);
        f.graph.add_edge(idx, b_outer_c, 1);

        // left now becomes (a outer c)
        f.graph.add_edge(left, a, 0);
        f.graph.add_edge(left, c, 1);
    }
    // (a outer c) inner (b outer c) -> (a inner b) outer c
    else if op_of(f, idx) == Some(inner)
        && op_of(f, left) == Some(outer)
        && op_of(f, right) == Some(outer)
    {
        let c_left = f.right(left).unwrap();
        let c_right = f.right(right).unwrap();

        // Check common operand c
        if c_left == c_right {
            let a = f.left(left).unwrap();
            let b = f.left(right).unwrap();
            let c = c_left;

            // Remove old edges
            f.graph.remove_edge(f.left_edge(left));
            f.graph.remove_edge(f.right_edge(left));
            f.graph.remove_edge(f.left_edge(idx));
            f.graph.remove_edge(f.right_edge(idx));

            // Change idx from inner to outer
            f.set_operator(idx, outer);

            // Change left from outer to inner
            f.set_operator(left, inner);

            // Add new edges: (a inner b) -> idx, c -> idx
            f.graph.add_edge(idx, left, 0);
            f.graph.add_edge(idx, c, 1);

            // left now becomes (a inner b)
            f.graph.add_edge(left, a, 0);
            f.graph.add_edge(left, b, 1);
        }
    }
}

fn do_identity(f: &mut Forest, idx: NodeIndex, op: Operator, _ty: &Type) {
    if op_of(f, idx) == Some(op) {
        // a op identity -> a
        let left = f.left(idx).unwrap();
        let right = f.right(idx).unwrap();
        let is_left_identity = is_identity(f, left, op, &f.ty(left));
        let keep = if is_left_identity { right } else { left };

        // Collect incoming edges before modification
        let incoming: Vec<_> = f.graph.edges_directed(idx, Direction::Incoming)
            .map(|e| (e.source(), *e.weight(), e.id()))
            .collect();

        // Remove incoming edges to idx
        for (_, _, edge_id) in &incoming {
            f.graph.remove_edge(*edge_id);
        }

        // Add edges from parents to keep
        for (parent, slot, _) in incoming {
            f.graph.add_edge(parent, keep, slot);
        }
    } else {
        // a -> a op identity
        let incoming: Vec<_> = f.graph.edges_directed(idx, Direction::Incoming)
            .map(|e| (e.source(), *e.weight(), e.id()))
            .collect();

        let ty = f.ty(idx);
        let id = make_identity(f, &ty, op);
        let new = f.operator(op, ty, idx, Some(id));

        // Remove incoming edges to idx
        for (_, _, edge_id) in &incoming {
            f.graph.remove_edge(*edge_id);
        }

        // Add edges from parents to new
        for (parent, slot, _) in incoming {
            f.graph.add_edge(parent, new, slot);
        }
    }
}

fn do_absorb(f: &mut Forest, idx: NodeIndex, op: Operator, ctx: &Context, scope: &Scope) {
    if op_of(f, idx) == Some(op) {
        let left = f.left(idx);
        let right = f.right(idx);
        if left.is_some_and(|l| is_absorbing(f, l, op, &f.ty(l))) ||
            right.is_some_and(|r| is_absorbing(f, r, op, &f.ty(r)))
        {
            let ty = ret_of(f, idx);
            let absorb = make_absorbing(f, &ty, op);
            f.redirect_edges(idx, absorb);
        }
        return;
    }

    let ty = f.ty(idx);
    if !is_absorbing(f, idx, op, &ty) {
        return;
    }

    let incoming = f.incoming_edges(idx);
    let value = ty.random_value(&mut rand::rng(), ctx, scope);
    let a = f.literal(value, ty.clone());
    let new = f.operator(op, ty, a, Some(idx));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_self_inverse(f: &mut Forest, idx: NodeIndex, op: Operator, ctx: &Context, scope: &Scope) {
    if op_of(f, idx) == Some(op) && f.left(idx) == f.right(idx) {
        let ty = ret_of(f, idx);
        let lit = match op {
            Sub | Xor => make_zero(f, &ty),
            Div => make_one(f, &ty),
            _ => return,
        };
        f.redirect_edges(idx, lit);
        return;
    }

    let ty = f.ty(idx);
    let is_ident = match op {
        Sub | Xor => is_zero(f, idx),
        Div => is_one(f, idx),
        _ => return,
    };
    if !is_ident {
        return;
    }

    let incoming = f.incoming_edges(idx);
    let mut value = ty.random_value(&mut rand::rng(), ctx, scope);
    if op == Div && (value.starts_with('0') || value.starts_with("-0") || value == "false") {
        value = match &ty {
            Type::Boolean => "true".into(),
            Type::Field => "1Field".into(),
            Type::Integer(i) => format!("1{}{}", if i.signed { "i" } else { "u" }, i.bits),
            _ => format!("1{ty}"),
        };
    }
    let a = f.literal(value, ty.clone());
    let new = f.operator(op, ty, a, Some(a));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_idempotent(f: &mut Forest, idx: NodeIndex, op: Operator) {
    if op_of(f, idx) == Some(op) && f.left(idx) == f.right(idx) {
        if let Some(operand) = f.left(idx) {
            f.redirect_edges(idx, operand);
        }
        return;
    }
    let ty = f.ty(idx);
    if !matches!(ty, Type::Boolean | Type::Integer(_)) {
        return;
    }
    let incoming = f.incoming_edges(idx);
    let new = f.operator(op, ty, idx, Some(idx));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_double_unary(f: &mut Forest, idx: NodeIndex, op: Operator) {
    if op_of(f, idx) == Some(op) && f.right(idx).is_none() {
        if let Some(inner) =
            f.left(idx).filter(|&i| op_of(f, i) == Some(op) && f.right(i).is_none())
        {
            if let Some(x) = f.left(inner) {
                f.redirect_edges(idx, x);
            }
        }
        return;
    }
    let incoming = f.incoming_edges(idx);
    let ty = f.ty(idx);
    let inner = f.operator(op, ty.clone(), idx, None);
    let outer = f.operator(op, ty, inner, None);
    for (parent, slot) in incoming {
        f.set_child(parent, slot, outer);
    }
}

fn do_add_neg_sub(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Sub) => {
            let right = f.right(idx).unwrap();
            let ty = f.ty(right);
            let neg = f.operator(Neg, ty, right, None);
            f.set_operator(idx, Add);
            f.set_child(idx, 1, neg);
        }
        Some(Add) if f.right(idx).is_some_and(|r| op_of(f, r) == Some(Neg)) => {
            let r = f.right(idx).unwrap();
            let b = f.left(r).unwrap();
            f.set_operator(idx, Sub);
            f.set_child(idx, 1, b);
        }
        _ => {}
    }
}

fn do_neg_zero_sub(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Neg) => {
            let operand = f.left(idx).unwrap();
            let ty = ret_of(f, idx);
            let zero = make_zero(f, &ty);
            f.set_operator(idx, Sub);
            f.set_child(idx, 0, zero);
            f.add_operand(idx, 1, operand, &f.ty(operand));
        }
        Some(Sub) if f.left(idx).is_some_and(|l| is_zero(f, l)) => {
            let right = f.right(idx).unwrap();
            f.set_operator(idx, Neg);
            f.set_child(idx, 0, right);
            f.remove_operand(idx, 1);
        }
        _ => {}
    }
}

fn do_flip_comparison(f: &mut Forest, idx: NodeIndex) {
    let flipped = match op_of(f, idx) {
        Some(Less) => Greater,
        Some(Greater) => Less,
        Some(LessOrEqual) => GreaterOrEqual,
        Some(GreaterOrEqual) => LessOrEqual,
        _ => return,
    };
    f.set_operator(idx, flipped);
    f.swap_operands(idx);
}

fn do_negate_comparison(f: &mut Forest, idx: NodeIndex, condition_nodes: &HashSet<NodeIndex>) {
    if op_of(f, idx) == Some(Not) {
        if let Some(inner) = f.left(idx) {
            if condition_nodes.contains(&inner) {
                return;
            }
            if let Some(negated) = op_of(f, inner).and_then(negate_cmp) {
                f.set_operator(inner, negated);
                f.redirect_edges(idx, inner);
                return;
            }
        }
    }
    if let Some(negated) = op_of(f, idx).and_then(negate_cmp) {
        let incoming = f.incoming_edges(idx);
        f.set_operator(idx, negated);
        let not_node = f.operator(Not, Type::Boolean, idx, None);
        for (parent, slot) in incoming {
            f.set_child(parent, slot, not_node);
        }
    }
}

fn do_expand_comparison(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(LessOrEqual) => {
            let (a, b) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            let less = f.operator(Less, Type::Boolean, a, Some(b));
            let eq = f.operator(Equal, Type::Boolean, a, Some(b));
            let new = f.operator(Or, Type::Boolean, less, Some(eq));
            f.redirect_edges(idx, new);
        }
        Some(GreaterOrEqual) => {
            let (a, b) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            let greater = f.operator(Greater, Type::Boolean, a, Some(b));
            let eq = f.operator(Equal, Type::Boolean, a, Some(b));
            let new = f.operator(Or, Type::Boolean, greater, Some(eq));
            f.redirect_edges(idx, new);
        }
        Some(Or) => {
            let (left, right) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            let left_op = op_of(f, left);
            let right_op = op_of(f, right);

            let new = if left_op == Some(Less) && right_op == Some(Equal) {
                let (a, b) = (f.left(left).unwrap(), f.right(left).unwrap());
                f.operator(LessOrEqual, Type::Boolean, a, Some(b))
            } else if left_op == Some(Greater) && right_op == Some(Equal) {
                let (a, b) = (f.left(left).unwrap(), f.right(left).unwrap());
                f.operator(GreaterOrEqual, Type::Boolean, a, Some(b))
            } else if left_op == Some(Equal) && right_op == Some(Less) {
                let (a, b) = (f.left(right).unwrap(), f.right(right).unwrap());
                f.operator(LessOrEqual, Type::Boolean, a, Some(b))
            } else if left_op == Some(Equal) && right_op == Some(Greater) {
                let (a, b) = (f.left(right).unwrap(), f.right(right).unwrap());
                f.operator(GreaterOrEqual, Type::Boolean, a, Some(b))
            } else {
                return;
            };
            f.redirect_edges(idx, new);
        }
        _ => {}
    }
}

fn do_demorgan(f: &mut Forest, idx: NodeIndex) {
    let op = op_of(f, idx);

    if op == Some(Not) {
        if let Some(inner) = f.left(idx) {
            let dual = match op_of(f, inner) {
                Some(And) => Or,
                Some(Or) => And,
                _ => return,
            };
            let (a, b) = (f.left(inner).unwrap(), f.right(inner).unwrap());
            let ty = ret_of(f, inner);
            let not_a = f.operator(Not, ty.clone(), a, None);
            let not_b = f.operator(Not, ty.clone(), b, None);
            let new = f.operator(dual, ty, not_a, Some(not_b));
            f.redirect_edges(idx, new);
            return;
        }
    }

    if matches!(op, Some(And | Or)) {
        let (left, right) = (f.left(idx), f.right(idx));
        if left.is_some_and(|l| op_of(f, l) == Some(Not)) &&
            right.is_some_and(|r| op_of(f, r) == Some(Not))
        {
            let dual = if op == Some(And) { Or } else { And };
            let (a, b) = (f.left(left.unwrap()).unwrap(), f.left(right.unwrap()).unwrap());
            let ty = f.ty(a);
            let inner = f.operator(dual, ty.clone(), a, Some(b));
            let new = f.operator(Not, ty, inner, None);
            f.redirect_edges(idx, new);
        }
    }
}

fn do_complement_xor(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Not) => {
            let operand = f.left(idx).unwrap();
            let ty = f.ty(operand);
            let one = make_one(f, &ty);
            f.set_operator(idx, Xor);
            f.add_operand(idx, 1, one, &ty);
        }
        Some(Xor) => {
            let left = f.left(idx);
            let right = f.right(idx);
            if right.is_some_and(|r| is_one(f, r)) {
                f.set_operator(idx, Not);
                f.remove_operand(idx, 1);
            } else if left.is_some_and(|l| is_one(f, l)) {
                let a = right.unwrap();
                f.set_operator(idx, Not);
                f.set_child(idx, 0, a);
                f.remove_operand(idx, 1);
            }
        }
        _ => {}
    }
}

fn do_xor_to_and_or(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Xor) => {
            let (a, b) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            let ty = ret_of(f, idx);
            let not_a = f.operator(Not, ty.clone(), a, None);
            let not_b = f.operator(Not, ty.clone(), b, None);
            let left_and = f.operator(And, ty.clone(), not_a, Some(b));
            let right_and = f.operator(And, ty.clone(), a, Some(not_b));
            let new = f.operator(Or, ty, left_and, Some(right_and));
            f.redirect_edges(idx, new);
        }
        Some(Or) => {
            let (left, right) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            if op_of(f, left) != Some(And) || op_of(f, right) != Some(And) {
                return;
            }
            let (ll, lr) = (f.left(left).unwrap(), f.right(left).unwrap());
            let (rl, rr) = (f.left(right).unwrap(), f.right(right).unwrap());
            if op_of(f, ll) != Some(Not) || op_of(f, rr) != Some(Not) {
                return;
            }
            let a_left = f.left(ll).unwrap();
            let a_right = rl;
            let b_left = lr;
            let b_right = f.left(rr).unwrap();
            if a_left == a_right && b_left == b_right {
                let ty = f.ty(a_left);
                let new = f.operator(Xor, ty, a_left, Some(b_left));
                f.redirect_edges(idx, new);
            }
        }
        _ => {}
    }
}

fn do_mod_one(f: &mut Forest, idx: NodeIndex) {
    if op_of(f, idx) == Some(Mod) && f.right(idx).is_some_and(|r| is_one(f, r)) {
        let ty = ret_of(f, idx);
        let zero = make_zero(f, &ty);
        f.redirect_edges(idx, zero);
    } else if is_zero(f, idx) {
        let incoming = f.incoming_edges(idx);
        let ty = f.ty(idx);
        let one = make_one(f, &ty);
        let new = f.operator(Mod, ty, idx, Some(one));
        for (parent, slot) in incoming {
            f.set_child(parent, slot, new);
        }
    }
}

fn do_and_to_mod(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(And) if f.right(idx).is_some_and(|r| is_one(f, r)) => {
            let ty = ret_of(f, idx);
            let two = make_two(f, &ty);
            f.set_operator(idx, Mod);
            f.set_child(idx, 1, two);
        }
        Some(Mod) if f.right(idx).is_some_and(|r| is_two(f, r)) => {
            let ty = ret_of(f, idx);
            let one = make_one(f, &ty);
            f.set_operator(idx, And);
            f.set_child(idx, 1, one);
        }
        _ => {}
    }
}

fn do_shift_zero(f: &mut Forest, idx: NodeIndex) {
    if matches!(op_of(f, idx), Some(Shl | Shr)) && f.right(idx).is_some_and(|r| is_zero(f, r)) {
        if let Some(left) = f.left(idx) {
            f.redirect_edges(idx, left);
        }
    }
}

fn do_inject(
    f: &mut Forest,
    idx: NodeIndex,
    op1: Operator,
    op2: Operator,
    ctx: &Context,
    scope: &Scope,
) {
    let incoming = f.incoming_edges(idx);
    let ty = f.ty(idx);
    let value = ty.random_value(&mut rand::rng(), ctx, scope);
    let r = f.literal(value, ty.clone());
    let first = f.operator(op1, ty.clone(), idx, Some(r));
    let second = f.operator(op2, ty, first, Some(r));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, second);
    }
}

fn do_inject_nonzero(f: &mut Forest, idx: NodeIndex, ctx: &Context, scope: &Scope) {
    let incoming = f.incoming_edges(idx);
    let ty = f.ty(idx);
    let mut value = ty.random_value(&mut rand::rng(), ctx, scope);
    if value.starts_with('0') || value.starts_with("-0") || value == "false" {
        value = match &ty {
            Type::Boolean => "true".into(),
            Type::Field => "1Field".into(),
            Type::Integer(i) => format!("1{}{}", if i.signed { "i" } else { "u" }, i.bits),
            _ => format!("1{ty}"),
        };
    }
    let r = f.literal(value, ty.clone());
    let first = f.operator(Mul, ty.clone(), idx, Some(r));
    let second = f.operator(Div, ty, first, Some(r));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, second);
    }
}

fn do_inject_div_div(f: &mut Forest, idx: NodeIndex, ctx: &Context, scope: &Scope) {
    if !is_one(f, idx) {
        return;
    }
    let incoming = f.incoming_edges(idx);
    let ty = f.ty(idx);
    let mut value = ty.random_value(&mut rand::rng(), ctx, scope);
    if value.starts_with('0') || value.starts_with("-0") || value == "false" {
        value = match &ty {
            Type::Boolean => "true".into(),
            Type::Field => "1Field".into(),
            Type::Integer(i) => format!("1{}{}", if i.signed { "i" } else { "u" }, i.bits),
            _ => format!("1{ty}"),
        };
    }
    let r = f.literal(value, ty.clone());
    let new = f.operator(Div, ty, r, Some(r));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_inject_or_zero(f: &mut Forest, idx: NodeIndex) {
    let incoming = f.incoming_edges(idx);
    let ty = f.ty(idx);
    let zero = make_zero(f, &ty);
    let new = f.operator(Or, ty, idx, Some(zero));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_inject_and_self(f: &mut Forest, idx: NodeIndex) {
    let incoming = f.incoming_edges(idx);
    let ty = f.ty(idx);
    let new = f.operator(And, ty, idx, Some(idx));
    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_double_mul_two(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Add) if f.left(idx) == f.right(idx) => {
            let ty = ret_of(f, idx);
            let two = make_two(f, &ty);
            f.set_operator(idx, Mul);
            f.set_child(idx, 1, two);
        }
        Some(Mul) if f.right(idx).is_some_and(|r| is_two(f, r)) => {
            let left = f.left(idx).unwrap();
            f.set_operator(idx, Add);
            f.set_child(idx, 1, left);
        }
        _ => {}
    }
}

fn do_mul_neg_one_neg(f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Mul) if f.right(idx).is_some_and(|r| is_neg_one(f, r)) => {
            f.set_operator(idx, Neg);
            f.remove_operand(idx, 1);
        }
        Some(Neg) => {
            let ty = ret_of(f, idx);
            let neg_one = make_neg_one(f, &ty);
            f.set_operator(idx, Mul);
            f.add_operand(idx, 1, neg_one, &ty);
        }
        _ => {}
    }
}

// Helpers
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
    if matches!(op, Mul | Div | And) {
        make_one(f, ty)
    } else {
        make_zero(f, ty)
    }
}
#[inline(always)]
fn make_absorbing(f: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    if op == Or {
        make_one(f, ty)
    } else {
        make_zero(f, ty)
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════════

#[inline(always)]
fn op_of(f: &Forest, idx: NodeIndex) -> Option<Operator> {
    match &f.graph[idx] {
        Node::Operator { op, .. } => Some(*op),
        _ => None,
    }
}

#[inline(always)]
fn ret_of(f: &Forest, idx: NodeIndex) -> Type {
    match &f.graph[idx] {
        Node::Operator { ret, .. } => ret.clone(),
        _ => f.ty(idx),
    }
}

#[inline(always)]
fn set_op(f: &mut Forest, idx: NodeIndex, new_op: Operator) {
    if let Node::Operator { op, .. } = &mut f.graph[idx] {
        *op = new_op;
    }
}

#[inline(always)]
fn set_ret(f: &mut Forest, idx: NodeIndex, new_ret: Type) {
    if let Node::Operator { ret, .. } = &mut f.graph[idx] {
        *ret = new_ret;
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Literal predicates
// ─────────────────────────────────────────────────────────────────────────────

/// Check if node is a literal with numeric value (handles bool/field/int)
#[inline(always)]
fn is_numeric_literal(f: &Forest, idx: NodeIndex, num: i8) -> bool {
    match &f.graph[idx] {
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
fn is_zero(f: &Forest, idx: NodeIndex) -> bool {
    is_numeric_literal(f, idx, 0)
}

#[inline(always)]
fn is_one(f: &Forest, idx: NodeIndex) -> bool {
    is_numeric_literal(f, idx, 1)
}

#[inline(always)]
fn is_neg_one(f: &Forest, idx: NodeIndex) -> bool {
    is_numeric_literal(f, idx, -1)
}

#[inline(always)]
fn is_two(f: &Forest, idx: NodeIndex) -> bool {
    is_numeric_literal(f, idx, 2)
}

// ─────────────────────────────────────────────────────────────────────────────
// Structural predicates
// ─────────────────────────────────────────────────────────────────────────────

/// Check if node is the left-hand side of an assignment (position 0 of Assignment node)
/// Such nodes must NOT be transformed as they must remain valid lvalues
#[inline(always)]
fn is_assignment_lhs(f: &Forest, idx: NodeIndex) -> bool {
    f.graph
        .edges_directed(idx, Direction::Incoming)
        .any(|e| *e.weight() == 0 && matches!(f.graph[e.source()], Node::Assignment { .. }))
}

// ─────────────────────────────────────────────────────────────────────────────
// Operator properties
// ─────────────────────────────────────────────────────────────────────────────

/// Check if `idx` is an identity element for `op` when used with operand type `operand_ty`.
/// For And, 1 is only identity for booleans (true & a = a), not integers (1 & 3 = 1 ≠ 3).
#[inline(always)]
fn is_identity(f: &Forest, idx: NodeIndex, op: Operator, ty: &Type) -> bool {
    match op {
        Operator::Add |
        Operator::Sub |
        Operator::Xor |
        Operator::Or |
        Operator::Shl |
        Operator::Shr => is_zero(f, idx),
        Operator::Mul | Operator::Div => is_one(f, idx),
        // For And, 1 (true) is only identity for booleans
        Operator::And => matches!(ty, Type::Boolean) && is_one(f, idx),
        _ => false,
    }
}

/// Check if `idx` is an absorbing element for `op` when used with operand type `operand_ty`.
/// For Or, 1 (true) is only absorbing for booleans (true | a = true), not integers.
#[inline(always)]
fn is_absorbing(f: &Forest, idx: NodeIndex, op: Operator, ty: &Type) -> bool {
    match op {
        Operator::Mul | Operator::And => is_zero(f, idx),
        // For Or, 1 (true) is only absorbing for booleans
        Operator::Or => matches!(ty, Type::Boolean) && is_one(f, idx),
        _ => false,
    }
}

/// Check if `ty` have `op` as an idempotent operation
#[inline(always)]
const fn is_idempotent(op: Operator, ty: &Type) -> bool {
    match op {
        Operator::And | Operator::Or => ty.is_bool() || ty.is_integer(),
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
