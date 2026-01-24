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
                            if matches_rule(forest, None, Some(idx), None, &self.rules[i].kind, idx) {
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
                self.apply(
                    random,
                    forest,
                    selected_node,
                    &selected_rule,
                    ctx,
                    scope,
                    &condition_nodes,
                );
                return Some(rule_name);
            }
        }

        // @todo apply to nested forests too
        None
    }

    fn apply(
        &self,
        random: &mut impl Rng,
        forest: &mut Forest,
        idx: NodeIndex,
        kind: &RuleKind,
        ctx: &Context,
        scope: &Scope,
        condition_nodes: &HashSet<NodeIndex>,
    ) {
        // Safety: Never transform condition nodes - they're stored as direct NodeIndex
        // in If/Assert nodes, not as edges, so redirect_edges won't update them
        if condition_nodes.contains(&idx) {
            return;
        }

        match kind {
            // Structural
            RuleKind::SwapOperands => forest.swap_operands(idx),
            RuleKind::Associate => do_associate(random, forest, idx),
            RuleKind::AssociateSub => do_associate_sub(random, forest, idx),
            RuleKind::AssociateDiv => do_associate_div(random, forest, idx),
            RuleKind::DivCommute => do_div_commute(random, forest, idx),
            RuleKind::DistributeMulAdd => do_distribute(random, forest, idx, Mul, Add),
            RuleKind::DistributeMulSub => do_distribute(random, forest, idx, Mul, Sub),
            RuleKind::DistributeAndOr => do_distribute(random, forest, idx, And, Or),
            RuleKind::DistributeOrAnd => do_distribute(random, forest, idx, Or, And),

            // Identity
            RuleKind::IdentityAdd => {
                do_identity(random, forest, idx, Add, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentitySub => {
                do_identity(random, forest, idx, Sub, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityMul => {
                do_identity(random, forest, idx, Mul, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityDiv => {
                do_identity(random, forest, idx, Div, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityXor => {
                do_identity(random, forest, idx, Xor, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityOr => {
                do_identity(random, forest, idx, Or, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityAnd => {
                do_identity(random, forest, idx, And, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityShl => {
                do_identity(random, forest, idx, Shl, &forest.ty(forest.left(idx).unwrap()))
            }
            RuleKind::IdentityShr => {
                do_identity(random, forest, idx, Shr, &forest.ty(forest.left(idx).unwrap()))
            }

            // Absorb
            RuleKind::AbsorbMul => do_absorb(random, forest, idx, Mul, ctx, scope),
            RuleKind::AbsorbAnd => do_absorb(random, forest, idx, And, ctx, scope),
            RuleKind::AbsorbOr => do_absorb(random, forest, idx, Or, ctx, scope),

            // SelfInverse
            RuleKind::SelfInverseSub => do_self_inverse(random, forest, idx, Sub, ctx, scope),
            RuleKind::SelfInverseXor => do_self_inverse(random, forest, idx, Xor, ctx, scope),
            RuleKind::SelfInverseDiv => do_self_inverse(random, forest, idx, Div, ctx, scope),

            // Idempotent
            RuleKind::IdempotentAnd => do_idempotent(random, forest, idx, And),
            RuleKind::IdempotentOr => do_idempotent(random, forest, idx, Or),

            // Unary
            RuleKind::DoubleNeg => do_double_unary(random, forest, idx, Neg),
            RuleKind::DoubleNot => do_double_unary(random, forest, idx, Not),
            RuleKind::AddNegSub => do_add_neg_sub(random, forest, idx),
            RuleKind::NegZeroSub => do_neg_zero_sub(random, forest, idx),

            // Comparison
            RuleKind::FlipComparison => do_flip_comparison(forest, idx),
            RuleKind::NegateComparison => {
                do_negate_comparison(random, forest, idx, condition_nodes)
            }
            RuleKind::ExpandComparison => do_expand_comparison(random, forest, idx),

            // Boolean logic
            RuleKind::DeMorgan => do_demorgan(random, forest, idx),
            RuleKind::ComplementXor => do_complement_xor(random, forest, idx),
            RuleKind::XorToAndOr => do_xor_to_and_or(random, forest, idx),

            // Modulo
            RuleKind::ModOne => do_mod_one(random, forest, idx),
            RuleKind::AndToMod => do_and_to_mod(random, forest, idx),

            // Shift
            RuleKind::ShiftZero => do_shift_zero(random, forest, idx),

            // Obfuscation
            RuleKind::InjectAddSub => do_inject(random, forest, idx, Add, Sub, ctx, scope),
            RuleKind::InjectSubAdd => do_inject(random, forest, idx, Sub, Add, ctx, scope),
            RuleKind::InjectMulDiv => do_inject_nonzero(random, forest, idx, ctx, scope),
            RuleKind::InjectXorXor => do_inject(random, forest, idx, Xor, Xor, ctx, scope),
            RuleKind::InjectDivDiv => do_inject_div_div(random, forest, idx, ctx, scope),
            RuleKind::InjectOrZero => do_inject_or_zero(random, forest, idx),
            RuleKind::InjectAndSelf => do_inject_and_self(random, forest, idx),

            // Simplification
            RuleKind::DoubleMulTwo => do_double_mul_two(random, forest, idx),
            RuleKind::MulNegOneNeg => do_mul_neg_one_neg(random, forest, idx),
        }

        // Remove the node if it is orphaned
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
    idx: NodeIndex
) -> bool {
    let is_binary = left.is_some() && right.is_some();
    let is_unary = left.is_some() && right.is_none();
    let left_op = left.and_then(|l| op_of(forest, l));
    let right_op = right.and_then(|r| op_of(forest, r));

    match (kind, op) {
        // ─────────────────────────────────────────────────────────────────────────
        // Structural rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::SwapOperands, Some(o)) => {
            is_binary && matches!(o, Add | Mul | And | Or | Xor | Equal | NotEqual)
        }

        (RuleKind::Associate, Some(o)) => {
            matches!(o, Add | Mul | And | Or | Xor) &&
                is_binary &&
                (left_op == Some(o) || right_op == Some(o))
        }

        // ((a - b) - c) <-> (a - (b + c))
        (RuleKind::AssociateSub, Some(Sub)) => {
            is_binary && (left_op == Some(Sub) || right_op == Some(Add))
        }

        // ((a / b) * c) <-> (a * (c / b)) - only valid for Field as integers round down
        (RuleKind::AssociateDiv, Some(Mul)) => {
            is_binary &&
                left.is_some_and(|l| matches!(forest.ty(l), Type::Field)) &&
                (left_op == Some(Div) || right_op == Some(Div))
        }

        // (a / b) -> ((1 / b) * a) - only valid for Field as integers round down
        (RuleKind::DivCommute, Some(Div)) => {
            is_binary && left.is_some_and(|l| matches!(forest.ty(l), Type::Field))
        }

        // ((1 / b) * a) -> (a / b) - only valid for Field as integers round down
        (RuleKind::DivCommute, Some(Mul)) => {
            is_binary &&
                left.is_some_and(|l| matches!(forest.ty(l), Type::Field)) &&
                left_op == Some(Div) &&
                left.and_then(|l| forest.left(l)).is_some_and(|l| is_one(forest, l))
        }

        // (a * (b + c)) <-> ((a * b) + (a * c))
        // Forward: Mul with Add/Sub child
        (RuleKind::DistributeMulAdd, Some(Mul)) => {
            is_binary && (right_op == Some(Add) || left_op == Some(Add))
        }
        (RuleKind::DistributeMulSub, Some(Mul)) => {
            is_binary && (right_op == Some(Sub) || left_op == Some(Sub))
        }
        // Reverse: Add/Sub with two Mul children
        (RuleKind::DistributeMulAdd, Some(Add)) => {
            is_binary && left_op == Some(Mul) && right_op == Some(Mul)
        }
        (RuleKind::DistributeMulSub, Some(Sub)) => {
            is_binary && left_op == Some(Mul) && right_op == Some(Mul)
        }
        // And/Or distribution
        (RuleKind::DistributeAndOr, Some(And)) => {
            is_binary && (right_op == Some(Or) || left_op == Some(Or))
        }
        (RuleKind::DistributeAndOr, Some(Or)) => {
            is_binary && left_op == Some(And) && right_op == Some(And)
        }
        (RuleKind::DistributeOrAnd, Some(Or)) => {
            is_binary && (right_op == Some(And) || left_op == Some(And))
        }
        (RuleKind::DistributeOrAnd, Some(And)) => {
            is_binary && left_op == Some(Or) && right_op == Some(Or)
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
            ty.is_bool() &&
                (is_identity(forest, l, And, &ty) || is_identity(forest, r, And, &ty))
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
        (RuleKind::IdentityAdd | RuleKind::IdentitySub | RuleKind::IdentityMul | RuleKind::IdentityDiv, None) => {
            left.is_some_and(|l| forest.ty(l).is_numeric())
        }
        (RuleKind::IdentityShl | RuleKind::IdentityShr, None) => {
            left.is_some_and(|l| forest.ty(l).is_integer())
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Absorb rules
        // ─────────────────────────────────────────────────────────────────────────
        // Simplification case: a op absorbing -> absorbing
        (RuleKind::AbsorbMul, Some(Mul)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_absorbing(forest, l, Mul, &forest.ty(l)) || is_absorbing(forest, r, Mul, &forest.ty(r))
        }
        (RuleKind::AbsorbAnd, Some(And)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_absorbing(forest, l, And, &forest.ty(l)) || is_absorbing(forest, r, And, &forest.ty(r))
        }
        (RuleKind::AbsorbOr, Some(Or)) if is_binary => {
            let (l, r) = (left.unwrap(), right.unwrap());
            is_absorbing(forest, l, Or, &forest.ty(l)) || is_absorbing(forest, r, Or, &forest.ty(r))
        }

        // Injection case: absorbing -> a op absorbing
        (RuleKind::AbsorbMul, None) => left.is_some_and(|l| is_absorbing(forest, l, Mul, &forest.ty(l))),
        (RuleKind::AbsorbAnd, None) => left.is_some_and(|l| is_absorbing(forest, l, And, &forest.ty(l))),
        (RuleKind::AbsorbOr, None) => left.is_some_and(|l| is_absorbing(forest, l, Or, &forest.ty(l))),

        // ─────────────────────────────────────────────────────────────────────────
        // SelfInverse rules: a op a = identity
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::SelfInverseSub, Some(Sub)) if is_binary && left == right => {
            let l = left.unwrap();
            forest.ty(l).is_numeric()
        }
        (RuleKind::SelfInverseXor, Some(Xor)) if is_binary && left == right => {
            let l = left.unwrap();
            let ty = forest.ty(l);
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
            let l = left.unwrap();
            let ty = forest.ty(l);
            ty.is_integer() || ty.is_bool()
        }
        (RuleKind::IdempotentOr, Some(Or)) if is_binary && left == right => {
            let l = left.unwrap();
            let ty = forest.ty(l);
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
        (RuleKind::DoubleNot, None) => {
            left.is_some_and(|l| {
                let ty = forest.ty(l);
                ty.is_bool() || ty.is_signed()
            })
        }

        // (a - b) -> (a + (-b))
        (RuleKind::AddNegSub, Some(Operator::Sub)) if is_binary => {
            let left = left.unwrap();
            let ty = forest.ty(left);

            ty.is_signed()
        }

        // (a + (-b)) -> (a - b) @todo ((-b) + a)
        (RuleKind::AddNegSub, Some(Operator::Add)) if is_binary => {
            right_op.is_some_and(|r| {
                matches!(r, Operator::Neg)
            })
        }

        // (-a) -> (0 - a) 
        (RuleKind::NegZeroSub, Some(Operator::Neg)) if is_unary => true,

        // (0 - a) -> (-a) @todo ((-a) + 0) and ((-a) - 0) and equivalent
        (RuleKind::NegZeroSub, Some(Operator::Sub)) if is_binary => {
            let left = left.unwrap();
            let right = right.unwrap();

            let ty = forest.ty(right);
            ty.is_signed() && is_zero(forest, left)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Comparison rules
        // ─────────────────────────────────────────────────────────────────────────
        // a cmp b -> b flip(cmp) a
        (RuleKind::FlipComparison, Some(o)) if is_binary => {
            o.is_comparison()
        }

        // a cmp b -> !(a !cmp b) @todo should do the reverse
        (RuleKind::NegateComparison, Some(o)) if is_binary => {
            o.is_comparison()
        }

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
            let l = left.unwrap();
            forest.ty(l).is_bool() && ret_of(forest, idx).is_bool()
        }

        // a ^ true -> !a
        (RuleKind::ComplementXor, Some(Xor)) if is_binary => {
            let l = left.unwrap();
            let r = right.unwrap();
            let ty = forest.ty(l);
            ty.is_bool() && (is_one(forest, r) || is_one(forest, l))
        }

        // (a ^ b) -> ((!a & b) | (a & !b))
        // Exclude cases where an operand is a literal (handled by ComplementXor)
        (RuleKind::XorToAndOr, Some(Xor)) if is_binary => {
            let l = left.unwrap();
            let r = right.unwrap();
            forest.ty(l).is_bool() && !is_one(forest, l) && !is_one(forest, r)
                && !is_zero(forest, l) && !is_zero(forest, r)
        }
        // ((!a & b) | (a & !b)) -> (a ^ b)
        (RuleKind::XorToAndOr, Some(Or)) if is_binary => {
            left_op == Some(And) && right_op == Some(And)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Modulo rules
        // ─────────────────────────────────────────────────────────────────────────
        // a % 1 -> 0
        (RuleKind::ModOne, Some(Operator::Mod)) if is_binary => {
            let right = right.unwrap();

            is_one(forest, right)
        }

        // 0 -> a % 1
        (RuleKind::ModOne, None) if is_unary => {
            let left = left.unwrap();
            let ty = forest.ty(left);

            ty.is_integer()
        }

        // a & 1 -> a % 2
        (RuleKind::AndToMod, Some(Operator::And)) if is_binary => {
            let left = left.unwrap();
            let right = right.unwrap();
            let ty = forest.ty(left);

            // `u1` can't hold `2`
            is_one(forest, right) && matches!(ty, Type::Integer(i) if i.bits > 1)
        }

        // a % 2 -> a & 1
        (RuleKind::AndToMod, Some(Operator::Mod)) if is_binary => {
            let left = left.unwrap();
            let right = right.unwrap();
            let ty = forest.ty(left);

            is_two(forest, right) && ty.is_integer()
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Shift rules
        // ─────────────────────────────────────────────────────────────────────────
        // a << 0 -> a, a >> 0 -> a
        (RuleKind::ShiftZero, Some(Shl | Shr)) if is_binary => {
            let r = right.unwrap();
            is_zero(forest, r)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Simplification rules
        // ─────────────────────────────────────────────────────────────────────────
        // a + a -> a * 2
        (RuleKind::DoubleMulTwo, Some(Operator::Add)) if is_binary => {
            let left = left.unwrap();
            let right = right.unwrap();
            let ty = forest.ty(left);

            // `u1` can't hold `2`
            left == right && (matches!(ty, Type::Field) || matches!(ty, Type::Integer(i) if i.bits > 1))
        }

        // a * 2 -> a + a
        (RuleKind::DoubleMulTwo, Some(Operator::Mul)) if is_binary => {
            let left = left.unwrap();
            let right = right.unwrap();
            let ty = forest.ty(left); // @audit this kind of checks, are redundant as Operator::Mul restrict to numeric?? i think so

            is_two(forest, right) && ty.is_numeric()
        }

        // (a * (-1)) -> (-a) @todo ((-1) * a)
        (RuleKind::MulNegOneNeg, Some(Mul)) if is_binary => {
            let l = left.unwrap();
            let r = right.unwrap();
            let ty = forest.ty(l);

            is_neg_one(forest, r) && (ty.is_signed() || matches!(ty, Type::Field))
        }

        // (-a) -> (a * (-1)) @todo ((-1) * a)
        (RuleKind::MulNegOneNeg, Some(Neg)) if is_unary => {
            let l = left.unwrap();
            let ty = forest.ty(l);

            ty.is_signed() || matches!(ty, Type::Field)
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Injection rules (obfuscation) - only when there's no existing operator
        // ─────────────────────────────────────────────────────────────────────────
        // a -> ((a - n) + n) @todo subadd
        (RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv, None) => {
            let left = left.unwrap();
            let ty = forest.ty(left);

            matches!(ty, Type::Field) || ty.is_signed()
        }

        // a -> ((a ^ n) ^ n) // a -> a & a
        (RuleKind::InjectXorXor | RuleKind::InjectAndSelf, None) => {
            let left = left.unwrap();
            let ty = forest.ty(left);
            
            ty.is_integer() || ty.is_bool()
        }

        // 1 -> r / r
        (RuleKind::InjectDivDiv, None) => {
            let left = left.unwrap();
            let ty = forest.ty(left);

            is_one(forest, left) && ty.is_numeric()
        }

        // a -> a | 0
        (RuleKind::InjectOrZero, None) => {
            let left = left.unwrap();
            let ty = forest.ty(left);

            ty.is_integer()
        }

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

fn do_associate(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    let op = match op_of(f, idx) {
        Some(op) => op,
        None => return,
    };
    let ret = ret_of(f, idx);
    let (left, right) = (f.left(idx), f.right(idx));

    // (a op b) op c -> a op (b op c)
    if let Some(l) = left.filter(|&l| op_of(f, l) == Some(op)) {
        let (a, b, c) = (f.left(l).unwrap(), f.right(l).unwrap(), right.unwrap());
        let new_right = f.operator(random, op, ret, b, Some(c));
        f.set_child(idx, 0, a);
        f.set_child(idx, 1, new_right);

        return;
    }

    // a op (b op c) -> (a op b) op c
    if let Some(r) = right.filter(|&r| op_of(f, r) == Some(op)) {
        let (a, b, c) = (left.unwrap(), f.left(r).unwrap(), f.right(r).unwrap());
        let new_left = f.operator(random, op, ret, a, Some(b));
        f.set_child(idx, 0, new_left);
        f.set_child(idx, 1, c);
    }
}

/// ((a - b) - c) <-> (a - (b + c))
fn do_associate_sub(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    if op_of(f, idx) != Some(Operator::Sub) {
        return;
    }
    let ret = ret_of(f, idx);
    let (left, right) = (f.left(idx), f.right(idx));

    // (a - b) - c -> a - (b + c)
    if let Some(l) = left.filter(|&l| op_of(f, l) == Some(Operator::Sub)) {
        let (a, b, c) = (f.left(l).unwrap(), f.right(l).unwrap(), right.unwrap());
        let new_right = f.operator(random, Operator::Add, ret, b, Some(c));
        f.set_child(idx, 0, a);
        f.set_child(idx, 1, new_right);
        return;
    }
    // a - (b + c) -> (a - b) - c
    if let Some(r) = right.filter(|&r| op_of(f, r) == Some(Operator::Add)) {
        let (a, b, c) = (left.unwrap(), f.left(r).unwrap(), f.right(r).unwrap());
        let new_left = f.operator(random, Operator::Sub, ret, a, Some(b));
        f.set_child(idx, 0, new_left);
        f.set_child(idx, 1, c);
    }
}

/// ((a / b) * c) <-> (a * (c / b))
fn do_associate_div(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    if op_of(f, idx) != Some(Operator::Mul) {
        return;
    }
    let ret = ret_of(f, idx);
    let (left, right) = (f.left(idx), f.right(idx));

    // (a / b) * c -> a * (c / b)
    if let Some(l) = left.filter(|&l| op_of(f, l) == Some(Operator::Div)) {
        let (a, b, c) = (f.left(l).unwrap(), f.right(l).unwrap(), right.unwrap());
        let new_right = f.operator(random, Operator::Div, ret, c, Some(b));
        f.set_child(idx, 0, a);
        f.set_child(idx, 1, new_right);
        return;
    }
    // a * (c / b) -> (a / b) * c
    if let Some(r) = right.filter(|&r| op_of(f, r) == Some(Operator::Div)) {
        let (a, c, b) = (left.unwrap(), f.left(r).unwrap(), f.right(r).unwrap());
        let new_left = f.operator(random, Operator::Div, ret, a, Some(b));
        f.set_child(idx, 0, new_left);
        f.set_child(idx, 1, c);
    }
}

/// (a / b) <-> ((1 / b) * a)
fn do_div_commute(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        // Forward: (a / b) -> ((1 / b) * a)
        Some(Operator::Div) => {
            let (a, b) = match (f.left(idx), f.right(idx)) {
                (Some(a), Some(b)) => (a, b),
                _ => return,
            };

            let ty = ret_of(f, idx);
            let one = make_one(random, f, &ty);
            let one_div_b = f.operator(random, Operator::Div, ty.clone(), one, Some(b));
            set_op(f, idx, Operator::Mul);
            f.set_child(idx, 1, a);
            f.set_child(idx, 0, one_div_b);
        }
        // Reverse: ((1 / b) * a) -> (a / b)
        Some(Operator::Mul) => {
            let left = match f.left(idx) {
                Some(l) if op_of(f, l) == Some(Operator::Div) => l,
                _ => return,
            };
            // Check that left child is (1 / b)
            if !f.left(left).is_some_and(|o| is_one(f, o)) {
                return;
            }
            let b = match f.right(left) {
                Some(b) => b,
                _ => return,
            };
            let a = match f.right(idx) {
                Some(a) => a,
                _ => return,
            };

            // Transform to (a / b)
            set_op(f, idx, Operator::Div);
            f.set_child(idx, 0, a);
            f.set_child(idx, 1, b);
        }
        _ => {}
    }
}

fn do_distribute(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    outer: Operator,
    inner: Operator,
) {
    let op = op_of(f, idx);
    let ret = ret_of(f, idx);

    // Forward: outer(inner(a, b), c) -> inner(outer(a, c), outer(b, c))
    // Also:    outer(a, inner(b, c)) -> inner(outer(a, b), outer(a, c))
    if op == Some(outer) {
        let (left, right) = (f.left(idx), f.right(idx));
        let left_is_inner = left.is_some_and(|l| op_of(f, l) == Some(inner));
        let right_is_inner = right.is_some_and(|r| op_of(f, r) == Some(inner));

        if left_is_inner {
            // outer(inner(a, b), c) -> inner(outer(a, c), outer(b, c))
            let l = left.unwrap();
            let (a, b, c) = (f.left(l).unwrap(), f.right(l).unwrap(), right.unwrap());
            let new_left = f.operator(random, outer, ret.clone(), a, Some(c));
            let new_right = f.operator(random, outer, ret, b, Some(c));
            set_op(f, idx, inner);
            f.set_child(idx, 0, new_left);
            f.set_child(idx, 1, new_right);
        } else if right_is_inner {
            // outer(a, inner(b, c)) -> inner(outer(a, b), outer(a, c))
            let r = right.unwrap();
            let (a, b, c) = (left.unwrap(), f.left(r).unwrap(), f.right(r).unwrap());
            let new_left = f.operator(random, outer, ret.clone(), a, Some(b));
            let new_right = f.operator(random, outer, ret, a, Some(c));
            set_op(f, idx, inner);
            f.set_child(idx, 0, new_left);
            f.set_child(idx, 1, new_right);
        }
        return;
    }

    // Reverse: inner(outer(a, c), outer(b, c)) -> outer(inner(a, b), c)
    // Only valid if both children are outer ops and share a common operand
    if op == Some(inner) {
        let (left, right) = (f.left(idx), f.right(idx));
        if !left.is_some_and(|l| op_of(f, l) == Some(outer)) ||
            !right.is_some_and(|r| op_of(f, r) == Some(outer))
        {
            return;
        }
        let (l, r) = (left.unwrap(), right.unwrap());
        let (a, c1) = (f.left(l).unwrap(), f.right(l).unwrap());
        let (b, c2) = (f.left(r).unwrap(), f.right(r).unwrap());

        // Check if they share a common right operand
        if c1 == c2 {
            let new_inner = f.operator(random, inner, ret, a, Some(b));
            set_op(f, idx, outer);
            f.set_child(idx, 0, new_inner);
            f.set_child(idx, 1, c1);
        }
    }
}

fn do_identity(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex, op: Operator, _ty: &Type) {
    // Simplification: a op identity -> a
    if op_of(f, idx) == Some(op) {
        let left = f.left(idx).unwrap();
        let right = f.right(idx).unwrap();

        let is_left_identity = is_identity(f, left, op, &f.ty(left));
        let keep = if is_left_identity { right } else { left };

        // Redirect edges from the operator node to the non-identity operand
        f.redirect_edges(idx, keep);
    } else {
        // Injection: x -> x op identity (only when there's no operator)
        // Capture incoming edges BEFORE creating new nodes to avoid cycles
        let incoming = f.incoming_edges(idx);

        let ty = f.ty(idx);
        let id = make_identity(random, f, &ty, op);
        let new = f.operator(random, op, ty, idx, Some(id));

        for (parent, slot) in incoming {
            f.set_child(parent, slot, new);
        }
    }
}

fn do_absorb(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    op: Operator,
    ctx: &Context,
    scope: &Scope,
) {
    // Simplification: a * 0 -> 0, a & 0 -> 0, a | true -> true
    // Only applies when we have the operator and one operand is absorbing
    if op_of(f, idx) == Some(op) {
        let left = f.left(idx);
        let right = f.right(idx);
        if left.is_some_and(|l| is_absorbing(f, l, op, &f.ty(l))) ||
            right.is_some_and(|r| is_absorbing(f, r, op, &f.ty(r)))
        {
            let ty = ret_of(f, idx);
            let absorb = make_absorbing(random, f, &ty, op);
            f.redirect_edges(idx, absorb);
        }
        return;
    }

    // Injection: 0 -> a * 0 (for Mul), 0 -> a & 0 (for And), true -> a | true (for Or)
    // Only when idx is the absorbing element for the operator
    let ty = f.ty(idx);
    if !is_absorbing(f, idx, op, &ty) {
        return;
    }

    // Capture incoming edges BEFORE creating new nodes to avoid cycles
    let incoming = f.incoming_edges(idx);

    let ty = f.ty(idx);
    let value = ty.random_value(random, ctx, scope, true);
    let a = f.literal(random, value, ty.clone());
    let new = f.operator(random, op, ty, a, Some(idx));

    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_self_inverse(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    op: Operator,
    ctx: &Context,
    scope: &Scope,
) {
    // Simplification: a op a -> identity (0 for Sub/Xor, 1 for Div)
    if op_of(f, idx) == Some(op) && f.left(idx) == f.right(idx) {
        let ty = ret_of(f, idx);
        let lit = match op {
            Operator::Sub | Operator::Xor => make_zero(random, f, &ty),
            Operator::Div => make_one(random, f, &ty),
            _ => return,
        };
        f.redirect_edges(idx, lit);
        return;
    }

    // Injection: identity -> a op a (0 -> a - a or a ^ a, 1 -> a / a)
    let ty = f.ty(idx);
    let is_identity = match op {
        Operator::Sub | Operator::Xor => is_zero(f, idx),
        Operator::Div => is_one(f, idx),
        _ => return,
    };

    if !is_identity {
        return;
    }

    let mut value = ty.random_value(random, ctx, scope, true);
    // For Div, ensure non-zero value
    if op == Operator::Div &&
        (value.starts_with('0') || value.starts_with("-0") || value == "false")
    {
        value = match &ty {
            Type::Boolean => "true".into(),
            Type::Field => "1Field".into(),
            Type::Integer(i) => format!("1{}{}", if i.signed { "i" } else { "u" }, i.bits),
            _ => format!("1{ty}"),
        };
    }

    let a = f.literal(random, value, ty.clone());
    let new = f.operator(random, op, ty, a, Some(a));
    f.redirect_edges(idx, new);
}

fn do_idempotent(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex, op: Operator) {
    // Simplification: a op a -> a (for And/Or)
    if op_of(f, idx) == Some(op) {
        if f.left(idx) == f.right(idx) {
            if let Some(operand) = f.left(idx) {
                f.redirect_edges(idx, operand);
            }
        }
        // If we have the operator but idempotent check failed, do nothing (don't inject)
        return;
    }
    // Injection: a -> a op a (for booleans and integers)
    // Capture incoming edges BEFORE creating new nodes to avoid cycles
    let incoming = f.incoming_edges(idx);

    let ty = f.ty(idx);
    if matches!(ty, Type::Boolean | Type::Integer(_)) {
        let new = f.operator(random, op, ty, idx, Some(idx));

        for (parent, slot) in incoming {
            f.set_child(parent, slot, new);
        }
    }
}

fn do_double_unary(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex, op: Operator) {
    // Simplification: op(op(x)) -> x (e.g., --a -> a, !!a -> a)
    if op_of(f, idx) == Some(op) && f.right(idx).is_none() {
        if let Some(inner) =
            f.left(idx).filter(|&i| op_of(f, i) == Some(op) && f.right(i).is_none())
        {
            if let Some(x) = f.left(inner) {
                f.redirect_edges(idx, x);
            }
        }
        // If we have the operator but double unary check failed, do nothing (don't inject)
        return;
    }
    // Injection: x -> op(op(x)) (only when there's no operator)
    // Capture incoming edges BEFORE creating new nodes to avoid cycles
    let incoming = f.incoming_edges(idx);

    let ty = f.ty(idx);
    let inner = f.operator(random, op, ty.clone(), idx, None);
    let outer = f.operator(random, op, ty, inner, None);

    for (parent, slot) in incoming {
        f.set_child(parent, slot, outer);
    }
}

fn do_add_neg_sub(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Operator::Sub) => {
            let right = f.right(idx).unwrap();
            let neg = f.operator(random, Operator::Neg, f.ty(right), right, None);
            set_op(f, idx, Operator::Add);
            f.set_child(idx, 1, neg);
        }
        Some(Operator::Add) if f.right(idx).is_some_and(|r| op_of(f, r) == Some(Operator::Neg)) => {
            let b = f.left(f.right(idx).unwrap()).unwrap();
            set_op(f, idx, Operator::Sub);
            f.set_child(idx, 1, b);
        }
        _ => {}
    }
}

fn do_neg_zero_sub(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Operator::Neg) => {
            // -x -> 0 - x
            let operand = f.left(idx).unwrap();
            let ty = ret_of(f, idx);
            let zero = make_zero(random, f, &ty);
            set_op(f, idx, Operator::Sub);
            f.add_operand(idx, 1, operand);
            f.set_child(idx, 0, zero);
        }
        Some(Operator::Sub) if f.left(idx).is_some_and(|l| is_zero(f, l)) => {
            // 0 - x -> -x
            let right = f.right(idx).unwrap();
            set_op(f, idx, Operator::Neg);
            f.set_child(idx, 0, right);
            f.remove_operand(idx, 1);
        }
        _ => {}
    }
}

fn do_flip_comparison(f: &mut Forest, idx: NodeIndex) {
    let flipped = match op_of(f, idx) {
        Some(Operator::Less) => Operator::Greater,
        Some(Operator::Greater) => Operator::Less,
        Some(Operator::LessOrEqual) => Operator::GreaterOrEqual,
        Some(Operator::GreaterOrEqual) => Operator::LessOrEqual,
        _ => return,
    };
    set_op(f, idx, flipped);
    f.swap_operands(idx);
}

fn do_negate_comparison(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    condition_nodes: &HashSet<NodeIndex>,
) {
    // not(a cmp b) -> a neg_cmp b
    if op_of(f, idx) == Some(Operator::Not) {
        if let Some(inner) = f.left(idx) {
            // Don't modify the inner comparison if it's used as a condition
            if condition_nodes.contains(&inner) {
                return;
            }
            if let Some(negated) = op_of(f, inner).and_then(negate_cmp) {
                set_op(f, inner, negated);
                f.redirect_edges(idx, inner);
                return;
            }
        }
    }
    // a cmp b -> not(a neg_cmp b)
    if let Some(negated) = op_of(f, idx).and_then(negate_cmp) {
        // Capture incoming edges BEFORE creating not_node
        let incoming = f.incoming_edges(idx);
        set_op(f, idx, negated);
        let not_node = f.operator(random, Operator::Not, Type::Boolean, idx, None);
        // Redirect original parents to point to not_node (idx won't be orphaned since not_node
        // points to it)
        for (parent, slot) in incoming {
            f.set_child(parent, slot, not_node);
        }
    }
}

/// (a <= b) <-> ((a < b) || (a == b))
fn do_expand_comparison(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Operator::LessOrEqual) => {
            // a <= b -> (a < b) || (a == b)
            let (a, b) = (f.left(idx).unwrap(), f.right(idx).unwrap());

            let less = f.operator(random, Operator::Less, Type::Boolean, a, Some(b));
            let eq = f.operator(random, Operator::Equal, Type::Boolean, a, Some(b));
            let new = f.operator(random, Operator::Or, Type::Boolean, less, Some(eq));
            f.redirect_edges(idx, new);
        }
        Some(Operator::GreaterOrEqual) => {
            // a >= b -> (a > b) || (a == b)
            let (a, b) = (f.left(idx).unwrap(), f.right(idx).unwrap());

            let greater = f.operator(random, Operator::Greater, Type::Boolean, a, Some(b));
            let eq = f.operator(random, Operator::Equal, Type::Boolean, a, Some(b));
            let new = f.operator(random, Operator::Or, Type::Boolean, greater, Some(eq));
            f.redirect_edges(idx, new);
        }
        Some(Operator::Or) => {
            // (a < b) || (a == b) -> a <= b
            // Also handles: (a == b) || (a < b) -> a <= b
            let (left, right) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            let left_op = op_of(f, left);
            let right_op = op_of(f, right);

            if left_op == Some(Operator::Less) && right_op == Some(Operator::Equal) {
                let a = f.left(left).unwrap();
                let b = f.right(left).unwrap();
                let new = f.operator(random, Operator::LessOrEqual, Type::Boolean, a, Some(b));
                f.redirect_edges(idx, new);
            } else if left_op == Some(Operator::Greater) && right_op == Some(Operator::Equal) {
                let a = f.left(left).unwrap();
                let b = f.right(left).unwrap();
                let new = f.operator(random, Operator::GreaterOrEqual, Type::Boolean, a, Some(b));
                f.redirect_edges(idx, new);
            } else if left_op == Some(Operator::Equal) && right_op == Some(Operator::Less) {
                // (a == b) || (a < b) -> a <= b
                let a = f.left(right).unwrap();
                let b = f.right(right).unwrap();
                let new = f.operator(random, Operator::LessOrEqual, Type::Boolean, a, Some(b));
                f.redirect_edges(idx, new);
            } else if left_op == Some(Operator::Equal) && right_op == Some(Operator::Greater) {
                // (a == b) || (a > b) -> a >= b
                let a = f.left(right).unwrap();
                let b = f.right(right).unwrap();
                let new = f.operator(random, Operator::GreaterOrEqual, Type::Boolean, a, Some(b));
                f.redirect_edges(idx, new);
            }
        }
        _ => {}
    }
}

fn do_demorgan(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    let op = op_of(f, idx);

    // not(a and/or b) -> not(a) or/and not(b)
    if op == Some(Operator::Not) {
        if let Some(inner) = f.left(idx) {
            let dual = match op_of(f, inner) {
                Some(Operator::And) => Operator::Or,
                Some(Operator::Or) => Operator::And,
                _ => return,
            };

            let (a, b) = (f.left(inner).unwrap(), f.right(inner).unwrap());
            let ty = ret_of(f, inner);
            let not_a = f.operator(random, Operator::Not, ty.clone(), a, None);
            let not_b = f.operator(random, Operator::Not, ty.clone(), b, None);
            let new = f.operator(random, dual, ty, not_a, Some(not_b));
            f.redirect_edges(idx, new);
            return;
        }
    }

    // not(a) and/or not(b) -> not(a or/and b)
    if matches!(op, Some(Operator::And | Operator::Or)) {
        let (left, right) = (f.left(idx), f.right(idx));
        if left.is_some_and(|l| op_of(f, l) == Some(Operator::Not)) &&
            right.is_some_and(|r| op_of(f, r) == Some(Operator::Not))
        {
            let dual = if op == Some(Operator::And) { Operator::Or } else { Operator::And };
            let (a, b) = (f.left(left.unwrap()).unwrap(), f.left(right.unwrap()).unwrap());
            let ty = f.ty(a);
            let inner = f.operator(random, dual, ty.clone(), a, Some(b));
            let new = f.operator(random, Operator::Not, ty, inner, None);
            f.redirect_edges(idx, new);
        }
    }
}

fn do_complement_xor(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        // !a -> a ^ true
        Some(Operator::Not) => {
            let operand = f.left(idx).unwrap();
            let ty = f.ty(operand);
            let one = make_one(random, f, &ty);
            set_op(f, idx, Operator::Xor);
            set_ret(f, idx, ty);
            f.add_operand(idx, 1, one);
        }
        // a ^ true -> !a, or true ^ a -> !a
        Some(Operator::Xor) => {
            let left = f.left(idx);
            let right = f.right(idx);
            if right.is_some_and(|r| is_one(f, r)) {
                // a ^ true -> !a
                let ty = f.ty(left.unwrap());
                set_op(f, idx, Operator::Not);
                set_ret(f, idx, ty);
                f.remove_operand(idx, 1);
            } else if left.is_some_and(|l| is_one(f, l)) {
                // true ^ a -> !a
                let a = right.unwrap();
                let ty = f.ty(a);
                set_op(f, idx, Operator::Not);
                set_ret(f, idx, ty);
                f.set_child(idx, 0, a);
                f.remove_operand(idx, 1);
            }
        }
        _ => {}
    }
}

/// (a ^ b) <-> ((!a & b) | (a & !b))
fn do_xor_to_and_or(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Operator::Xor) => {
            // a ^ b -> (!a & b) | (a & !b)
            let (a, b) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            let ty = ret_of(f, idx);

            let not_a = f.operator(random, Operator::Not, ty.clone(), a, None);
            let not_b = f.operator(random, Operator::Not, ty.clone(), b, None);
            let left_and = f.operator(random, Operator::And, ty.clone(), not_a, Some(b));
            let right_and = f.operator(random, Operator::And, ty.clone(), a, Some(not_b));
            let new = f.operator(random, Operator::Or, ty, left_and, Some(right_and));
            f.redirect_edges(idx, new);
        }
        Some(Operator::Or) => {
            // (!a & b) | (a & !b) -> a ^ b
            let (left, right) = (f.left(idx).unwrap(), f.right(idx).unwrap());
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
                        let ty = f.ty(a_from_left);
                        let new = f.operator(
                            random,
                            Operator::Xor,
                            ty,
                            a_from_left,
                            Some(b_from_left),
                        );
                        f.redirect_edges(idx, new);
                    }
                }
            }
        }
        _ => {}
    }
}

/// a % 1 <-> 0
fn do_mod_one(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    if op_of(f, idx) == Some(Operator::Mod) && f.right(idx).is_some_and(|r| is_one(f, r)) {
        // a % 1 -> 0
        let ty = ret_of(f, idx);
        let zero = make_zero(random, f, &ty);
        f.redirect_edges(idx, zero);
    } else if is_zero(f, idx) {
        // 0 -> r % 1 (inject) - matches_rule already ensures type is Integer
        // Capture incoming edges BEFORE creating new nodes to avoid cycles
        let incoming = f.incoming_edges(idx);

        let ty = f.ty(idx);
        let one = make_one(random, f, &ty);
        let new = f.operator(random, Operator::Mod, ty, idx, Some(one));

        for (parent, slot) in incoming {
            f.set_child(parent, slot, new);
        }
    }
}

/// a & 1 <-> a % 2
fn do_and_to_mod(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Operator::And) if f.right(idx).is_some_and(|r| is_one(f, r)) => {
            // a & 1 -> a % 2
            let ty = ret_of(f, idx);
            let two = make_two(random, f, &ty);
            set_op(f, idx, Operator::Mod);
            f.set_child(idx, 1, two);
        }
        Some(Operator::Mod) if f.right(idx).is_some_and(|r| is_two(f, r)) => {
            // a % 2 -> a & 1
            let ty = ret_of(f, idx);
            let one = make_one(random, f, &ty);
            set_op(f, idx, Operator::And);
            f.set_child(idx, 1, one);
        }
        _ => {}
    }
}

/// (a << 0) -> a, (a >> 0) -> a
/// Note: Injection (a -> a << 0) is handled by IdentityShl/IdentityShr rules
fn do_shift_zero(_random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    // Simplification: a << 0 -> a, a >> 0 -> a
    if matches!(op_of(f, idx), Some(Operator::Shl | Operator::Shr)) {
        if f.right(idx).is_some_and(|r| is_zero(f, r)) {
            if let Some(left) = f.left(idx) {
                f.redirect_edges(idx, left);
            }
        }
    }
}

fn do_inject(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    op1: Operator,
    op2: Operator,
    ctx: &Context,
    scope: &Scope,
) {
    // Capture incoming edges BEFORE creating new nodes to avoid redirecting edges from the new
    // nodes themselves (which would create cycles)
    let incoming = f.incoming_edges(idx);

    let ty = f.ty(idx);
    let value = ty.random_value(random, ctx, scope, true);
    let r = f.literal(random, value, ty.clone());
    let first = f.operator(random, op1, ty.clone(), idx, Some(r));
    let second = f.operator(random, op2, ty, first, Some(r));

    // Redirect only the original parents to the new node
    for (parent, slot) in incoming {
        f.set_child(parent, slot, second);
    }
}

fn do_inject_nonzero(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    ctx: &Context,
    scope: &Scope,
) {
    // Capture incoming edges BEFORE creating new nodes to avoid redirecting edges from the new
    // nodes themselves (which would create cycles)
    let incoming = f.incoming_edges(idx);

    let ty = f.ty(idx);
    let mut value = ty.random_value(random, ctx, scope, true);
    // Use 1 if value is zero (can't divide by zero)
    if value.starts_with('0') || value.starts_with("-0") || value == "false" {
        value = match &ty {
            Type::Boolean => "true".into(),
            Type::Field => "1Field".into(),
            Type::Integer(i) => format!("1{}{}", if i.signed { "i" } else { "u" }, i.bits),
            _ => format!("1{ty}"),
        };
    }

    let r = f.literal(random, value, ty.clone());
    let first = f.operator(random, Operator::Mul, ty.clone(), idx, Some(r));
    let second = f.operator(random, Operator::Div, ty, first, Some(r));

    // Redirect only the original parents to the new node
    for (parent, slot) in incoming {
        f.set_child(parent, slot, second);
    }
}

/// 1 -> r / r
fn do_inject_div_div(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    ctx: &Context,
    scope: &Scope,
) {
    if !is_one(f, idx) {
        return;
    }
    let ty = f.ty(idx);
    let mut value = ty.random_value(random, ctx, scope, true);
    // Use 1 if value is zero (can't divide by zero)
    if value.starts_with('0') || value.starts_with("-0") || value == "false" {
        value = match &ty {
            Type::Boolean => "true".into(),
            Type::Field => "1Field".into(),
            Type::Integer(i) => format!("1{}{}", if i.signed { "i" } else { "u" }, i.bits),
            _ => format!("1{ty}"),
        };
    }

    let r = f.literal(random, value, ty.clone());
    let new = f.operator(random, Operator::Div, ty, r, Some(r));
    f.redirect_edges(idx, new);
}

/// a -> a | 0
fn do_inject_or_zero(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    // Capture incoming edges BEFORE creating new nodes to avoid cycles
    let incoming = f.incoming_edges(idx);

    let ty = f.ty(idx);
    let zero = make_zero(random, f, &ty);
    let new = f.operator(random, Operator::Or, ty, idx, Some(zero));

    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

/// a -> a & a
fn do_inject_and_self(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    // Capture incoming edges BEFORE creating new nodes to avoid cycles
    let incoming = f.incoming_edges(idx);

    let ty = f.ty(idx);
    let new = f.operator(random, Operator::And, ty, idx, Some(idx));

    for (parent, slot) in incoming {
        f.set_child(parent, slot, new);
    }
}

fn do_double_mul_two(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    let ty = f.ty(idx);
    match op_of(f, idx) {
        Some(Operator::Add) if f.left(idx) == f.right(idx) => {
            let two = make_two(random, f, &ty);
            set_op(f, idx, Operator::Mul);
            f.set_child(idx, 1, two);
        }
        Some(Operator::Mul) if f.right(idx).is_some_and(|r| is_two(f, r)) => {
            let left = f.left(idx).unwrap();
            set_op(f, idx, Operator::Add);
            f.set_child(idx, 1, left);
        }
        _ => {}
    }
}

fn do_mul_neg_one_neg(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        Some(Operator::Mul) if f.right(idx).is_some_and(|r| is_neg_one(f, r)) => {
            set_op(f, idx, Operator::Neg);
            f.remove_operand(idx, 1);
        }
        Some(Operator::Neg) => {
            let ty = ret_of(f, idx);
            let neg_one = make_neg_one(random, f, &ty);
            set_op(f, idx, Operator::Mul);
            f.add_operand(idx, 1, neg_one);
        }
        _ => {}
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
        _ => false
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
fn make_literal(random: &mut impl Rng, f: &mut Forest, ty: &Type, num: i8) -> NodeIndex {
    let val = match (num, ty) {
        (0, Type::Boolean) => "false".into(),
        (1, Type::Boolean) => "true".into(),
        (-1, Type::Field) => "-1Field".into(),
        (-1, Type::Integer(i)) => format!("-1i{}", i.bits),
        (2, Type::Field) => "2Field".into(),
        (2, Type::Integer(i)) => format!("2{}{}", if i.signed { "i" } else { "u" }, i.bits),
        (idx, _) => format!("{idx}{ty}"),
    };
    f.literal(random, val, ty.clone())
}

#[inline(always)]
fn make_zero(random: &mut impl Rng, f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(random, f, ty, 0)
}

#[inline(always)]
fn make_one(random: &mut impl Rng, f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(random, f, ty, 1)
}

#[inline(always)]
fn make_neg_one(random: &mut impl Rng, f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(random, f, ty, -1)
}

#[inline(always)]
fn make_two(random: &mut impl Rng, f: &mut Forest, ty: &Type) -> NodeIndex {
    make_literal(random, f, ty, 2)
}

#[inline(always)]
fn make_identity(random: &mut impl Rng, f: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    if matches!(op, Operator::Mul | Operator::Div | Operator::And) {
        make_one(random, f, ty)
    } else {
        make_zero(random, f, ty)
    }
}

#[inline(always)]
fn make_absorbing(random: &mut impl Rng, f: &mut Forest, ty: &Type, op: Operator) -> NodeIndex {
    if op == Operator::Or {
        make_one(random, f, ty)
    } else {
        make_zero(random, f, ty)
    }
}
