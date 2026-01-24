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
    /// Get rule names in the order defined in RULES (for ordered display)
    pub fn rule_names_ordered() -> Vec<String> {
        RULES.iter().map(|r| r.kind.name().to_string()).collect()
    }

    /// Randomly select a rule, then apply it to ALL matching instances in the forest
    /// Returns (rule name, count) if a rule was applied, None otherwise
    pub fn apply_random(
        &self,
        random: &mut impl Rng,
        forest: &mut Forest,
        ctx: &Context,
        scope: &Scope,
    ) -> Option<String> {
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
                        // Skip rules that use redirect on condition nodes
                        /* if uses_redirect(&self.rules[i].kind) &&
                         * condition_nodes.contains(&idx) { @todo */
                        if condition_nodes.contains(&idx) {
                            continue;
                        }
                        if matches_rule(forest, Some(op), left, right, &self.rules[i].kind) {
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
                            if matches_rule(forest, None, Some(idx), None, &self.rules[i].kind) {
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
                return Some(selected_rule.name().to_string());
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

        // (a * (b + c)) <-> ((b + c) * a) <-> ((a * b) + (a * c))
        // Forward: Mul with Add child
        (RuleKind::DistributeMulAdd, Some(Mul)) => {
            is_binary && (right_op == Some(Add) || left_op == Some(Add))
        }
        // Reverse: Add with two Mul children
        (RuleKind::DistributeMulAdd, Some(Add)) => {
            is_binary && left_op == Some(Mul) && right_op == Some(Mul)
        }
        (RuleKind::DistributeMulSub, Some(Mul)) => {
            is_binary && (right_op == Some(Sub) || left_op == Some(Sub))
        }
        (RuleKind::DistributeMulSub, Some(Sub)) => {
            is_binary && left_op == Some(Mul) && right_op == Some(Mul)
        }
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
        (RuleKind::NegZeroSub, Some(Operator::Neg)) if is_unary => {
            let op = left_op.unwrap();
            
            matches!(op, Operator::Neg)
        }

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

        // !a -> a ^ true
        (RuleKind::ComplementXor, Some(Not)) if is_unary => {
            let l = left.unwrap();
            forest.ty(l).is_bool()
        }

        // a ^ true -> !a
        (RuleKind::ComplementXor, Some(Xor)) if is_binary => {
            let l = left.unwrap();
            let r = right.unwrap();
            let ty = forest.ty(l);
            ty.is_bool() && (is_one(forest, r) || is_one(forest, l))
        }

        // (a ^ b) -> ((!a & b) | (a & !b))
        (RuleKind::XorToAndOr, Some(Xor)) if is_binary => {
            let l = left.unwrap();
            forest.ty(l).is_bool()
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

        // a -> ((a ^ n) ^ n)
        (RuleKind::InjectXorXor, None) => {
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

        // a -> a & a
        (RuleKind::InjectAndSelf, None) => {
            let left = left.unwrap();
            let ty = forest.ty(left);

            ty.is_integer() || ty.is_bool()
        }

        _ => false,
    }
}

/// Get all operators that a rule can target (for building the index)
fn operators_for_rule(kind: &RuleKind) -> Vec<Operator> {
    match kind {
        RuleKind::SwapOperands => vec![Add, Mul, And, Or, Xor, Equal, NotEqual],
        RuleKind::Associate => vec![Add, Mul, And, Or, Xor],
        RuleKind::AssociateSub => vec![Sub],
        RuleKind::AssociateDiv => vec![Mul],
        RuleKind::DivCommute => vec![Div, Mul],
        RuleKind::DistributeMulAdd => vec![Mul, Add],
        RuleKind::DistributeMulSub => vec![Mul, Sub],
        RuleKind::DistributeAndOr => vec![And, Or],
        RuleKind::DistributeOrAnd => vec![Or, And],
        RuleKind::IdentityAdd => vec![Add],
        RuleKind::IdentitySub => vec![Sub],
        RuleKind::IdentityMul => vec![Mul],
        RuleKind::IdentityDiv => vec![Div],
        RuleKind::IdentityXor => vec![Xor],
        RuleKind::IdentityOr => vec![Or],
        RuleKind::IdentityAnd => vec![And],
        RuleKind::IdentityShl => vec![Shl],
        RuleKind::IdentityShr => vec![Shr],
        RuleKind::AbsorbMul => vec![Mul],
        RuleKind::AbsorbAnd => vec![And],
        RuleKind::AbsorbOr => vec![Or],
        RuleKind::SelfInverseSub => vec![Sub],
        RuleKind::SelfInverseXor => vec![Xor],
        RuleKind::SelfInverseDiv => vec![Div],
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

/// (a / b) -> ((1 / b) * a)
fn do_div_commute(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    if op_of(f, idx) != Some(Operator::Div) {
        return;
    }
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

fn do_distribute(
    random: &mut impl Rng,
    f: &mut Forest,
    idx: NodeIndex,
    outer: Operator,
    inner: Operator,
) {
    if op_of(f, idx) != Some(outer) {
        return;
    }
    let left = match f.left(idx).filter(|&l| op_of(f, l) == Some(inner)) {
        Some(l) => l,
        None => return,
    };

    let ret = ret_of(f, idx);
    let (a, b, c) = (f.left(left).unwrap(), f.right(left).unwrap(), f.right(idx).unwrap());
    let new_left = f.operator(random, outer, ret.clone(), a, Some(c));
    let new_right = f.operator(random, outer, ret, b, Some(c));
    set_op(f, idx, inner);
    f.set_child(idx, 0, new_left);
    f.set_child(idx, 1, new_right);
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
            let ty = f.ty(idx);
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
        let ty = f.ty(idx);
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
            f.add_operand(idx, 0, right);
            f.remove_operand(idx, 1);
            // Remove old pos 0 (the duplicate edge)
            if let Some(edge) = f
                .graph
                .edges_directed(idx, Direction::Outgoing)
                .find(|e| *e.weight() == 0 && e.target() != right)
            {
                let id = edge.id();
                f.graph.remove_edge(id);
            }
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
            let (left, right) = (f.left(idx).unwrap(), f.right(idx).unwrap());
            let left_op = op_of(f, left);
            if left_op == Some(Operator::Less) && op_of(f, right) == Some(Operator::Equal) {
                let a = f.left(left).unwrap();
                let b = f.right(left).unwrap();

                let new = f.operator(random, Operator::LessOrEqual, Type::Boolean, a, Some(b));
                f.redirect_edges(idx, new);
            } else if left_op == Some(Operator::Greater) && op_of(f, right) == Some(Operator::Equal)
            {
                let a = f.left(left).unwrap();
                let b = f.right(left).unwrap();

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
            let not_a = f.operator(random, Operator::Not, Type::Boolean, a, None);
            let not_b = f.operator(random, Operator::Not, Type::Boolean, b, None);
            let new = f.operator(random, dual, Type::Boolean, not_a, Some(not_b));
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
            let inner = f.operator(random, dual, Type::Boolean, a, Some(b));
            let new = f.operator(random, Operator::Not, Type::Boolean, inner, None);
            f.redirect_edges(idx, new);
        }
    }
}

fn do_complement_xor(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    match op_of(f, idx) {
        // !a -> a ^ true
        Some(Operator::Not) => {
            let ty = ret_of(f, idx);
            let one = make_one(random, f, &ty);
            set_op(f, idx, Operator::Xor);
            f.add_operand(idx, 1, one);
        }
        // a ^ true -> !a
        Some(Operator::Xor) if f.right(idx).is_some_and(|r| is_one(f, r)) => {
            set_op(f, idx, Operator::Not);
            f.remove_operand(idx, 1);
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

            let not_a = f.operator(random, Operator::Not, Type::Boolean, a, None);
            let not_b = f.operator(random, Operator::Not, Type::Boolean, b, None);
            let left_and = f.operator(random, Operator::And, Type::Boolean, not_a, Some(b));
            let right_and = f.operator(random, Operator::And, Type::Boolean, a, Some(not_b));
            let new = f.operator(random, Operator::Or, Type::Boolean, left_and, Some(right_and));
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
                        let new = f.operator(
                            random,
                            Operator::Xor,
                            Type::Boolean,
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
        let ty = f.ty(idx);
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

/// (a << 0) -> a, (a >> 0) -> a, or a -> a << 0 (injection)
fn do_shift_zero(random: &mut impl Rng, f: &mut Forest, idx: NodeIndex) {
    if matches!(op_of(f, idx), Some(Operator::Shl | Operator::Shr)) {
        // Simplification: a << 0 -> a, a >> 0 -> a
        if f.right(idx).is_some_and(|r| is_zero(f, r)) {
            if let Some(left) = f.left(idx) {
                f.redirect_edges(idx, left);
            }
        }
    } else {
        // Injection: a -> a << 0 (randomly choose Shl or Shr)
        // Capture incoming edges BEFORE creating new nodes to avoid cycles
        let incoming = f.incoming_edges(idx);

        let ty = f.ty(idx);
        let zero = make_zero(random, f, &ty);
        let op = if random.random_bool(0.5) { Operator::Shl } else { Operator::Shr };
        let new = f.operator(random, op, ty, idx, Some(zero));

        for (parent, slot) in incoming {
            f.set_child(parent, slot, new);
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
// Type predicates
// ─────────────────────────────────────────────────────────────────────────────

fn can_apply_unary(f: &Forest, node: Option<NodeIndex>, op: Operator) -> bool {
    node.is_some_and(|idx| match f.ty(idx) {
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
fn is_idempotent(op: Operator, ty: &Type) -> bool {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builders::CircuitBuilder;
    use std::fs;

    const RED: &str = "\x1b[31m";
    const GREEN: &str = "\x1b[32m";
    const RESET: &str = "\x1b[0m";

    /// Helper to check if a rule matches for a given node
    fn assert_matches(f: &Forest, idx: NodeIndex, kind: &RuleKind) {
        let op = op_of(f, idx);
        let left = f.left(idx);
        let right = f.right(idx);
        assert!(
            matches_rule(f, op, left, right, kind),
            "Rule {:?} should match for node with op={:?}, left={:?}, right={:?}",
            kind,
            op,
            left,
            right
        );
    }

    /// Find common prefix length between two strings
    fn common_prefix_len(a: &str, b: &str) -> usize {
        a.chars().zip(b.chars()).take_while(|(ca, cb)| ca == cb).count()
    }

    /// Find common suffix length between two strings
    fn common_suffix_len(a: &str, b: &str) -> usize {
        a.chars().rev().zip(b.chars().rev()).take_while(|(ca, cb)| ca == cb).count()
    }

    /// Highlight the different parts of a line compared to another line
    fn highlight_diff(line: &str, other: &str, color: &str) -> String {
        // Handle empty cases
        if line.is_empty() {
            return String::new();
        }
        if other.is_empty() {
            return format!("{color}{line}{RESET}");
        }

        let prefix_len = common_prefix_len(line, other);
        let suffix_len = common_suffix_len(line, other);

        let line_chars: Vec<char> = line.chars().collect();
        let line_len = line_chars.len();

        // Ensure suffix doesn't overlap with prefix
        let effective_suffix_len = suffix_len.min(line_len.saturating_sub(prefix_len));
        let diff_start = prefix_len;
        let diff_end = line_len.saturating_sub(effective_suffix_len);

        // If there's no difference, return the line as-is
        if diff_start >= diff_end {
            return line.to_string();
        }

        let prefix: String = line_chars[..diff_start].iter().collect();
        let diff: String = line_chars[diff_start..diff_end].iter().collect();
        let suffix: String = line_chars[diff_end..].iter().collect();

        format!("{prefix}{color}{diff}{RESET}{suffix}")
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    enum DiffOp {
        Equal,
        Delete,
        Insert,
    }

    /// Compute line-based diff using LCS algorithm
    fn compute_diff<'a>(
        before_lines: &[&'a str],
        after_lines: &[&'a str],
    ) -> Vec<(DiffOp, usize, usize)> {
        let idx = before_lines.len();
        let m = after_lines.len();

        // Build LCS table
        let mut lcs = vec![vec![0; m + 1]; idx + 1];
        for i in 1..=idx {
            for j in 1..=m {
                if before_lines[i - 1] == after_lines[j - 1] {
                    lcs[i][j] = lcs[i - 1][j - 1] + 1;
                } else {
                    lcs[i][j] = lcs[i - 1][j].max(lcs[i][j - 1]);
                }
            }
        }

        // Backtrack to build diff operations
        let mut result = Vec::new();
        let mut i = idx;
        let mut j = m;

        while i > 0 || j > 0 {
            if i > 0 && j > 0 && before_lines[i - 1] == after_lines[j - 1] {
                result.push((DiffOp::Equal, i - 1, j - 1));
                i -= 1;
                j -= 1;
            } else if j > 0 && (i == 0 || lcs[i][j - 1] >= lcs[i - 1][j]) {
                result.push((DiffOp::Insert, 0, j - 1));
                j -= 1;
            } else if i > 0 {
                result.push((DiffOp::Delete, i - 1, 0));
                i -= 1;
            }
        }

        result.reverse();
        result
    }

    fn print_diff(before: &str, after: &str) {
        let before_lines: Vec<&str> = before.lines().collect();
        let after_lines: Vec<&str> = after.lines().collect();

        println!("{}", "=".repeat(80));
        println!("CIRCUIT COMPARISON");
        println!("{}", "=".repeat(80));

        let diff_ops = compute_diff(&before_lines, &after_lines);

        let mut i = 0;
        while i < diff_ops.len() {
            let (op, before_idx, after_idx) = diff_ops[i];

            match op {
                DiffOp::Equal => {
                    println!("  {}", before_lines[before_idx]);
                    i += 1;
                }
                DiffOp::Delete => {
                    // Check if next operation is an Insert (potential modification)
                    if i + 1 < diff_ops.len() && diff_ops[i + 1].0 == DiffOp::Insert {
                        let (_, _, next_after_idx) = diff_ops[i + 1];
                        let before_line = before_lines[before_idx];
                        let after_line = after_lines[next_after_idx];

                        // Highlight the differences
                        let highlighted_before = highlight_diff(before_line, after_line, RED);
                        let highlighted_after = highlight_diff(after_line, before_line, GREEN);

                        println!("{}-{} {}", RED, RESET, highlighted_before);
                        println!("{}+{} {}", GREEN, RESET, highlighted_after);
                        i += 2; // Skip both delete and insert
                    } else {
                        // Pure deletion - show entire line in red
                        println!("{}-{} {}{}{}", RED, RESET, RED, before_lines[before_idx], RESET);
                        i += 1;
                    }
                }
                DiffOp::Insert => {
                    // Pure insertion - show entire line in green
                    println!("{}+{} {}{}{}", GREEN, RESET, GREEN, after_lines[after_idx], RESET);
                    i += 1;
                }
            }
        }

        println!("{}", "=".repeat(80));
    }

    #[test]
    fn test_rewriter() {
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rewriter = Rewriter::default();

        for (name, ty, _) in &scope.inputs {
            forest.input(&mut random, name.clone(), ty.clone());
        }
        for (name, ty, _) in &scope.globals {
            forest.input(&mut random, name.clone(), ty.clone());
        }

        forest.random(&mut random, &ctx, &scope, true);

        // Clone forest before rewriting to compare
        let forest_before = forest.clone();
        let before = builder.format_circuit(&scope, &forest_before);

        // Apply rewriter
        for _ in 0..1 {
            rewriter.apply_random(&mut random, &mut forest, &ctx, &scope);
        }

        let after = builder.format_circuit(&scope, &forest);

        print_diff(&before, &after);
    }

    #[test]
    fn test_associate() {
        // Test Associate rule: (a + b) + c <-> a + (b + c)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::Associate;

        // Create inputs: a, b, c
        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);
        let c = forest.input(&mut random, "c".into(), Type::Field);

        // Build (a + b) + c (left-associated)
        let a_plus_b = forest.operator(&mut random, Operator::Add, Type::Field, a, Some(b));
        let root = forest.operator(&mut random, Operator::Add, Type::Field, a_plus_b, Some(c));

        // Verify initial structure: left child is (a + b), right child is c
        assert_eq!(forest.left(root), Some(a_plus_b));
        assert_eq!(forest.right(root), Some(c));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Add));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply associate: (a + b) + c -> a + (b + c)
        assert_matches(&forest, root, &rule);
        do_associate(&mut random, &mut forest, root);
        println!("After associate: {}", forest.get_expr_for_node(root));

        // Verify new structure: left child is a, right child is (b + c)
        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Add));
        assert_eq!(forest.left(new_right), Some(b));
        assert_eq!(forest.right(new_right), Some(c));

        // Apply associate again (reverse direction): a + (b + c) -> (a + b) + c
        assert_matches(&forest, root, &rule);
        do_associate(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        // Verify we're back to left-associated form: (a + b) + c
        // root's left child is the new (a + b) operator node
        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Add));
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        // root's right child is c directly
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_associate_sub() {
        // Test AssociateSub rule: (a - b) - c <-> a - (b + c)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::AssociateSub;

        // Create inputs: a, b, c
        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);
        let c = forest.input(&mut random, "c".into(), Type::Field);

        // Build (a - b) - c
        let a_minus_b = forest.operator(&mut random, Operator::Sub, Type::Field, a, Some(b));
        let root = forest.operator(&mut random, Operator::Sub, Type::Field, a_minus_b, Some(c));

        // Verify initial structure: (a - b) - c
        assert_eq!(forest.left(root), Some(a_minus_b));
        assert_eq!(forest.right(root), Some(c));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Sub));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply associate_sub: (a - b) - c -> a - (b + c)
        assert_matches(&forest, root, &rule);
        do_associate_sub(&mut random, &mut forest, root);
        println!("After associate_sub: {}", forest.get_expr_for_node(root));

        // Verify new structure: a - (b + c)
        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Add)); // b + c
        assert_eq!(forest.left(new_right), Some(b));
        assert_eq!(forest.right(new_right), Some(c));

        // Apply associate_sub again (reverse direction): a - (b + c) -> (a - b) - c
        assert_matches(&forest, root, &rule);
        do_associate_sub(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        // Verify we're back to (a - b) - c form
        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Sub)); // a - b
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_swap_operands() {
        // Test SwapOperands rule: (a + b) <-> (b + a)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::SwapOperands;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Add, Type::Field, a, Some(b));

        // Initial: a + b
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Swap: a + b -> b + a
        assert_matches(&forest, root, &rule);
        forest.swap_operands(root);
        println!("After swap: {}", forest.get_expr_for_node(root));

        assert_eq!(forest.left(root), Some(b));
        assert_eq!(forest.right(root), Some(a));

        // Swap back: b + a -> a + b
        assert_matches(&forest, root, &rule);
        forest.swap_operands(root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_associate_div() {
        // Test AssociateDiv rule: ((a / b) * c) <-> (a * (c / b))
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::AssociateDiv;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);
        let c = forest.input(&mut random, "c".into(), Type::Field);

        // Build (a / b) * c
        let a_div_b = forest.operator(&mut random, Operator::Div, Type::Field, a, Some(b));
        let root = forest.operator(&mut random, Operator::Mul, Type::Field, a_div_b, Some(c));

        // Verify initial: (a / b) * c
        assert_eq!(forest.left(root), Some(a_div_b));
        assert_eq!(forest.right(root), Some(c));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: (a / b) * c -> a * (c / b)
        assert_matches(&forest, root, &rule);
        do_associate_div(&mut random, &mut forest, root);
        println!("After associate_div: {}", forest.get_expr_for_node(root));

        // Verify: a * (c / b)
        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Div));
        assert_eq!(forest.left(new_right), Some(c));
        assert_eq!(forest.right(new_right), Some(b));

        // Reverse: a * (c / b) -> (a / b) * c
        assert_matches(&forest, root, &rule);
        do_associate_div(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Div));
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_distribute() {
        // Test Distribute rule: ((a + b) * c) <-> ((a * c) + (b * c))
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::DistributeMulAdd;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);
        let c = forest.input(&mut random, "c".into(), Type::Field);

        // Build (a + b) * c
        let a_plus_b = forest.operator(&mut random, Operator::Add, Type::Field, a, Some(b));
        let root = forest.operator(&mut random, Operator::Mul, Type::Field, a_plus_b, Some(c));

        // Verify initial: (a + b) * c
        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Add));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply distribute: (a + b) * c -> (a * c) + (b * c)
        assert_matches(&forest, root, &rule);
        do_distribute(&mut random, &mut forest, root, Operator::Mul, Operator::Add);
        println!("After distribute: {}", forest.get_expr_for_node(root));

        // Verify: (a * c) + (b * c)
        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        let left = forest.left(root).unwrap();
        let right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, left), Some(Operator::Mul));
        assert_eq!(op_of(&forest, right), Some(Operator::Mul));
        assert_eq!(forest.left(left), Some(a));
        assert_eq!(forest.right(left), Some(c));
        assert_eq!(forest.left(right), Some(b));
        assert_eq!(forest.right(right), Some(c));

        // Reverse: (a * c) + (b * c) -> (a + b) * c
        assert_matches(&forest, root, &rule);
    }

    #[test]
    fn test_identity_add() {
        // Test Identity rule for Add: (a + 0) <-> a
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::IdentityAdd;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let zero = forest.literal(&mut random, "0Field".into(), Type::Field);

        // Build a + 0
        let root = forest.operator(&mut random, Operator::Add, Type::Field, a, Some(zero));

        // Add an incoming edge so redirect works
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, root);

        // Verify initial: a + 0
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(zero));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply identity: a + 0 -> a (redirects var to point to a)
        assert_matches(&forest, root, &rule);
        do_identity(&mut random, &mut forest, root, Operator::Add, &Type::Field);
        println!("After identity: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        // The variable should now point to a
        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_identity_mul() {
        // Test Identity rule for Mul: (a * 1) <-> a
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::IdentityMul;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let one = forest.literal(&mut random, "1Field".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Mul, Type::Field, a, Some(one));
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply identity: a * 1 -> a
        assert_matches(&forest, root, &rule);
        do_identity(&mut random, &mut forest, root, Operator::Mul, &Type::Field);
        println!("After identity: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_self_inverse_sub() {
        // Test SelfInverse rule for Sub: (a - a) -> 0
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::SelfInverseSub;

        let a = forest.input(&mut random, "a".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Sub, Type::Field, a, Some(a));
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a - a -> 0
        assert_matches(&forest, root, &rule);
        do_self_inverse(&mut random, &mut forest, root, Operator::Sub, &ctx, &scope);

        // var should now point to a zero literal
        let new_target = forest.left(var).unwrap();
        println!("After self_inverse: {}", forest.get_expr_for_node(new_target));
        assert!(is_zero(&forest, new_target));
    }

    #[test]
    fn test_self_inverse_xor() {
        // Test SelfInverse rule for Xor: (a ^ a) -> 0
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::SelfInverseXor;

        let a = forest.input(
            &mut random,
            "a".into(),
            Type::Integer(Integer { bits: 32, signed: false }),
        );

        let root = forest.operator(
            &mut random,
            Operator::Xor,
            Type::Integer(Integer { bits: 32, signed: false }),
            a,
            Some(a),
        );
        let var = forest.variable(
            &mut random,
            "v".into(),
            Type::Integer(Integer { bits: 32, signed: false }),
            false,
            false,
            root,
        );
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_self_inverse(&mut random, &mut forest, root, Operator::Xor, &ctx, &scope);

        let new_target = forest.left(var).unwrap();
        println!("After self_inverse: {}", forest.get_expr_for_node(new_target));
        assert!(is_zero(&forest, new_target));
    }

    #[test]
    fn test_idempotent_and() {
        // Test Idempotent rule for And: (a & a) <-> a
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        let root = forest.operator(&mut random, Operator::And, Type::Boolean, a, Some(a));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a & a -> a
        do_idempotent(&mut random, &mut forest, root, Operator::And);
        println!("After idempotent: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_idempotent_or() {
        // Test Idempotent rule for Or: (a | a) <-> a
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        let root = forest.operator(&mut random, Operator::Or, Type::Boolean, a, Some(a));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a | a -> a
        do_idempotent(&mut random, &mut forest, root, Operator::Or);
        println!("After idempotent: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_unary_neg() {
        // Test DoubleUnary rule for Neg: --a <-> a
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);

        // Build --a
        let neg_a = forest.operator(&mut random, Operator::Neg, Type::Field, a, None);
        let neg_neg_a = forest.operator(&mut random, Operator::Neg, Type::Field, neg_a, None);
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, neg_neg_a);
        println!("Initial: {}", forest.get_expr_for_node(neg_neg_a));

        // Apply: --a -> a
        do_double_unary(&mut random, &mut forest, neg_neg_a, Operator::Neg);
        println!("After double_unary: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_unary_not() {
        // Test DoubleUnary rule for Not: !!a <-> a
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        let not_a = forest.operator(&mut random, Operator::Not, Type::Boolean, a, None);
        let not_not_a = forest.operator(&mut random, Operator::Not, Type::Boolean, not_a, None);
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, not_not_a);
        println!("Initial: {}", forest.get_expr_for_node(not_not_a));

        // Apply: !!a -> a
        do_double_unary(&mut random, &mut forest, not_not_a, Operator::Not);
        println!("After double_unary: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_add_neg_sub() {
        // Test AddNegSub rule: (a - b) <-> (a + (-b))
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        // Build a - b
        let root = forest.operator(&mut random, Operator::Sub, Type::Field, a, Some(b));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a - b -> a + (-b)
        do_add_neg_sub(&mut random, &mut forest, root);
        println!("After add_neg_sub: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        assert_eq!(forest.left(root), Some(a));
        let neg_b = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, neg_b), Some(Operator::Neg));
        assert_eq!(forest.left(neg_b), Some(b));

        // Reverse: a + (-b) -> a - b
        do_add_neg_sub(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Sub));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_neg_zero_sub() {
        // Test NegZeroSub rule: (-a) <-> (0 - a)
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);

        // Build -a
        let root = forest.operator(&mut random, Operator::Neg, Type::Field, a, None);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: -a -> 0 - a
        do_neg_zero_sub(&mut random, &mut forest, root);
        println!("After neg_zero_sub: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Sub));
        assert!(is_zero(&forest, forest.left(root).unwrap()));
        assert_eq!(forest.right(root), Some(a));

        // Reverse: 0 - a -> -a
        do_neg_zero_sub(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Neg));
        assert_eq!(forest.left(root), Some(a));
        assert!(forest.right(root).is_none());
    }

    #[test]
    fn test_flip_comparison() {
        // Test FlipComparison rule: (a < b) <-> (b > a)
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Less, Type::Boolean, a, Some(b));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a < b -> b > a
        do_flip_comparison(&mut forest, root);
        println!("After flip_comparison: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Greater));
        assert_eq!(forest.left(root), Some(b));
        assert_eq!(forest.right(root), Some(a));

        // Reverse: b > a -> a < b
        do_flip_comparison(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Less));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_negate_comparison() {
        // Test NegateComparison rule: (a < b) <-> !(a >= b)
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Less, Type::Boolean, a, Some(b));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a < b -> !(a >= b)
        let empty_conditions = HashSet::new();
        do_negate_comparison(&mut random, &mut forest, root, &empty_conditions);

        // root is now a >= b, and var points to a Not node
        assert_eq!(op_of(&forest, root), Some(Operator::GreaterOrEqual));
        let not_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, not_node), Some(Operator::Not));
        assert_eq!(forest.left(not_node), Some(root));
        println!("After negate_comparison: {}", forest.get_expr_for_node(not_node));

        // Reverse: !(a >= b) -> a < b
        do_negate_comparison(&mut random, &mut forest, not_node, &empty_conditions);
        println!("After reverse: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(op_of(&forest, root), Some(Operator::Less));
    }

    #[test]
    fn test_expand_comparison() {
        // Test ExpandComparison rule: (a <= b) <-> ((a < b) | (a == b))
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ExpandComparison;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::LessOrEqual, Type::Boolean, a, Some(b));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a <= b -> (a < b) | (a == b)
        assert_matches(&forest, root, &rule);
        do_expand_comparison(&mut random, &mut forest, root);

        // var should now point to the new Or node
        let or_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, or_node), Some(Operator::Or));
        println!("After expand_comparison: {}", forest.get_expr_for_node(or_node));

        let less_node = forest.left(or_node).unwrap();
        let eq_node = forest.right(or_node).unwrap();
        assert_eq!(op_of(&forest, less_node), Some(Operator::Less));
        assert_eq!(op_of(&forest, eq_node), Some(Operator::Equal));

        // Reverse: (a < b) | (a == b) -> a <= b
        assert_matches(&forest, or_node, &rule);
        do_expand_comparison(&mut random, &mut forest, or_node);

        let leq_node = forest.left(var).unwrap();
        println!("After reverse: {}", forest.get_expr_for_node(leq_node));
        assert_eq!(op_of(&forest, leq_node), Some(Operator::LessOrEqual));
    }

    #[test]
    fn test_demorgan_and() {
        // Test DeMorgan rule: !(a & b) <-> (!a | !b)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::DeMorgan;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let b = forest.input(&mut random, "b".into(), Type::Boolean);

        // Build !(a & b)
        let a_and_b = forest.operator(&mut random, Operator::And, Type::Boolean, a, Some(b));
        let not_and = forest.operator(&mut random, Operator::Not, Type::Boolean, a_and_b, None);
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, not_and);
        println!("Initial: {}", forest.get_expr_for_node(not_and));

        // Apply: !(a & b) -> (!a | !b)
        assert_matches(&forest, not_and, &rule);
        do_demorgan(&mut random, &mut forest, not_and);

        let or_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, or_node), Some(Operator::Or));
        println!("After demorgan: {}", forest.get_expr_for_node(or_node));

        let not_a = forest.left(or_node).unwrap();
        let not_b = forest.right(or_node).unwrap();
        assert_eq!(op_of(&forest, not_a), Some(Operator::Not));
        assert_eq!(op_of(&forest, not_b), Some(Operator::Not));
        assert_eq!(forest.left(not_a), Some(a));
        assert_eq!(forest.left(not_b), Some(b));

        // Reverse: (!a | !b) -> !(a & b)
        assert_matches(&forest, or_node, &rule);
        do_demorgan(&mut random, &mut forest, or_node);

        let not_and_new = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, not_and_new), Some(Operator::Not));
        let and_node = forest.left(not_and_new).unwrap();
        assert_eq!(op_of(&forest, and_node), Some(Operator::And));
        println!("After reverse: {}", forest.get_expr_for_node(not_and_new));
    }

    #[test]
    fn test_complement_xor() {
        // Test ComplementXor rule: !a <-> (a ^ true)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ComplementXor;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        // Build !a
        let not_a = forest.operator(&mut random, Operator::Not, Type::Boolean, a, None);
        println!("Initial: {}", forest.get_expr_for_node(not_a));

        // Apply: !a -> a ^ true
        assert_matches(&forest, not_a, &rule);
        do_complement_xor(&mut random, &mut forest, not_a);
        println!("After complement_xor: {}", forest.get_expr_for_node(not_a));

        assert_eq!(op_of(&forest, not_a), Some(Operator::Xor));
        assert_eq!(forest.left(not_a), Some(a));
        assert!(is_one(&forest, forest.right(not_a).unwrap()));

        // Reverse: a ^ true -> !a
        assert_matches(&forest, not_a, &rule);
        do_complement_xor(&mut random, &mut forest, not_a);
        println!("After reverse: {}", forest.get_expr_for_node(not_a));

        assert_eq!(op_of(&forest, not_a), Some(Operator::Not));
        assert_eq!(forest.left(not_a), Some(a));
        assert!(forest.right(not_a).is_none());
    }

    #[test]
    fn test_xor_to_and_or() {
        // Test XorToAndOr rule: (a ^ b) <-> ((!a & b) | (a & !b))
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::XorToAndOr;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let b = forest.input(&mut random, "b".into(), Type::Boolean);

        let xor = forest.operator(&mut random, Operator::Xor, Type::Boolean, a, Some(b));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, xor);
        println!("Initial: {}", forest.get_expr_for_node(xor));

        // Apply: a ^ b -> (!a & b) | (a & !b)
        assert_matches(&forest, xor, &rule);
        do_xor_to_and_or(&mut random, &mut forest, xor);

        let or_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, or_node), Some(Operator::Or));
        println!("After xor_to_and_or: {}", forest.get_expr_for_node(or_node));

        let left_and = forest.left(or_node).unwrap();
        let right_and = forest.right(or_node).unwrap();
        assert_eq!(op_of(&forest, left_and), Some(Operator::And));
        assert_eq!(op_of(&forest, right_and), Some(Operator::And));

        // Reverse: (!a & b) | (a & !b) -> a ^ b
        assert_matches(&forest, or_node, &rule);
        do_xor_to_and_or(&mut random, &mut forest, or_node);

        let new_xor = forest.left(var).unwrap();
        println!("After reverse: {}", forest.get_expr_for_node(new_xor));
        assert_eq!(op_of(&forest, new_xor), Some(Operator::Xor));
    }

    #[test]
    fn test_mod_one() {
        // Test ModOne rule: (a % 1) -> 0
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ModOne;

        let a = forest.input(
            &mut random,
            "a".into(),
            Type::Integer(Integer { bits: 32, signed: false }),
        );
        let one = forest.literal(
            &mut random,
            "1u32".into(),
            Type::Integer(Integer { bits: 32, signed: false }),
        );

        let root = forest.operator(
            &mut random,
            Operator::Mod,
            Type::Integer(Integer { bits: 32, signed: false }),
            a,
            Some(one),
        );
        let var = forest.variable(
            &mut random,
            "v".into(),
            Type::Integer(Integer { bits: 32, signed: false }),
            false,
            false,
            root,
        );
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a % 1 -> 0
        assert_matches(&forest, root, &rule);
        do_mod_one(&mut random, &mut forest, root);

        let result = forest.left(var).unwrap();
        println!("After mod_one: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_and_to_mod() {
        // Test AndToMod rule: (a & 1) <-> (a % 2)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::AndToMod;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input(&mut random, "a".into(), ty.clone());
        let one = forest.literal(&mut random, "1u32".into(), ty.clone());

        let root = forest.operator(&mut random, Operator::And, ty.clone(), a, Some(one));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a & 1 -> a % 2
        assert_matches(&forest, root, &rule);
        do_and_to_mod(&mut random, &mut forest, root);
        println!("After and_to_mod: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mod));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_two(&forest, forest.right(root).unwrap()));

        // Reverse: a % 2 -> a & 1
        assert_matches(&forest, root, &rule);
        do_and_to_mod(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::And));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_one(&forest, forest.right(root).unwrap()));
    }

    #[test]
    fn test_shift_zero() {
        // Test ShiftZero rule: (a << 0) -> a
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ShiftZero;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input(&mut random, "a".into(), ty.clone());
        let zero = forest.literal(&mut random, "0u32".into(), ty.clone());

        let root = forest.operator(&mut random, Operator::Shl, ty.clone(), a, Some(zero));
        let var = forest.variable(&mut random, "v".into(), ty.clone(), false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a << 0 -> a
        assert_matches(&forest, root, &rule);
        do_shift_zero(&mut random, &mut forest, root);
        println!("After shift_zero: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_mul_two() {
        // Test DoubleMulTwo rule: (a + a) <-> (a * 2)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::DoubleMulTwo;

        let a = forest.input(&mut random, "a".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Add, Type::Field, a, Some(a));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a + a -> a * 2
        assert_matches(&forest, root, &rule);
        do_double_mul_two(&mut random, &mut forest, root);
        println!("After double_mul_two: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_two(&forest, forest.right(root).unwrap()));

        // Reverse: a * 2 -> a + a
        assert_matches(&forest, root, &rule);
        do_double_mul_two(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(a));
    }

    #[test]
    fn test_mul_neg_one_neg() {
        // Test MulNegOneNeg rule: (a * -1) <-> (-a)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::MulNegOneNeg;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let neg_one = forest.literal(&mut random, "-1Field".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Mul, Type::Field, a, Some(neg_one));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a * -1 -> -a
        assert_matches(&forest, root, &rule);
        do_mul_neg_one_neg(&mut random, &mut forest, root);
        println!("After mul_neg_one_neg: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Neg));
        assert_eq!(forest.left(root), Some(a));
        assert!(forest.right(root).is_none());

        // Reverse: -a -> a * -1
        assert_matches(&forest, root, &rule);
        do_mul_neg_one_neg(&mut random, &mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_neg_one(&forest, forest.right(root).unwrap()));
    }

    #[test]
    fn test_absorb_mul() {
        // Test Absorb rule for Mul: (a * 0) -> 0
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::AbsorbMul;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let zero = forest.literal(&mut random, "0Field".into(), Type::Field);

        let root = forest.operator(&mut random, Operator::Mul, Type::Field, a, Some(zero));
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a * 0 -> 0
        assert_matches(&forest, root, &rule);
        do_absorb(&mut random, &mut forest, root, Operator::Mul, &ctx, &scope);

        let result = forest.left(var).unwrap();
        println!("After absorb: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_absorb_and() {
        // Test Absorb rule for And: (a & 0) -> 0 (for booleans: a & false -> false)
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::AbsorbAnd;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let false_lit = forest.literal(&mut random, "false".into(), Type::Boolean);

        let root = forest.operator(&mut random, Operator::And, Type::Boolean, a, Some(false_lit));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a & false -> false
        assert_matches(&forest, root, &rule);
        do_absorb(&mut random, &mut forest, root, Operator::And, &ctx, &scope);

        let result = forest.left(var).unwrap();
        println!("After absorb: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_sub_identity_rejects_boolean() {
        // Test that Identity rule for Sub does NOT match boolean operands
        // This prevents invalid transformations like (bool_expr - false)
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::IdentitySub;

        // Create a boolean XOR expression
        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let b = forest.literal(&mut random, "true".into(), Type::Boolean);
        let xor_expr = forest.operator(&mut random, Operator::Xor, Type::Boolean, a, Some(b));

        // Create Sub expression with boolean operands: xor_expr - false
        let false_lit = forest.literal(&mut random, "false".into(), Type::Boolean);
        let sub_expr =
            forest.operator(&mut random, Operator::Sub, Type::Boolean, xor_expr, Some(false_lit));

        // The Identity rule should NOT match this because Sub is not valid for booleans
        let op = op_of(&forest, sub_expr);
        let left = forest.left(sub_expr);
        let right = forest.right(sub_expr);
        assert!(
            !matches_rule(&forest, op, left, right, &rule),
            "Identity(Sub) rule should NOT match boolean operands"
        );
    }
}
