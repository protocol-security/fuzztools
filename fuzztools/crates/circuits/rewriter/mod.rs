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
        RULES.iter().map(|r| r.kind.name()).collect()
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
                for &n in nodes {
                    if is_assignment_lhs(forest, n) {
                        continue;
                    }
                    let (left, right) = (forest.left(n), forest.right(n));
                    for &i in rules {
                        // Skip rules that use redirect on condition nodes
                        if uses_redirect(&self.rules[i].kind) && condition_nodes.contains(&n) {
                            continue;
                        }
                        if matches_rule(forest, Some(op), left, right, &self.rules[i].kind) {
                            matches.entry(i).or_default().push(n);
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
                                matches.entry(i).or_default().push(n);
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
                return Some(selected_rule.name());
            }
        }

        // No matches in main forest, try nested forests (ForLoop and If bodies)
        // Use the stored nested_forests list for O(1) lookup instead of traversing the graph
        for &idx in &forest.nested_forests.clone() {
            if !forest.graph.contains_node(idx) {
                continue;
            }

            match &mut forest.graph[idx] {
                Node::ForLoop { body, .. } => {
                    if let Some(rule_name) = self.apply_random(random, body, ctx, scope) {
                        return Some(rule_name);
                    }
                }
                Node::If { then_body, else_ifs, else_body, .. } => {
                    if let Some(rule_name) = self.apply_random(random, then_body, ctx, scope) {
                        return Some(rule_name);
                    }
                    for (_cond, body) in else_ifs {
                        if let Some(rule_name) = self.apply_random(random, body, ctx, scope) {
                            return Some(rule_name);
                        }
                    }
                    if let Some(body) = else_body {
                        if let Some(rule_name) = self.apply_random(random, body, ctx, scope) {
                            return Some(rule_name);
                        }
                    }
                }
                _ => {}
            }
        }

        None
    }

    fn apply(
        &self,
        random: &mut impl Rng,
        forest: &mut Forest,
        n: NodeIndex,
        kind: &RuleKind,
        ctx: &Context,
        scope: &Scope,
        condition_nodes: &HashSet<NodeIndex>,
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
            RuleKind::NegateComparison => do_negate_comparison(forest, n, condition_nodes),
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

        // Remove the node if it is orphaned
        forest.remove_if_orphan(n);
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

        // ((a / b) * c) ↔ (a * (c / b)) - only valid for Field as integers round down
        (RuleKind::AssociateDiv, Some(Operator::Mul)) => {
            is_binary &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Field)) &&
                (left_op == Some(Operator::Div) || right_op == Some(Operator::Div))
        }

        // (a / b) ↔ ((1 / b) * a) - only valid for Field as integers round down
        (RuleKind::DivCommute, Some(Operator::Div)) => {
            is_binary && left.is_some_and(|l| matches!(f.ty(l), Type::Field))
        }
        // Reverse: ((1 / b) * a) → (a / b)
        (RuleKind::DivCommute, Some(Operator::Mul)) => {
            is_binary &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Field)) &&
                left_op == Some(Operator::Div) &&
                left.and_then(|l| f.left(l)).is_some_and(|ll| is_one(f, ll))
        }

        // (a * (b + c)) ↔ ((a * b) + (a * c))
        (RuleKind::Distribute { outer, inner }, Some(o)) => {
            is_binary &&
                ((o == *outer && left_op == Some(*inner)) ||
                    (o == *inner && left_op == Some(*outer) && right_op == Some(*outer)))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Identity/Absorb rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::Identity { op: rule_op, identity_right }, Some(o))
            if o == *rule_op && is_binary =>
        {
            // Get the operand (non-identity) type to check And correctness
            let operand = if *identity_right { left } else { right };
            let operand_ty = operand.map(|n| f.ty(n));

            let check = if *identity_right { right } else { left };
            let is_valid_identity = operand_ty
                .clone()
                .is_some_and(|op_ty| check.is_some_and(|c| is_identity_for_type(f, c, o, &op_ty)));

            // For arithmetic operators (Add/Sub/Mul/Div) and shifts, only allow Field/Integer types
            let is_type_compatible = match o {
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                    operand_ty.is_some_and(|ty| matches!(ty, Type::Field | Type::Integer(_)))
                }
                Operator::Shl | Operator::Shr => {
                    operand_ty.is_some_and(|ty| matches!(ty, Type::Integer(_)))
                }
                _ => true, // Other operators (Xor, Or, And) are already handled correctly
            };

            is_valid_identity && is_type_compatible
        }
        // Injection case: a → a op identity (only when there's no existing operator)
        (RuleKind::Identity { op, .. }, None) => {
            // For And injection (a -> a & 1), only apply to booleans
            // For Or/Xor injection (a -> a | 0, a -> a ^ 0), apply to all types
            // For Add/Sub/Mul/Div/Shl/Shr injection, only apply to Field/Integer (not Boolean)
            // because booleans don't support arithmetic or shift operations
            match op {
                Operator::And => left.is_some_and(|l| matches!(f.ty(l), Type::Boolean)),
                Operator::Or | Operator::Xor => left.is_some_and(|l| is_numeric_or_bool(f, l)),
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                    left.is_some_and(|l| matches!(f.ty(l), Type::Field | Type::Integer(_)))
                }
                Operator::Shl | Operator::Shr => {
                    left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_)))
                }
                _ => false,
            }
        }

        (RuleKind::Absorb { op: rule_op }, Some(o)) => {
            if o != *rule_op || !is_binary {
                return false;
            }
            // a & 0 = 0 is correct for all types (booleans and numeric)
            // a * 0 = 0 is correct for all numeric types
            // a | 1 = 1 is only correct for booleans (for numeric, a | 1 sets only LSB, not all
            // bits)
            let operand_ty = left.map(|l| f.ty(l));
            match o {
                Operator::And | Operator::Mul => {
                    left.is_some_and(|l| is_absorbing(f, l, o)) ||
                        right.is_some_and(|r| is_absorbing(f, r, o))
                }
                Operator::Or => {
                    operand_ty.is_some_and(|ty| matches!(ty, Type::Boolean)) &&
                        (left.is_some_and(|l| is_absorbing(f, l, o)) ||
                            right.is_some_and(|r| is_absorbing(f, r, o)))
                }
                _ => false,
            }
        }
        // Note: Absorb injection (0 → a * 0, etc.) is NOT handled here
        // since it requires random expression generation which needs access to random/ctx/scope.
        // Could be added as a separate InjectAbsorb rule in the future.
        (RuleKind::SelfInverse { op: rule_op }, Some(o)) => {
            if o != *rule_op || !is_binary || left != right {
                return false;
            }
            // Check type compatibility for the operator
            let ty = left.map(|l| f.ty(l));
            match o {
                // a - a = 0: valid for Field and integers
                Operator::Sub => ty.is_some_and(|t| matches!(t, Type::Field | Type::Integer(_))),
                // a ^ a = 0: valid for integers and booleans
                Operator::Xor => ty.is_some_and(|t| matches!(t, Type::Integer(_) | Type::Boolean)),
                // a / a = 1: only if a != 0. Only apply to known non-zero literals.
                Operator::Div => {
                    ty.is_some_and(|t| matches!(t, Type::Field | Type::Integer(_))) &&
                        left.is_some_and(|l| {
                            matches!(&f.graph[l], Node::Literal { value, .. }
                                if !value.starts_with('0') && !value.starts_with("-0") && value != "false")
                        })
                }
                _ => false,
            }
        }
        // Note: Injection (0 → a - a, etc.) is handled by InjectXorXor and similar rules
        // since it requires random expression generation
        (RuleKind::Idempotent { op: rule_op }, Some(o)) => {
            if o != *rule_op || !is_binary || left != right {
                return false;
            }
            // Check type compatibility: And/Or work on booleans and integers
            left.is_some_and(|l| {
                let ty = f.ty(l);
                match o {
                    Operator::And | Operator::Or => matches!(ty, Type::Boolean | Type::Integer(_)),
                    _ => false,
                }
            })
        }
        // Injection: a → a & a or a → a | a (only for booleans/integers that support these ops)
        (RuleKind::Idempotent { op }, None) => left.is_some_and(|l| {
            let ty = f.ty(l);
            match op {
                Operator::And | Operator::Or => matches!(ty, Type::Boolean | Type::Integer(_)),
                _ => false,
            }
        }),

        // ─────────────────────────────────────────────────────────────────────────
        // Unary rules
        // ─────────────────────────────────────────────────────────────────────────
        // Simplification: --a → a or !!a → a
        (RuleKind::DoubleUnary { op: rule_op }, Some(o)) => {
            o == *rule_op &&
                is_unary &&
                left.is_some_and(|i| op_of(f, i) == Some(o) && f.right(i).is_none())
        }
        // Injection: a → --a or a → !!a
        (RuleKind::DoubleUnary { op }, None) => can_apply_unary(f, left, *op),

        (RuleKind::AddNegSub, Some(Operator::Sub)) => {
            is_binary && left.is_some_and(|l| is_signed_type(f, l))
        }
        (RuleKind::AddNegSub, Some(Operator::Add)) => {
            is_binary &&
                right_op == Some(Operator::Neg) &&
                left.is_some_and(|l| is_signed_type(f, l))
        }

        (RuleKind::NegZeroSub, Some(Operator::Neg)) => {
            is_unary && left.is_some_and(|l| is_signed_type(f, l))
        }
        (RuleKind::NegZeroSub, Some(Operator::Sub)) => {
            is_binary &&
                left.is_some_and(|l| is_zero(f, l)) &&
                right.is_some_and(|r| is_signed_type(f, r))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Comparison rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::FlipComparison, Some(o)) => {
            is_binary &&
                matches!(
                    o,
                    Operator::Less |
                        Operator::Greater |
                        Operator::LessOrEqual |
                        Operator::GreaterOrEqual
                )
        }

        (RuleKind::NegateComparison, Some(o)) => {
            (o.is_comparison() && is_binary) ||
                (o == Operator::Not && is_unary && left_op.is_some_and(|x| x.is_comparison()))
        }

        // (a <= b) ↔ ((a < b) || (a == b))
        (RuleKind::ExpandComparison, Some(o)) => {
            is_binary &&
                (matches!(o, Operator::LessOrEqual | Operator::GreaterOrEqual) ||
                    (o == Operator::Or &&
                        left_op.is_some_and(|x| {
                            matches!(x, Operator::Less | Operator::Greater)
                        }) &&
                        right_op == Some(Operator::Equal)))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Boolean rules
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::DeMorgan, Some(Operator::Not)) => {
            is_unary && left_op.is_some_and(|x| matches!(x, Operator::And | Operator::Or))
        }
        (RuleKind::DeMorgan, Some(Operator::And | Operator::Or)) => {
            is_binary && left_op == Some(Operator::Not) && right_op == Some(Operator::Not)
        }

        // !a ↔ a ^ true only works for booleans (for integers, !a is bitwise NOT ≠ a ^ 1)
        (RuleKind::ComplementXor, Some(Operator::Not)) => {
            is_unary && left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }
        (RuleKind::ComplementXor, Some(Operator::Xor)) => {
            is_binary &&
                right.is_some_and(|r| is_one(f, r)) &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }

        // (a ^ b) ↔ ((!a & b) | (a & !b))
        (RuleKind::XorToAndOr, Some(Operator::Xor)) => {
            is_binary && left.is_some_and(|l| matches!(f.ty(l), Type::Boolean))
        }
        (RuleKind::XorToAndOr, Some(Operator::Or)) => {
            // Match pattern: ((!a & b) | (a & !b))
            is_binary && left_op == Some(Operator::And) && right_op == Some(Operator::And) && {
                let ll_op = left.and_then(|l| f.left(l)).and_then(|ll| op_of(f, ll));
                let rl_op = right.and_then(|r| f.right(r)).and_then(|rr| op_of(f, rr));
                ll_op == Some(Operator::Not) && rl_op == Some(Operator::Not)
            }
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Modulo rules
        // ─────────────────────────────────────────────────────────────────────────
        // a % 1 → 0
        (RuleKind::ModOne, Some(Operator::Mod)) => is_binary && right.is_some_and(|r| is_one(f, r)),
        // 0 → r % 1 (injection) - only when there's no operator (injection case)
        (RuleKind::ModOne, None) => {
            left.is_some_and(|l| is_zero(f, l) && matches!(f.ty(l), Type::Integer(_)))
        }

        // a & 1 ↔ a % 2 (only valid when type can hold value 2, i.e., not u1)
        (RuleKind::AndToMod, Some(Operator::And)) => {
            is_binary &&
                right.is_some_and(|r| is_one(f, r)) &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Integer(i) if i.bits > 1))
        }
        (RuleKind::AndToMod, Some(Operator::Mod)) => {
            is_binary &&
                right.is_some_and(|r| is_two(f, r)) &&
                left.is_some_and(|l| matches!(f.ty(l), Type::Integer(i) if i.bits > 1))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Shift rules
        // ─────────────────────────────────────────────────────────────────────────
        // Simplification: a << 0 → a, a >> 0 → a
        (RuleKind::ShiftZero, Some(Operator::Shl | Operator::Shr)) => {
            is_binary && right.is_some_and(|r| is_zero(f, r))
        }
        // Injection: a → a << 0 or a → a >> 0 (for integers)
        (RuleKind::ShiftZero, None) => left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_))),

        // ─────────────────────────────────────────────────────────────────────────
        // Simplification rules
        // ─────────────────────────────────────────────────────────────────────────
        // a + a ↔ a * 2 (only valid when type can hold value 2, i.e., not u1)
        (RuleKind::DoubleMulTwo, Some(Operator::Add)) => {
            is_binary &&
                left == right &&
                left.is_some_and(|l| {
                    matches!(f.ty(l), Type::Integer(i) if i.bits > 1) ||
                        matches!(f.ty(l), Type::Field)
                })
        }
        (RuleKind::DoubleMulTwo, Some(Operator::Mul)) => {
            is_binary &&
                right.is_some_and(|r| is_two(f, r)) &&
                left.is_some_and(|l| {
                    matches!(f.ty(l), Type::Integer(i) if i.bits > 1) ||
                        matches!(f.ty(l), Type::Field)
                })
        }

        (RuleKind::MulNegOneNeg, Some(Operator::Mul)) => {
            is_binary &&
                right.is_some_and(|r| is_neg_one(f, r)) &&
                left.is_some_and(|l| is_signed_type(f, l))
        }
        (RuleKind::MulNegOneNeg, Some(Operator::Neg)) => {
            is_unary && left.is_some_and(|l| is_signed_type(f, l))
        }

        // ─────────────────────────────────────────────────────────────────────────
        // Injection rules (obfuscation) - only when there's no existing operator
        // ─────────────────────────────────────────────────────────────────────────
        (RuleKind::InjectAddSub | RuleKind::InjectSubAdd | RuleKind::InjectMulDiv, None) => {
            left.is_some_and(|l| matches!(f.ty(l), Type::Field))
        }

        (RuleKind::InjectXorXor, None) => {
            left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_) | Type::Boolean))
        }

        // 1 → r / r (only for Field since integer division truncates and we need r != 0)
        (RuleKind::InjectDivDiv, None) => {
            left.is_some_and(|l| is_one(f, l) && matches!(f.ty(l), Type::Field))
        }

        // a → a | 0 (for integers)
        (RuleKind::InjectOrZero, None) => left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_))),

        // a → a & a (for integers and booleans)
        (RuleKind::InjectAndSelf, None) => {
            left.is_some_and(|l| matches!(f.ty(l), Type::Integer(_) | Type::Boolean))
        }

        _ => false,
    }
}

/// Get all operators that a rule can target (for building the index)
fn operators_for_rule(kind: &RuleKind) -> Vec<Operator> {
    match kind {
        RuleKind::SwapOperands { ops } | RuleKind::Associate { ops } => ops.to_vec(),
        RuleKind::AssociateSub => vec![Sub],
        RuleKind::AssociateDiv => vec![Mul],
        RuleKind::DivCommute => vec![Div, Mul], // Div for forward, Mul for reverse
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

/// Check if a rule uses `redirect_edges` internally (which breaks `If/Assert` conditions)
const fn uses_redirect(kind: &RuleKind) -> bool {
    matches!(
        kind,
        RuleKind::NegateComparison |
            RuleKind::ExpandComparison |
            RuleKind::DeMorgan |
            RuleKind::XorToAndOr |
            RuleKind::InjectAddSub |
            RuleKind::InjectSubAdd |
            RuleKind::InjectMulDiv |
            RuleKind::InjectXorXor |
            RuleKind::InjectDivDiv |
            RuleKind::InjectOrZero |
            RuleKind::InjectAndSelf |
            RuleKind::ModOne
    )
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
    // Simplification: a op identity → a
    if op_of(f, n) == Some(op) {
        let (keep, check) =
            if identity_right { (f.left(n), f.right(n)) } else { (f.right(n), f.left(n)) };
        if check.is_some_and(|c| is_identity(f, c, op)) {
            if let Some(k) = keep {
                redirect(f, n, k);
            }
        }
        // If we have the operator but identity check failed, do nothing (don't inject)
        return;
    }
    // Injection: x → x op identity (only when there's no operator)
    let ty = f.ty(n);
    let edges = f.incoming_edges(n);
    let id = make_identity(f, &ty, op);
    let (lhs, rhs) = if identity_right { (n, id) } else { (id, n) };
    let new = f.operator(op, ty, lhs, Some(rhs));
    f.redirect_edges(n, new, &edges);
}

fn do_absorb(f: &mut Forest, n: NodeIndex, op: Operator) {
    // Simplification: a * 0 → 0, a & 0 → 0, a | true → true
    // Only applies when we have the operator and one operand is absorbing
    if op_of(f, n) == Some(op) {
        let left = f.left(n);
        let right = f.right(n);
        if left.is_some_and(|l| is_absorbing(f, l, op)) ||
            right.is_some_and(|r| is_absorbing(f, r, op))
        {
            let ty = f.ty(n);
            let absorb = make_absorbing(f, &ty, op);
            redirect(f, n, absorb);
        }
    }
    // Injection case (0 → a * 0) is NOT handled here - it requires random expression generation
    // which needs access to random/ctx/scope. Could be added as a separate InjectAbsorb rule.
}

fn do_self_inverse(f: &mut Forest, n: NodeIndex, op: Operator) {
    // Simplification: a op a → identity (0 for Sub/Xor, 1 for Div)
    if op_of(f, n) == Some(op) && f.left(n) == f.right(n) {
        let ty = f.ty(n);
        let lit = match op {
            Operator::Sub | Operator::Xor => make_zero(f, &ty),
            Operator::Div => make_one(f, &ty),
            _ => return,
        };
        redirect(f, n, lit);
    }
    // Injection case is NOT handled here - it requires random expression generation
    // which needs access to random/ctx/scope, so injection is done via separate InjectXorXor etc.
}

fn do_idempotent(f: &mut Forest, n: NodeIndex, op: Operator) {
    // Simplification: a op a → a (for And/Or)
    if op_of(f, n) == Some(op) {
        if f.left(n) == f.right(n) {
            if let Some(operand) = f.left(n) {
                redirect(f, n, operand);
            }
        }
        // If we have the operator but idempotent check failed, do nothing (don't inject)
        return;
    }
    // Injection: a → a op a (only for booleans, only when there's no operator)
    if matches!(f.ty(n), Type::Boolean) {
        let edges = f.incoming_edges(n);
        let new = f.operator(op, Type::Boolean, n, Some(n));
        f.redirect_edges(n, new, &edges);
    }
}

fn do_double_unary(f: &mut Forest, n: NodeIndex, op: Operator) {
    // Simplification: op(op(x)) → x (e.g., --a → a, !!a → a)
    if op_of(f, n) == Some(op) && f.right(n).is_none() {
        if let Some(inner) = f.left(n).filter(|&i| op_of(f, i) == Some(op) && f.right(i).is_none())
        {
            if let Some(x) = f.left(inner) {
                redirect(f, n, x);
            }
        }
        // If we have the operator but double unary check failed, do nothing (don't inject)
        return;
    }
    // Injection: x → op(op(x)) (only when there's no operator)
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
            let zero = make_zero(f, &ty);
            set_op(f, n, Operator::Sub);
            f.add_operand(n, 1, operand);
            f.replace_operand(n, 0, zero);
        }
        Some(Operator::Sub) if f.left(n).is_some_and(|l| is_zero(f, l)) => {
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
                let id = edge.id();
                f.graph.remove_edge(id);
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

fn do_negate_comparison(f: &mut Forest, n: NodeIndex, condition_nodes: &HashSet<NodeIndex>) {
    // not(a cmp b) -> a neg_cmp b
    if op_of(f, n) == Some(Operator::Not) {
        if let Some(inner) = f.left(n) {
            // Don't modify the inner comparison if it's used as a condition
            if condition_nodes.contains(&inner) {
                return;
            }
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
    let r = f.literal(ty.random_value(random, ctx, scope, true), ty.clone());
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
    let value = ty.random_value(random, ctx, scope, true);
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
    let value = ty.random_value(random, ctx, scope, true);
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
    // Safety check: don't apply for u1 types - can't hold value 2
    let ty = f.ty(n);
    if matches!(&ty, Type::Integer(i) if i.bits == 1) {
        return;
    }

    match op_of(f, n) {
        Some(Operator::Add) if f.left(n) == f.right(n) => {
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

/// Check if `n` is an identity element for `op` when used with operand type `operand_ty`.
/// For And, 1 is only identity for booleans (true & a = a), not integers (1 & 3 = 1 ≠ 3).
#[inline(always)]
fn is_identity_for_type(f: &Forest, n: NodeIndex, op: Operator, operand_ty: &Type) -> bool {
    match op {
        Operator::Add |
        Operator::Sub |
        Operator::Xor |
        Operator::Or |
        Operator::Shl |
        Operator::Shr => is_zero(f, n),
        Operator::Mul | Operator::Div => is_one(f, n),
        // For And, 1 (true) is only identity for booleans
        Operator::And => matches!(operand_ty, Type::Boolean) && is_one(f, n),
        _ => false,
    }
}

/// Check if `n` is an identity element for `op` (legacy version, does NOT check type for And).
/// Callers must ensure type correctness separately when using this for And.
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
    use super::*;
    use crate::{builders::CircuitBuilder, circuits::ast::nodes::NodeKind};
    use std::fs;

    const RED: &str = "\x1b[31m";
    const GREEN: &str = "\x1b[32m";
    const RESET: &str = "\x1b[0m";

    /// Helper to check if a rule matches for a given node
    fn assert_matches(f: &Forest, n: NodeIndex, kind: &RuleKind) {
        let op = op_of(f, n);
        let left = f.left(n);
        let right = f.right(n);
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

    fn print_diff(before: &str, after: &str) {
        let before_lines: Vec<&str> = before.lines().collect();
        let after_lines: Vec<&str> = after.lines().collect();

        println!("\n{}", "=".repeat(80));
        println!("CIRCUIT COMPARISON");
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
        let before = builder.format_circuit(&scope, &forest_before);

        // Apply rewriter
        for _ in 0..15 {
            rewriter.apply_random(&mut random, &mut forest, &ctx, &scope);
        }

        let after = builder.format_circuit(&scope, &forest);

        print_diff(&before, &after);
    }

    #[test]
    fn test_associate() {
        // Test Associate rule: (a + b) + c ↔ a + (b + c)
        let mut forest = Forest::default();
        let rule = RuleKind::Associate { ops: &[Operator::Add] };

        // Create inputs: a, b, c
        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);
        let c = forest.input("c".into(), Type::Field);

        // Build (a + b) + c (left-associated)
        let a_plus_b = forest.operator(Operator::Add, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Add, Type::Field, a_plus_b, Some(c));

        // Verify initial structure: left child is (a + b), right child is c
        assert_eq!(forest.left(root), Some(a_plus_b));
        assert_eq!(forest.right(root), Some(c));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Add));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply associate: (a + b) + c -> a + (b + c)
        assert_matches(&forest, root, &rule);
        do_associate(&mut forest, root);
        println!("After associate: {}", forest.get_expr_for_node(root));

        // Verify new structure: left child is a, right child is (b + c)
        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Add));
        assert_eq!(forest.left(new_right), Some(b));
        assert_eq!(forest.right(new_right), Some(c));

        // Apply associate again (reverse direction): a + (b + c) -> (a + b) + c
        assert_matches(&forest, root, &rule);
        do_associate(&mut forest, root);
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
        // Test AssociateSub rule: (a - b) - c ↔ a - (b + c)
        let mut forest = Forest::default();
        let rule = RuleKind::AssociateSub;

        // Create inputs: a, b, c
        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);
        let c = forest.input("c".into(), Type::Field);

        // Build (a - b) - c
        let a_minus_b = forest.operator(Operator::Sub, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Sub, Type::Field, a_minus_b, Some(c));

        // Verify initial structure: (a - b) - c
        assert_eq!(forest.left(root), Some(a_minus_b));
        assert_eq!(forest.right(root), Some(c));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Sub));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply associate_sub: (a - b) - c -> a - (b + c)
        assert_matches(&forest, root, &rule);
        do_associate_sub(&mut forest, root);
        println!("After associate_sub: {}", forest.get_expr_for_node(root));

        // Verify new structure: a - (b + c)
        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Add)); // b + c
        assert_eq!(forest.left(new_right), Some(b));
        assert_eq!(forest.right(new_right), Some(c));

        // Apply associate_sub again (reverse direction): a - (b + c) -> (a - b) - c
        assert_matches(&forest, root, &rule);
        do_associate_sub(&mut forest, root);
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
        // Test SwapOperands rule: (a + b) ↔ (b + a)
        let mut forest = Forest::default();
        let rule = RuleKind::SwapOperands { ops: &[Operator::Add] };

        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);

        let root = forest.operator(Operator::Add, Type::Field, a, Some(b));

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
        // Test AssociateDiv rule: ((a / b) * c) ↔ (a * (c / b))
        let mut forest = Forest::default();
        let rule = RuleKind::AssociateDiv;

        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);
        let c = forest.input("c".into(), Type::Field);

        // Build (a / b) * c
        let a_div_b = forest.operator(Operator::Div, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Mul, Type::Field, a_div_b, Some(c));

        // Verify initial: (a / b) * c
        assert_eq!(forest.left(root), Some(a_div_b));
        assert_eq!(forest.right(root), Some(c));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: (a / b) * c -> a * (c / b)
        assert_matches(&forest, root, &rule);
        do_associate_div(&mut forest, root);
        println!("After associate_div: {}", forest.get_expr_for_node(root));

        // Verify: a * (c / b)
        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Div));
        assert_eq!(forest.left(new_right), Some(c));
        assert_eq!(forest.right(new_right), Some(b));

        // Reverse: a * (c / b) -> (a / b) * c
        assert_matches(&forest, root, &rule);
        do_associate_div(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Div));
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_distribute() {
        // Test Distribute rule: ((a + b) * c) ↔ ((a * c) + (b * c))
        let mut forest = Forest::default();
        let rule = RuleKind::Distribute { outer: Operator::Mul, inner: Operator::Add };

        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);
        let c = forest.input("c".into(), Type::Field);

        // Build (a + b) * c
        let a_plus_b = forest.operator(Operator::Add, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Mul, Type::Field, a_plus_b, Some(c));

        // Verify initial: (a + b) * c
        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Add));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply distribute: (a + b) * c -> (a * c) + (b * c)
        assert_matches(&forest, root, &rule);
        do_distribute(&mut forest, root, Operator::Mul, Operator::Add);
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
        // Test Identity rule for Add: (a + 0) ↔ a
        let mut forest = Forest::default();
        let rule = RuleKind::Identity { op: Operator::Add, identity_right: true };

        let a = forest.input("a".into(), Type::Field);
        let zero = forest.literal("0Field".into(), Type::Field);

        // Build a + 0
        let root = forest.operator(Operator::Add, Type::Field, a, Some(zero));

        // Add an incoming edge so redirect works
        let var = forest.variable("v".into(), Type::Field, false, false, root);

        // Verify initial: a + 0
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(zero));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply identity: a + 0 -> a (redirects var to point to a)
        assert_matches(&forest, root, &rule);
        do_identity(&mut forest, root, Operator::Add, true);
        println!("After identity: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        // The variable should now point to a
        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_identity_mul() {
        // Test Identity rule for Mul: (a * 1) ↔ a
        let mut forest = Forest::default();
        let rule = RuleKind::Identity { op: Operator::Mul, identity_right: true };

        let a = forest.input("a".into(), Type::Field);
        let one = forest.literal("1Field".into(), Type::Field);

        let root = forest.operator(Operator::Mul, Type::Field, a, Some(one));
        let var = forest.variable("v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply identity: a * 1 -> a
        assert_matches(&forest, root, &rule);
        do_identity(&mut forest, root, Operator::Mul, true);
        println!("After identity: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_self_inverse_sub() {
        // Test SelfInverse rule for Sub: (a - a) -> 0
        let mut forest = Forest::default();
        let rule = RuleKind::SelfInverse { op: Operator::Sub };

        let a = forest.input("a".into(), Type::Field);

        let root = forest.operator(Operator::Sub, Type::Field, a, Some(a));
        let var = forest.variable("v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a - a -> 0
        assert_matches(&forest, root, &rule);
        do_self_inverse(&mut forest, root, Operator::Sub);

        // var should now point to a zero literal
        let new_target = forest.left(var).unwrap();
        println!("After self_inverse: {}", forest.get_expr_for_node(new_target));
        assert!(is_zero(&forest, new_target));
    }

    #[test]
    fn test_self_inverse_xor() {
        // Test SelfInverse rule for Xor: (a ^ a) -> 0
        let mut forest = Forest::default();
        let rule = RuleKind::SelfInverse { op: Operator::Xor };

        let a = forest.input("a".into(), Type::Integer(Integer { bits: 32, signed: false }));

        let root = forest.operator(
            Operator::Xor,
            Type::Integer(Integer { bits: 32, signed: false }),
            a,
            Some(a),
        );
        let var = forest.variable(
            "v".into(),
            Type::Integer(Integer { bits: 32, signed: false }),
            false,
            false,
            root,
        );
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_self_inverse(&mut forest, root, Operator::Xor);

        let new_target = forest.left(var).unwrap();
        println!("After self_inverse: {}", forest.get_expr_for_node(new_target));
        assert!(is_zero(&forest, new_target));
    }

    #[test]
    fn test_idempotent_and() {
        // Test Idempotent rule for And: (a & a) ↔ a
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Boolean);

        let root = forest.operator(Operator::And, Type::Boolean, a, Some(a));
        let var = forest.variable("v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a & a -> a
        do_idempotent(&mut forest, root, Operator::And);
        println!("After idempotent: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_idempotent_or() {
        // Test Idempotent rule for Or: (a | a) ↔ a
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Boolean);

        let root = forest.operator(Operator::Or, Type::Boolean, a, Some(a));
        let var = forest.variable("v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a | a -> a
        do_idempotent(&mut forest, root, Operator::Or);
        println!("After idempotent: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_unary_neg() {
        // Test DoubleUnary rule for Neg: --a ↔ a
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Field);

        // Build --a
        let neg_a = forest.operator(Operator::Neg, Type::Field, a, None);
        let neg_neg_a = forest.operator(Operator::Neg, Type::Field, neg_a, None);
        let var = forest.variable("v".into(), Type::Field, false, false, neg_neg_a);
        println!("Initial: {}", forest.get_expr_for_node(neg_neg_a));

        // Apply: --a -> a
        do_double_unary(&mut forest, neg_neg_a, Operator::Neg);
        println!("After double_unary: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_unary_not() {
        // Test DoubleUnary rule for Not: !!a ↔ a
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Boolean);

        let not_a = forest.operator(Operator::Not, Type::Boolean, a, None);
        let not_not_a = forest.operator(Operator::Not, Type::Boolean, not_a, None);
        let var = forest.variable("v".into(), Type::Boolean, false, false, not_not_a);
        println!("Initial: {}", forest.get_expr_for_node(not_not_a));

        // Apply: !!a -> a
        do_double_unary(&mut forest, not_not_a, Operator::Not);
        println!("After double_unary: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_add_neg_sub() {
        // Test AddNegSub rule: (a - b) ↔ (a + (-b))
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);

        // Build a - b
        let root = forest.operator(Operator::Sub, Type::Field, a, Some(b));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a - b -> a + (-b)
        do_add_neg_sub(&mut forest, root);
        println!("After add_neg_sub: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        assert_eq!(forest.left(root), Some(a));
        let neg_b = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, neg_b), Some(Operator::Neg));
        assert_eq!(forest.left(neg_b), Some(b));

        // Reverse: a + (-b) -> a - b
        do_add_neg_sub(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Sub));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_neg_zero_sub() {
        // Test NegZeroSub rule: (-a) ↔ (0 - a)
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Field);

        // Build -a
        let root = forest.operator(Operator::Neg, Type::Field, a, None);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: -a -> 0 - a
        do_neg_zero_sub(&mut forest, root);
        println!("After neg_zero_sub: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Sub));
        assert!(is_zero(&forest, forest.left(root).unwrap()));
        assert_eq!(forest.right(root), Some(a));

        // Reverse: 0 - a -> -a
        do_neg_zero_sub(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Neg));
        assert_eq!(forest.left(root), Some(a));
        assert!(forest.right(root).is_none());
    }

    #[test]
    fn test_flip_comparison() {
        // Test FlipComparison rule: (a < b) ↔ (b > a)
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);

        let root = forest.operator(Operator::Less, Type::Boolean, a, Some(b));
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
        // Test NegateComparison rule: (a < b) ↔ !(a >= b)
        let mut forest = Forest::default();

        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);

        let root = forest.operator(Operator::Less, Type::Boolean, a, Some(b));
        let var = forest.variable("v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a < b -> !(a >= b)
        let empty_conditions = HashSet::new();
        do_negate_comparison(&mut forest, root, &empty_conditions);

        // root is now a >= b, and var points to a Not node
        assert_eq!(op_of(&forest, root), Some(Operator::GreaterOrEqual));
        let not_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, not_node), Some(Operator::Not));
        assert_eq!(forest.left(not_node), Some(root));
        println!("After negate_comparison: {}", forest.get_expr_for_node(not_node));

        // Reverse: !(a >= b) -> a < b
        do_negate_comparison(&mut forest, not_node, &empty_conditions);
        println!("After reverse: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(op_of(&forest, root), Some(Operator::Less));
    }

    #[test]
    fn test_expand_comparison() {
        // Test ExpandComparison rule: (a <= b) ↔ ((a < b) | (a == b))
        let mut forest = Forest::default();
        let rule = RuleKind::ExpandComparison;

        let a = forest.input("a".into(), Type::Field);
        let b = forest.input("b".into(), Type::Field);

        let root = forest.operator(Operator::LessOrEqual, Type::Boolean, a, Some(b));
        let var = forest.variable("v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a <= b -> (a < b) | (a == b)
        assert_matches(&forest, root, &rule);
        do_expand_comparison(&mut forest, root);

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
        do_expand_comparison(&mut forest, or_node);

        let leq_node = forest.left(var).unwrap();
        println!("After reverse: {}", forest.get_expr_for_node(leq_node));
        assert_eq!(op_of(&forest, leq_node), Some(Operator::LessOrEqual));
    }

    #[test]
    fn test_demorgan_and() {
        // Test DeMorgan rule: !(a & b) ↔ (!a | !b)
        let mut forest = Forest::default();
        let rule = RuleKind::DeMorgan;

        let a = forest.input("a".into(), Type::Boolean);
        let b = forest.input("b".into(), Type::Boolean);

        // Build !(a & b)
        let a_and_b = forest.operator(Operator::And, Type::Boolean, a, Some(b));
        let not_and = forest.operator(Operator::Not, Type::Boolean, a_and_b, None);
        let var = forest.variable("v".into(), Type::Boolean, false, false, not_and);
        println!("Initial: {}", forest.get_expr_for_node(not_and));

        // Apply: !(a & b) -> (!a | !b)
        assert_matches(&forest, not_and, &rule);
        do_demorgan(&mut forest, not_and);

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
        do_demorgan(&mut forest, or_node);

        let not_and_new = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, not_and_new), Some(Operator::Not));
        let and_node = forest.left(not_and_new).unwrap();
        assert_eq!(op_of(&forest, and_node), Some(Operator::And));
        println!("After reverse: {}", forest.get_expr_for_node(not_and_new));
    }

    #[test]
    fn test_complement_xor() {
        // Test ComplementXor rule: !a ↔ (a ^ true)
        let mut forest = Forest::default();
        let rule = RuleKind::ComplementXor;

        let a = forest.input("a".into(), Type::Boolean);

        // Build !a
        let not_a = forest.operator(Operator::Not, Type::Boolean, a, None);
        println!("Initial: {}", forest.get_expr_for_node(not_a));

        // Apply: !a -> a ^ true
        assert_matches(&forest, not_a, &rule);
        do_complement_xor(&mut forest, not_a);
        println!("After complement_xor: {}", forest.get_expr_for_node(not_a));

        assert_eq!(op_of(&forest, not_a), Some(Operator::Xor));
        assert_eq!(forest.left(not_a), Some(a));
        assert!(is_one(&forest, forest.right(not_a).unwrap()));

        // Reverse: a ^ true -> !a
        assert_matches(&forest, not_a, &rule);
        do_complement_xor(&mut forest, not_a);
        println!("After reverse: {}", forest.get_expr_for_node(not_a));

        assert_eq!(op_of(&forest, not_a), Some(Operator::Not));
        assert_eq!(forest.left(not_a), Some(a));
        assert!(forest.right(not_a).is_none());
    }

    #[test]
    fn test_xor_to_and_or() {
        // Test XorToAndOr rule: (a ^ b) ↔ ((!a & b) | (a & !b))
        let mut forest = Forest::default();
        let rule = RuleKind::XorToAndOr;

        let a = forest.input("a".into(), Type::Boolean);
        let b = forest.input("b".into(), Type::Boolean);

        let xor = forest.operator(Operator::Xor, Type::Boolean, a, Some(b));
        let var = forest.variable("v".into(), Type::Boolean, false, false, xor);
        println!("Initial: {}", forest.get_expr_for_node(xor));

        // Apply: a ^ b -> (!a & b) | (a & !b)
        assert_matches(&forest, xor, &rule);
        do_xor_to_and_or(&mut forest, xor);

        let or_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, or_node), Some(Operator::Or));
        println!("After xor_to_and_or: {}", forest.get_expr_for_node(or_node));

        let left_and = forest.left(or_node).unwrap();
        let right_and = forest.right(or_node).unwrap();
        assert_eq!(op_of(&forest, left_and), Some(Operator::And));
        assert_eq!(op_of(&forest, right_and), Some(Operator::And));

        // Reverse: (!a & b) | (a & !b) -> a ^ b
        assert_matches(&forest, or_node, &rule);
        do_xor_to_and_or(&mut forest, or_node);

        let new_xor = forest.left(var).unwrap();
        println!("After reverse: {}", forest.get_expr_for_node(new_xor));
        assert_eq!(op_of(&forest, new_xor), Some(Operator::Xor));
    }

    #[test]
    fn test_mod_one() {
        // Test ModOne rule: (a % 1) -> 0
        let mut forest = Forest::default();
        let rule = RuleKind::ModOne;

        let a = forest.input("a".into(), Type::Integer(Integer { bits: 32, signed: false }));
        let one = forest.literal("1u32".into(), Type::Integer(Integer { bits: 32, signed: false }));

        let root = forest.operator(
            Operator::Mod,
            Type::Integer(Integer { bits: 32, signed: false }),
            a,
            Some(one),
        );
        let var = forest.variable(
            "v".into(),
            Type::Integer(Integer { bits: 32, signed: false }),
            false,
            false,
            root,
        );
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a % 1 -> 0
        assert_matches(&forest, root, &rule);
        do_mod_one(&mut forest, root);

        let result = forest.left(var).unwrap();
        println!("After mod_one: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_and_to_mod() {
        // Test AndToMod rule: (a & 1) ↔ (a % 2)
        let mut forest = Forest::default();
        let rule = RuleKind::AndToMod;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input("a".into(), ty.clone());
        let one = forest.literal("1u32".into(), ty.clone());

        let root = forest.operator(Operator::And, ty.clone(), a, Some(one));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a & 1 -> a % 2
        assert_matches(&forest, root, &rule);
        do_and_to_mod(&mut forest, root);
        println!("After and_to_mod: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mod));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_two(&forest, forest.right(root).unwrap()));

        // Reverse: a % 2 -> a & 1
        assert_matches(&forest, root, &rule);
        do_and_to_mod(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::And));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_one(&forest, forest.right(root).unwrap()));
    }

    #[test]
    fn test_shift_zero() {
        // Test ShiftZero rule: (a << 0) -> a
        let mut forest = Forest::default();
        let rule = RuleKind::ShiftZero;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input("a".into(), ty.clone());
        let zero = forest.literal("0u32".into(), ty.clone());

        let root = forest.operator(Operator::Shl, ty.clone(), a, Some(zero));
        let var = forest.variable("v".into(), ty.clone(), false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a << 0 -> a
        assert_matches(&forest, root, &rule);
        do_shift_zero(&mut forest, root);
        println!("After shift_zero: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_mul_two() {
        // Test DoubleMulTwo rule: (a + a) ↔ (a * 2)
        let mut forest = Forest::default();
        let rule = RuleKind::DoubleMulTwo;

        let a = forest.input("a".into(), Type::Field);

        let root = forest.operator(Operator::Add, Type::Field, a, Some(a));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a + a -> a * 2
        assert_matches(&forest, root, &rule);
        do_double_mul_two(&mut forest, root);
        println!("After double_mul_two: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_two(&forest, forest.right(root).unwrap()));

        // Reverse: a * 2 -> a + a
        assert_matches(&forest, root, &rule);
        do_double_mul_two(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(a));
    }

    #[test]
    fn test_mul_neg_one_neg() {
        // Test MulNegOneNeg rule: (a * -1) ↔ (-a)
        let mut forest = Forest::default();
        let rule = RuleKind::MulNegOneNeg;

        let a = forest.input("a".into(), Type::Field);
        let neg_one = forest.literal("-1Field".into(), Type::Field);

        let root = forest.operator(Operator::Mul, Type::Field, a, Some(neg_one));
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a * -1 -> -a
        assert_matches(&forest, root, &rule);
        do_mul_neg_one_neg(&mut forest, root);
        println!("After mul_neg_one_neg: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Neg));
        assert_eq!(forest.left(root), Some(a));
        assert!(forest.right(root).is_none());

        // Reverse: -a -> a * -1
        assert_matches(&forest, root, &rule);
        do_mul_neg_one_neg(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_neg_one(&forest, forest.right(root).unwrap()));
    }

    #[test]
    fn test_absorb_mul() {
        // Test Absorb rule for Mul: (a * 0) -> 0
        let mut forest = Forest::default();
        let rule = RuleKind::Absorb { op: Operator::Mul };

        let a = forest.input("a".into(), Type::Field);
        let zero = forest.literal("0Field".into(), Type::Field);

        let root = forest.operator(Operator::Mul, Type::Field, a, Some(zero));
        let var = forest.variable("v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a * 0 -> 0
        assert_matches(&forest, root, &rule);
        do_absorb(&mut forest, root, Operator::Mul);

        let result = forest.left(var).unwrap();
        println!("After absorb: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_absorb_and() {
        // Test Absorb rule for And: (a & 0) -> 0 (for booleans: a & false -> false)
        let mut forest = Forest::default();
        let rule = RuleKind::Absorb { op: Operator::And };

        let a = forest.input("a".into(), Type::Boolean);
        let false_lit = forest.literal("false".into(), Type::Boolean);

        let root = forest.operator(Operator::And, Type::Boolean, a, Some(false_lit));
        let var = forest.variable("v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        // Apply: a & false -> false
        assert_matches(&forest, root, &rule);
        do_absorb(&mut forest, root, Operator::And);

        let result = forest.left(var).unwrap();
        println!("After absorb: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_sub_identity_rejects_boolean() {
        // Test that Identity rule for Sub does NOT match boolean operands
        // This prevents invalid transformations like (bool_expr - false)
        let mut forest = Forest::default();
        let rule = RuleKind::Identity { op: Operator::Sub, identity_right: true };

        // Create a boolean XOR expression
        let a = forest.input("a".into(), Type::Boolean);
        let b = forest.literal("true".into(), Type::Boolean);
        let xor_expr = forest.operator(Operator::Xor, Type::Boolean, a, Some(b));

        // Create Sub expression with boolean operands: xor_expr - false
        let false_lit = forest.literal("false".into(), Type::Boolean);
        let sub_expr = forest.operator(Operator::Sub, Type::Boolean, xor_expr, Some(false_lit));

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
