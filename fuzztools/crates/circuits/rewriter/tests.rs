
#[cfg(test)]
mod tests {
    use petgraph::graph::NodeIndex;

    use crate::circuits::{ast::{forest::*, operators::*}, rewriter::*};
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
            matches_rule(f, op, left, right, kind, idx),
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
        for _ in 0..15 {
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
            !matches_rule(&forest, op, left, right, &rule, sub_expr),
            "Identity(Sub) rule should NOT match boolean operands"
        );
    }
}
