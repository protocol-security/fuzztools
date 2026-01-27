#[cfg(test)]
mod tests {
    use petgraph::graph::NodeIndex;

    use crate::{
        builders::CircuitBuilder,
        circuits::{
            ast::{forest::*, operators::*, types::*},
            rewriter::*,
        },
    };
    use std::{collections::HashSet, fs};

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

        let effective_suffix_len = suffix_len.min(line_len.saturating_sub(prefix_len));
        let diff_start = prefix_len;
        let diff_end = line_len.saturating_sub(effective_suffix_len);

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

    fn compute_diff<'a>(
        before_lines: &[&'a str],
        after_lines: &[&'a str],
    ) -> Vec<(DiffOp, usize, usize)> {
        let idx = before_lines.len();
        let m = after_lines.len();

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
                    if i + 1 < diff_ops.len() && diff_ops[i + 1].0 == DiffOp::Insert {
                        let (_, _, next_after_idx) = diff_ops[i + 1];
                        let before_line = before_lines[before_idx];
                        let after_line = after_lines[next_after_idx];

                        let highlighted_before = highlight_diff(before_line, after_line, RED);
                        let highlighted_after = highlight_diff(after_line, before_line, GREEN);

                        println!("{}-{} {}", RED, RESET, highlighted_before);
                        println!("{}+{} {}", GREEN, RESET, highlighted_after);
                        i += 2;
                    } else {
                        println!("{}-{} {}{}{}", RED, RESET, RED, before_lines[before_idx], RESET);
                        i += 1;
                    }
                }
                DiffOp::Insert => {
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
        let rewriter = Rewriter::default();

        for (name, ty, _) in &scope.inputs {
            forest.input(&mut random, name.clone(), ty.clone());
        }
        for (name, ty, _) in &scope.globals {
            forest.input(&mut random, name.clone(), ty.clone());
        }

        forest.random(&mut random, &ctx, &scope, true);

        let forest_before = forest.clone();
        let before = builder.format_circuit(&scope, &forest_before);

        for _ in 0..15 {
            rewriter.apply_random(&mut random, &mut forest, &ctx, &scope);
        }

        let after = builder.format_circuit(&scope, &forest);

        print_diff(&before, &after);
    }

    #[test]
    fn test_associate() {
        let mut forest = Forest::default();
        let mut rng = rand::rng();
        let rule = RuleKind::Associate;

        let a = forest.input(&mut rng, "a".into(), Type::Field);
        let b = forest.input(&mut rng, "b".into(), Type::Field);
        let c = forest.input(&mut rng, "c".into(), Type::Field);

        let a_plus_b = forest.operator(Operator::Add, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Add, Type::Field, a_plus_b, Some(c));

        assert_eq!(forest.left(root), Some(a_plus_b));
        assert_eq!(forest.right(root), Some(c));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Add));

        assert_matches(&forest, root, &rule);
        do_associate(&mut forest, root);

        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Add));
        assert_eq!(forest.left(new_right), Some(b));
        assert_eq!(forest.right(new_right), Some(c));

        assert_matches(&forest, root, &rule);
        do_associate(&mut forest, root);

        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Add));
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_associate_sub() {
        let mut forest = Forest::default();
        let mut rng = rand::rng();
        let rule = RuleKind::AssociateSub;

        let a = forest.input(&mut rng, "a".into(), Type::Field);
        let b = forest.input(&mut rng, "b".into(), Type::Field);
        let c = forest.input(&mut rng, "c".into(), Type::Field);

        let a_minus_b = forest.operator(Operator::Sub, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Sub, Type::Field, a_minus_b, Some(c));

        assert_eq!(forest.left(root), Some(a_minus_b));
        assert_eq!(forest.right(root), Some(c));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Sub));

        println!("Initial: {}", forest.get_expr_for_node(root));
        assert_matches(&forest, root, &rule);

        // ((a - b) - c) -> (a - (b + c))
        do_associate_sub(&mut forest, root);
        println!("After associate_sub: {}", forest.get_expr_for_node(root));

        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Add));
        assert_eq!(forest.left(new_right), Some(b));
        assert_eq!(forest.right(new_right), Some(c));

        assert_matches(&forest, root, &rule);

        // (a - (b + c)) -> ((a - b) - c)
        do_associate_sub(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Sub));
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_swap_operands() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::SwapOperands;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(Operator::Add, Type::Field, a, Some(b));

        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        forest.swap_operands(root);
        println!("After swap: {}", forest.get_expr_for_node(root));

        assert_eq!(forest.left(root), Some(b));
        assert_eq!(forest.right(root), Some(a));

        assert_matches(&forest, root, &rule);
        forest.swap_operands(root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_associate_div() {
        let mut forest = Forest::default();
        let mut rng = rand::rng();
        let rule = RuleKind::AssociateDiv;

        let a = forest.input(&mut rng, "a".into(), Type::Field);
        let b = forest.input(&mut rng, "b".into(), Type::Field);
        let c = forest.input(&mut rng, "c".into(), Type::Field);

        let a_div_b = forest.operator(Operator::Div, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Mul, Type::Field, a_div_b, Some(c));

        assert_eq!(forest.left(root), Some(a_div_b));
        assert_eq!(forest.right(root), Some(c));

        println!("Initial: {}", forest.get_expr_for_node(root));
        assert_matches(&forest, root, &rule);

        // ((a / b) * c) -> (a * (c / b))
        do_associate_div(&mut forest, root);
        println!("After associate_div: {}", forest.get_expr_for_node(root));

        assert_eq!(forest.left(root), Some(a));
        let new_right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, new_right), Some(Operator::Div));
        assert_eq!(forest.left(new_right), Some(c));
        assert_eq!(forest.right(new_right), Some(b));

        assert_matches(&forest, root, &rule);

        // (a * (c / b)) -> ((a / b) * c)
        do_associate_div(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Div));
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_div_commute() {
        let mut forest = Forest::default();
        let mut rng = rand::rng();
        let rule = RuleKind::DivCommute;

        let a = forest.input(&mut rng, "a".into(), Type::Field);
        let b = forest.input(&mut rng, "b".into(), Type::Field);

        let root = forest.operator(Operator::Div, Type::Field, a, Some(b));

        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
        assert_eq!(op_of(&forest, root), Some(Operator::Div));

        println!("Initial: {}", forest.get_expr_for_node(root));
        assert_matches(&forest, root, &rule);

        // (a / b) -> ((1 / b) * a)
        do_div_commute(&mut forest, root);
        println!("After reciprocal: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        let one_div_b = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, one_div_b), Some(Operator::Div));
        assert!(is_one(&forest, forest.left(one_div_b).unwrap()));
        assert_eq!(forest.right(one_div_b), Some(b));
        assert_eq!(forest.right(root), Some(a));

        assert_matches(&forest, root, &rule);

        // ((1 / b) * a) -> (a / b)
        do_div_commute(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Div));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_distribute() {
        let mut forest = Forest::default();
        let mut rng = rand::rng();
        let rule = RuleKind::DistributeMulAdd;

        let a = forest.input(&mut rng, "a".into(), Type::Field);
        let b = forest.input(&mut rng, "b".into(), Type::Field);
        let c = forest.input(&mut rng, "c".into(), Type::Field);

        let a_plus_b = forest.operator(Operator::Add, Type::Field, a, Some(b));
        let root = forest.operator(Operator::Mul, Type::Field, a_plus_b, Some(c));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(op_of(&forest, forest.left(root).unwrap()), Some(Operator::Add));

        println!("Initial: {}", forest.get_expr_for_node(root));
        assert_matches(&forest, root, &rule);

        // (a + b) * c -> (a * c) + (b * c)
        do_distribute(&mut forest, root, Operator::Mul, Operator::Add);
        println!("After distribute: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        let left = forest.left(root).unwrap();
        let right = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, left), Some(Operator::Mul));
        assert_eq!(op_of(&forest, right), Some(Operator::Mul));
        assert_eq!(forest.left(left), Some(a));
        assert_eq!(forest.right(left), Some(c));
        assert_eq!(forest.left(right), Some(b));
        assert_eq!(forest.right(right), Some(c));

        assert_matches(&forest, root, &rule);

        // (a * c) + (b * c) -> (a + b) * c
        do_distribute(&mut forest, root, Operator::Mul, Operator::Add);
        println!("After factor: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        let new_left = forest.left(root).unwrap();
        assert_eq!(op_of(&forest, new_left), Some(Operator::Add));
        assert_eq!(forest.left(new_left), Some(a));
        assert_eq!(forest.right(new_left), Some(b));
        assert_eq!(forest.right(root), Some(c));
    }

    #[test]
    fn test_identity_add() {
        let mut forest = Forest::default();
        let mut rng = rand::rng();
        let rule = RuleKind::IdentityAdd;

        let a = forest.input(&mut rng, "a".into(), Type::Field);
        let zero = forest.literal("0Field".into(), Type::Field);
        let root = forest.operator(Operator::Add, Type::Field, a, Some(zero));
        let var = forest.variable(&mut rng, "v".into(), Type::Field, false, false, root);

        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(zero));

        println!("Initial: {}", forest.get_expr_for_node(root));
        assert_matches(&forest, root, &rule);

        // a + 0 -> a
        do_identity(&mut forest, root, Operator::Add, &Type::Field);
        println!("After identity: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));

        // a -> a + 0
        do_identity(&mut forest, a, Operator::Add, &Type::Field);
        println!("After injection: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        let new_root = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, new_root), Some(Operator::Add));
        assert_eq!(forest.left(new_root), Some(a));
        assert!(is_identity(&forest, forest.right(new_root).unwrap(), Operator::Add, &Type::Field));
    }

    #[test]
    fn test_identity_mul() {
        let mut forest = Forest::default();
        let mut rng = rand::rng();
        let rule = RuleKind::IdentityAdd;

        let a = forest.input(&mut rng, "a".into(), Type::Field);
        let zero = forest.literal("1Field".into(), Type::Field);
        let root = forest.operator(Operator::Mul, Type::Field, a, Some(zero));
        let var = forest.variable(&mut rng, "v".into(), Type::Field, false, false, root);

        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(zero));

        println!("Initial: {}", forest.get_expr_for_node(root));
        assert_matches(&forest, root, &rule);

        // a * 1 -> a
        do_identity(&mut forest, root, Operator::Mul, &Type::Field);
        println!("After identity: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));

        // a -> a * 1
        do_identity(&mut forest, a, Operator::Mul, &Type::Field);
        println!("After injection: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        let new_root = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, new_root), Some(Operator::Mul));
        assert_eq!(forest.left(new_root), Some(a));
        assert!(is_identity(&forest, forest.right(new_root).unwrap(), Operator::Mul, &Type::Field));
    }

    #[test]
    fn test_self_inverse_sub() {
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::SelfInverseSub;

        let a = forest.input(&mut random, "a".into(), Type::Field);

        let root = forest.operator(Operator::Sub, Type::Field, a, Some(a));
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_self_inverse(&mut forest, root, Operator::Sub, &ctx, &scope);

        let new_target = forest.left(var).unwrap();
        println!("After self_inverse: {}", forest.get_expr_for_node(new_target));
        assert!(is_zero(&forest, new_target));
    }

    #[test]
    fn test_self_inverse_xor() {
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::SelfInverseXor;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input(&mut random, "a".into(), ty.clone());

        let root = forest.operator(Operator::Xor, ty.clone(), a, Some(a));
        let var = forest.variable(&mut random, "v".into(), ty, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_self_inverse(&mut forest, root, Operator::Xor, &ctx, &scope);

        let new_target = forest.left(var).unwrap();
        println!("After self_inverse: {}", forest.get_expr_for_node(new_target));
        assert!(is_zero(&forest, new_target));
    }

    #[test]
    fn test_idempotent_and() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        let root = forest.operator(Operator::And, Type::Boolean, a, Some(a));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        do_idempotent(&mut forest, root, Operator::And);
        println!("After idempotent: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_idempotent_or() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        let root = forest.operator(Operator::Or, Type::Boolean, a, Some(a));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        do_idempotent(&mut forest, root, Operator::Or);
        println!("After idempotent: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_unary_neg() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);

        let neg_a = forest.operator(Operator::Neg, Type::Field, a, None);
        let neg_neg_a = forest.operator(Operator::Neg, Type::Field, neg_a, None);
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, neg_neg_a);
        println!("Initial: {}", forest.get_expr_for_node(neg_neg_a));

        do_double_unary(&mut forest, neg_neg_a, Operator::Neg);
        println!("After double_unary: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_unary_not() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        let not_a = forest.operator(Operator::Not, Type::Boolean, a, None);
        let not_not_a = forest.operator(Operator::Not, Type::Boolean, not_a, None);
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, not_not_a);
        println!("Initial: {}", forest.get_expr_for_node(not_not_a));

        do_double_unary(&mut forest, not_not_a, Operator::Not);
        println!("After double_unary: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_add_neg_sub() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(Operator::Sub, Type::Field, a, Some(b));
        println!("Initial: {}", forest.get_expr_for_node(root));

        do_add_neg_sub(&mut forest, root);
        println!("After add_neg_sub: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        assert_eq!(forest.left(root), Some(a));
        let neg_b = forest.right(root).unwrap();
        assert_eq!(op_of(&forest, neg_b), Some(Operator::Neg));
        assert_eq!(forest.left(neg_b), Some(b));

        do_add_neg_sub(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Sub));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_neg_zero_sub() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);

        let root = forest.operator(Operator::Neg, Type::Field, a, None);
        println!("Initial: {}", forest.get_expr_for_node(root));

        do_neg_zero_sub(&mut forest, root);
        println!("After neg_zero_sub: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Sub));
        assert!(is_zero(&forest, forest.left(root).unwrap()));
        assert_eq!(forest.right(root), Some(a));

        do_neg_zero_sub(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Neg));
        assert_eq!(forest.left(root), Some(a));
        assert!(forest.right(root).is_none());
    }

    #[test]
    fn test_flip_comparison() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(Operator::Less, Type::Boolean, a, Some(b));
        println!("Initial: {}", forest.get_expr_for_node(root));

        do_flip_comparison(&mut forest, root);
        println!("After flip_comparison: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Greater));
        assert_eq!(forest.left(root), Some(b));
        assert_eq!(forest.right(root), Some(a));

        do_flip_comparison(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Less));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(b));
    }

    #[test]
    fn test_negate_comparison() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(Operator::Less, Type::Boolean, a, Some(b));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        let empty_conditions = HashSet::new();
        do_negate_comparison(&mut forest, root, &empty_conditions);

        assert_eq!(op_of(&forest, root), Some(Operator::GreaterOrEqual));
        let not_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, not_node), Some(Operator::Not));
        assert_eq!(forest.left(not_node), Some(root));
        println!("After negate_comparison: {}", forest.get_expr_for_node(not_node));

        do_negate_comparison(&mut forest, not_node, &empty_conditions);
        println!("After reverse: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(op_of(&forest, root), Some(Operator::Less));
    }

    #[test]
    fn test_expand_comparison() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ExpandComparison;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let b = forest.input(&mut random, "b".into(), Type::Field);

        let root = forest.operator(Operator::LessOrEqual, Type::Boolean, a, Some(b));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_expand_comparison(&mut forest, root);

        let or_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, or_node), Some(Operator::Or));
        println!("After expand_comparison: {}", forest.get_expr_for_node(or_node));

        let less_node = forest.left(or_node).unwrap();
        let eq_node = forest.right(or_node).unwrap();
        assert_eq!(op_of(&forest, less_node), Some(Operator::Less));
        assert_eq!(op_of(&forest, eq_node), Some(Operator::Equal));

        assert_matches(&forest, or_node, &rule);
        do_expand_comparison(&mut forest, or_node);

        let leq_node = forest.left(var).unwrap();
        println!("After reverse: {}", forest.get_expr_for_node(leq_node));
        assert_eq!(op_of(&forest, leq_node), Some(Operator::LessOrEqual));
    }

    #[test]
    fn test_demorgan_and() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::DeMorgan;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let b = forest.input(&mut random, "b".into(), Type::Boolean);

        let a_and_b = forest.operator(Operator::And, Type::Boolean, a, Some(b));
        let not_and = forest.operator(Operator::Not, Type::Boolean, a_and_b, None);
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, not_and);
        println!("Initial: {}", forest.get_expr_for_node(not_and));

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
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ComplementXor;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);

        let not_a = forest.operator(Operator::Not, Type::Boolean, a, None);
        println!("Initial: {}", forest.get_expr_for_node(not_a));

        assert_matches(&forest, not_a, &rule);
        do_complement_xor(&mut forest, not_a);
        println!("After complement_xor: {}", forest.get_expr_for_node(not_a));

        assert_eq!(op_of(&forest, not_a), Some(Operator::Xor));
        assert_eq!(forest.left(not_a), Some(a));
        assert!(is_one(&forest, forest.right(not_a).unwrap()));

        assert_matches(&forest, not_a, &rule);
        do_complement_xor(&mut forest, not_a);
        println!("After reverse: {}", forest.get_expr_for_node(not_a));

        assert_eq!(op_of(&forest, not_a), Some(Operator::Not));
        assert_eq!(forest.left(not_a), Some(a));
        assert!(forest.right(not_a).is_none());
    }

    #[test]
    fn test_xor_to_and_or() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::XorToAndOr;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let b = forest.input(&mut random, "b".into(), Type::Boolean);

        let xor = forest.operator(Operator::Xor, Type::Boolean, a, Some(b));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, xor);
        println!("Initial: {}", forest.get_expr_for_node(xor));

        assert_matches(&forest, xor, &rule);
        do_xor_to_and_or(&mut forest, xor);

        let or_node = forest.left(var).unwrap();
        assert_eq!(op_of(&forest, or_node), Some(Operator::Or));
        println!("After xor_to_and_or: {}", forest.get_expr_for_node(or_node));

        let left_and = forest.left(or_node).unwrap();
        let right_and = forest.right(or_node).unwrap();
        assert_eq!(op_of(&forest, left_and), Some(Operator::And));
        assert_eq!(op_of(&forest, right_and), Some(Operator::And));

        assert_matches(&forest, or_node, &rule);
        do_xor_to_and_or(&mut forest, or_node);

        let new_xor = forest.left(var).unwrap();
        println!("After reverse: {}", forest.get_expr_for_node(new_xor));
        assert_eq!(op_of(&forest, new_xor), Some(Operator::Xor));
    }

    #[test]
    fn test_mod_one() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ModOne;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input(&mut random, "a".into(), ty.clone());
        let one = forest.literal("1u32".into(), ty.clone());

        let root = forest.operator(Operator::Mod, ty.clone(), a, Some(one));
        let var = forest.variable(&mut random, "v".into(), ty, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_mod_one(&mut forest, root);

        let result = forest.left(var).unwrap();
        println!("After mod_one: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_and_to_mod() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::AndToMod;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input(&mut random, "a".into(), ty.clone());
        let one = forest.literal("1u32".into(), ty.clone());

        let root = forest.operator(Operator::And, ty.clone(), a, Some(one));
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_and_to_mod(&mut forest, root);
        println!("After and_to_mod: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mod));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_two(&forest, forest.right(root).unwrap()));

        assert_matches(&forest, root, &rule);
        do_and_to_mod(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::And));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_one(&forest, forest.right(root).unwrap()));
    }

    #[test]
    fn test_shift_zero() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::ShiftZero;
        let ty = Type::Integer(Integer { bits: 32, signed: false });

        let a = forest.input(&mut random, "a".into(), ty.clone());
        let zero = forest.literal("0u32".into(), ty.clone());

        let root = forest.operator(Operator::Shl, ty.clone(), a, Some(zero));
        let var = forest.variable(&mut random, "v".into(), ty, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_shift_zero(&mut forest, root);
        println!("After shift_zero: {}", forest.get_expr_for_node(forest.left(var).unwrap()));

        assert_eq!(forest.left(var), Some(a));
    }

    #[test]
    fn test_double_mul_two() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::DoubleMulTwo;

        let a = forest.input(&mut random, "a".into(), Type::Field);

        let root = forest.operator(Operator::Add, Type::Field, a, Some(a));
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_double_mul_two(&mut forest, root);
        println!("After double_mul_two: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_two(&forest, forest.right(root).unwrap()));

        assert_matches(&forest, root, &rule);
        do_double_mul_two(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Add));
        assert_eq!(forest.left(root), Some(a));
        assert_eq!(forest.right(root), Some(a));
    }

    #[test]
    fn test_mul_neg_one_neg() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::MulNegOneNeg;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let neg_one = forest.literal("-1Field".into(), Type::Field);

        let root = forest.operator(Operator::Mul, Type::Field, a, Some(neg_one));
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_mul_neg_one_neg(&mut forest, root);
        println!("After mul_neg_one_neg: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Neg));
        assert_eq!(forest.left(root), Some(a));
        assert!(forest.right(root).is_none());

        assert_matches(&forest, root, &rule);
        do_mul_neg_one_neg(&mut forest, root);
        println!("After reverse: {}", forest.get_expr_for_node(root));

        assert_eq!(op_of(&forest, root), Some(Operator::Mul));
        assert_eq!(forest.left(root), Some(a));
        assert!(is_neg_one(&forest, forest.right(root).unwrap()));
    }

    #[test]
    fn test_absorb_mul() {
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::AbsorbMul;

        let a = forest.input(&mut random, "a".into(), Type::Field);
        let zero = forest.literal("0Field".into(), Type::Field);

        let root = forest.operator(Operator::Mul, Type::Field, a, Some(zero));
        let var = forest.variable(&mut random, "v".into(), Type::Field, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_absorb(&mut forest, root, Operator::Mul, &ctx, &scope);

        let result = forest.left(var).unwrap();
        println!("After absorb: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_absorb_and() {
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let scope = builder.create_scope(&mut random, &ctx);

        let mut forest = Forest::default();
        let rule = RuleKind::AbsorbAnd;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let false_lit = forest.literal("false".into(), Type::Boolean);

        let root = forest.operator(Operator::And, Type::Boolean, a, Some(false_lit));
        let var = forest.variable(&mut random, "v".into(), Type::Boolean, false, false, root);
        println!("Initial: {}", forest.get_expr_for_node(root));

        assert_matches(&forest, root, &rule);
        do_absorb(&mut forest, root, Operator::And, &ctx, &scope);

        let result = forest.left(var).unwrap();
        println!("After absorb: {}", forest.get_expr_for_node(result));
        assert!(is_zero(&forest, result));
    }

    #[test]
    fn test_sub_identity_rejects_boolean() {
        let mut forest = Forest::default();
        let mut random = rand::rng();
        let rule = RuleKind::IdentitySub;

        let a = forest.input(&mut random, "a".into(), Type::Boolean);
        let b = forest.literal("true".into(), Type::Boolean);
        let xor_expr = forest.operator(Operator::Xor, Type::Boolean, a, Some(b));

        let false_lit = forest.literal("false".into(), Type::Boolean);
        let sub_expr = forest.operator(Operator::Sub, Type::Boolean, xor_expr, Some(false_lit));

        let op = op_of(&forest, sub_expr);
        let left = forest.left(sub_expr);
        let right = forest.right(sub_expr);
        assert!(
            !matches_rule(&forest, op, left, right, &rule, sub_expr),
            "Identity(Sub) rule should NOT match boolean operands"
        );
    }
}
