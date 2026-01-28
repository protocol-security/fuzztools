mod rules;

pub use rules::Rule;

use crate::circuits::ir::{Expr, Operator, Statement, Type, AST};
use rand::{seq::IndexedRandom, Rng};
use strum::IntoEnumIterator;

// ────────────────────────────────────────────────────────────────────────────────
// Public API
// ────────────────────────────────────────────────────────────────────────────────

pub fn collect_applicables(ast: &AST) -> Vec<(Rule, usize, Vec<usize>)> {
    let mut applicables = Vec::new();

    for rule in Rule::iter() {
        for (stmt_idx, stmt) in ast.statements.iter().enumerate() {
            let mut path = Vec::new();
            collect_matches(get_value(stmt), &rule, stmt_idx, &mut path, &mut applicables);
        }
    }

    applicables
}

pub fn apply_random_rule(random: &mut impl Rng, ast: &mut AST) -> Option<Rule> {
    let rules: Vec<_> = Rule::iter().collect();

    // Shuffle rules and try each until one applies
    for rule in rules.choose_multiple(random, rules.len()) {
        let mut matches = Vec::new();
        for (stmt_idx, stmt) in ast.statements.iter().enumerate() {
            let mut path = Vec::new();
            collect_matches(get_value(stmt), rule, stmt_idx, &mut path, &mut matches);
        }

        if let Some((_, stmt_idx, path)) = matches.choose(random) {
            apply_at_path(get_value_mut(&mut ast.statements[*stmt_idx]), path, rule);
            return Some(*rule);
        }
    }

    None
}

pub fn apply_rule(random: &mut impl Rng, ast: &mut AST, rule: Rule) {
    let mut matches = Vec::new();
    for (stmt_idx, stmt) in ast.statements.iter().enumerate() {
        let mut path = Vec::new();
        collect_matches(get_value(stmt), &rule, stmt_idx, &mut path, &mut matches);
    }

    if let Some((_, stmt_idx, path)) = matches.choose(random) {
        apply_at_path(get_value_mut(&mut ast.statements[*stmt_idx]), path, &rule);
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Internal helpers
// ────────────────────────────────────────────────────────────────────────────────

const fn get_value(stmt: &Statement) -> &Expr {
    match stmt {
        Statement::Let { value, .. } |
        Statement::DirectAssignment { value, .. } |
        Statement::IndexAssignment { value, .. } |
        Statement::TupleAssignment { value, .. } |
        Statement::StructAssignment { value, .. } => value,
    }
}

const fn get_value_mut(stmt: &mut Statement) -> &mut Expr {
    match stmt {
        Statement::Let { value, .. } |
        Statement::DirectAssignment { value, .. } |
        Statement::IndexAssignment { value, .. } |
        Statement::TupleAssignment { value, .. } |
        Statement::StructAssignment { value, .. } => value,
    }
}

fn collect_matches(
    expr: &Expr,
    rule: &Rule,
    stmt_idx: usize,
    path: &mut Vec<usize>,
    matches: &mut Vec<(Rule, usize, Vec<usize>)>,
) {
    if rule.apply(expr).is_some() {
        matches.push((*rule, stmt_idx, path.clone()));
    }

    match expr {
        Expr::Binary { left, right, .. } => {
            path.push(0);
            collect_matches(left, rule, stmt_idx, path, matches);
            path.pop();

            path.push(1);
            collect_matches(right, rule, stmt_idx, path, matches);
            path.pop();
        }
        Expr::Unary { operand, .. } |
        Expr::Index { expr: operand, .. } |
        Expr::TupleIndex { expr: operand, .. } |
        Expr::StructField { expr: operand, .. } |
        Expr::Cast { expr: operand, .. } => {
            path.push(0);
            collect_matches(operand, rule, stmt_idx, path, matches);
            path.pop();
        }
        Expr::Ref { .. } => {}
    }
}

fn apply_at_path(expr: &mut Expr, path: &[usize], rule: &Rule) {
    if path.is_empty() {
        if let Some(new_expr) = rule.apply(expr) {
            *expr = new_expr;
        }
        return;
    }

    match expr {
        Expr::Binary { left, right, .. } => {
            apply_at_path(if path[0] == 0 { left } else { right }, &path[1..], rule);
        }
        Expr::Unary { operand, .. } |
        Expr::Index { expr: operand, .. } |
        Expr::TupleIndex { expr: operand, .. } |
        Expr::StructField { expr: operand, .. } |
        Expr::Cast { expr: operand, .. } => {
            apply_at_path(operand, &path[1..], rule);
        }
        Expr::Ref { .. } => {}
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Rule implementations
// ────────────────────────────────────────────────────────────────────────────────

impl Rule {
    pub fn apply(&self, expr: &Expr) -> Option<Expr> {
        match self {
            Rule::SwapOperands => swap_operands(expr),
            Rule::Associate => associate(expr),
            Rule::AssociateSub => associate_sub(expr),
            Rule::AssociateDiv => associate_div(expr),
            Rule::DivCommute => div_commute(expr),
            Rule::DistributeMulAdd => distribute_mul_add(expr),
            Rule::DistributeMulSub => distribute_mul_sub(expr),
            Rule::DistributeAndOr => distribute_and_or(expr),
            Rule::DistributeOrAnd => distribute_or_and(expr),

            Rule::IdentityAdd => identity_add(expr),
            Rule::IdentitySub => identity_sub(expr),
            Rule::IdentityMul => identity_mul(expr),
            Rule::IdentityDiv => identity_div(expr),
            Rule::IdentityXor => identity_xor(expr),
            Rule::IdentityOr => identity_or(expr),
            Rule::IdentityAnd => identity_and(expr),
            Rule::IdentityShl => identity_shl(expr),
            Rule::IdentityShr => identity_shr(expr),

            Rule::AbsorbMul => absorb_mul(expr),
            Rule::AbsorbAnd => absorb_and(expr),
            Rule::AbsorbOr => absorb_or(expr),

            Rule::SelfInverseSub => self_inverse_sub(expr),
            Rule::SelfInverseXor => self_inverse_xor(expr),
            Rule::SelfInverseDiv => self_inverse_div(expr),

            Rule::IdempotentAnd => idempotent_and(expr),
            Rule::IdempotentOr => idempotent_or(expr),

            Rule::DoubleNeg => double_neg(expr),
            Rule::DoubleNot => double_not(expr),
            Rule::AddNegSub => add_neg_sub(expr),
            Rule::NegZeroSub => neg_zero_sub(expr),

            Rule::FlipComparison => flip_comparison(expr),
            Rule::NegateComparison => negate_comparison(expr),
            Rule::ExpandComparison => expand_comparison(expr),

            Rule::DeMorgan => de_morgan(expr),
            Rule::ComplementXor => complement_xor(expr),
            Rule::XorToAndOr => xor_to_and_or(expr),

            Rule::ModOne => mod_one(expr),
            Rule::AndToMod => and_to_mod(expr),

            Rule::ShiftZero => shift_zero(expr),

            Rule::InjectAddSub => inject_add_sub(expr),
            Rule::InjectSubAdd => inject_sub_add(expr),
            Rule::InjectMulDiv => inject_mul_div(expr),
            Rule::InjectXorXor => inject_xor_xor(expr),
            Rule::InjectDivDiv => inject_div_div(expr),
            Rule::InjectOrZero => inject_or_zero(expr),
            Rule::InjectAndSelf => inject_and_self(expr),

            Rule::DoubleMulTwo => double_mul_two(expr),
            Rule::MulNegOneNeg => mul_neg_one_neg(expr),
        }
    }
}

fn typed_zero(ty: &Type) -> String {
    match ty {
        Type::Bool => "false".to_string(),
        Type::Field => "0Field".to_string(),
        Type::Integer(int) => format!("0{}{}", if int.signed { "i" } else { "u" }, int.bits),
        _ => "0".to_string(),
    }
}

fn typed_one(ty: &Type) -> String {
    match ty {
        Type::Bool => "true".to_string(),
        Type::Field => "1Field".to_string(),
        Type::Integer(int) => format!("1{}{}", if int.signed { "i" } else { "u" }, int.bits),
        _ => "1".to_string(),
    }
}

fn typed_literal(value: &str, ty: &Type) -> String {
    match ty {
        Type::Bool => value.to_string(),
        Type::Field => format!("{value}Field"),
        Type::Integer(int) => {
            format!("{}{}{}", value, if int.signed { "i" } else { "u" }, int.bits)
        }
        _ => value.to_string(),
    }
}

// (a op b) <-> (b op a)
fn swap_operands(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op, ty, left, right } if op.is_commutative() => {
            Some(Expr::Binary { op: *op, ty: ty.clone(), left: right.clone(), right: left.clone() })
        }
        _ => None,
    }
}

// ((a op b) op c) <-> (a op (b op c))
fn associate(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: outer_op, ty, left, right } => match left.as_ref() {
            Expr::Binary { op: inner_op, left: a, right: b, .. }
                if outer_op == inner_op && outer_op.is_associative() =>
            {
                Some(Expr::Binary {
                    op: *outer_op,
                    ty: ty.clone(),
                    left: a.clone(),
                    right: Box::new(Expr::Binary {
                        op: *inner_op,
                        ty: ty.clone(),
                        left: b.clone(),
                        right: right.clone(),
                    }),
                })
            }
            _ => None,
        },
        _ => None,
    }
}

// ((a - b) - c) <-> (a - (b + c))
fn associate_sub(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Sub, ty, left, right: c } => match left.as_ref() {
            Expr::Binary { op: Operator::Sub, ty: inner_ty, left: a, right: b } => {
                Some(Expr::Binary {
                    op: Operator::Sub,
                    ty: ty.clone(),
                    left: a.clone(),
                    right: Box::new(Expr::Binary {
                        op: Operator::Add,
                        ty: inner_ty.clone(),
                        left: b.clone(),
                        right: c.clone(),
                    }),
                })
            }
            _ => None,
        },
        _ => None,
    }
}

// ((a / b) * c) <-> (a * (c / b))
fn associate_div(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Mul, ty, left, right: c } => match left.as_ref() {
            Expr::Binary { op: Operator::Div, ty: div_ty, left: a, right: b } => {
                Some(Expr::Binary {
                    op: Operator::Mul,
                    ty: ty.clone(),
                    left: a.clone(),
                    right: Box::new(Expr::Binary {
                        op: Operator::Div,
                        ty: div_ty.clone(),
                        left: c.clone(),
                        right: b.clone(),
                    }),
                })
            }
            _ => None,
        },
        _ => None,
    }
}

// (a / b) <-> ((1 / b) * a)
fn div_commute(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Div, ty, left: a, right: b } => Some(Expr::Binary {
            op: Operator::Mul,
            ty: ty.clone(),
            left: Box::new(Expr::Binary {
                op: Operator::Div,
                ty: ty.clone(),
                left: Box::new(Expr::Ref(typed_one(ty), ty.clone())),
                right: b.clone(),
            }),
            right: a.clone(),
        }),
        _ => None,
    }
}

// (a + b) * c <-> (a * c) + (b * c)
fn distribute_mul_add(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Mul, ty, left, right: c } => match left.as_ref() {
            Expr::Binary { op: Operator::Add, ty: add_ty, left: a, right: b } => {
                Some(Expr::Binary {
                    op: Operator::Add,
                    ty: add_ty.clone(),
                    left: Box::new(Expr::Binary {
                        op: Operator::Mul,
                        ty: ty.clone(),
                        left: a.clone(),
                        right: c.clone(),
                    }),
                    right: Box::new(Expr::Binary {
                        op: Operator::Mul,
                        ty: ty.clone(),
                        left: b.clone(),
                        right: c.clone(),
                    }),
                })
            }
            _ => None,
        },
        _ => None,
    }
}

// (a - b) * c <-> (a * c) - (b * c)
fn distribute_mul_sub(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Mul, ty, left, right: c } => match left.as_ref() {
            Expr::Binary { op: Operator::Sub, ty: sub_ty, left: a, right: b } => {
                Some(Expr::Binary {
                    op: Operator::Sub,
                    ty: sub_ty.clone(),
                    left: Box::new(Expr::Binary {
                        op: Operator::Mul,
                        ty: ty.clone(),
                        left: a.clone(),
                        right: c.clone(),
                    }),
                    right: Box::new(Expr::Binary {
                        op: Operator::Mul,
                        ty: ty.clone(),
                        left: b.clone(),
                        right: c.clone(),
                    }),
                })
            }
            _ => None,
        },
        _ => None,
    }
}

// (a | b) & c <-> (a & c) | (b & c)
fn distribute_and_or(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::And, ty, left, right: c } => match left.as_ref() {
            Expr::Binary { op: Operator::Or, ty: or_ty, left: a, right: b } => Some(Expr::Binary {
                op: Operator::Or,
                ty: or_ty.clone(),
                left: Box::new(Expr::Binary {
                    op: Operator::And,
                    ty: ty.clone(),
                    left: a.clone(),
                    right: c.clone(),
                }),
                right: Box::new(Expr::Binary {
                    op: Operator::And,
                    ty: ty.clone(),
                    left: b.clone(),
                    right: c.clone(),
                }),
            }),
            _ => None,
        },
        _ => None,
    }
}

// (a & b) | c <-> (a | c) & (b | c)
fn distribute_or_and(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Or, ty, left, right: c } => match left.as_ref() {
            Expr::Binary { op: Operator::And, ty: and_ty, left: a, right: b } => {
                Some(Expr::Binary {
                    op: Operator::And,
                    ty: and_ty.clone(),
                    left: Box::new(Expr::Binary {
                        op: Operator::Or,
                        ty: ty.clone(),
                        left: a.clone(),
                        right: c.clone(),
                    }),
                    right: Box::new(Expr::Binary {
                        op: Operator::Or,
                        ty: ty.clone(),
                        left: b.clone(),
                        right: c.clone(),
                    }),
                })
            }
            _ => None,
        },
        _ => None,
    }
}

// (a + 0) <-> a
fn identity_add(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Add, left, right, .. } => {
            if is_zero(right) {
                Some(*left.clone())
            } else if is_zero(left) {
                Some(*right.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

// (a - 0) <-> a
fn identity_sub(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Sub, left, right, .. } if is_zero(right) => {
            Some(*left.clone())
        }
        _ => None,
    }
}

// (a * 1) <-> a
fn identity_mul(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Mul, left, right, .. } => {
            if is_one(right) {
                Some(*left.clone())
            } else if is_one(left) {
                Some(*right.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

// (a / 1) <-> a
fn identity_div(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Div, left, right, .. } if is_one(right) => Some(*left.clone()),
        _ => None,
    }
}

// (a ^ 0) <-> a
fn identity_xor(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Xor, left, right, .. } => {
            let left_bool = left.ty().is_bool();
            let right_bool = right.ty().is_bool();

            if (right_bool && is_false(right)) || (!left_bool && is_zero(right)) {
                Some(*left.clone())
            } else if (left_bool && is_false(left)) || (!right_bool && is_zero(left)) {
                Some(*right.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

// (a | 0) <-> a
fn identity_or(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Or, left, right, .. } => {
            let left_bool = left.ty().is_bool();
            let right_bool = right.ty().is_bool();

            if (right_bool && is_false(right)) || (!left_bool && is_zero(right)) {
                Some(*left.clone())
            } else if (left_bool && is_false(left)) || (!right_bool && is_zero(left)) {
                Some(*right.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

// (a & 1) <-> a (only valid for booleans)
fn identity_and(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::And, left, right, .. } => {
            let left_bool = left.ty().is_bool();
            let right_bool = right.ty().is_bool();

            if (right_bool && is_true(right)) || (!left_bool && is_one(right)) {
                Some(*left.clone())
            } else if (left_bool && is_true(left)) || (!right_bool && is_one(left)) {
                Some(*right.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

// (a << 0) <-> a
fn identity_shl(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Shl | Operator::Shr, left, right, .. } if is_zero(right) => {
            Some(*left.clone())
        }
        _ => None,
    }
}

// (a >> 0) <-> a
fn identity_shr(expr: &Expr) -> Option<Expr> {
    identity_shl(expr)
}

// (a * 0) <-> 0
fn absorb_mul(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Mul, ty, left, right } if is_zero(right) || is_zero(left) => {
            Some(Expr::Ref(typed_zero(ty), ty.clone()))
        }
        _ => None,
    }
}

// (a & 0) <-> 0
fn absorb_and(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::And, ty, left, right } => {
            let left_bool = left.ty().is_bool();
            let right_bool = right.ty().is_bool();

            if (right_bool && is_false(right)) || (left_bool && is_false(left)) {
                Some(Expr::Ref("false".to_string(), ty.clone()))
            } else if (!left_bool && is_zero(right)) || (!right_bool && is_zero(left)) {
                Some(Expr::Ref(typed_zero(ty), ty.clone()))
            } else {
                None
            }
        }
        _ => None,
    }
}

// (a | true) <-> true (only valid for booleans)
fn absorb_or(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Or, ty, left, right }
            if (left.ty().is_bool() && is_true(right)) ||
                (right.ty().is_bool() && is_true(left)) =>
        {
            Some(Expr::Ref("true".to_string(), ty.clone()))
        }
        _ => None,
    }
}

// (a - a) <-> 0
fn self_inverse_sub(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Sub, ty, left, right } if same_expr(left, right) => {
            Some(Expr::Ref(typed_zero(ty), ty.clone()))
        }
        _ => None,
    }
}

// (a ^ a) <-> 0
fn self_inverse_xor(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Xor, ty, left, right } if same_expr(left, right) => {
            Some(Expr::Ref(typed_zero(ty), ty.clone()))
        }
        _ => None,
    }
}

// (a / a) <-> 1 (only for non-zero a)
fn self_inverse_div(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Div, ty, left, right } if same_expr(left, right) => {
            Some(Expr::Ref(typed_one(ty), ty.clone()))
        }
        _ => None,
    }
}

// (a & a) <-> a
fn idempotent_and(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::And | Operator::Or, left, right, .. }
            if same_expr(left, right) =>
        {
            Some(*left.clone())
        }
        _ => None,
    }
}

// (a | a) <-> a
fn idempotent_or(expr: &Expr) -> Option<Expr> {
    idempotent_and(expr)
}

// --a <-> a
fn double_neg(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Unary { op: Operator::Neg, operand, .. } => match operand.as_ref() {
            Expr::Unary { op: Operator::Neg, operand: inner, .. } => Some(*inner.clone()),
            _ => None,
        },
        _ => None,
    }
}

// !!a <-> a
fn double_not(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Unary { op: Operator::Not, operand, .. } => match operand.as_ref() {
            Expr::Unary { op: Operator::Not, operand: inner, .. } => Some(*inner.clone()),
            _ => None,
        },
        _ => None,
    }
}

// (a - b) <-> (a + (-b))
fn add_neg_sub(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Sub, ty, left: a, right: b } => {
            // Only apply if the type supports negation (signed integers or Field)
            if ty.is_signed() || ty.is_field() {
                Some(Expr::Binary {
                    op: Operator::Add,
                    ty: ty.clone(),
                    left: a.clone(),
                    right: Box::new(Expr::Unary {
                        op: Operator::Neg,
                        ty: b.ty().clone(),
                        operand: b.clone(),
                    }),
                })
            } else {
                None
            }
        }
        // Reverse: (a + (-b)) -> (a - b)
        Expr::Binary { op: Operator::Add, ty, left: a, right } => {
            if let Expr::Unary { op: Operator::Neg, operand: b_inner, .. } = right.as_ref() {
                Some(Expr::Binary {
                    op: Operator::Sub,
                    ty: ty.clone(),
                    left: a.clone(),
                    right: b_inner.clone(),
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

// (-a) <-> (0 - a)
fn neg_zero_sub(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Unary { op: Operator::Neg, ty, operand: a } => Some(Expr::Binary {
            op: Operator::Sub,
            ty: ty.clone(),
            left: Box::new(Expr::Ref(typed_zero(ty), ty.clone())),
            right: a.clone(),
        }),
        _ => None,
    }
}

// (a < b) <-> (b > a)
fn flip_comparison(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op, ty, left, right } => {
            let flipped_op = match op {
                Operator::Less => Operator::Greater,
                Operator::Greater => Operator::Less,
                Operator::LessOrEqual => Operator::GreaterOrEqual,
                Operator::GreaterOrEqual => Operator::LessOrEqual,
                Operator::Equal | Operator::NotEqual => *op,
                _ => return None,
            };
            Some(Expr::Binary {
                op: flipped_op,
                ty: ty.clone(),
                left: right.clone(),
                right: left.clone(),
            })
        }
        _ => None,
    }
}

// (a < b) <-> !(a >= b)
fn negate_comparison(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op, left, right, .. } => {
            let negated_op = match op {
                Operator::Less => Operator::GreaterOrEqual,
                Operator::GreaterOrEqual => Operator::Less,
                Operator::Greater => Operator::LessOrEqual,
                Operator::LessOrEqual => Operator::Greater,
                Operator::Equal => Operator::NotEqual,
                Operator::NotEqual => Operator::Equal,
                _ => return None,
            };
            Some(Expr::Unary {
                op: Operator::Not,
                ty: Type::Bool,
                operand: Box::new(Expr::Binary {
                    op: negated_op,
                    ty: Type::Bool,
                    left: left.clone(),
                    right: right.clone(),
                }),
            })
        }
        _ => None,
    }
}

// (a <= b) <-> ((a < b) || (a == b))
fn expand_comparison(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::LessOrEqual, left: a, right: b, .. } => Some(Expr::Binary {
            op: Operator::Or,
            ty: Type::Bool,
            left: Box::new(Expr::Binary {
                op: Operator::Less,
                ty: Type::Bool,
                left: a.clone(),
                right: b.clone(),
            }),
            right: Box::new(Expr::Binary {
                op: Operator::Equal,
                ty: Type::Bool,
                left: a.clone(),
                right: b.clone(),
            }),
        }),
        Expr::Binary { op: Operator::GreaterOrEqual, left: a, right: b, .. } => {
            Some(Expr::Binary {
                op: Operator::Or,
                ty: Type::Bool,
                left: Box::new(Expr::Binary {
                    op: Operator::Greater,
                    ty: Type::Bool,
                    left: a.clone(),
                    right: b.clone(),
                }),
                right: Box::new(Expr::Binary {
                    op: Operator::Equal,
                    ty: Type::Bool,
                    left: a.clone(),
                    right: b.clone(),
                }),
            })
        }
        _ => None,
    }
}

// !(a && b) <-> (!a || !b), !(a || b) <-> (!a && !b)
fn de_morgan(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Unary { op: Operator::Not, ty, operand } => match operand.as_ref() {
            Expr::Binary { op: op @ (Operator::And | Operator::Or), left: a, right: b, .. }
                if a.ty().is_bool() && b.ty().is_bool() =>
            {
                let new_op = if *op == Operator::And { Operator::Or } else { Operator::And };
                Some(Expr::Binary {
                    op: new_op,
                    ty: ty.clone(),
                    left: Box::new(Expr::Unary {
                        op: Operator::Not,
                        ty: ty.clone(),
                        operand: a.clone(),
                    }),
                    right: Box::new(Expr::Unary {
                        op: Operator::Not,
                        ty: ty.clone(),
                        operand: b.clone(),
                    }),
                })
            }
            _ => None,
        },
        _ => None,
    }
}

// !a <-> (a ^ true) (only for booleans)
fn complement_xor(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Unary { op: Operator::Not, ty, operand: a } if a.ty().is_bool() => {
            Some(Expr::Binary {
                op: Operator::Xor,
                ty: ty.clone(),
                left: a.clone(),
                right: Box::new(Expr::Ref("true".to_string(), ty.clone())),
            })
        }
        _ => None,
    }
}

// (a ^ b) <-> ((!a & b) | (a & !b))
fn xor_to_and_or(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Xor, ty, left: a, right: b }
            if a.ty().is_bool() && b.ty().is_bool() =>
        {
            Some(Expr::Binary {
                op: Operator::Or,
                ty: ty.clone(),
                left: Box::new(Expr::Binary {
                    op: Operator::And,
                    ty: ty.clone(),
                    left: Box::new(Expr::Unary {
                        op: Operator::Not,
                        ty: ty.clone(),
                        operand: a.clone(),
                    }),
                    right: b.clone(),
                }),
                right: Box::new(Expr::Binary {
                    op: Operator::And,
                    ty: ty.clone(),
                    left: a.clone(),
                    right: Box::new(Expr::Unary {
                        op: Operator::Not,
                        ty: ty.clone(),
                        operand: b.clone(),
                    }),
                }),
            })
        }
        _ => None,
    }
}

// (a % 1) <-> 0
fn mod_one(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Mod, ty, right, .. } if is_one(right) => {
            Some(Expr::Ref(typed_zero(ty), ty.clone()))
        }
        _ => None,
    }
}

// (a & 1) <-> (a % 2)
fn and_to_mod(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::And, ty, left: a, right }
            if is_one(right) && can_hold_two(ty) =>
        {
            Some(Expr::Binary {
                op: Operator::Mod,
                ty: ty.clone(),
                left: a.clone(),
                right: Box::new(Expr::Ref(typed_literal("2", ty), ty.clone())),
            })
        }
        _ => None,
    }
}

// (a << 0) <-> a, (a >> 0) <-> a
fn shift_zero(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Shl | Operator::Shr, left, right, .. } if is_zero(right) => {
            Some(*left.clone())
        }
        _ => None,
    }
}

// a <-> ((a + r) - r) or a <-> ((a - r) + r) (only for fields)
fn inject_add_sub(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Ref(_, ty) if ty.is_field() => Some(Expr::Binary {
            op: Operator::Sub,
            ty: ty.clone(),
            left: Box::new(Expr::Binary {
                op: Operator::Add,
                ty: ty.clone(),
                left: Box::new(expr.clone()),
                right: Box::new(Expr::Ref(typed_one(ty), ty.clone())),
            }),
            right: Box::new(Expr::Ref(typed_one(ty), ty.clone())),
        }),
        _ => None,
    }
}

// a <-> ((a - r) + r) (only for fields)
fn inject_sub_add(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Ref(_, ty) if ty.is_field() => Some(Expr::Binary {
            op: Operator::Add,
            ty: ty.clone(),
            left: Box::new(Expr::Binary {
                op: Operator::Sub,
                ty: ty.clone(),
                left: Box::new(expr.clone()),
                right: Box::new(Expr::Ref(typed_one(ty), ty.clone())),
            }),
            right: Box::new(Expr::Ref(typed_one(ty), ty.clone())),
        }),
        _ => None,
    }
}

// a <-> ((a * r) / r) (only for fields)
fn inject_mul_div(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Ref(_, ty) if ty.is_field() => Some(Expr::Binary {
            op: Operator::Div,
            ty: ty.clone(),
            left: Box::new(Expr::Binary {
                op: Operator::Mul,
                ty: ty.clone(),
                left: Box::new(expr.clone()),
                right: Box::new(Expr::Ref(typed_literal("2", ty), ty.clone())),
            }),
            right: Box::new(Expr::Ref(typed_literal("2", ty), ty.clone())),
        }),
        _ => None,
    }
}

// a <-> ((a ^ r) ^ r)
fn inject_xor_xor(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Ref(_, ty) if ty.is_integer() || ty.is_bool() => {
            let literal = typed_one(ty);
            Some(Expr::Binary {
                op: Operator::Xor,
                ty: ty.clone(),
                left: Box::new(Expr::Binary {
                    op: Operator::Xor,
                    ty: ty.clone(),
                    left: Box::new(expr.clone()),
                    right: Box::new(Expr::Ref(literal.clone(), ty.clone())),
                }),
                right: Box::new(Expr::Ref(literal, ty.clone())),
            })
        }
        _ => None,
    }
}

// 1 <-> (r / r)
fn inject_div_div(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Ref(s, ty) if s.starts_with('1') && ty.is_numeric() && can_hold_two(ty) => {
            Some(Expr::Binary {
                op: Operator::Div,
                ty: ty.clone(),
                left: Box::new(Expr::Ref(typed_literal("2", ty), ty.clone())),
                right: Box::new(Expr::Ref(typed_literal("2", ty), ty.clone())),
            })
        }
        _ => None,
    }
}

// a <-> (a | 0)
fn inject_or_zero(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Ref(_, ty) if ty.is_integer() => Some(Expr::Binary {
            op: Operator::Or,
            ty: ty.clone(),
            left: Box::new(expr.clone()),
            right: Box::new(Expr::Ref(typed_zero(ty), ty.clone())),
        }),
        _ => None,
    }
}

// a <-> (a & a)
fn inject_and_self(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Ref(_, ty) if ty.is_integer() || ty.is_bool() => Some(Expr::Binary {
            op: Operator::And,
            ty: ty.clone(),
            left: Box::new(expr.clone()),
            right: Box::new(expr.clone()),
        }),
        _ => None,
    }
}

// (a + a) <-> (a * 2)
fn double_mul_two(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Add, ty, left, right }
            if same_expr(left, right) && can_hold_two(ty) =>
        {
            Some(Expr::Binary {
                op: Operator::Mul,
                ty: ty.clone(),
                left: left.clone(),
                right: Box::new(Expr::Ref(typed_literal("2", ty), ty.clone())),
            })
        }
        _ => None,
    }
}

// (a * -1) <-> (-a) (only for field and signed integers)
fn mul_neg_one_neg(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Binary { op: Operator::Mul, ty, left, right } => {
            let operand = if is_neg_one(right) {
                Some(left)
            } else if is_neg_one(left) {
                Some(right)
            } else {
                None
            };
            operand.map(|a| Expr::Unary { op: Operator::Neg, ty: ty.clone(), operand: a.clone() })
        }
        _ => None,
    }
}

fn is_zero(expr: &Expr) -> bool {
    matches!(expr, Expr::Ref(s, _) if s == "0" || s.starts_with('0') && (s.ends_with("Field") || s.contains('i') || s.contains('u')))
}

fn is_one(expr: &Expr) -> bool {
    matches!(expr, Expr::Ref(s, _) if s == "1" || s.starts_with('1') && (s.ends_with("Field") || s.contains('i') || s.contains('u')))
}

fn is_neg_one(expr: &Expr) -> bool {
    matches!(expr, Expr::Unary { op: Operator::Neg, operand, .. } if matches!(operand.as_ref(), Expr::Ref(s, _) if s == "1" || s.starts_with('1')))
}

fn is_true(expr: &Expr) -> bool {
    matches!(expr, Expr::Ref(s, _) if s == "true")
}

fn is_false(expr: &Expr) -> bool {
    matches!(expr, Expr::Ref(s, _) if s == "false")
}

fn same_expr(a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Ref(a, _), Expr::Ref(b, _)) => a == b,
        (
            Expr::Binary { op: op1, left: l1, right: r1, .. },
            Expr::Binary { op: op2, left: l2, right: r2, .. },
        ) => op1 == op2 && same_expr(l1, l2) && same_expr(r1, r2),
        (Expr::Unary { op: op1, operand: o1, .. }, Expr::Unary { op: op2, operand: o2, .. }) => {
            op1 == op2 && same_expr(o1, o2)
        }
        _ => false,
    }
}

const fn can_hold_two(ty: &Type) -> bool {
    match ty {
        Type::Field => true,
        Type::Integer(int) => {
            if int.signed {
                int.bits > 2
            } else {
                int.bits > 1
            }
        }
        _ => false,
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Tests
// ────────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::circuits::{context::Context, ir::Forest};
    use rand::Rng;

    const RED: &str = "\x1b[31m";
    const GREEN: &str = "\x1b[32m";
    const RESET: &str = "\x1b[0m";

    fn common_prefix_len(a: &str, b: &str) -> usize {
        a.chars().zip(b.chars()).take_while(|(ca, cb)| ca == cb).count()
    }

    fn common_suffix_len(a: &str, b: &str) -> usize {
        a.chars().rev().zip(b.chars().rev()).take_while(|(ca, cb)| ca == cb).count()
    }

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
        let n = before_lines.len();
        let m = after_lines.len();

        let mut lcs = vec![vec![0; m + 1]; n + 1];
        for i in 1..=n {
            for j in 1..=m {
                if before_lines[i - 1] == after_lines[j - 1] {
                    lcs[i][j] = lcs[i - 1][j - 1] + 1;
                } else {
                    lcs[i][j] = lcs[i - 1][j].max(lcs[i][j - 1]);
                }
            }
        }

        let mut result = Vec::new();
        let (mut i, mut j) = (n, m);

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
                    let mut deletes = vec![before_idx];
                    let mut j = i + 1;
                    while j < diff_ops.len() && diff_ops[j].0 == DiffOp::Delete {
                        deletes.push(diff_ops[j].1);
                        j += 1;
                    }

                    let mut inserts = vec![];
                    while j < diff_ops.len() && diff_ops[j].0 == DiffOp::Insert {
                        inserts.push(diff_ops[j].2);
                        j += 1;
                    }

                    let num_pairs = deletes.len().min(inserts.len());
                    for k in 0..num_pairs {
                        let before_line = before_lines[deletes[k]];
                        let after_line = after_lines[inserts[k]];
                        println!("{RED}-{RESET} {}", highlight_diff(before_line, after_line, RED));
                        println!(
                            "{GREEN}+{RESET} {}",
                            highlight_diff(after_line, before_line, GREEN)
                        );
                    }

                    for k in num_pairs..deletes.len() {
                        println!(
                            "{RED}-{RESET} {}",
                            highlight_diff(before_lines[deletes[k]], "", RED)
                        );
                    }

                    for k in num_pairs..inserts.len() {
                        println!(
                            "{GREEN}+{RESET} {}",
                            highlight_diff(after_lines[inserts[k]], "", GREEN)
                        );
                    }

                    i = j;
                }
                DiffOp::Insert => {
                    println!(
                        "{GREEN}+{RESET} {}",
                        highlight_diff(after_lines[after_idx], "", GREEN)
                    );
                    i += 1;
                }
            }
        }
    }

    #[test]
    fn test_random_rewrites() {
        let ctx: Context =
            serde_json::from_str(&std::fs::read_to_string("../configs/noiruzz.json").unwrap())
                .unwrap();
        let mut random = rand::rng();

        let mut forest = Forest::default();
        forest.random(&mut random, &ctx, &[]);

        let mut ast = AST::from(&forest);
        let before = ast.to_string();

        let num_rules = random.random_range(5..15);
        println!("\n{GREEN}Applying {} random rules...{RESET}", num_rules);

        for i in 0..num_rules {
            if let Some(rule) = apply_random_rule(&mut random, &mut ast) {
                println!("  {}. Applied {:?}", i + 1, rule);
            } else {
                println!("  {}. No applicable rules found", i + 1);
            }
        }

        let after = ast.to_string();
        println!();
        print_diff(&before, &after);
    }
}
