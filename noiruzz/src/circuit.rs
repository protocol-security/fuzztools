use crate::{
    config::Config,
    nodes::*,
    types::{ExpressionKind, Operator},
    utils::random_id,
};
use fuzztools::{
    math::{random_field_element, weighted_select},
    utils::RandomChoice,
};
use rand::Rng;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Circuit {
    pub name: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub statements: Vec<Statement>,
    pub arithmetic_variables: Vec<String>,
}

impl Circuit {
    pub fn new(random: &mut impl Rng, config: &Config) -> Self {
        let name = format!("circuit_{}", random_id(random, 10));

        let input_count = random.random_range(
            config.circuit.min_number_of_input_variables
                ..=config.circuit.max_number_of_input_variables,
        );
        let output_count = random.random_range(
            config.circuit.min_number_of_output_variables
                ..=config.circuit.max_number_of_output_variables,
        );

        let inputs: Vec<String> = (0..input_count).map(|i| format!("in{}", i)).collect();
        let outputs: Vec<String> = (0..output_count).map(|i| format!("out{}", i)).collect();

        let mut arithmetic_variables = inputs.clone();
        let mut statements = Vec::new();

        // Generate assignments for output variables
        for var_name in &outputs {
            let expression =
                Self::random_arithmetic_expression(random, config, &arithmetic_variables, 0);
            statements.push(Statement::AssignStatement(AssignStatement {
                lhs: Box::new(Expression::Identifier(Identifier { name: var_name.clone() })),
                rhs: Box::new(expression),
            }));
            arithmetic_variables.push(var_name.clone());
        }

        // Generate assertions
        let assertion_count = random.random_range(
            config.circuit.min_number_of_assertions..=config.circuit.max_number_of_assertions,
        );
        for _ in 0..assertion_count {
            let condition =
                Self::random_boolean_expression(random, config, &arithmetic_variables, 0);
            statements.push(Statement::AssertStatement(AssertStatement {
                condition: Box::new(condition),
                message: None,
            }));
        }

        Self { name, inputs, outputs, statements, arithmetic_variables }
    }

    fn random_arithmetic_expression(
        random: &mut impl Rng,
        config: &Config,
        arithmetic_variables: &[String],
        depth: u64,
    ) -> Expression {
        if depth >= config.circuit.max_expression_depth {
            return Expression::IntegerLiteral(IntegerLiteral {
                value: random_field_element(
                    "bn254",
                    random,
                    false,
                    config.circuit.boundary_value_probability,
                    config.circuit.small_upper_bound_probability,
                    10,
                ),
            });
        }

        let kind = Self::random_expr_kind(random, config, ExpressionKind::ARITHMETIC_KINDS);

        match kind {
            ExpressionKind::Constant => Expression::IntegerLiteral(IntegerLiteral {
                value: random_field_element(
                    "bn254",
                    random,
                    false,
                    config.circuit.boundary_value_probability,
                    config.circuit.small_upper_bound_probability,
                    10,
                ),
            }),
            ExpressionKind::Variable => Expression::Identifier(Identifier {
                name: random.choice(arithmetic_variables).clone(),
            }),
            ExpressionKind::Unary => {
                let operator = Operator::from(random.choice(&config.operators.unary_operators));
                let value = Self::random_arithmetic_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                Expression::UnaryExpression(UnaryExpression { operator, value: Box::new(value) })
            },
            ExpressionKind::Binary => {
                let operator = Operator::from(random.choice(&config.operators.binary_operators));
                let lhs = Self::random_arithmetic_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                let rhs = Self::random_arithmetic_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                Expression::BinaryExpression(BinaryExpression {
                    operator,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            },
            ExpressionKind::Relation => {
                let operator = Operator::from(random.choice(&config.operators.unary_operators));
                let lhs = Self::random_arithmetic_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                let rhs = Self::random_arithmetic_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                Expression::BinaryExpression(BinaryExpression {
                    operator,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            },
        }
    }

    fn random_boolean_expression(
        random: &mut impl Rng,
        config: &Config,
        arithmetic_variables: &[String],
        depth: u64,
    ) -> Expression {
        if depth >= config.circuit.max_expression_depth {
            return Expression::BooleanLiteral(BooleanLiteral { value: random.random_bool(0.5) });
        }

        let kind = Self::random_expr_kind(random, config, ExpressionKind::BOOLEAN_KINDS);

        match kind {
            ExpressionKind::Constant => {
                Expression::BooleanLiteral(BooleanLiteral { value: random.random_bool(0.5) })
            },
            ExpressionKind::Relation => {
                let operator = Operator::from(random.choice(&config.operators.unary_operators));
                let lhs = Self::random_arithmetic_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                let rhs = Self::random_arithmetic_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                Expression::BinaryExpression(BinaryExpression {
                    operator,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            },
            ExpressionKind::Unary => {
                let operator = Operator::from(random.choice(&config.operators.unary_operators));
                let value = Self::random_boolean_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                Expression::UnaryExpression(UnaryExpression { operator, value: Box::new(value) })
            },
            ExpressionKind::Binary => {
                let operator = Operator::from(random.choice(&config.operators.unary_operators));
                let lhs = Self::random_boolean_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                let rhs = Self::random_boolean_expression(
                    random,
                    config,
                    arithmetic_variables,
                    depth + 1,
                );
                Expression::BinaryExpression(BinaryExpression {
                    operator,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            },
            ExpressionKind::Variable => unreachable!("Variable not allowed in boolean expressions"),
        }
    }

    fn random_expr_kind(
        random: &mut impl Rng,
        config: &Config,
        allowed_kinds: &[ExpressionKind],
    ) -> ExpressionKind {
        let weights: HashMap<_, _> =
            allowed_kinds.iter().map(|kind| (*kind, kind.weight(config))).collect();
        weighted_select(allowed_kinds, &weights, random).expect("Failed to select expression kind")
    }
}
