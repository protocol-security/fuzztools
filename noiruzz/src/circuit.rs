use crate::{
    config::Config,
    nodes::*,
    rewriter::{RewriteRule, RuleBasedRewriter},
    types::{ExpressionKind, MetamorphicKind, Operator},
    utils::random_id,
};
use fuzztools::{
    math::{random_field_element, weighted_select},
    utils::RandomChoice,
};
use rand::Rng;
use std::collections::HashMap;

#[derive(Debug, Clone)]
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

    pub fn apply_metamorphic_mutation(
        &mut self,
        random: &mut impl Rng,
        config: &Config,
        metamorphic_kind: MetamorphicKind,
        _num_rewrites: Option<usize>,
    ) -> Result<(), String> {
        // Load the appropriate rewrite rules based on metamorphic kind
        let rule_configs: Vec<_> = match metamorphic_kind {
            MetamorphicKind::Equal => config.rewrite.rules.equivalence.clone(),
            MetamorphicKind::Weaker => {
                let mut configs = config.rewrite.rules.equivalence.clone();
                configs.extend(config.rewrite.rules.weakening.iter().cloned());
                configs
            },
        };

        // Parse all rules
        let rules: Vec<_> = rule_configs
            .iter()
            .filter_map(|rule_config| {
                RewriteRule::from_config(rule_config)
                    .map_err(|e| {
                        eprintln!("Warning: Failed to parse rule '{}': {}", rule_config.name, e);
                        e
                    })
                    .ok()
            })
            .collect();

        if rules.is_empty() {
            return Err("No valid rewrite rules available".to_string());
        }

        let mut rewriter = RuleBasedRewriter::new(config.clone(), rules);
        Self::mutate_statements(&mut self.statements, 0, config, random, &mut rewriter)?;

        Ok(())
    }

    fn mutate_statements(
        statements: &mut [Statement],
        depth: u64,
        config: &Config,
        random: &mut impl Rng,
        rewriter: &mut RuleBasedRewriter,
    ) -> Result<(), String> {
        if depth >= config.circuit.max_expression_depth {
            return Ok(());
        }

        for statement in statements {
            match statement {
                Statement::BasicBlock(basic_block) => {
                    Self::mutate_boxed_statements(
                        &mut basic_block.statements,
                        depth + 1,
                        config,
                        random,
                        rewriter,
                    )?;
                },
                Statement::IfStatement(if_stmt) => {
                    Self::mutate_boxed_statements(
                        std::slice::from_mut(&mut if_stmt.true_stmt),
                        depth + 1,
                        config,
                        random,
                        rewriter,
                    )?;
                    if let Some(false_stmt) = &mut if_stmt.false_stmt {
                        Self::mutate_boxed_statements(
                            std::slice::from_mut(false_stmt),
                            depth + 1,
                            config,
                            random,
                            rewriter,
                        )?;
                    }
                },
                Statement::ForStatement(for_stmt) => {
                    Self::mutate_boxed_statements(
                        &mut for_stmt.statements,
                        depth + 1,
                        config,
                        random,
                        rewriter,
                    )?;
                },
                Statement::AssignStatement(assign_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*assign_stmt.rhs).clone(), None, random);
                    assign_stmt.rhs = Box::new(rewritten_expr);
                },
                Statement::AssertStatement(assert_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*assert_stmt.condition).clone(), None, random);
                    assert_stmt.condition = Box::new(rewritten_expr);
                },
                Statement::LetStatement(let_stmt) => {
                    if let Some(expr) = &mut let_stmt.expr {
                        let (_applied_rules, rewritten_expr) =
                            rewriter.run_on_expression((**expr).clone(), None, random);
                        *expr = Box::new(rewritten_expr);
                    }
                },
                Statement::ExpressionStatement(expr_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*expr_stmt.expr).clone(), None, random);
                    expr_stmt.expr = Box::new(rewritten_expr);
                },
                Statement::ReturnStatement(return_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*return_stmt.value).clone(), None, random);
                    return_stmt.value = Box::new(rewritten_expr);
                },
            }
        }

        Ok(())
    }

    fn mutate_boxed_statements(
        statements: &mut [Box<Statement>],
        depth: u64,
        config: &Config,
        random: &mut impl Rng,
        rewriter: &mut RuleBasedRewriter,
    ) -> Result<(), String> {
        if depth >= config.circuit.max_expression_depth {
            return Ok(());
        }

        for statement in statements {
            match statement.as_mut() {
                Statement::BasicBlock(basic_block) => {
                    Self::mutate_boxed_statements(
                        &mut basic_block.statements,
                        depth + 1,
                        config,
                        random,
                        rewriter,
                    )?;
                },
                Statement::IfStatement(if_stmt) => {
                    Self::mutate_boxed_statements(
                        std::slice::from_mut(&mut if_stmt.true_stmt),
                        depth + 1,
                        config,
                        random,
                        rewriter,
                    )?;
                    if let Some(false_stmt) = &mut if_stmt.false_stmt {
                        Self::mutate_boxed_statements(
                            std::slice::from_mut(false_stmt),
                            depth + 1,
                            config,
                            random,
                            rewriter,
                        )?;
                    }
                },
                Statement::ForStatement(for_stmt) => {
                    Self::mutate_boxed_statements(
                        &mut for_stmt.statements,
                        depth + 1,
                        config,
                        random,
                        rewriter,
                    )?;
                },
                Statement::AssignStatement(assign_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*assign_stmt.rhs).clone(), None, random);
                    assign_stmt.rhs = Box::new(rewritten_expr);
                },
                Statement::AssertStatement(assert_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*assert_stmt.condition).clone(), None, random);
                    assert_stmt.condition = Box::new(rewritten_expr);
                },
                Statement::LetStatement(let_stmt) => {
                    if let Some(expr) = &mut let_stmt.expr {
                        let (_applied_rules, rewritten_expr) =
                            rewriter.run_on_expression((**expr).clone(), None, random);
                        *expr = Box::new(rewritten_expr);
                    }
                },
                Statement::ExpressionStatement(expr_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*expr_stmt.expr).clone(), None, random);
                    expr_stmt.expr = Box::new(rewritten_expr);
                },
                Statement::ReturnStatement(return_stmt) => {
                    let (_applied_rules, rewritten_expr) =
                        rewriter.run_on_expression((*return_stmt.value).clone(), None, random);
                    return_stmt.value = Box::new(rewritten_expr);
                },
            }
        }

        Ok(())
    }

    pub fn mutate(
        &self,
        random: &mut impl Rng,
        config: &Config,
        metamorphic_kind: MetamorphicKind,
        _num_rewrites: Option<usize>,
    ) -> Result<Self, String> {
        let mut mutated = self.clone();
        mutated.apply_metamorphic_mutation(random, config, metamorphic_kind, _num_rewrites)?;
        Ok(mutated)
    }
}
