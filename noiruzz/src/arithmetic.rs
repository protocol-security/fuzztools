use crate::base::{BaseCircuitGenerator, ExpressionKind};
use crate::boolean::Circuit;
use crate::config::IRConfig;
use crate::nodes::*;
use crate::operators::Operator;
use rand::Rng;

#[allow(dead_code)]
/// The arithmetic circuit generator treats all input and output variables
/// as arithmetic variables. Besides that, all the different expression kinds
/// are implemented for boolean and arithmetic expressions.
///
/// Furthermore, relations are only supported for arithmetic expressions.
///
/// All assertions are ordered by occurrence, i.e. the first assertion
/// has id "assertion (id: 0)", the second "assertion (id: 1)", and so on.
pub struct ArithmeticCircuitGenerator<R: Rng> {
    base: BaseCircuitGenerator<R>,
    assertion_ordering: u32,
    assertion_budget: u32,
    unassigned_variables: Vec<String>,
    preconditions: Vec<Statement>,
}

#[allow(dead_code)]
impl<R: Rng> ArithmeticCircuitGenerator<R> {
    pub fn new(curve_prime: String, config: IRConfig, rng: R, exclude_prime: bool) -> Self {
        let base = BaseCircuitGenerator::new(curve_prime, config, rng, exclude_prime);
        
        Self {
            base,
            assertion_ordering: 0,
            assertion_budget: 0,
            unassigned_variables: Vec::new(),
            preconditions: Vec::new(),
        }
    }

    pub fn run(&mut self) -> Circuit {
        let input_variables_amount = self.base.rng.random_range(
            self.base.config.min_number_of_input_variables
                ..=self.base.config.max_number_of_input_variables,
        );
        let output_variables_amount = self.base.rng.random_range(
            self.base.config.min_number_of_output_variables
                ..=self.base.config.max_number_of_output_variables,
        );

        let input_variables: Vec<String> = (0..input_variables_amount)
            .map(|i| format!("in{}", i))
            .collect();
        let output_variables: Vec<String> = (0..output_variables_amount)
            .map(|i| format!("out{}", i))
            .collect();

        self.base.arithmetic_variables = input_variables.clone();
        self.assertion_budget = self.base.rng.random_range(
            self.base.config.min_number_of_assertions..=self.base.config.max_number_of_assertions,
        );
        self.assertion_ordering = 0;
        self.unassigned_variables = output_variables.clone();

        let statements = self.random_statements();

        let circuit_name = format!("circuit_{}", self.base.random_id(10));
        
        Circuit {
            name: circuit_name,
            inputs: input_variables,
            outputs: output_variables,
            statements,
        }
    }

    fn random_boolean_logic_unary_expression(&mut self, depth: u32) -> Expression {
        let op = self.base.random_boolean_unary_operation();
        let value = self.random_boolean_expression(depth + 1);
        
        Expression::UnaryExpression(UnaryExpression {
            operator: op,
            value: Box::new(value),
        })
    }

    fn random_boolean_logic_binary_expression(&mut self, depth: u32) -> Expression {
        let op = self.base.random_boolean_binary_operation();
        let lhs = self.random_boolean_expression(depth + 1);
        let rhs = self.random_boolean_expression(depth + 1);
        
        Expression::BinaryExpression(BinaryExpression {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn random_boolean_logic_ternary_expression(&mut self, depth: u32) -> Expression {
        let condition = self.random_boolean_expression(depth + 1);
        let true_val = self.random_boolean_expression(depth + 1);
        let false_val = self.random_boolean_expression(depth + 1);
        
        Expression::CallExpression(CallExpression {
            reference: Box::new(Expression::Identifier(Identifier {
                name: "if".to_string(),
            })),
            arguments: vec![
                Box::new(condition),
                Box::new(true_val),
                Box::new(false_val),
            ],
        })
    }

    fn random_relation_expression(&mut self, depth: u32) -> Expression {
        let op = self.base.random_relation();
        let lhs = self.random_arithmetic_expression(depth + 1);
        let rhs = self.random_arithmetic_expression(depth + 1);
        
        Expression::BinaryExpression(BinaryExpression {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn random_boolean_expression(&mut self, depth: u32) -> Expression {
        let kinds = self.base.allowed_boolean_expression_kinds(depth);
        let kind = self.base.random_expr_kind_with_weight(&kinds);
        
        match kind {
            ExpressionKind::Constant => {
                let value = self.base.random_boolean();
                Expression::BooleanLiteral(BooleanLiteral { value })
            }
            ExpressionKind::Relation => self.random_relation_expression(depth),
            ExpressionKind::Unary => self.random_boolean_logic_unary_expression(depth),
            ExpressionKind::Binary => self.random_boolean_logic_binary_expression(depth),
            ExpressionKind::Ternary => self.random_boolean_logic_ternary_expression(depth),
            ExpressionKind::Variable => {
                // Boolean variables are not supported in arithmetic mode,
                // fallback to constant
                let value = self.base.random_boolean();
                Expression::BooleanLiteral(BooleanLiteral { value })
            }
        }
    }

    fn random_arithmetic_unary_expression(&mut self, depth: u32) -> Expression {
        let op = self.base.random_arithmetic_unary_operation();
        let value = self.random_arithmetic_expression(depth + 1);
        
        Expression::UnaryExpression(UnaryExpression {
            operator: op,
            value: Box::new(value),
        })
    }

    fn random_arithmetic_binary_expression(&mut self, depth: u32) -> Expression {
        let op = self.base.random_arithmetic_binary_operation();

        // Special handling for remainder (%) and division (/),
        // right side is either non zero constant or expression with an assumption.
        if matches!(op, Operator::Rem | Operator::Div) {
            let divisor = if self.base.rng.random_bool(0.5) {
                let value = self.base.random_non_zero_number();
                Expression::IntegerLiteral(IntegerLiteral { value })
            } else {
                let divisor_expr = self.random_arithmetic_expression(depth + 1);
                
                // If divisor is an expression we need to be careful of div by zero -> precondition
                let precondition = Expression::BinaryExpression(BinaryExpression {
                    operator: Operator::Neq,
                    lhs: Box::new(divisor_expr.clone()),
                    rhs: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 0 })),
                });
                
                self.preconditions.push(Statement::AssertStatement(AssertStatement {
                    condition: Box::new(precondition),
                    message: Some(StringLiteral {
                        value: format!("division-by-zero (id: {})", self.assertion_ordering),
                    }),
                }));
                self.assertion_ordering += 1;
                
                divisor_expr
            };

            let lhs = self.random_arithmetic_expression(depth + 1);
            return Expression::BinaryExpression(BinaryExpression {
                operator: op,
                lhs: Box::new(lhs),
                rhs: Box::new(divisor),
            });
        }

        // Default operator
        let lhs = self.random_arithmetic_expression(depth + 1);
        let rhs = self.random_arithmetic_expression(depth + 1);
        
        Expression::BinaryExpression(BinaryExpression {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn random_arithmetic_ternary_expression(&mut self, depth: u32) -> Expression {
        let condition = self.random_boolean_expression(depth + 1);
        let true_val = self.random_arithmetic_expression(depth + 1);
        let false_val = self.random_arithmetic_expression(depth + 1);
        
        Expression::CallExpression(CallExpression {
            reference: Box::new(Expression::Identifier(Identifier {
                name: "if".to_string(),
            })),
            arguments: vec![
                Box::new(condition),
                Box::new(true_val),
                Box::new(false_val),
            ],
        })
    }

    fn random_arithmetic_expression(&mut self, depth: u32) -> Expression {
        let kinds = self.base.allowed_arithmetic_expression_kinds(depth);
        let kind = self.base.random_expr_kind_with_weight(&kinds);
        
        match kind {
            ExpressionKind::Constant => {
                let value = self.base.random_number();
                Expression::IntegerLiteral(IntegerLiteral { value })
            }
            ExpressionKind::Variable => {
                let name = self.base.random_arithmetic_variable();
                Expression::Identifier(Identifier { name })
            }
            ExpressionKind::Unary => self.random_arithmetic_unary_expression(depth),
            ExpressionKind::Binary => self.random_arithmetic_binary_expression(depth),
            ExpressionKind::Ternary => self.random_arithmetic_ternary_expression(depth),
            ExpressionKind::Relation => {
                // Relations are not arithmetic expressions, fallback to constant
                let value = self.base.random_number();
                Expression::IntegerLiteral(IntegerLiteral { value })
            }
        }
    }

    fn random_assignment(&mut self) -> Vec<Statement> {
        assert!(
            !self.unassigned_variables.is_empty(),
            "no unassigned variables left to create an assignment"
        );
        
        let name = self.unassigned_variables.remove(0);
        let expr = self.random_arithmetic_expression(0);
        
        self.base.arithmetic_variables.push(name.clone());
        
        let mut statements = std::mem::take(&mut self.preconditions);
        statements.push(Statement::AssignStatement(AssignStatement {
            lhs: Box::new(Expression::Identifier(Identifier { name })),
            rhs: Box::new(expr),
        }));
        
        statements
    }

    fn random_assertion(&mut self) -> Vec<Statement> {
        assert!(self.assertion_budget > 0, "no budget left to create an assertion");
        
        self.assertion_budget -= 1;
        
        // Visit this first to have correct assertion id ordering
        let condition = self.random_boolean_expression(0);
        let assertion_id = self.assertion_ordering;
        self.assertion_ordering += 1;
        
        let mut statements = std::mem::take(&mut self.preconditions);
        statements.push(Statement::AssertStatement(AssertStatement {
            condition: Box::new(condition),
            message: Some(StringLiteral {
                value: format!("assertion (id: {})", assertion_id),
            }),
        }));
        
        statements
    }

    fn random_statements(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        // Generate assignments for all output variables
        let num_assignments = self.unassigned_variables.len();
        for _ in 0..num_assignments {
            statements.extend(self.random_assignment());
        }

        // Generate assertions
        let num_assertions = self.assertion_budget;
        for _ in 0..num_assertions {
            statements.extend(self.random_assertion());
        }

        // Sanity check
        assert_eq!(
            self.assertion_budget, 0,
            "still available budget for assertions"
        );
        assert!(
            self.unassigned_variables.is_empty(),
            "still pending variable assignments"
        );

        statements
    }
}

#[test]
fn test_arithmetic_circuit_generator() {
    let mut rng = rand::thread_rng();
    let config = IRConfig::default();
    let mut generator = ArithmeticCircuitGenerator::new(String::from("bn128"), config, rng, false);
    let circuit = generator.run();
    println!("{:#?}", circuit);
}