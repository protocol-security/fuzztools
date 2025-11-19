use crate::base::{BaseCircuitGenerator, ExpressionKind};
use crate::config::IRConfig;
use crate::nodes::*;
use rand::Rng;

#[allow(dead_code)]
/// The Boolean circuit generator treats all input and output variables
/// as boolean variables. Even if arithmetic operations are available,
/// this generator will only produce boolean expressions.
///
/// Furthermore, relations are only supported for boolean expressions.
///
/// All assertions are ordered by occurrence, i.e. the first assertion
/// has id "assertion-0", the second "assertion-1", and so on.
pub struct BooleanCircuitGenerator<R: Rng> {
    base: BaseCircuitGenerator<R>,
    assertion_ordering: u32,
    assertion_budget: u32,
    unassigned_variables: Vec<String>,
}

#[allow(dead_code)]
impl<R: Rng> BooleanCircuitGenerator<R> {
    pub fn new(curve_prime: String, config: IRConfig, rng: R, exclude_prime: bool) -> Self {
        let base = BaseCircuitGenerator::new(curve_prime, config, rng, exclude_prime);
        
        Self {
            base,
            assertion_ordering: 0,
            assertion_budget: 0,
            unassigned_variables: Vec::new(),
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

        self.base.boolean_variables = input_variables.clone();
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

    fn random_boolean_logic_relation_expression(&mut self, depth: u32) -> Expression {
        let op = self.base.random_relation();
        let lhs = self.random_boolean_expression(depth + 1);
        let rhs = self.random_boolean_expression(depth + 1);
        
        Expression::BinaryExpression(BinaryExpression {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn random_boolean_logic_ternary_expression(&mut self, depth: u32) -> Expression {
        let cond = self.random_boolean_expression(depth + 1);
        let if_expr = self.random_boolean_expression(depth + 1);
        let else_expr = self.random_boolean_expression(depth + 1);
        
        Expression::CallExpression(CallExpression {
            reference: Box::new(Expression::Identifier(Identifier {
                name: "if".to_string(),
            })),
            arguments: vec![
                Box::new(cond),
                Box::new(if_expr),
                Box::new(else_expr),
            ],
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
            ExpressionKind::Variable => {
                let name = self.base.random_boolean_variable();
                Expression::Identifier(Identifier { name })
            }
            ExpressionKind::Unary => self.random_boolean_logic_unary_expression(depth),
            ExpressionKind::Binary => self.random_boolean_logic_binary_expression(depth),
            ExpressionKind::Relation => self.random_boolean_logic_relation_expression(depth),
            ExpressionKind::Ternary => self.random_boolean_logic_ternary_expression(depth),
        }
    }

    fn random_assignment(&mut self) -> Statement {
        assert!(
            !self.unassigned_variables.is_empty(),
            "no unassigned variables left to create an assignment"
        );
        
        let name = self.unassigned_variables.remove(0);
        let expr = self.random_boolean_expression(0);
        
        self.base.boolean_variables.push(name.clone());
        
        Statement::AssignStatement(AssignStatement {
            lhs: Box::new(Expression::Identifier(Identifier { name })),
            rhs: Box::new(expr),
        })
    }

    fn random_assertion(&mut self) -> Statement {
        assert!(self.assertion_budget > 0, "no budget left to create an assertion");
        
        self.assertion_budget -= 1;
        let condition = self.random_boolean_expression(0);
        let message = format!("assertion-{}", self.assertion_ordering);
        self.assertion_ordering += 1;
        
        Statement::AssertStatement(AssertStatement {
            condition: Box::new(condition),
            message: Some(StringLiteral { value: message }),
        })
    }

    fn random_statements(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        // Generate assignments for all output variables
        let num_assignments = self.unassigned_variables.len();
        for _ in 0..num_assignments {
            statements.push(self.random_assignment());
        }

        // Generate assertions
        let num_assertions = self.assertion_budget;
        for _ in 0..num_assertions {
            statements.push(self.random_assertion());
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

// IR nodes for the circuit (simplified versions)
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Circuit {
    pub name: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub statements: Vec<Statement>,
}

