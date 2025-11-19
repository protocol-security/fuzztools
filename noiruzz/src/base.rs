use crate::config::{IRConfig, IROpsConfig, IRGenConfig};
use crate::operators::Operator;
use crate::utils::random_id;
use alloy::primitives::U256;
use fuzztools::math::{random_field_element, random_non_zero_field_element, weighted_select};
use rand::Rng;
use std::collections::HashMap;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpressionKind {
    Constant,
    Variable,
    Unary,
    Binary,
    Relation,
    Ternary,
}

impl std::fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionKind::Constant => write!(f, "constant"),
            ExpressionKind::Variable => write!(f, "variable"),
            ExpressionKind::Unary => write!(f, "unary"),
            ExpressionKind::Binary => write!(f, "binary"),
            ExpressionKind::Relation => write!(f, "relation"),
            ExpressionKind::Ternary => write!(f, "ternary"),
        }
    }
}

#[allow(dead_code)]
pub struct BaseCircuitGenerator<R: Rng> {
    // Randomness
    pub rng: R,

    // Generation specific settings
    pub config: IRGenConfig,
    pub operators: IROpsConfig,

    // Curve information
    pub curve_prime: String,
    pub exclude_prime: bool,

    // Available Variables
    pub boolean_variables: Vec<String>,
    pub arithmetic_variables: Vec<String>,
}

#[allow(dead_code)]
impl<R: Rng> BaseCircuitGenerator<R> {
    pub fn new(curve_prime: String, config: IRConfig, rng: R, exclude_prime: bool) -> Self {
        // Validation of configurations
        assert!(
            config.generation.max_number_of_assertions >= config.generation.min_number_of_assertions,
            "number of maximal and minimal assertions are inconsistent"
        );
        assert!(
            config.generation.max_number_of_input_variables >= config.generation.min_number_of_input_variables,
            "number of maximal and minimal input variables are inconsistent"
        );
        assert!(
            config.generation.max_number_of_output_variables >= config.generation.min_number_of_output_variables,
            "number of maximal and minimal output variables are inconsistent"
        );
        // Note: u32 types are always >= 0, so these checks are redundant but kept for clarity
        assert!(
            (config.generation.constant_probability_weight + config.generation.variable_probability_weight) > 0.0,
            "sum of weighted probabilities for leaf nodes must not be 0"
        );
        assert!(
            config.generation.max_exponent_value >= 2,
            "constant number for exponent cannot be smaller than 2"
        );

        Self {
            rng,
            config: config.generation,
            operators: config.operators,
            curve_prime,
            exclude_prime,
            boolean_variables: Vec::new(),
            arithmetic_variables: Vec::new(),
        }
    }

    pub fn random_id(&mut self, size: usize) -> String {
        random_id(&mut self.rng, size)
    }

    pub fn random_expr_kind_with_weight(&mut self, allowed_exprs: &[ExpressionKind]) -> ExpressionKind {
        let mut weight_lookup = HashMap::new();
        weight_lookup.insert(ExpressionKind::Constant, self.config.constant_probability_weight);
        weight_lookup.insert(ExpressionKind::Variable, self.config.variable_probability_weight);
        weight_lookup.insert(ExpressionKind::Unary, self.config.unary_probability_weight);
        weight_lookup.insert(ExpressionKind::Binary, self.config.binary_probability_weight);
        weight_lookup.insert(ExpressionKind::Relation, self.config.relation_probability_weight);
        weight_lookup.insert(ExpressionKind::Ternary, self.config.ternary_probability_weight);

        weighted_select(allowed_exprs, &weight_lookup, &mut self.rng)
            .expect("Failed to select expression kind")
    }

    pub fn random_number_for_pow(&mut self) -> i64 {
        self.rng.random_range(2..=self.config.max_exponent_value as i64)
    }

    pub fn random_number(&mut self) -> i64 {
        let value = random_field_element(
            &self.curve_prime,
            &mut self.rng,
            self.exclude_prime,
            self.config.boundary_value_probability,
            self.config.small_upper_bound_probability,
            self.config.small_upper_bound,
        );
        
        // Convert U256 to i64, taking modulo if necessary
        u256_to_i64(value)
    }

    pub fn random_non_zero_number(&mut self) -> i64 {
        let value = random_non_zero_field_element(
            &self.curve_prime,
            &mut self.rng,
            self.config.boundary_value_probability,
            self.config.small_upper_bound_probability,
            self.config.small_upper_bound,
        );
        
        u256_to_i64(value)
    }

    pub fn random_boolean(&mut self) -> bool {
        self.rng.random_bool(0.5)
    }

    pub fn random_boolean_variable(&mut self) -> String {
        assert!(!self.boolean_variables.is_empty(), "no boolean variables available");
        let idx = self.rng.random_range(0..self.boolean_variables.len());
        self.boolean_variables[idx].clone()
    }

    pub fn random_arithmetic_variable(&mut self) -> String {
        assert!(!self.arithmetic_variables.is_empty(), "no arithmetic variables available");
        let idx = self.rng.random_range(0..self.arithmetic_variables.len());
        self.arithmetic_variables[idx].clone()
    }

    pub fn random_relation(&mut self) -> Operator {
        let idx = self.rng.random_range(0..self.operators.relations.len());
        self.operators.relations[idx].clone()
    }

    pub fn random_boolean_unary_operation(&mut self) -> Operator {
        let idx = self.rng.random_range(0..self.operators.boolean_unary_operators.len());
        self.operators.boolean_unary_operators[idx].clone()
    }

    pub fn random_boolean_binary_operation(&mut self) -> Operator {
        let idx = self.rng.random_range(0..self.operators.boolean_binary_operators.len());
        self.operators.boolean_binary_operators[idx].clone()
    }

    pub fn random_arithmetic_unary_operation(&mut self) -> Operator {
        let idx = self.rng.random_range(0..self.operators.arithmetic_unary_operators.len());
        self.operators.arithmetic_unary_operators[idx].clone()
    }

    pub fn random_arithmetic_binary_operation(&mut self) -> Operator {
        let idx = self.rng.random_range(0..self.operators.arithmetic_binary_operators.len());
        self.operators.arithmetic_binary_operators[idx].clone()
    }

    pub fn allowed_boolean_expression_kinds(&self, depth: u32) -> Vec<ExpressionKind> {
        let mut allowed_expr = Vec::new();
        allowed_expr.push(ExpressionKind::Constant);
        
        if !self.boolean_variables.is_empty() {
            allowed_expr.push(ExpressionKind::Variable);
        }
        
        if depth < self.config.max_expression_depth {
            if !self.operators.relations.is_empty() {
                allowed_expr.push(ExpressionKind::Relation);
            }
            if !self.operators.boolean_unary_operators.is_empty() {
                allowed_expr.push(ExpressionKind::Unary);
            }
            if !self.operators.boolean_binary_operators.is_empty() {
                allowed_expr.push(ExpressionKind::Binary);
            }
            if self.operators.is_boolean_ternary_supported {
                allowed_expr.push(ExpressionKind::Ternary);
            }
        }
        
        allowed_expr
    }

    pub fn allowed_arithmetic_expression_kinds(&self, depth: u32) -> Vec<ExpressionKind> {
        let mut allowed_expr = Vec::new();
        allowed_expr.push(ExpressionKind::Constant);
        
        if !self.arithmetic_variables.is_empty() {
            allowed_expr.push(ExpressionKind::Variable);
        }
        
        if depth < self.config.max_expression_depth {
            if !self.operators.arithmetic_unary_operators.is_empty() {
                allowed_expr.push(ExpressionKind::Unary);
            }
            if !self.operators.arithmetic_binary_operators.is_empty() {
                allowed_expr.push(ExpressionKind::Binary);
            }
            if self.operators.is_arithmetic_ternary_supported {
                allowed_expr.push(ExpressionKind::Ternary);
            }
        }
        
        allowed_expr
    }
}

// Helper function to convert U256 to i64
fn u256_to_i64(value: U256) -> i64 {
    // Take only the lower 64 bits and convert to i64
    let lower = value.as_limbs()[0];
    // Handle conversion from u64 to i64
    if lower > i64::MAX as u64 {
        // Wrap around for values larger than i64::MAX
        lower as i64
    } else {
        lower as i64
    }
}

