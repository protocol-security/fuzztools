use crate::operators::Operator;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct IROpsConfig {
    pub relations: Vec<Operator>,
    pub boolean_unary_operators: Vec<Operator>,
    pub boolean_binary_operators: Vec<Operator>,
    pub arithmetic_unary_operators: Vec<Operator>,
    pub arithmetic_binary_operators: Vec<Operator>,
    pub is_arithmetic_ternary_supported: bool,
    pub is_boolean_ternary_supported: bool,
}

impl Default for IROpsConfig {
    fn default() -> Self {
        Self {
            relations: vec![
                Operator::Eq,
                Operator::Neq,
                Operator::Lt,
                Operator::Lte,
                Operator::Gt,
                Operator::Gte,
            ],
            boolean_unary_operators: vec![Operator::Not],
            boolean_binary_operators: vec![Operator::And, Operator::Or, Operator::Xor],
            arithmetic_unary_operators: vec![Operator::Sub, Operator::Comp],
            arithmetic_binary_operators: vec![
                Operator::Add,
                Operator::Sub,
                Operator::Mul,
                Operator::Div,
                Operator::Rem,
            ],
            is_arithmetic_ternary_supported: true,
            is_boolean_ternary_supported: true,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct IRGenConfig {
    // weighted probability for generation nodes
    pub constant_probability_weight: f64,
    pub variable_probability_weight: f64,
    pub unary_probability_weight: f64,
    pub binary_probability_weight: f64,
    pub relation_probability_weight: f64,
    pub ternary_probability_weight: f64,

    // generation limits
    pub max_expression_depth: u32,
    pub min_number_of_assertions: u32,
    pub max_number_of_assertions: u32,
    pub min_number_of_input_variables: u32,
    pub max_number_of_input_variables: u32,
    pub min_number_of_output_variables: u32,
    pub max_number_of_output_variables: u32,

    // random max values
    pub max_exponent_value: u32,

    // probability for boundary values
    pub boundary_value_probability: f64,

    // probability for using small numbers if it is not a boundary value
    pub small_upper_bound_probability: f64,
    pub small_upper_bound: u128,
}

impl Default for IRGenConfig {
    fn default() -> Self {
        Self {
            constant_probability_weight: 1.0,
            variable_probability_weight: 1.0,
            unary_probability_weight: 1.0,
            binary_probability_weight: 1.0,
            relation_probability_weight: 1.0,
            ternary_probability_weight: 1.0,
            max_expression_depth: 5,
            min_number_of_assertions: 1,
            max_number_of_assertions: 10,
            min_number_of_input_variables: 1,
            max_number_of_input_variables: 5,
            min_number_of_output_variables: 1,
            max_number_of_output_variables: 5,
            max_exponent_value: 10,
            boundary_value_probability: 0.1,
            small_upper_bound_probability: 0.3,
            small_upper_bound: 10,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct IRConfig {
    pub generation: IRGenConfig,
    pub rewrite: IRRewriteConfig,
    pub operators: IROpsConfig,
}

impl Default for IRConfig {
    fn default() -> Self {
        Self {
            generation: IRGenConfig::default(),
            operators: IROpsConfig::default(),
            rewrite: IRRewriteConfig::default(),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct IRRewriteConfig {
    pub equivalence: Vec<RewriteRule>,
    pub weakening: Vec<RewriteRule>,
    pub weakening_probability: f64,
    pub min_rewrites: u32,
    pub max_rewrites: u32,
}

impl Default for IRRewriteConfig {
    fn default() -> Self {
        Self {
            equivalence: Vec::new(),
            weakening: Vec::new(),
            weakening_probability: 0.5,
            min_rewrites: 1,
            max_rewrites: 10,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct RewriteRule {
    pub name: String,
    pub match_pattern: String,
    pub rewrite_pattern: String,
}

impl Default for RewriteRule {
    fn default() -> Self {
        Self {
            name: String::new(),
            match_pattern: String::new(),
            rewrite_pattern: String::new(),
        }
    }
}