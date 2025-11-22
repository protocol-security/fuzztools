use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub operators: Operators,
    pub circuit: Circuit,
    pub rewrite: Rewrite,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operators {
    pub relation_operators: Vec<String>,
    pub unary_operators: Vec<String>,
    pub binary_operators: Vec<String>,
    pub assign_operators: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Circuit {
    pub constant_probability_weight: f64,
    pub variable_probability_weight: f64,
    pub unary_probability_weight: f64,
    pub binary_probability_weight: f64,
    pub relation_probability_weight: f64,

    pub max_expression_depth: u64,
    pub min_number_of_assertions: u64,
    pub max_number_of_assertions: u64,
    pub min_number_of_input_variables: u64,
    pub max_number_of_input_variables: u64,
    pub min_number_of_output_variables: u64,
    pub max_number_of_output_variables: u64,
    pub max_exponent_value: u64,

    pub boundary_value_probability: f64,
    pub small_upper_bound_probability: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rewrite {
    pub weakening_probability: f64,
    pub min_rewrites: u64,
    pub max_rewrites: u64,
    pub rules: Rules,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rules {
    pub equivalence: Vec<RewriteRule>,
    pub weakening: Vec<RewriteRule>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RewriteRule {
    pub name: String,
    #[serde(rename = "match")]
    pub match_pattern: String,
    pub rewrite: String,
}
