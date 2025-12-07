use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub max_inputs_count: usize,

    pub max_element_count: usize,
    pub max_string_size: usize,
    pub max_for_size: usize,

    pub max_expression_depth: usize,
    pub max_type_depth: usize,

    pub max_alias_count: usize,
    pub max_structs_count: usize,
    pub max_globals_count: usize,

    pub max_functions_count: usize,
    pub max_function_parameters_count: usize,
    pub max_function_return_types_count: usize,
    pub max_function_expressions_count: usize,

    pub max_main_expressions_count: usize,

    pub max_raw_string_hashes_count: usize,

    pub small_upper_bound: u128,

    pub name_size: usize,

    pub is_unconstrained_probability: f64,
    pub is_oracle_probability: f64,
    pub integer_signed_probability: f64,
    pub boundary_value_probability: f64,
    pub small_upper_bound_probability: f64,
    pub exclude_prime_probability: f64,
    pub mutable_probability: f64,
    pub raw_string_probability: f64,
}

#[test]
fn test_block() {
    let arr = [0u8; 1335923];
    println!("{:?}", arr);

}