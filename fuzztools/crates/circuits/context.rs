use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
/// Context that controls the generation of the program. It handles the probability of creating
/// different types, maximum sizes of stuff, allow/disallow certain types, etc.
pub struct Context {
    // This controls the size of any expression tree
    pub min_expression_count: usize,
    pub max_expression_count: usize,

    // This controls the size of any collection type
    pub min_element_count: usize,
    pub max_element_count: usize,

    // This controls the size of any string type
    pub min_string_size: usize,
    pub max_string_size: usize,

    // This controls the number of parameters in any function
    pub min_function_parameters_count: usize,
    pub max_function_parameters_count: usize,

    // This controls the number of fields in any struct
    pub min_struct_fields_count: usize,
    pub max_struct_fields_count: usize,

    // This controls the biggest small value
    pub max_small_upper_bound: usize,

    // This controls the probability of creating each `Type`
    pub field_weight: usize,
    pub unsigned_weight: usize,
    pub signed_weight: usize,
    pub boolean_weight: usize,
    pub string_weight: usize,
    pub array_weight: usize,
    pub slice_weight: usize,
    pub tuple_weight: usize,
    pub struct_weight: usize,
    pub lambda_weight: usize,

    // This controls the probability of creating each `Node`
    pub literal_weight: usize,
    pub variable_weight: usize,
    pub operator_weight: usize,
    pub index_weight: usize,
    pub tuple_index_weight: usize,
    pub field_access_weight: usize,

    // This controls the probability of creating boundary values
    pub boundary_value_probability: f64,

    // This controls the probability of excluding the prime when creating random field elements
    pub exclude_prime_probability: f64,

    // This controls the probability of creating small values
    pub small_upper_bound_probability: f64,

    // This controls the probability of creating raw strings
    pub raw_string_probability: f64,

    // This controls the probability of creating unary operators
    pub unary_probability: f64,

    // This controls the depth of the generated types
    pub max_type_depth: usize,
}
