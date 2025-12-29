use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
/// Context that controls the generation of the program. It handles the probability of creating
/// different types, maximum sizes of stuff, allow/disallow certain types, etc.
pub struct Context {
    pub max_inputs_count: usize,

    pub min_element_count: usize,
    pub max_element_count: usize,

    pub min_string_size: usize,
    pub max_string_size: usize,

    pub max_expression_depth: usize,
    pub max_type_depth: usize,

    pub max_structs_count: usize,

    pub min_struct_fields_count: usize,
    pub max_struct_fields_count: usize,

    pub max_globals_count: usize,
    pub max_functions_count: usize,

    pub max_function_parameters_count: usize,
    pub max_function_return_types_count: usize,

    pub max_main_expressions_count: usize,
    pub max_block_expressions_count: usize,

    pub max_string_hashes_count: usize,
    pub max_name_characters_count: usize,

    pub max_if_else_branch_count: usize,

    pub max_small_upper_bound: u128,

    // ────────────────────────────────────────────────────────────────────────────────
    pub integer_signed_probability: f64,
    pub boundary_value_probability: f64,
    pub small_upper_bound_probability: f64,
    pub exclude_prime_probability: f64,
    pub mutable_probability: f64,
    pub raw_string_probability: f64,
    pub new_variable_probability: f64,

    // ────────────────────────────────────────────────────────────────────────────────
    pub field_weight: f64,
    pub unsigned_weight: f64,
    pub signed_weight: f64,
    pub boolean_weight: f64,
    pub string_weight: f64,
    pub array_weight: f64,
    pub slice_weight: f64,
    pub tuple_weight: f64,
    pub struct_weight: f64,
    pub lambda_weight: f64,
    pub null_weight: f64,

    // ────────────────────────────────────────────────────────────────────────────────
    pub literal_weight: f64,
    pub lambda_def_weight: f64,
    pub variable_weight: f64,
    pub unary_weight: f64,
    pub field_binary_weight: f64,
    pub boolean_binary_weight: f64,
    pub integer_binary_weight: f64,
    pub index_array_weight: f64,
    pub index_slice_weight: f64,
    pub index_tuple_weight: f64,
    pub field_access_weight: f64,
    pub lambda_call_weight: f64,

    // ────────────────────────────────────────────────────────────────────────────────
    pub type_depth: usize,
    pub expression_depth: usize,
}
