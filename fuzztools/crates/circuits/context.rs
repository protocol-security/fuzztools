use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
/// Context that controls the generation of the program.
pub struct Context {
    // ─────────────────────────────────────────────────────────────────────────
    // Size limits
    // ─────────────────────────────────────────────────────────────────────────
    /// Number of statements in main body
    pub min_expression_count: usize,
    pub max_expression_count: usize,

    /// Size of collections (arrays, slices, tuples)
    pub min_element_count: usize,
    pub max_element_count: usize,

    /// Size of strings
    pub min_string_size: usize,
    pub max_string_size: usize,

    /// Max hashes in raw strings (r#"..."#)
    pub max_hashes_count: usize,

    /// Function parameters
    pub min_function_parameters_count: usize,
    pub max_function_parameters_count: usize,

    /// Struct fields
    pub min_struct_fields_count: usize,
    pub max_struct_fields_count: usize,

    /// Number of structs, globals, inputs, functions
    pub min_struct_count: usize,
    pub max_struct_count: usize,
    pub min_globals_count: usize,
    pub max_globals_count: usize,
    pub min_input_count: usize,
    pub max_input_count: usize,
    pub min_function_count: usize,
    pub max_function_count: usize,

    /// Function/lambda body sizes
    pub min_function_body_size: usize,
    pub max_function_body_size: usize,
    pub min_lambda_body_size: usize,
    pub max_lambda_body_size: usize,

    /// Rewriter rules to apply
    pub min_rewrites_count: usize,
    pub max_rewrites_count: usize,

    /// For loop/if body sizes
    pub min_for_loop_body_size: usize,
    pub max_for_loop_body_size: usize,
    pub min_if_body_size: usize,
    pub max_if_body_size: usize,

    /// Else-if branches
    pub min_else_if_count: usize,
    pub max_else_if_count: usize,

    /// Max number of loops/ifs/asserts
    pub max_for_count: usize,
    pub max_if_count: usize,
    pub max_assert_count: usize,

    /// Max number of lambdas
    pub max_lambda_count: usize,

    // ─────────────────────────────────────────────────────────────────────────
    // Type weights (relative probability of generating each type)
    // ─────────────────────────────────────────────────────────────────────────
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
    pub empty_weight: usize,

    // ─────────────────────────────────────────────────────────────────────────
    // Statement weights (relative probability of generating each statement)
    // ─────────────────────────────────────────────────────────────────────────
    pub operator_weight: usize,
    pub index_weight: usize,
    pub tuple_index_weight: usize,
    pub field_access_weight: usize,
    pub call_weight: usize,
    pub cast_weight: usize,
    pub assignment_weight: usize,
    pub for_loop_weight: usize,
    pub if_weight: usize,
    pub assert_weight: usize,

    // ─────────────────────────────────────────────────────────────────────────
    // Probabilities
    // ─────────────────────────────────────────────────────────────────────────
    /// Probability of else branch
    pub else_probability: f64,

    /// Probability of boundary values (0, MAX, MIN)
    pub boundary_value_probability: f64,

    /// Probability of small values vs full range
    pub small_value_probability: f64,

    /// Max value for "small" numbers
    pub max_small_value: i128,

    /// Probability of raw strings
    pub raw_string_probability: f64,

    /// Probability of unary vs binary operators
    pub unary_probability: f64,

    /// Probability of mutable variables
    pub mutable_probability: f64,

    /// Probability of compound assignment (+=, -=, etc.)
    pub compound_assignment_probability: f64,

    /// Probability of assert message
    pub assert_message_probability: f64,

    /// Probability of function having return type
    pub function_return_probability: f64,

    /// Probability of public inputs in main
    pub public_input_probability: f64,

    /// Probability of main having return type
    pub main_return_probability: f64,

    /// Probability of generating comparison vs boolean operation
    pub comparison_probability: f64,

    /// Probability of using mixed types with casts in binary ops
    pub mixed_types_probability: f64,

    /// Probability of generating array vs slice in index expressions
    pub array_vs_slice_probability: f64,

    /// Probability of Field (vs Boolean) as cast source to Integer
    pub cast_source_field_probability: f64,

    /// Probability of signed vs unsigned loop variable in for loops
    pub signed_loop_probability: f64,

    // ─────────────────────────────────────────────────────────────────────────
    // Expression weights
    // ─────────────────────────────────────────────────────────────────────────
    /// Weight for generating a leaf expression
    pub leaf_expr_weight: usize,

    /// Weight for reusing an existing variable in expressions
    pub use_existing_expr_weight: usize,

    // ─────────────────────────────────────────────────────────────────────────
    // Depth limits
    // ─────────────────────────────────────────────────────────────────────────
    /// Max depth of nested types (array of array of...)
    pub max_type_depth: usize,

    /// Max depth of expression trees
    pub max_expr_depth: usize,

    /// Probability of generating leaf vs compound in expression tree
    pub leaf_probability: f64,

    /// Probability of reusing existing variable vs new literal
    pub reuse_variable_probability: f64,

    /// Multiplier for type weights when `type_bias` is set (higher = stronger bias)
    pub type_bias_multiplier: usize,
}
