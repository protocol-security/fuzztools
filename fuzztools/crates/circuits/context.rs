use serde::{Deserialize, Serialize};

#[derive(Default, Serialize, Deserialize, Clone, Copy, Debug)]
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
    pub max_struct_fields_count: usize,
    pub max_globals_count: usize,
    pub max_functions_count: usize,

    pub max_function_parameters_count: usize,
    pub max_function_return_types_count: usize,

    pub max_main_expressions_count: usize,
    pub max_block_expressions_count: usize,

    pub max_string_hashes_count: usize,
    pub max_name_characters_count: usize,

    pub max_small_upper_bound: u128,

    // ------------------------------------------------------------
    pub integer_signed_probability: f64,
    pub boundary_value_probability: f64,
    pub small_upper_bound_probability: f64,
    pub exclude_prime_probability: f64,
    pub mutable_probability: f64,
    pub raw_string_probability: f64,

    // ------------------------------------------------------------
    pub allow_slices: bool,
    pub allow_references: bool,
    pub allow_structs: bool,

    pub filter_entrypoint_structs: bool,

    pub type_depth: usize,
    pub expression_depth: usize,
}

impl Context {
    /// Returns the context being used to create struct, globals and functions definitions (not its
    /// body)
    pub fn top(&self) -> Self {
        Self {
            allow_slices: true,
            allow_references: false,
            allow_structs: true,
            min_element_count: 0,
            min_string_size: 0,
            filter_entrypoint_structs: false,
            type_depth: 0,
            ..*self
        }
    }

    /// Returns the context being used to create the main function definition (not its body)
    pub fn entrypoint(&self) -> Self {
        Self {
            allow_slices: false,
            allow_references: false,
            allow_structs: true,
            min_element_count: 1,
            min_string_size: 1,
            filter_entrypoint_structs: true,
            type_depth: 0,
            ..*self
        }
    }

    /// Returns the context being used to create sub-types (arrays of arrays, tuples of arrays,
    /// arrays of structs, etc.)
    pub fn inner_type(&self) -> Self {
        Self {
            allow_slices: false,
            // @audit recursion ?? -> allow_structs: false
            type_depth: self.type_depth + 1,
            ..*self
        }
    }

    /// Returns the context being used to create sub-expressions (binary operations, unary
    /// operations, etc.)
    pub fn inner_expression(&self) -> Self {
        Self { expression_depth: self.expression_depth + 1, ..*self }
    }
}
