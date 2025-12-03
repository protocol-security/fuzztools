#[derive(Default)]
pub struct Config {
    /// Maximum number of input variables used
    pub max_input_count: u32,
    /// Maximum number of nodes in the circuit AST
    pub max_node_count: u32,
    /// Maximum depth of expressions
    pub max_expression_depth: u32,
    /// Maximum number of elements in a tuple/array/struct
    pub max_element_count: u32,
    /// Maximum size of a string
    pub max_string_size: u32,
    /// Maximum depth of complex types
    pub max_type_depth: u32,
    /// Maximum number of nodes in a for statement
    pub max_for_size: u32,
    /// Maximum number of globals
    pub max_globals_count: u32,
    /// Maximum number of hashes in raw strings
    pub max_hash_count: u32,
    /// Small upper bound
    pub small_upper_bound: u128,
    /// Odds for a signed integer
    pub signed_probability: f64,
    /// Odds for a boundary value
    pub boundary_value_probability: f64,
    /// Odds for a small upper bound
    pub small_upper_bound_probability: f64,
    /// Odds for excluding the prime value
    pub prime_exclusion_probability: f64,
    /// Odds for a raw string
    pub raw_string_probability: f64,
}
