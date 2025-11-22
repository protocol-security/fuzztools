pub struct Config {
    /// Maximum number of input variables used
    pub max_input_count: u64,
    /// Maximum number of nodes in the circuit AST
    pub max_node_count: u64,
    /// Maximum depth of expressions, that is, maximum depth in the AST tree for a single node
    pub max_expression_depth: u64,
    /// Maximum number of elements in a tuple/array/struct
    pub max_element_count: u64,
    /// Maximum size of a string
    pub max_string_size: u64,
    /// Maximum depth of complex types
    pub max_type_depth: u64,
    /// Maximum number of nodes in a for statement
    pub max_for_size: u64,
    /// Odds for a signed integer
    pub signed_probability: f64,
    /// Odds for a boundary value
    pub boundary_value_probability: f64,
    /// Odds for a small upper bound
    pub small_upper_bound_probability: f64,
}
