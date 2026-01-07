//! Implements scope management to improve the quality of the generated AST

use crate::circuits::{ast::types::*, functions::Function};

#[derive(Clone, Default)]
pub struct Scope {
    /// Main function inputs: (name, type, is_public)
    pub inputs: Vec<(String, Type, bool)>,
    pub structs: Vec<Struct>,
    pub globals: Vec<(String, Type, String)>,
    pub functions: Vec<Function>,

    /// Main function return type and whether it's public
    pub main_return: Option<(Type, bool)>,

    // This is used to prevent infinite recursion due to Type::random() -> Lambda ->
    // Type::random_value() -> Type::random() with depth reset to 0...
    pub lambda_depth: usize,
}
