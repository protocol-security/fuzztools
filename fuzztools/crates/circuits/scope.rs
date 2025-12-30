//! Implements scope management to improve the quality of the generated AST

use crate::circuits::{ast::types::*, functions::Function};

#[derive(Clone, Default)]
pub struct Scope {
    pub inputs: Vec<(String, Type)>,
    pub structs: Vec<Struct>,
    pub globals: Vec<(String, Type, String)>,
    pub functions: Vec<Function>,
}
