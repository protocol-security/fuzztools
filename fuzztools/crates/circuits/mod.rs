//! Noir IR.

pub mod ast;
pub mod context;
pub mod generators;
pub mod rewriter;
pub mod scope;
pub mod tests;
pub mod utils;
pub mod formatter;

pub type Circuit = String;
