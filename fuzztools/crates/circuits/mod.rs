//! Noir IR.

pub mod ast;
pub mod context;
pub mod formatter;
pub mod functions;
pub mod generators;
pub mod rewriter;
pub mod scope;
pub mod tests;
pub mod utils;

pub type Circuit = String;
