//! Noir IR.

pub mod ast;
pub mod context;
pub mod formatter;
pub mod functions;
pub mod generators;
// @todo pub mod rewriter;
pub mod scope;
pub mod utils;

pub type Circuit = String;
