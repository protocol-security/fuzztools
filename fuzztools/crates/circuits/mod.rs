//! Noir IR.

mod ast;
mod context;
mod generators;
mod rewritter;
mod utils;

pub type Circuit = String;

// Re-exports
pub use ast::{Forest, Type, TypeKind};
pub use context::Context;
pub use rewritter::Rewriter;
