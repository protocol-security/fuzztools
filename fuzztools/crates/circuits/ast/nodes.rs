use crate::circuits::ast::{operators::Operator, types::Type};

// ────────────────────────────────────────────────────────────────────────────────
// Node definitions
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    Input,
    Literal,
    Variable,
    Operator,
    Index,
    TupleIndex,
    FieldAccess,
    // @todo call, if, else if, else, for, loop, while, cast, sub_block, assert, comptime y unsafe
    // y las funciones oracle y todo eso
}

#[derive(Clone)]
pub enum Node {
    /// Input node in case of main/functions/lambdas
    Input { name: String, ty: Type },

    /// A variable node, `a`, `b`, `c`, etc...
    Variable { name: String, ty: Type },

    /// A literal value of the given type, `23`, `false`, `"hello"`, etc...
    Literal { value: String, ty: Type },

    /// An operator node, `+`, `-`, `*`, `/`, `%`, etc...
    Operator { op: Operator },

    /// An index node, `a[2]`, `b[3]`, etc...
    Index { value: usize },

    /// A tuple index node, `a.0`, `b.1`, etc...
    TupleIndex { value: usize },

    /// A field access node, `a.field`, `b.field`, etc...
    FieldAccess { name: String },
    // @todo call, if, else if, else, for, loop, while, cast, sub_block, assert, comptime y unsafe
    // y las funciones oracle y todo eso
}

// ────────────────────────────────────────────────────────────────────────────────
// Node implementations
// ────────────────────────────────────────────────────────────────────────────────

impl Node {
    pub fn kind(&self) -> NodeKind {
        match self {
            Self::Input { .. } => NodeKind::Input,
            Self::Variable { .. } => NodeKind::Variable,
            Self::Literal { .. } => NodeKind::Literal,
            Self::Operator { .. } => NodeKind::Operator,
            Self::Index { .. } => NodeKind::Index,
            Self::TupleIndex { .. } => NodeKind::TupleIndex,
            Self::FieldAccess { .. } => NodeKind::FieldAccess,
        }
    }

    pub fn color(&self) -> &'static str {
        match self.kind() {
            NodeKind::Input => "#d56b4bff",  // red
            NodeKind::Literal => "#a0d8ef",  // light blue
            NodeKind::Variable => "#98d98e", // light green
            NodeKind::Operator => "#ffd700", // yellow
            NodeKind::Index | NodeKind::TupleIndex | NodeKind::FieldAccess => "#ffb6c1", /* light pink */
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Input { name, ty } => write!(f, "{}: {}", name, ty),
            Self::Variable { name, .. } => write!(f, "{}", name),
            Self::Literal { value, .. } => write!(f, "{}", value),
            Self::Operator { op, .. } => write!(f, "{}", op),
            Self::Index { value } => write!(f, "[{}]", value),
            Self::TupleIndex { value } => write!(f, ".{}", value),
            Self::FieldAccess { name } => write!(f, ".{}", name),
        }
    }
}
