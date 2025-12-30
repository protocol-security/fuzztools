use crate::circuits::ast::{operators::Operator, types::Type};

// ────────────────────────────────────────────────────────────────────────────────
// Node definitions
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    Input,
    Literal,
    Variable,
    Operator,
    Index,
    TupleIndex,
    FieldAccess,
    Call,
}

#[derive(Debug, Clone)]
pub enum Node {
    /// Input node in case of main/functions/lambdas
    Input { name: String, ty: Type },

    /// A variable node, `a`, `b`, `c`, etc...
    Variable { name: String, ty: Type },

    /// A literal value of the given type, `23`, `false`, `"hello"`, etc...
    Literal { value: String, ty: Type },

    /// An operator node, `+`, `-`, `*`, `/`, `%`, etc...
    Operator { op: Operator, ret: Type },

    /// An index node, `a[2]`, `b[3]`, etc...
    Index { value: usize },

    /// A tuple index node, `a.0`, `b.1`, etc...
    TupleIndex { value: usize },

    /// A field access node, `a.field`, `b.field`, etc...
    FieldAccess { name: String },

    /// A function call node, `foo(a, b)`, `bar(x)`, etc...
    Call { name: String, ret: Type },
    // @todo if, else if, else, for, loop, while, cast, sub_block, assert, comptime y unsafe
    // y las funciones oracle y todo eso

    // @audit hay assert_eq(a, b), assert_gt/lt(a, b), assert_constant(a) that ensures the value
    // is known at compile-time
}

// ────────────────────────────────────────────────────────────────────────────────
// Node implementations
// ────────────────────────────────────────────────────────────────────────────────

impl Node {
    #[inline(always)]
    pub fn kind(&self) -> NodeKind {
        match self {
            Self::Input { .. } => NodeKind::Input,
            Self::Variable { .. } => NodeKind::Variable,
            Self::Literal { .. } => NodeKind::Literal,
            Self::Operator { .. } => NodeKind::Operator,
            Self::Index { .. } => NodeKind::Index,
            Self::TupleIndex { .. } => NodeKind::TupleIndex,
            Self::FieldAccess { .. } => NodeKind::FieldAccess,
            Self::Call { .. } => NodeKind::Call,
        }
    }

    #[inline(always)]
    pub fn color(&self) -> &'static str {
        match self {
            Self::Input { .. } => "#d56b4bff",                       // red
            Self::Literal { .. } => "#a0d8ef",                       // light blue
            Self::Variable { .. } => "#98d98e",                      // light green
            Self::Operator { op, .. } if op.is_unary() => "#ffa500", // orange for unary
            Self::Operator { .. } => "#ffd700",                      // yellow for binary
            Self::Index { .. } | Self::TupleIndex { .. } | Self::FieldAccess { .. } => "#ffb6c1", /* light pink */
            Self::Call { .. } => "#c8a2c8", // lilac for calls
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Input { name, ty } => write!(f, "{}: {}", name, ty),
            Self::Variable { name, .. } => write!(f, "{}", name),
            Self::Literal { ty, .. } => write!(f, "{}", ty),
            Self::Operator { op, .. } => write!(f, "{}", op),
            Self::Index { value } => write!(f, "[{}]", value),
            Self::TupleIndex { value } => write!(f, ".{}", value),
            Self::FieldAccess { name } => write!(f, ".{}", name),
            Self::Call { name, ret } => write!(f, "{}(..) -> {}", name, ret),
        }
    }
}
