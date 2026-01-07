use crate::circuits::ast::{forest::Forest, operators::Operator, types::Type};
use petgraph::graph::NodeIndex;

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
    Cast,
    Assignment,
    ForLoop,
    If,
    Assert,
}

#[derive(Debug, Clone)]
pub enum Node {
    /// Input node in case of main/functions/lambdas
    Input { name: String, ty: Type },

    /// A variable node, `a`, `b`, `c`, etc...
    Variable { name: String, ty: Type, mutable: bool, shadow: bool },

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

    /// A cast expression, `expr as Type`
    Cast { target: Type },

    /// An assignment to a mutable variable or a component of it (e.g., `x = 5`, `arr[0] = 5`,
    /// `s.field = 5`).
    /// - Edge 0 points to the source variable being assigned to.
    /// - Edge 1 points to the value expression.
    ///
    /// If `op` is Some, this is a compound assignment (e.g., `x += 5`, `x -= 5`).
    /// After assignment, a new Variable node with the same name is created pointing to this node.
    Assignment { op: Option<Operator> },

    /// A for loop statement: `for var in start..end { body }`.
    ForLoop { var: String, ty: Type, start: String, end: String, body: Box<Forest> },

    /// An if/else if/else statement
    If {
        condition: NodeIndex,
        then_body: Box<Forest>,
        else_ifs: Vec<(NodeIndex, Box<Forest>)>,
        else_body: Option<Box<Forest>>,
    },

    /// An assert statement: `assert(COND)` or `assert(COND, MSG)`
    Assert { condition: NodeIndex, message: Option<String> },
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
            Self::Cast { .. } => NodeKind::Cast,
            Self::Assignment { .. } => NodeKind::Assignment,
            Self::ForLoop { .. } => NodeKind::ForLoop,
            Self::If { .. } => NodeKind::If,
            Self::Assert { .. } => NodeKind::Assert,
        }
    }

    #[inline(always)]
    pub fn color(&self) -> &'static str {
        match self {
            Self::Input { .. } => "#dc4e23ff",
            Self::Literal { .. } => "#24b3ecff",
            Self::Variable { shadow: false, .. } => "#6bd85aff",
            Self::Variable { shadow: true, .. } => "#4a9640ff",
            Self::Operator { op, .. } if op.is_unary() => "#ffa500",
            Self::Operator { .. } => "#ffd700",
            Self::Index { .. } | Self::TupleIndex { .. } | Self::FieldAccess { .. } => "#fe8fa2ff",
            Self::Call { .. } => "#e22be2ff",
            Self::Cast { .. } => "#4f47a6ff",
            Self::Assignment { .. } => "#8a006dff",
            Self::ForLoop { .. } => "#115976ff",
            Self::If { .. } => "#005a00ff",
            Self::Assert { .. } => "#a62c00ff",
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
            Self::Call { name, .. } => write!(f, "{}()", name),
            Self::Cast { target } => write!(f, "as {}", target),
            Self::Assignment { op: Some(op), .. } => write!(f, "{}=", op),
            Self::Assignment { op: None, .. } => write!(f, "="),
            Self::ForLoop { .. } => write!(f, "for"),
            Self::If { .. } => {
                write!(f, "if")
            }
            Self::Assert { .. } => {
                write!(f, "assert")
            }
        }
    }
}
