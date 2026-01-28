use super::{operators::Operator, types::Type};

// ────────────────────────────────────────────────────────────────────────────────
// Node definition
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub(crate) enum Node {
    /// Input node.
    Input { name: String, ty: Type },

    /// A literal value of the given type, `23`, `false`, `"hello"`, etc...
    Literal { value: String, ty: Type },

    /// A variable node, `a`, `b`, `c`, etc...
    Variable { name: String, ty: Type, mutable: bool },

    /// An operator node, `+`, `-`, `*`, `/`, `%`, etc...
    Operator { op: Operator, ty: Type },

    /// An index node, `a[2]`, `b[3]`, etc...
    Index { index: usize, ty: Type },

    /// A tuple index node, `a.0`, `b.1`, etc...
    TupleIndex { index: usize, ty: Type },

    /// A cast expression, `expr as Type`.
    Cast { ty: Type },

    /// An assignment to a mutable variable/element (`x = 5`, `arr[0] = 5`, etc...).
    /// - Edge 0 points to the variable being assigned to.
    /// - Edge 1 points to the value expression.
    ///
    /// If `op` is Some, this is a compound assignment (`x += 5`, `x -= 5`, etc...).
    Assignment { name: String, op: Option<Operator>, ty: Type },
}

// ────────────────────────────────────────────────────────────────────────────────
// Node implementation
// ────────────────────────────────────────────────────────────────────────────────

impl Node {
    #[inline(always)]
    pub(crate) const fn color(&self) -> &'static str {
        match self {
            Self::Input { .. } => "#dc4e23ff",
            Self::Literal { .. } => "#24b3ecff",
            Self::Variable { .. } => "#6bd85aff",
            Self::Operator { .. } => "#ffa500",
            Self::Index { .. } | Self::TupleIndex { .. } => "#e22be2ff",
            Self::Cast { .. } => "#4f47a6ff",
            Self::Assignment { .. } => "#8a006dff",
        }
    }

    #[inline(always)]
    pub(crate) const fn is_single_use(&self) -> bool {
        matches!(
            self,
            Node::Literal { .. } |
                Node::Operator { .. } |
                Node::Index { .. } |
                Node::TupleIndex { .. } |
                Node::Cast { .. }
        )
    }

    #[inline(always)]
    pub(crate) fn ty(&self) -> Type {
        match self {
            Self::Input { ty, .. } |
            Self::Literal { ty, .. } |
            Self::Variable { ty, .. } |
            Self::Operator { ty, .. } |
            Self::Index { ty, .. } |
            Self::TupleIndex { ty, .. } |
            Self::Cast { ty } |
            Self::Assignment { ty, .. } => ty.clone(),
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Input { name, ty } => write!(f, "{}: {}", name, ty),
            Self::Variable { name, .. } => write!(f, "{}", name),
            Self::Literal { ty, .. } => write!(f, "{}", ty),
            Self::Operator { op, .. } => write!(f, "{}", op),
            Self::Index { index: value, .. } => write!(f, "[{}]", value),
            Self::TupleIndex { index: value, .. } => write!(f, ".{}", value),
            Self::Cast { ty } => write!(f, "as {}", ty),
            Self::Assignment { op: Some(op), .. } => write!(f, "{}=", op),
            Self::Assignment { op: None, .. } => write!(f, "="),
        }
    }
}
