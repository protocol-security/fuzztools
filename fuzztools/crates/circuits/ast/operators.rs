//! Implements the Noir IR operators

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    Not,
    Neg
}

impl Operator {
    pub const fn binary_field() -> &'static [Operator] {
        &[Self::Add, Self::Sub, Self::Mul, Self::Div, Self::Equal, Self::NotEqual]
    }

    pub const fn unary_field() -> &'static [Operator] {
        &[Self::Neg]
    }

    pub const fn binary_integer_signed() -> &'static [Operator] {
        &[
            Self::Add,
            Self::Sub,
            Self::Mul,
            Self::Div,
            Self::Mod,
            Self::Less,
            Self::LessOrEqual,
            Self::Greater,
            Self::GreaterOrEqual,
            Self::Equal,
            Self::NotEqual,
            Self::And,
            Self::Or,
            Self::Xor,
            Self::Shl,
            Self::Shr,
        ]
    }

    pub const fn unary_integer_signed() -> &'static [Operator] {
        &[Self::Neg, Self::Not]
    }

    pub const fn binary_integer_unsigned() -> &'static [Operator] {
        &[
            Self::Add,
            Self::Sub,
            Self::Mul,
            Self::Div,
            Self::Mod,
            Self::Less,
            Self::LessOrEqual,
            Self::Greater,
            Self::GreaterOrEqual,
            Self::Equal,
            Self::NotEqual,
            Self::And,
            Self::Or,
            Self::Xor,
            Self::Shl,
            Self::Shr,
        ]
    }

    pub const fn unary_integer_unsigned() -> &'static [Operator] {
        &[Self::Not]
    }

    pub const fn binary_boolean() -> &'static [Operator] {
        &[Self::And, Self::Or, Self::Xor, Self::Equal, Self::NotEqual]
    }

    pub const fn unary_boolean() -> &'static [Operator] {
        &[Self::Not]
    }

    pub const fn is_comparison(&self) -> bool {
        matches!(
            self,
            Self::Less |
                Self::LessOrEqual |
                Self::Greater |
                Self::GreaterOrEqual |
                Self::Equal |
                Self::NotEqual
        )
    }

    pub const fn is_unary(&self) -> bool {
        matches!(self, Self::Neg | Self::Not)
    }

    /// Returns true if this operator can be used in compound assignment (e.g., +=, -=)
    pub const fn is_compound_assignable(&self) -> bool {
        matches!(
            self,
            Self::Add |
                Self::Sub |
                Self::Mul |
                Self::Div |
                Self::Mod |
                Self::And |
                Self::Or |
                Self::Xor |
                Self::Shl |
                Self::Shr
        )
    }

    /// Returns compound assignment operators valid for Field type
    pub const fn compound_field() -> &'static [Operator] {
        &[Self::Add, Self::Sub, Self::Mul, Self::Div]
    }

    /// Returns compound assignment operators valid for signed integers
    pub const fn compound_integer_signed() -> &'static [Operator] {
        &[
            Self::Add,
            Self::Sub,
            Self::Mul,
            Self::Div,
            Self::Mod,
            Self::And,
            Self::Or,
            Self::Xor,
            Self::Shl,
            Self::Shr,
        ]
    }

    /// Returns compound assignment operators valid for unsigned integers
    pub const fn compound_integer_unsigned() -> &'static [Operator] {
        &[
            Self::Add,
            Self::Sub,
            Self::Mul,
            Self::Div,
            Self::Mod,
            Self::And,
            Self::Or,
            Self::Xor,
            Self::Shl,
            Self::Shr,
        ]
    }

    /// Returns compound assignment operators valid for booleans
    pub const fn compound_boolean() -> &'static [Operator] {
        &[Self::And, Self::Or, Self::Xor]
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Xor => write!(f, "^"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
            Self::Less => write!(f, "<"),
            Self::LessOrEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterOrEqual => write!(f, ">="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Not => write!(f, "!"),
            Self::Neg => write!(f, "-"),
        }
    }
}
