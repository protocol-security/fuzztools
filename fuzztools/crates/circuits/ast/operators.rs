// ────────────────────────────────────────────────────────────────────────────────
// Operator definition
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Operator {
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
    Neg,
}

// ────────────────────────────────────────────────────────────────────────────────
// Operator implementation
// ────────────────────────────────────────────────────────────────────────────────

impl Operator {
    pub(crate) const fn is_commutative(&self) -> bool {
        matches!(
            self,
            Self::Add | Self::Mul | Self::And | Self::Or | Self::Xor | Self::Equal | Self::NotEqual
        )
    }

    pub(crate) const fn is_associative(&self) -> bool {
        matches!(self, Self::Add | Self::Mul | Self::And | Self::Or | Self::Xor)
    }

    pub(crate) const fn is_comparison(&self) -> bool {
        matches!(
            self,
            Self::Equal |
                Self::NotEqual |
                Self::Less |
                Self::LessOrEqual |
                Self::Greater |
                Self::GreaterOrEqual
        )
    }

    pub(crate) const fn flip_comparison(&self) -> Option<Self> {
        match self {
            Self::Less => Some(Self::Greater),
            Self::Greater => Some(Self::Less),
            Self::LessOrEqual => Some(Self::GreaterOrEqual),
            Self::GreaterOrEqual => Some(Self::LessOrEqual),
            Self::Equal => Some(Self::Equal),
            Self::NotEqual => Some(Self::NotEqual),
            _ => None,
        }
    }

    pub(crate) const fn negate_comparison(&self) -> Option<Self> {
        match self {
            Self::Less => Some(Self::GreaterOrEqual),
            Self::GreaterOrEqual => Some(Self::Less),
            Self::Greater => Some(Self::LessOrEqual),
            Self::LessOrEqual => Some(Self::Greater),
            Self::Equal => Some(Self::NotEqual),
            Self::NotEqual => Some(Self::Equal),
            _ => None,
        }
    }

    pub(crate) const fn binary_field() -> &'static [Self] {
        &[Self::Add, Self::Sub, Self::Mul, Self::Div]
    }

    pub(crate) const fn unary_field() -> &'static [Self] {
        &[Self::Neg]
    }

    pub(crate) const fn binary_signed_integer() -> &'static [Self] {
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

    pub(crate) const fn unary_signed_integer() -> &'static [Self] {
        &[Self::Neg, Self::Not]
    }

    pub(crate) const fn binary_unsigned_integer() -> &'static [Self] {
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

    pub(crate) const fn unary_unsigned_integer() -> &'static [Self] {
        &[Self::Not]
    }

    pub(crate) const fn binary_boolean() -> &'static [Self] {
        &[Self::And, Self::Or, Self::Xor]
    }

    pub(crate) const fn unary_boolean() -> &'static [Self] {
        &[Self::Not]
    }

    pub(crate) const fn field_comparison() -> &'static [Self] {
        &[Self::Equal, Self::NotEqual]
    }

    pub(crate) const fn comparison() -> &'static [Self] {
        &[
            Self::Less,
            Self::LessOrEqual,
            Self::Greater,
            Self::GreaterOrEqual,
            Self::Equal,
            Self::NotEqual,
        ]
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub | Self::Neg => write!(f, "-"),
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
        }
    }
}
