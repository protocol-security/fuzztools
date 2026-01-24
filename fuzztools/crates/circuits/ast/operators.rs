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
    Neg,
}

impl Operator {
    pub const fn binary_field() -> &'static [Self] {
        &[Self::Add, Self::Sub, Self::Mul, Self::Div]
    }

    pub const fn unary_field() -> &'static [Self] {
        &[Self::Neg]
    }

    pub const fn binary_integer_signed() -> &'static [Self] {
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

    pub const fn unary_integer_signed() -> &'static [Self] {
        &[Self::Neg, Self::Not]
    }

    pub const fn binary_integer_unsigned() -> &'static [Self] {
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

    pub const fn unary_integer_unsigned() -> &'static [Self] {
        &[Self::Not]
    }

    pub const fn binary_boolean() -> &'static [Self] {
        &[Self::And, Self::Or, Self::Xor]
    }

    pub const fn unary_boolean() -> &'static [Self] {
        &[Self::Not]
    }

    pub const fn field_comparison() -> &'static [Self] {
        &[Self::Equal, Self::NotEqual]
    }

    pub const fn comparison() -> &'static [Self] {
        &[
            Self::Less,
            Self::LessOrEqual,
            Self::Greater,
            Self::GreaterOrEqual,
            Self::Equal,
            Self::NotEqual,
        ]
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

    pub const fn compound_field() -> &'static [Self] {
        &[Self::Add, Self::Sub, Self::Mul, Self::Div]
    }

    pub const fn compound_integer() -> &'static [Self] {
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

    pub const fn compound_boolean() -> &'static [Self] {
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
