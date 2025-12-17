#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    pub fn arithmetic_field() -> &'static [Operator] {
        &[Self::Add, Self::Sub, Self::Mul, Self::Div]
    }

    pub fn arithmetic_integer() -> &'static [Operator] {
        &[Self::Add, Self::Sub, Self::Mul, Self::Div, Self::Mod]
    }
    
    pub fn comparison_field() -> &'static [Operator] {
        &[Self::Equal, Self::NotEqual]
    }

    pub fn comparison_integer() -> &'static [Operator] {
        &[Self::Less, Self::LessOrEqual, Self::Greater, Self::GreaterOrEqual, Self::Equal, Self::NotEqual]
    }

    pub fn binary_boolean() -> &'static [Operator] {
        &[Self::And, Self::Or, Self::Xor]
    }

    pub fn binary_integer() -> &'static [Operator] {
        &[Self::And, Self::Or, Self::Xor, Self::Shl, Self::Shr]
    }
    
    pub fn unary_integer() -> &'static [Operator] {
        &[Self::Neg, Self::Not]
    }

    pub fn unary_boolean() -> &'static [Operator] {
        &[Self::Not]
    }
}

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
