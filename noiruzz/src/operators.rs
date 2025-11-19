use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub(crate) enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Xor,
    And,
    Or,
    Shl,
    Shr,
    Not,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    Rem,
    Comp,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Xor => "^",
            Operator::And => "&",
            Operator::Or => "|",
            Operator::Shl => "<<",
            Operator::Shr => ">>",
            Operator::Not => "!",
            Operator::Lt => "<",
            Operator::Lte => "<=",
            Operator::Gt => ">",
            Operator::Gte => ">=",
            Operator::Eq => "==",
            Operator::Neq => "!=",
            Operator::Rem => "%",
            Operator::Comp => "~",
        };
        write!(f, "{}", op)
    }
}