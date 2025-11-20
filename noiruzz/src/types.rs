use crate::config::Config;
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpressionKind {
    Constant,
    Variable,
    Relation,
    Unary,
    Binary,
}

impl ExpressionKind {
    pub const ARITHMETIC_KINDS: &'static [Self] =
        &[Self::Constant, Self::Variable, Self::Unary, Self::Binary, Self::Relation];
    pub const BOOLEAN_KINDS: &'static [Self] =
        &[Self::Constant, Self::Relation, Self::Unary, Self::Binary];

    pub fn weight(&self, config: &Config) -> f64 {
        match self {
            Self::Constant => config.circuit.constant_probability_weight,
            Self::Variable => config.circuit.variable_probability_weight,
            Self::Relation => config.circuit.relation_probability_weight,
            Self::Unary => config.circuit.unary_probability_weight,
            Self::Binary => config.circuit.binary_probability_weight,
        }
    }
}

impl Display for ExpressionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Constant => "constant",
            Self::Variable => "variable",
            Self::Relation => "relation",
            Self::Unary => "unary",
            Self::Binary => "binary",
        };
        write!(f, "{}", s)
    }
}

pub enum MetamorphicKind {
    Equal,
    Weaker,
}

impl MetamorphicKind {
    pub fn value(&self) -> &'static str {
        match self {
            Self::Equal => "equal",
            Self::Weaker => "weaker",
        }
    }
}

// ------------------------------------------------------------
//
// Noir types
//
// ------------------------------------------------------------

// @audit todo this all type_d on the string from the types
#[derive(Debug, Clone)]
pub enum NoirType {
    Field(Field),
    Integer(Integer), // @audit signed and unsigned, error on overflow
    Boolean(Boolean),
    String(String),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Struct(Struct),
    Reference(Reference),
    // Function(Function), // @audit todo
}

impl Display for NoirType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NoirType::Field(field) => write!(f, "{}", field),
            NoirType::Integer(integer) => write!(f, "{}", integer),
            NoirType::Boolean(boolean) => write!(f, "{}", boolean),
            NoirType::String(string) => write!(f, "{}", string),
            NoirType::Array(array) => write!(f, "{}", array),
            NoirType::Slice(slice) => write!(f, "{}", slice),
            NoirType::Tuple(tuple) => write!(f, "{}", tuple),
            NoirType::Struct(struct_) => write!(f, "{}", struct_),
            NoirType::Reference(reference) => write!(f, "{}", reference),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Field")
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    signed: bool,
    size: u64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.signed {
            write!(f, "i{}", self.size)
        } else {
            write!(f, "u{}", self.size)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bool")
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    size: u64,
}

// @audit what about scape hatchs \r, \n, \t, \0, \", \\ and raw strings r"...", r#"...",
// r######"..."###### as well as f"..." writing { with {{"
impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "str<{}>", self.size)
    }
}

// @audit you can call function in getters like
// let _ = array[f(x)];
//
// fn f(x: u32) -> u32 {
// x * 2
// }
#[derive(Debug, Clone)]
pub struct Array {
    type_: Box<NoirType>,
    size: u64,
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}; {}]", self.type_, self.size)
    }
}

// @audit must be initialized as = &[1, 2, 3, ...] or &[0; 3]
#[derive(Debug, Clone)]
pub struct Slice {
    type_: Box<NoirType>,
}

impl Display for Slice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.type_)
    }
}

// @audit accedemos a los fields con .0, .1, .2...
#[derive(Debug, Clone)]
pub struct Tuple {
    types: Vec<Box<NoirType>>,
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, type_) in self.types.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", type_)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub struct StructField {
    name: String,
    type_: Box<NoirType>,
    visibility: Visibility,
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Public,
    Crate,
    Private,
}

// @audit access fields via struct.field_name
#[derive(Debug, Clone)]
pub struct Struct {
    name: String,
    fields: Vec<Box<StructField>>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Reference {
    type_: Box<NoirType>,
    mutable: bool,
}

impl Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "&mut {}", self.type_)
        } else {
            write!(f, "&{}", self.type_)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operator {
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

impl From<&String> for Operator {
    fn from(value: &String) -> Self {
        match value.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Rem,
            "^" => Operator::Xor,
            "&" => Operator::And,
            "|" => Operator::Or,
            "<<" => Operator::Shl,
            ">>" => Operator::Shr,
            "!" => Operator::Not,
            "<" => Operator::Lt,
            "<=" => Operator::Lte,
            ">" => Operator::Gt,
            ">=" => Operator::Gte,
            "==" => Operator::Eq,
            "!=" => Operator::Neq,
            _ => Operator::Add, // default to add
        }
    }
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
