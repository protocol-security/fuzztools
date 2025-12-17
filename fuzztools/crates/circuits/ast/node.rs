use crate::circuits::ast::{types::Type, operators::Operator};

pub enum Node {
    Literal(Literal),
    Variable(Variable),
    Operator(Operator),
    FunctionCall(FunctionCall),
    StructAccess(StructAccess),
    TupleAccess(TupleAccess),
    IndexAccess(IndexAccess),
}

pub struct Literal {
    pub value: String,
    pub ty: Type,
}

pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
}

pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Node>,
}

pub struct StructAccess {
    pub name: Box<Node>,
    pub field: String,
}

pub struct TupleAccess {
    pub name: Box<Node>,
    pub index: usize,
}

pub struct IndexAccess {
    pub name: Box<Node>,
    pub index: usize,
}

