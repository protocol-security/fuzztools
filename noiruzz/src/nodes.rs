use crate::types::{NoirType, Operator};
use alloy::primitives::U256;

#[derive(Debug, Clone)]
pub enum ASTNode {
    Expression(Expression),
    Statement(Statement),
    Definition(Definition),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    CallExpression(CallExpression),
    IndexAccessExpression(IndexAccessExpression),
    FieldAccessExpression(FieldAccessExpression),
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    IntegerLiteral(IntegerLiteral),
    ListLiteral(ListLiteral),
    TupleLiteral(TupleLiteral),
}

#[derive(Debug, Clone)]
pub enum Statement {
    BasicBlock(BasicBlock),
    IfStatement(IfStatement),
    ForStatement(ForStatement),
    LetStatement(LetStatement),
    AssignStatement(AssignStatement),
    AssertStatement(AssertStatement),
    ExpressionStatement(ExpressionStatement),
    ReturnStatement(ReturnStatement),
}

#[derive(Debug, Clone)]
pub enum Definition {
    VariableDefinition(VariableDefinition),
    FunctionDefinition(FunctionDefinition),
    Document(Document),
}

// ------------------
//
// Expressions
//
// ------------------
#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: Operator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: Operator,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub reference: Box<Expression>,
    pub arguments: Vec<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct IndexAccessExpression {
    pub reference: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpression {
    pub reference: Box<Expression>,
    pub field: Identifier,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub value: U256,
}

#[derive(Debug, Clone)]
pub struct ListLiteral {
    pub value: Vec<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct TupleLiteral {
    pub value: Vec<Box<Expression>>,
}

// ------------------
//
// Statements
//
// ------------------
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub statements: Vec<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Box<Expression>,
    pub true_stmt: Box<Statement>,
    pub false_stmt: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub index: Identifier,
    pub start: Box<Expression>,
    pub end: Box<Expression>,
    pub statements: Vec<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub expr: Option<Box<Expression>>,
    pub type_: Option<NoirType>,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct AssignStatement {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssertStatement {
    pub condition: Box<Expression>,
    pub message: Option<StringLiteral>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: Box<Expression>,
    pub is_semicolon: bool,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Box<Expression>,
}

// ------------------
//
// Definitions
//
// ------------------
#[derive(Debug, Clone)]
pub struct VariableDefinition {
    pub name: Identifier,
    pub type_: NoirType,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub arguments: Vec<VariableDefinition>,
    pub body: Box<Statement>,
    pub is_public: bool,
    pub is_public_return: bool,
    pub type_: Option<NoirType>,
}

#[derive(Debug, Clone)]
pub struct Document {
    pub main: FunctionDefinition,
}
