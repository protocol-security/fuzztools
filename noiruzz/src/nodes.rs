use crate::{operators::Operator, types::NoirType};

#[derive(Debug, Clone)]
pub(crate) enum ASTNode {
    Expression(Expression),
    Statement(Statement),
    Definition(Definition),
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
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
pub(crate) enum Statement {
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
pub(crate) enum Definition {
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
pub(crate) struct Identifier {
    pub(crate) name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryExpression {
    pub(crate) operator: Operator,
    pub(crate) lhs: Box<Expression>,
    pub(crate) rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) struct UnaryExpression {
    pub(crate) operator: Operator,
    pub(crate) value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) struct CallExpression {
    pub(crate) reference: Box<Expression>,
    pub(crate) arguments: Vec<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub(crate) struct IndexAccessExpression {
    pub(crate) reference: Box<Expression>,
    pub(crate) index: Box<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) struct FieldAccessExpression {
    pub(crate) reference: Box<Expression>,
    pub(crate) field: Identifier,
}

#[derive(Debug, Clone)]
pub(crate) struct StringLiteral {
    pub(crate) value: String,
}

#[derive(Debug, Clone)]
pub(crate) struct BooleanLiteral {
    pub(crate) value: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct IntegerLiteral {
    pub(crate) value: i64,
}

#[derive(Debug, Clone)]
pub(crate) struct ListLiteral {
    pub(crate) value: Vec<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub(crate) struct TupleLiteral {
    pub(crate) value: Vec<Box<Expression>>,
}

// ------------------
//
// Statements
//
// ------------------
#[derive(Debug, Clone)]
pub(crate) struct BasicBlock {
    pub(crate) statements: Vec<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub(crate) struct IfStatement {
    pub(crate) condition: Box<Expression>,
    pub(crate) true_stmt: Box<Statement>,
    pub(crate) false_stmt: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub(crate) struct ForStatement {
    pub(crate) index: Identifier,
    pub(crate) start: Box<Expression>,
    pub(crate) end: Box<Expression>,
    pub(crate) statements: Vec<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub(crate) struct LetStatement {
    pub(crate) name: Identifier,
    pub(crate) expr: Option<Box<Expression>>,
    pub(crate) type_: Option<NoirType>,
    pub(crate) is_mutable: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct AssignStatement {
    pub(crate) lhs: Box<Expression>,
    pub(crate) rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) struct AssertStatement {
    pub(crate) condition: Box<Expression>,
    pub(crate) message: Option<StringLiteral>,
}

#[derive(Debug, Clone)]
pub(crate) struct ExpressionStatement {
    pub(crate) expr: Box<Expression>,
    pub(crate) is_semicolon: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct ReturnStatement {
    pub(crate) value: Box<Expression>,
}

// ------------------
//
// Definitions
//
// ------------------
#[derive(Debug, Clone)]
pub(crate) struct VariableDefinition {
    pub(crate) name: Identifier,
    pub(crate) type_: NoirType,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionDefinition {
    pub(crate) name: Identifier,
    pub(crate) arguments: Vec<VariableDefinition>,
    pub(crate) body: Box<Statement>,
    pub(crate) is_public: bool,
    pub(crate) is_public_return: bool,
    pub(crate) type_: Option<NoirType>,
}

#[derive(Debug, Clone)]
pub(crate) struct Document {
    pub(crate) main: FunctionDefinition,
}
