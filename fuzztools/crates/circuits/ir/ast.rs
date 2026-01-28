use super::{forest::Forest, nodes::Node, operators::Operator, types::Type};
use petgraph::algo::toposort;
use std::{collections::HashMap, fmt::Write};

// ────────────────────────────────────────────────────────────────────────────────
// AST definition
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct AST {
    pub statements: Vec<Statement>,
    pub return_expr: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let { name: String, ty: Type, mutable: bool, value: Expr },
    DirectAssignment { name: String, op: Option<Operator>, value: Expr },
    IndexAssignment { name: String, index: usize, op: Option<Operator>, value: Expr },
    TupleAssignment { name: String, field: usize, op: Option<Operator>, value: Expr },
    StructAssignment { name: String, field: String, op: Option<Operator>, value: Expr },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ref(String, Type),
    Binary { op: Operator, ty: Type, left: Box<Expr>, right: Box<Expr> },
    Unary { op: Operator, ty: Type, operand: Box<Expr> },
    Index { expr: Box<Expr>, ty: Type, index: usize },
    TupleIndex { expr: Box<Expr>, ty: Type, field: usize },
    StructField { expr: Box<Expr>, ty: Type, field: String },
    Cast { expr: Box<Expr>, ty: Type },
}

impl Expr {
    pub const fn ty(&self) -> &Type {
        match self {
            Expr::Ref(_, ty) => ty,
            Expr::Binary { ty, .. } => ty,
            Expr::Unary { ty, .. } => ty,
            Expr::Index { ty, .. } => ty,
            Expr::TupleIndex { ty, .. } => ty,
            Expr::StructField { ty, .. } => ty,
            Expr::Cast { ty, .. } => ty,
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// AST implementation
// ────────────────────────────────────────────────────────────────────────────────

impl From<&Forest> for AST {
    fn from(forest: &Forest) -> Self {
        let mut statements = Vec::new();
        let mut exprs: HashMap<usize, Expr> = HashMap::new();

        for &idx in toposort(&forest.inner, None).unwrap().iter().rev() {
            let left = forest.left(idx).map(|i| exprs[&i.index()].clone());
            let right = forest.right(idx).map(|i| exprs[&i.index()].clone());

            match &forest.inner[idx] {
                Node::Input { name, ty } | Node::Literal { value: name, ty } => {
                    exprs.insert(idx.index(), Expr::Ref(name.clone(), ty.clone()));
                }
                Node::Variable { name, ty, mutable } => {
                    statements.push(Statement::Let {
                        name: name.clone(),
                        ty: ty.clone(),
                        mutable: *mutable,
                        value: left.unwrap(),
                    });
                    exprs.insert(idx.index(), Expr::Ref(name.clone(), ty.clone()));
                }
                Node::Operator { op, ty } => {
                    exprs.insert(
                        idx.index(),
                        match right {
                            Some(r) => Expr::Binary {
                                op: *op,
                                ty: ty.clone(),
                                left: Box::new(left.unwrap()),
                                right: Box::new(r),
                            },
                            None => Expr::Unary {
                                op: *op,
                                ty: ty.clone(),
                                operand: Box::new(left.unwrap()),
                            },
                        },
                    );
                }
                Node::Index { index, ty } => {
                    exprs.insert(
                        idx.index(),
                        Expr::Index {
                            expr: Box::new(left.unwrap()),
                            ty: ty.clone(),
                            index: *index,
                        },
                    );
                }
                Node::TupleIndex { index, ty } => {
                    exprs.insert(
                        idx.index(),
                        Expr::TupleIndex {
                            expr: Box::new(left.unwrap()),
                            ty: ty.clone(),
                            field: *index,
                        },
                    );
                }
                Node::StructField { field, ty } => {
                    exprs.insert(
                        idx.index(),
                        Expr::StructField {
                            expr: Box::new(left.unwrap()),
                            ty: ty.clone(),
                            field: field.clone(),
                        },
                    );
                }
                Node::Cast { ty } => {
                    exprs.insert(
                        idx.index(),
                        Expr::Cast { expr: Box::new(left.unwrap()), ty: ty.clone() },
                    );
                }
                Node::Assignment { name, op, ty } => {
                    let value = right.unwrap();

                    statements.push(match left.unwrap() {
                        Expr::Ref(..) => {
                            Statement::DirectAssignment { name: name.clone(), op: *op, value }
                        }
                        Expr::Index { index, .. } => {
                            Statement::IndexAssignment { name: name.clone(), index, op: *op, value }
                        }
                        Expr::TupleIndex { field, .. } => {
                            Statement::TupleAssignment { name: name.clone(), field, op: *op, value }
                        }
                        Expr::StructField { field, .. } => Statement::StructAssignment {
                            name: name.clone(),
                            field,
                            op: *op,
                            value,
                        },
                        _ => unreachable!(),
                    });
                    exprs.insert(idx.index(), Expr::Ref(name.clone(), ty.clone()));
                }
            }
        }

        AST { statements, return_expr: forest.return_expr.clone() }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Formatting
// ────────────────────────────────────────────────────────────────────────────────

impl AST {
    pub fn format(&self, indent: &str) -> String {
        let mut out = String::new();

        for stmt in &self.statements {
            let _ = writeln!(out, "{indent}{}", stmt.format());
        }

        if let Some(ret) = &self.return_expr {
            let _ = writeln!(out, "{indent}{ret}");
        }

        out
    }
}

impl Statement {
    pub fn format(&self) -> String {
        match self {
            Statement::Let { name, ty, mutable, value } => {
                let m = if *mutable { "mut " } else { "" };
                format!("let {m}{name}: {ty} = {};", value.format())
            }
            Statement::DirectAssignment { name, op, value } => match op {
                Some(o) => format!("{name} {o}= {};", value.format()),
                None => format!("{name} = {};", value.format()),
            },
            Statement::IndexAssignment { name, index, op, value } => match op {
                Some(o) => format!("{name}[{index}] {o}= {};", value.format()),
                None => format!("{name}[{index}] = {};", value.format()),
            },
            Statement::TupleAssignment { name, field, op, value } => match op {
                Some(o) => format!("{name}.{field} {o}= {};", value.format()),
                None => format!("{name}.{field} = {};", value.format()),
            },
            Statement::StructAssignment { name, field, op, value } => match op {
                Some(o) => format!("{name}.{field} {o}= {};", value.format()),
                None => format!("{name}.{field} = {};", value.format()),
            },
        }
    }
}

impl Expr {
    pub fn format(&self) -> String {
        match self {
            Expr::Ref(name, _) => name.clone(),
            Expr::Binary { op, left, right, .. } => {
                format!("({} {op} {})", left.format(), right.format())
            }
            Expr::Unary { op, operand, .. } => format!("({op}{})", operand.format()),
            Expr::Index { expr, index, .. } => format!("{}[{index}]", expr.format()),
            Expr::TupleIndex { expr, field, .. } => format!("{}.{field}", expr.format()),
            Expr::StructField { expr, field, .. } => format!("{}.{field}", expr.format()),
            Expr::Cast { expr, ty } => format!("({} as {ty})", expr.format()),
        }
    }
}
