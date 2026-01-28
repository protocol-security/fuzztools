use super::{forest::Forest, nodes::Node, operators::Operator, types::Type};
use petgraph::algo::toposort;
use std::collections::HashMap;

// ────────────────────────────────────────────────────────────────────────────────
// AST definition
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct AST {
    pub statements: Vec<Statement>,
    pub return_expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let { name: String, ty: Type, mutable: bool, value: Expr },
    DirectAssignment { name: String, op: Option<Operator>, value: Expr },
    IndexAssignment { name: String, index: usize, op: Option<Operator>, value: Expr },
    TupleAssignment { name: String, field: usize, op: Option<Operator>, value: Expr },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ref(String, Type),
    Binary { op: Operator, ty: Type, left: Box<Expr>, right: Box<Expr> },
    Unary { op: Operator, ty: Type, operand: Box<Expr> },
    Index { expr: Box<Expr>, ty: Type, index: usize },
    TupleIndex { expr: Box<Expr>, ty: Type, field: usize },
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
                        _ => unreachable!(),
                    });
                    exprs.insert(idx.index(), Expr::Ref(name.clone(), ty.clone()));
                }
            }
        }

        let return_expr = forest.return_expr.as_ref().and_then(|name| {
            statements.iter().rev().find_map(|stmt| match stmt {
                Statement::Let { name: var_name, ty, .. } if var_name == name => {
                    Some(Expr::Ref(name.clone(), ty.clone()))
                }
                _ => None,
            })
        });

        AST { statements, return_expr }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{}", stmt)?;
        }
        if let Some(ret) = &self.return_expr {
            writeln!(f, "{}", ret)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { name, ty, mutable, value } => {
                write!(f, "let {}{name}: {ty} = {value};", if *mutable { "mut " } else { "" })
            }
            Statement::DirectAssignment { name, op, value } => match op {
                Some(o) => write!(f, "{name} {o}= {value};"),
                None => write!(f, "{name} = {value};"),
            },
            Statement::IndexAssignment { name, index, op, value } => match op {
                Some(o) => write!(f, "{name}[{index}] {o}= {value};"),
                None => write!(f, "{name}[{index}] = {value};"),
            },
            Statement::TupleAssignment { name, field, op, value } => match op {
                Some(o) => write!(f, "{name}.{field} {o}= {value};"),
                None => write!(f, "{name}.{field} = {value};"),
            },
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ref(name, _) => write!(f, "{name}"),
            Expr::Binary { op, left, right, .. } => write!(f, "({left} {op} {right})"),
            Expr::Unary { op, operand, .. } => write!(f, "({op}{operand})"),
            Expr::Index { expr, index, .. } => write!(f, "{expr}[{index}]"),
            Expr::TupleIndex { expr, field, .. } => write!(f, "{expr}.{field}"),
            Expr::Cast { expr, ty } => write!(f, "({expr} as {ty})"),
        }
    }
}
