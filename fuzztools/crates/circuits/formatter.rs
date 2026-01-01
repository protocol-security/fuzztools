use crate::circuits::ast::{forest::Forest, nodes::Node};
use petgraph::{algo::toposort, graph::NodeIndex, visit::EdgeRef, Direction};
use std::collections::HashMap;

impl Forest {
    /// Build expression string for a node
    fn build_expr(&self, idx: NodeIndex, expr: &HashMap<usize, String>) -> String {
        let left = |this: &Self| expr[&this.left(idx).unwrap().index()].clone();

        match &self.graph[idx] {
            Node::Input { name, .. } |
            Node::Literal { value: name, .. } |
            Node::Variable { name, .. } => name.clone(),
            Node::Operator { op, .. } => match self.right(idx) {
                Some(r) => format!("({} {} {})", left(self), op, &expr[&r.index()]),
                None => format!("({}{})", op, left(self)),
            },
            Node::Index { value } => format!("{}[{}]", left(self), value),
            Node::TupleIndex { value } => format!("{}.{}", left(self), value),
            Node::FieldAccess { name } => format!("{}.{}", left(self), name),
            Node::Call { name, .. } => {
                let mut args: Vec<_> = self
                    .graph
                    .edges_directed(idx, Direction::Outgoing)
                    .map(|e| (*e.weight(), e.target().index()))
                    .collect();
                args.sort_by_key(|(pos, _)| *pos);

                format!(
                    "{}({})",
                    name,
                    args.iter().map(|(_, i)| &expr[i]).cloned().collect::<Vec<_>>().join(", ")
                )
            }
            Node::Cast { target } => format!("({} as {})", left(self), target),
            Node::Assignment { .. } => left(self),
            Node::ForLoop { .. } | Node::If { .. } | Node::Assert { .. } => "()".to_string(),
        }
    }

    /// Get the expression string for a specific node
    pub fn get_expr_for_node(&self, target: NodeIndex) -> String {
        let sorted = toposort(&self.graph, None).unwrap();
        let mut expr: HashMap<usize, String> = HashMap::new();

        for &idx in sorted.iter().rev() {
            expr.insert(idx.index(), self.build_expr(idx, &expr));
        }

        expr[&target.index()].clone()
    }
}

impl std::fmt::Display for Forest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sorted = toposort(&self.graph, None).unwrap();
        let mut expr: HashMap<usize, String> = HashMap::new();

        for &idx in sorted.iter().rev() {
            match &self.graph[idx] {
                Node::Variable { name, mutable, .. } => {
                    let src = &expr[&self.left(idx).unwrap().index()];
                    let mut_kw = if *mutable { "mut " } else { "" };
                    writeln!(f, "    let {}{}: {} = {};", mut_kw, name, self.ty(idx), src)?;
                    expr.insert(idx.index(), name.clone());
                }
                Node::Assignment { target, op } => {
                    let target_expr = &expr[&target.index()];
                    let value_expr = &expr[&self.left(idx).unwrap().index()];
                    match op {
                        Some(op) => writeln!(f, "    {} {}= {};", target_expr, op, value_expr)?,
                        None => writeln!(f, "    {} = {};", target_expr, value_expr)?,
                    }
                    // After assignment, referencing this node gives the value
                    expr.insert(idx.index(), value_expr.clone());
                }
                Node::ForLoop { var, start, end, body, .. } => {
                    // Noir for loops don't need type annotation: for tmp in START..END
                    writeln!(f, "    for {} in {}..{} {{", var, start, end)?;
                    // Format the body with extra indentation
                    let body_str = body.to_string();
                    for line in body_str.lines() {
                        writeln!(f, "    {}", line)?;
                    }
                    writeln!(f, "    }}")?;
                    // ForLoop produces unit type
                    expr.insert(idx.index(), "()".to_string());
                }
                Node::If { condition, then_body, else_ifs, else_body } => {
                    // Format the if condition
                    let cond_expr = &expr[&condition.index()];
                    writeln!(f, "    if {} {{", cond_expr)?;
                    // Format the then body with extra indentation
                    let then_str = then_body.to_string();
                    for line in then_str.lines() {
                        writeln!(f, "    {}", line)?;
                    }
                    write!(f, "    }}")?;

                    // Format else-if branches
                    for (else_if_cond, else_if_body) in else_ifs {
                        let else_if_cond_expr = &expr[&else_if_cond.index()];
                        writeln!(f, " else if {} {{", else_if_cond_expr)?;
                        let else_if_str = else_if_body.to_string();
                        for line in else_if_str.lines() {
                            writeln!(f, "    {}", line)?;
                        }
                        write!(f, "    }}")?;
                    }

                    // Format else branch if present
                    if let Some(else_body) = else_body {
                        writeln!(f, " else {{")?;
                        let else_str = else_body.to_string();
                        for line in else_str.lines() {
                            writeln!(f, "    {}", line)?;
                        }
                        writeln!(f, "    }}")?;
                    } else {
                        writeln!(f)?;
                    }

                    // If produces unit type
                    expr.insert(idx.index(), "()".to_string());
                }
                Node::Assert { condition, message } => {
                    let cond_expr = &expr[&condition.index()];
                    match message {
                        Some(msg) => writeln!(f, "    assert({}, {});", cond_expr, msg)?,
                        None => writeln!(f, "    assert({});", cond_expr)?,
                    }
                    // Assert produces unit type
                    expr.insert(idx.index(), "()".to_string());
                }
                _ => {
                    expr.insert(idx.index(), self.build_expr(idx, &expr));
                }
            }
        }

        Ok(())
    }
}
