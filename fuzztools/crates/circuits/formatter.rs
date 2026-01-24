use crate::circuits::ast::{forest::Forest, nodes::Node};
use petgraph::{algo::toposort, graph::NodeIndex, visit::EdgeRef, Direction};
use std::collections::HashMap;

impl Forest {
    fn build_expr(&self, idx: NodeIndex, exprs: &HashMap<usize, String>) -> String {
        let left = || &exprs[&self.left(idx).unwrap().index()];

        match &self.graph[idx] {
            Node::Input { name, .. } |
            Node::Literal { value: name, .. } |
            Node::Variable { name, .. } => name.clone(),
            Node::Operator { op, .. } => match self.right(idx) {
                Some(r) => format!("({} {} {})", left(), op, exprs[&r.index()]),
                None => format!("({}{})", op, left()),
            },
            Node::Index { value } => format!("{}[{}]", left(), value),
            Node::TupleIndex { value } => format!("{}.{}", left(), value),
            Node::FieldAccess { name } => format!("{}.{}", left(), name),
            Node::Cast { target } => format!("({} as {})", left(), target),
            Node::Assignment { .. } => left().clone(),
            Node::Call { name, .. } => {
                let mut args: Vec<_> = self
                    .graph
                    .edges_directed(idx, Direction::Outgoing)
                    .map(|e| (*e.weight(), &exprs[&e.target().index()]))
                    .collect();
                args.sort_by_key(|(p, _)| *p);
                format!(
                    "{}({})",
                    name,
                    args.iter().skip(1).map(|(_, s)| s.as_str()).collect::<Vec<_>>().join(", ")
                )
            }
            _ => "()".to_string(),
        }
    }

    pub fn get_expr_for_node(&self, target: NodeIndex) -> String {
        let sorted = toposort(&self.graph, None).unwrap();
        let mut exprs = HashMap::new();
        for &idx in sorted.iter().rev() {
            exprs.insert(idx.index(), self.build_expr(idx, &exprs));
        }
        exprs[&target.index()].clone()
    }

    pub fn format_with_indent(&self, indent: &str) -> String {
        let mut exprs = HashMap::new();
        let mut out = String::new();
        let next_indent = format!("{indent}    ");

        let fmt_block = |body: &Forest| body.format_with_indent(&next_indent);

        for &idx in toposort(&self.graph, None).unwrap().iter().rev() {
            match &self.graph[idx] {
                Node::Variable { name, mutable, .. } => {
                    let val = &exprs[&self.left(idx).unwrap().index()];
                    out.push_str(&format!(
                        "{indent}let {}{}: {} = {val};\n",
                        if *mutable { "mut " } else { "" },
                        name,
                        self.ty(idx)
                    ));
                    exprs.insert(idx.index(), name.clone());
                }
                Node::Assignment { op } => {
                    let (src, val) = (
                        &exprs[&self.left(idx).unwrap().index()],
                        &exprs[&self.right(idx).unwrap().index()],
                    );
                    let op_str = op.map(|o| o.to_string()).unwrap_or_default();
                    out.push_str(&format!("{indent}{src} {op_str}= {val};\n"));
                    exprs.insert(idx.index(), src.clone());
                }
                Node::ForLoop { var, start, end, body, .. } => {
                    out.push_str(&format!(
                        "{indent}for {var} in {start}..{end} {{\n{}{indent}}}\n",
                        fmt_block(body)
                    ));
                    exprs.insert(idx.index(), "()".into());
                }
                Node::If { condition, then_body, else_ifs, else_body } => {
                    out.push_str(&format!(
                        "{indent}if {} {{\n{}{indent}}}",
                        exprs[&condition.index()],
                        fmt_block(then_body)
                    ));
                    for (cond, body) in else_ifs {
                        out.push_str(&format!(
                            " else if {} {{\n{}{indent}}}",
                            exprs[&cond.index()],
                            fmt_block(body)
                        ));
                    }
                    if let Some(body) = else_body {
                        out.push_str(&format!(" else {{\n{}{indent}}}\n", fmt_block(body)));
                    } else {
                        out.push('\n');
                    }
                    exprs.insert(idx.index(), "()".into());
                }
                Node::Assert { condition, message } => {
                    let msg = message.as_ref().map(|m| format!(", {m}")).unwrap_or_default();
                    out.push_str(&format!("{indent}assert({}{msg});\n", exprs[&condition.index()]));
                    exprs.insert(idx.index(), "()".into());
                }
                _ => {
                    exprs.insert(idx.index(), self.build_expr(idx, &exprs));
                }
            }
        }

        if let Some(return_expr) = &self.return_expr {
            out.push_str(&format!("{indent}{}\n", return_expr));
        }

        out
    }
}

impl std::fmt::Display for Forest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.format_with_indent(""))
    }
}
