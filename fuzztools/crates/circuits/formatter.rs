use crate::circuits::ast::{forest::Forest, nodes::Node};
use petgraph::{algo::toposort, graph::NodeIndex, visit::EdgeRef, Direction};
use std::collections::{HashMap, HashSet};

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

impl Forest {
    /// Compute set of used (non-idle) variable nodes.
    /// A variable is used if it has incoming edges from other nodes.
    fn compute_used_nodes(&self) -> HashSet<NodeIndex> {
        let mut used = HashSet::new();
        let mut stack: Vec<NodeIndex> = Vec::new();

        for idx in self.graph.node_indices() {
            let is_root = matches!(
                &self.graph[idx],
                Node::ForLoop { .. } |
                    Node::If { .. } |
                    Node::Assert { .. } |
                    Node::Assignment { .. }
            ) || self.graph.edges_directed(idx, Direction::Incoming).next().is_some();
            if is_root && used.insert(idx) {
                stack.push(idx);
            }
        }

        while let Some(idx) = stack.pop() {
            for edge in self.graph.edges_directed(idx, Direction::Outgoing) {
                if used.insert(edge.target()) {
                    stack.push(edge.target());
                }
            }
        }
        used
    }

    /// Format forest body with specified indentation prefix
    pub fn format_with_indent(&self, indent: &str) -> String {
        let sorted = toposort(&self.graph, None).unwrap();
        let mut expr: HashMap<usize, String> = HashMap::new();
        let mut out = String::new();

        // Compute used nodes when skip_idle_vars is enabled
        let used_nodes =
            if self.skip_idle_vars { self.compute_used_nodes() } else { HashSet::new() };

        for &idx in sorted.iter().rev() {
            match &self.graph[idx] {
                Node::Variable { name, mutable, .. } => {
                    // Skip idle variables when the flag is set
                    if self.skip_idle_vars && !used_nodes.contains(&idx) {
                        expr.insert(idx.index(), name.clone());
                        continue;
                    }
                    let src = &expr[&self.left(idx).unwrap().index()];
                    let mut_kw = if *mutable { "mut " } else { "" };
                    out.push_str(&format!(
                        "{}let {}{}: {} = {};\n",
                        indent,
                        mut_kw,
                        name,
                        self.ty(idx),
                        src
                    ));
                    expr.insert(idx.index(), name.clone());
                }
                Node::Assignment { op } => {
                    let (src, val) = (
                        &expr[&self.left(idx).unwrap().index()],
                        &expr[&self.right(idx).unwrap().index()],
                    );
                    let assign_op = op.map(|o| format!(" {o}")).unwrap_or_default();
                    out.push_str(&format!("{indent}{src} {assign_op}= {val};\n"));
                    expr.insert(idx.index(), src.clone());
                }
                Node::ForLoop { var, start, end, body, .. } => {
                    out.push_str(&format!("{}for {} in {}..{} {{\n", indent, var, start, end));
                    let nested_indent = format!("{}    ", indent);
                    out.push_str(&body.format_with_indent(&nested_indent));
                    out.push_str(&format!("{}}}\n", indent));
                    expr.insert(idx.index(), "()".to_string());
                }
                Node::If { condition, then_body, else_ifs, else_body } => {
                    let cond_expr = &expr[&condition.index()];
                    out.push_str(&format!("{}if {} {{\n", indent, cond_expr));
                    let nested_indent = format!("{}    ", indent);
                    out.push_str(&then_body.format_with_indent(&nested_indent));
                    out.push_str(&format!("{}}}", indent));

                    for (else_if_cond, else_if_body) in else_ifs {
                        let else_if_cond_expr = &expr[&else_if_cond.index()];
                        out.push_str(&format!(" else if {} {{\n", else_if_cond_expr));
                        out.push_str(&else_if_body.format_with_indent(&nested_indent));
                        out.push_str(&format!("{}}}", indent));
                    }

                    if let Some(else_body) = else_body {
                        out.push_str(" else {\n");
                        out.push_str(&else_body.format_with_indent(&nested_indent));
                        out.push_str(&format!("{}}}\n", indent));
                    } else {
                        out.push('\n');
                    }

                    expr.insert(idx.index(), "()".to_string());
                }
                Node::Assert { condition, message } => {
                    let cond = &expr[&condition.index()];
                    let msg_part = message.as_ref().map(|m| format!(", {m}")).unwrap_or_default();
                    out.push_str(&format!("{indent}assert({cond}{msg_part});\n"));
                    expr.insert(idx.index(), "()".into());
                }
                _ => {
                    expr.insert(idx.index(), self.build_expr(idx, &expr));
                }
            }
        }

        out
    }
}

impl std::fmt::Display for Forest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Default: no indentation (caller adds as needed)
        write!(f, "{}", self.format_with_indent(""))
    }
}
