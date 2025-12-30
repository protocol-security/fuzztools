use crate::circuits::ast::{forest::Forest, nodes::Node};
use petgraph::{algo::toposort, graph::NodeIndex, visit::EdgeRef, Direction};

impl Forest {
    /// Get the expression string for a specific node
    pub fn get_expr_for_node(&self, target: NodeIndex) -> String {
        let sorted = toposort(&self.graph, None).unwrap();
        let n = self.graph.node_count();
        let mut expr: Vec<String> = vec![String::new(); n];

        for &idx in sorted.iter().rev() {
            let i = idx.index();
            expr[i] = match &self.graph[idx] {
                Node::Input { name, .. } | Node::Variable { name, .. } => name.clone(),
                Node::Literal { value, .. } => value.clone(),
                Node::Operator { op, .. } => {
                    let l = &expr[self.left(idx).unwrap().index()];
                    match self.right(idx) {
                        Some(r) => format!("({} {} {})", l, op, &expr[r.index()]),
                        None => format!("({}{})", op, l),
                    }
                }
                Node::Index { value } => {
                    format!("{}[{}]", &expr[self.left(idx).unwrap().index()], value)
                }
                Node::TupleIndex { value } => {
                    format!("{}.{}", &expr[self.left(idx).unwrap().index()], value)
                }
                Node::FieldAccess { name } => {
                    format!("{}.{}", &expr[self.left(idx).unwrap().index()], name)
                }
                Node::Call { name, .. } => {
                    let mut args: Vec<_> = self
                        .graph
                        .edges_directed(idx, Direction::Outgoing)
                        .map(|e| (*e.weight(), e.target().index()))
                        .collect();
                    args.sort_by_key(|(pos, _)| *pos);
                    let arg_strs: Vec<_> = args.iter().map(|(_, i)| expr[*i].clone()).collect();
                    format!("{}({})", name, arg_strs.join(", "))
                }
            };
        }

        expr[target.index()].clone()
    }

    /// Format forest with specified indentation level
    pub fn format_indented(&self, indent: usize) -> String {
        let sorted = toposort(&self.graph, None).unwrap();
        let n = self.graph.node_count();
        let indent_str = "    ".repeat(indent);

        let mut out = String::new();
        let mut expr: Vec<String> = vec![String::new(); n];

        for &idx in sorted.iter().rev() {
            let i = idx.index();
            expr[i] = match &self.graph[idx] {
                Node::Input { name, .. } => name.clone(),
                Node::Literal { value, .. } => value.clone(),
                Node::Variable { name, .. } => {
                    let src = &expr[self.left(idx).unwrap().index()];
                    out.push_str(&format!(
                        "{}let {}: {} = {};\n",
                        indent_str,
                        name,
                        self.ty(idx),
                        src
                    ));
                    name.clone()
                }
                Node::Operator { op, .. } => {
                    let l = &expr[self.left(idx).unwrap().index()];
                    match self.right(idx) {
                        Some(r) => format!("({} {} {})", l, op, &expr[r.index()]),
                        None => format!("({}{})", op, l),
                    }
                }
                Node::Index { value } => {
                    format!("{}[{}]", &expr[self.left(idx).unwrap().index()], value)
                }
                Node::TupleIndex { value } => {
                    format!("{}.{}", &expr[self.left(idx).unwrap().index()], value)
                }
                Node::FieldAccess { name } => {
                    format!("{}.{}", &expr[self.left(idx).unwrap().index()], name)
                }
                Node::Call { name, .. } => {
                    let mut args: Vec<_> = self
                        .graph
                        .edges_directed(idx, Direction::Outgoing)
                        .map(|e| (*e.weight(), e.target().index()))
                        .collect();
                    args.sort_by_key(|(pos, _)| *pos);
                    let args_str =
                        args.iter().map(|(_, i)| expr[*i].clone()).collect::<Vec<_>>().join(", ");
                    format!("{}({})", name, args_str)
                }
            };
        }

        out
    }
}

impl std::fmt::Display for Forest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.format_indented(1))
    }
}
