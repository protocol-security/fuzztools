use crate::circuits::ast::{forest::Forest, nodes::Node};
use petgraph::algo::toposort;

impl std::fmt::Display for Forest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sorted = toposort(&self.graph, None).unwrap();
        let n = self.graph.node_count();

        // This is used as a lookup table, so that we can reference expressions in O(1) time
        let mut expr: Vec<String> = vec![String::new(); n];

        for &idx in sorted.iter().rev() {
            let i = idx.index();
            expr[i] = match &self.graph[idx] {
                Node::Input { name, .. } => name.clone(),
                Node::Literal { value, .. } => value.clone(),
                Node::Variable { name, .. } => {
                    // Emit let statement, then cache the variable name
                    let src = &expr[self.left(idx).unwrap().index()];
                    writeln!(f, "    let {}: {} = {};", name, self.ty(idx), src)?;
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
            };
        }

        Ok(())
    }
}