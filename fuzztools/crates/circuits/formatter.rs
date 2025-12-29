use crate::circuits::ast::tree::{Expr, ExprGraph};
use petgraph::{algo::toposort, visit::EdgeRef, Direction};
use std::{collections::HashMap};

impl std::fmt::Display for ExprGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sorted = match toposort(&self.graph, None) {
            Ok(nodes) => nodes,
            Err(_) => return write!(f, "Error: graph has cycles"),
        };

        // Map NodeIndex -> Variable Name
        let mut node_names = HashMap::new();
        let mut temp_counter = 0;

        // Iterate in reverse to define dependencies first
        for &idx in sorted.iter().rev() {
            // 1. Identify Variable Name
            let (var_name, is_explicit_var, is_input) = match &self.graph[idx] {
                Expr::Input { name, .. } => (name.clone(), false, true),
                Expr::Variable { name, .. } => (name.clone(), true, false),
                Expr::Literal { value, .. } => (format!("{}", value.clone()), false, false), /* Literals are their own name */
                _ => {
                    let name = format!("_t{}", temp_counter);
                    temp_counter += 1;
                    (name, false, false)
                }
            };

            node_names.insert(idx, var_name.clone());

            if is_input {
                // Don't print inputs, they are assumed to be in scope
                continue;
            }

            if let Expr::Literal { .. } = &self.graph[idx] {
                // Don't print standalone literals (e.g. `1;`), they are inlined, nor inputs, as
                // they are handled at the top level
                continue;
            }

            // 2. Resolve Dependencies (RHS)
            // Helper to get variable name of a dependency connected via a specific edge weight
            let get_arg = |weight: usize| -> String {
                self.graph
                    .edges_directed(idx, Direction::Outgoing)
                    .find(|e| *e.weight() == weight)
                    .and_then(|e| node_names.get(&e.target()))
                    .cloned()
                    .unwrap_or_else(|| "/* undefined */".to_string())
            };

            // Helper to get variable name of the base dependency (for Index/Tuple where weight
            // varies)
            let get_base = || -> String {
                self.graph
                    .edges_directed(idx, Direction::Outgoing)
                    .next()
                    .and_then(|e| node_names.get(&e.target()))
                    .cloned()
                    .unwrap_or_else(|| "/* undefined */".to_string())
            };

            let rhs = match &self.graph[idx] {
                Expr::Input { .. } => get_arg(0),
                Expr::Variable { .. } => get_arg(0), // Variable just points to its source
                Expr::Binary { op, .. } => format!("{} {} {}", get_arg(0), op, get_arg(1)),
                Expr::Unary { op, .. } => format!("{}{}", op, get_arg(0)),
                Expr::Index => {
                    // For Index, the edge weight IS the index value
                    let edge = self.graph.edges_directed(idx, Direction::Outgoing).next();
                    match edge {
                        Some(e) => format!("{}[{}]", node_names[&e.target()], e.weight()),
                        None => "/* missing base */".to_string(),
                    }
                }
                Expr::Tuple => {
                    let edge = self.graph.edges_directed(idx, Direction::Outgoing).next();
                    match edge {
                        Some(e) => format!("{}.{}", node_names[&e.target()], e.weight()),
                        None => "/* missing base */".to_string(),
                    }
                }
                Expr::Field { field } => format!("{}.{}", get_base(), field),
                _ => unreachable!(),
            };

            // 3. Print Statement
            if is_explicit_var {
                if let Some(ty) = self.ty(idx) {
                    writeln!(f, "    let {}: {} = {};", var_name, ty, rhs)?;
                } else {
                    writeln!(f, "    let {} = {};", var_name, rhs)?;
                }
            } else {
                // Temporary intermediate value
                writeln!(f, "    let {} = {};", var_name, rhs)?;
            }
        }

        Ok(())
    }
}
