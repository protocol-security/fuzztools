//! This module provides a directed graph structure for representing a block of expressions, that
//! is, the body of a function, the body of a loop, etc...

use crate::circuits::ast::{
    nodes::{Node, NodeKind},
    operators::Operator,
    types::*,
};
use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::EdgeRef,
    Direction,
};
use std::{collections::HashMap, fmt::Write, path::Path};

#[derive(Clone, Default)]
pub struct Forest {
    pub graph: DiGraph<Node, usize>,
    pub var_counter: usize,

    // TypeKind -> [NodeIndex]
    pub types: HashMap<TypeKind, Vec<NodeIndex>>,
    // NodeKind -> [NodeIndex]
    pub nodes: HashMap<NodeKind, Vec<NodeIndex>>,
    // Operator -> [NodeIndex]
    pub operators: HashMap<Operator, Vec<NodeIndex>>,
}

impl Forest {
    pub fn ty(&self, idx: NodeIndex) -> Type {
        match &self.graph[idx] {
            Node::Input { ty, .. } | Node::Literal { ty, .. } | Node::Variable { ty, .. } => {
                ty.clone()
            }
            Node::Operator { op } => {
                if op.is_comparison() {
                    Type::Boolean
                } else {
                    self.ty(self.left(idx).unwrap())
                }
            }
            Node::Index { .. } => {
                let parent_ty = self.ty(self.left(idx).unwrap());
                match parent_ty {
                    Type::Array(Array { ty, .. }) | Type::Slice(Slice { ty, .. }) => (*ty).clone(),
                    _ => unreachable!(),
                }
            }
            Node::TupleIndex { value } => {
                let parent_ty = self.ty(self.left(idx).unwrap());
                match parent_ty {
                    Type::Tuple(Tuple { elements, .. }) => elements[*value].clone(),
                    _ => unreachable!(),
                }
            }
            Node::FieldAccess { name } => {
                let parent_ty = self.ty(self.left(idx).unwrap());
                match parent_ty {
                    Type::Struct(Struct { fields, .. }) => {
                        fields.iter().find(|f| &f.name == name).unwrap().ty.as_ref().clone()
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Handlers for creating nodes
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn input(&mut self, name: String, ty: Type) -> NodeIndex {
        self.graph.add_node(Node::Input { name, ty })
    }

    pub fn variable(&mut self, name: String, ty: Type, value: NodeIndex) -> NodeIndex {
        let idx = self.graph.add_node(Node::Variable { name, ty });
        self.graph.add_edge(idx, value, 0);

        idx
    }

    pub fn literal(&mut self, value: String, ty: Type) -> NodeIndex {
        self.graph.add_node(Node::Literal { value, ty })
    }

    pub fn operator(&mut self, op: Operator, lhs: NodeIndex, rhs: Option<NodeIndex>) -> NodeIndex {
        let idx = self.graph.add_node(Node::Operator { op });
        self.graph.add_edge(idx, lhs, 0);

        // In unary operations, there is no right operand, so `rhs` is `None`
        if let Some(rhs) = rhs {
            self.graph.add_edge(idx, rhs, 1);
        }

        idx
    }

    pub fn index(&mut self, parent: NodeIndex, value: usize) -> NodeIndex {
        let idx = self.graph.add_node(Node::Index { value });
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    pub fn tuple_index(&mut self, parent: NodeIndex, value: usize) -> NodeIndex {
        let idx = self.graph.add_node(Node::TupleIndex { value });
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    pub fn field_access(&mut self, parent: NodeIndex, name: String) -> NodeIndex {
        let idx = self.graph.add_node(Node::FieldAccess { name });
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Handy mutations
    // ────────────────────────────────────────────────────────────────────────────────

    /// Swap the left and right operands of a binary operation
    pub fn swap_operands(&mut self, n: NodeIndex) {
        let edges: Vec<_> = self
            .graph
            .edges_directed(n, Direction::Incoming)
            .map(|e| (e.id(), *e.weight(), e.target()))
            .collect();

        if edges.len() == 2 {
            self.graph.remove_edge(edges[0].0);
            self.graph.remove_edge(edges[1].0);

            self.graph.add_edge(edges[0].2, n, 1 - edges[0].1);
            self.graph.add_edge(edges[1].2, n, 1 - edges[1].1);
        }
    }

    /// Given an operator at position `n`, replace the operand connected by the edge with weight
    /// `pos` with the node `new`, removing the old operand if it has no remaining edges
    pub fn replace_operand(&mut self, n: NodeIndex, pos: usize, new: NodeIndex) {
        let edge_to_remove =
            self.graph.edges_directed(n, Direction::Incoming).find(|e| *e.weight() == pos);

        if let Some(edge) = edge_to_remove {
            let old_operand = edge.source();
            let edge_id = edge.id();

            self.graph.remove_edge(edge_id);

            // Remove the old operand if it has no remaining edges
            let has_edges =
                self.graph.edges_directed(old_operand, Direction::Incoming).next().is_some() ||
                    self.graph.edges_directed(old_operand, Direction::Outgoing).next().is_some();

            if !has_edges {
                self.graph.remove_node(old_operand);
            }
        }

        self.graph.add_edge(new, n, pos);
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Queries to the expression graph
    // ────────────────────────────────────────────────────────────────────────────────

    /// Get the left operand (edge weight 0) of an operator
    pub fn left(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.graph
            .edges_directed(idx, Direction::Outgoing)
            .find(|e| *e.weight() == 0)
            .map(|e| e.target())
    }

    /// Get the right operand (edge weight 1) of a binary operation
    pub fn right(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.graph
            .edges_directed(idx, Direction::Outgoing)
            .find(|e| *e.weight() == 1)
            .map(|e| e.target())
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Handy helpers
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn next_var(&mut self) -> String {
        let n = self.var_counter;
        self.var_counter += 1;
        format!("v{n}")
    }

    /// Register a node in all tracking maps
    pub fn register(&mut self, idx: NodeIndex, kind: NodeKind, ty: &Type, op: Option<Operator>) {
        self.types.entry(ty.kind()).or_default().push(idx);
        self.nodes.entry(kind).or_default().push(idx);
        if let Some(op) = op {
            self.operators.entry(op).or_default().push(idx);
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Utils to export as DOT file
    // ────────────────────────────────────────────────────────────────────────────────

    /// Get the edge label based on the `Node` type
    fn dot_edge_label(&self, idx: NodeIndex, weight: usize) -> String {
        match &self.graph[idx] {
            Node::Operator { .. } => if weight == 0 { "lhs" } else { "rhs" }.into(),
            Node::Variable { .. } => "let".into(),
            _ => String::new(),
        }
    }

    pub fn save_as_dot(&self, path: &Path) {
        let mut out = String::new();
        out.push_str(
            "digraph {\n  rankdir=BT;\n  node [fontname=monospace shape=box style=\"filled,rounded\" margin=\"0.4,0.2\"];\n",
        );

        for n in self.graph.node_indices() {
            let label = self.graph[n].to_string();
            let color = self.graph[n].color();

            let _ = writeln!(
                out,
                "  {} [label=\"{}\" fillcolor=\"{color}\"]",
                n.index(),
                label.replace('"', "\\\"")
            );
        }

        for e in self.graph.edge_references() {
            let lbl = self.dot_edge_label(e.source(), *e.weight());
            let _ = writeln!(
                out,
                "  {} -> {} [label=\"{lbl}\" dir=back]",
                e.source().index(),
                e.target().index()
            );
        }

        out.push_str("}\n");
        std::fs::write(path, out).unwrap();
    }
}
