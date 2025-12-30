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
    pub types: HashMap<Type, Vec<NodeIndex>>,
    pub type_kinds: HashMap<TypeKind, Vec<NodeIndex>>,
    pub nodes: HashMap<NodeKind, Vec<NodeIndex>>,
    pub operators: HashMap<Operator, Vec<NodeIndex>>,
}

impl Forest {
    pub fn ty(&self, idx: NodeIndex) -> Type {
        match &self.graph[idx] {
            Node::Input { ty, .. } |
            Node::Literal { ty, .. } |
            Node::Variable { ty, .. } |
            Node::Call { ret: ty, .. } => ty.clone(),
            Node::Operator { ret, .. } => ret.clone(),
            Node::Index { .. } => match self.ty(self.left(idx).unwrap()) {
                Type::Array(a) => (*a.ty).clone(),
                Type::Slice(s) => (*s.ty).clone(),
                _ => unreachable!(),
            },
            Node::TupleIndex { value } => match self.ty(self.left(idx).unwrap()) {
                Type::Tuple(t) => t.elements[*value].clone(),
                _ => unreachable!(),
            },
            Node::FieldAccess { name } => match self.ty(self.left(idx).unwrap()) {
                Type::Struct(s) => {
                    s.fields.iter().find(|f| &f.name == name).unwrap().ty.as_ref().clone()
                }
                _ => unreachable!(),
            },
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Node creation
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    pub fn input(&mut self, name: String, ty: Type) -> NodeIndex {
        self.graph.add_node(Node::Input { name, ty })
    }

    #[inline(always)]
    pub fn variable(&mut self, name: String, ty: Type, value: NodeIndex) -> NodeIndex {
        let idx = self.graph.add_node(Node::Variable { name, ty });
        self.graph.add_edge(idx, value, 0);

        idx
    }

    #[inline(always)]
    pub fn literal(&mut self, value: String, ty: Type) -> NodeIndex {
        self.graph.add_node(Node::Literal { value, ty })
    }

    #[inline(always)]
    pub fn operator(
        &mut self,
        op: Operator,
        ret: Type,
        lhs: NodeIndex,
        rhs: Option<NodeIndex>,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Operator { op, ret });
        self.graph.add_edge(idx, lhs, 0);

        // In unary operations, there is no right operand, so `rhs` is `None`
        if let Some(rhs) = rhs {
            self.graph.add_edge(idx, rhs, 1);
        }

        idx
    }

    #[inline(always)]
    pub fn index(&mut self, parent: NodeIndex, value: usize) -> NodeIndex {
        let idx = self.graph.add_node(Node::Index { value });
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    #[inline(always)]
    pub fn tuple_index(&mut self, parent: NodeIndex, value: usize) -> NodeIndex {
        let idx = self.graph.add_node(Node::TupleIndex { value });
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    #[inline(always)]
    pub fn field_access(&mut self, parent: NodeIndex, name: String) -> NodeIndex {
        let idx = self.graph.add_node(Node::FieldAccess { name });
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    #[inline(always)]
    pub fn call(&mut self, name: String, ret: Type, args: Vec<NodeIndex>) -> NodeIndex {
        let idx = self.graph.add_node(Node::Call { name, ret });
        for (pos, arg) in args.into_iter().enumerate() {
            self.graph.add_edge(idx, arg, pos);
        }

        idx
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Graph queries
    // ────────────────────────────────────────────────────────────────────────────────

    /// Get operand at position `pos` (0 = left/primary, 1 = right)
    #[inline(always)]
    pub fn operand(&self, idx: NodeIndex, pos: usize) -> Option<NodeIndex> {
        self.graph
            .edges_directed(idx, Direction::Outgoing)
            .find(|e| *e.weight() == pos)
            .map(|e| e.target())
    }

    #[inline(always)]
    pub fn left(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.operand(idx, 0)
    }

    #[inline(always)]
    pub fn right(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.operand(idx, 1)
    }

    /// Collect incoming edges to a node
    #[inline(always)]
    pub fn incoming_edges(&self, node: NodeIndex) -> Vec<(NodeIndex, usize)> {
        self.graph
            .edges_directed(node, Direction::Incoming)
            .map(|e| (e.source(), *e.weight()))
            .collect()
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Graph mutations
    // ────────────────────────────────────────────────────────────────────────────────

    /// Remove node if it has no edges
    #[inline(always)]
    fn remove_if_orphan(&mut self, node: NodeIndex) {
        if self.graph.edges(node).next().is_none() {
            self.graph.remove_node(node);
        }
    }

    /// Swap left and right operands of a binary operation
    pub fn swap_operands(&mut self, n: NodeIndex) {
        let edges: Vec<_> = self
            .graph
            .edges_directed(n, Direction::Outgoing)
            .map(|e| (e.id(), *e.weight(), e.target()))
            .collect();

        if edges.len() == 2 {
            self.graph.remove_edge(edges[0].0);
            self.graph.remove_edge(edges[1].0);

            self.graph.add_edge(n, edges[0].2, 1 - edges[0].1);
            self.graph.add_edge(n, edges[1].2, 1 - edges[1].1);
        }
    }

    /// Replace operand at position `pos`, removing it if orphaned
    #[inline(always)]
    pub fn replace_operand(&mut self, n: NodeIndex, pos: usize, new: NodeIndex) {
        if let Some(edge) =
            self.graph.edges_directed(n, Direction::Outgoing).find(|e| *e.weight() == pos)
        {
            let old = edge.target();
            self.graph.remove_edge(edge.id());
            self.remove_if_orphan(old);
        }
        self.graph.add_edge(n, new, pos);
    }

    /// Add operand at position `pos`
    #[inline(always)]
    pub fn add_operand(&mut self, n: NodeIndex, pos: usize, operand: NodeIndex) {
        self.graph.add_edge(n, operand, pos);
    }

    /// Remove operand at position `pos`, removing target if orphaned
    #[inline(always)]
    pub fn remove_operand(&mut self, n: NodeIndex, pos: usize) {
        if let Some(edge) =
            self.graph.edges_directed(n, Direction::Outgoing).find(|e| *e.weight() == pos)
        {
            let old = edge.target();
            self.graph.remove_edge(edge.id());
            self.remove_if_orphan(old);
        }
    }

    /// Redirect incoming edges from `old` to `new`
    pub fn redirect_edges(&mut self, old: NodeIndex, new: NodeIndex, edges: &[(NodeIndex, usize)]) {
        for &(source, weight) in edges {
            if let Some(id) = self
                .graph
                .edges_directed(old, Direction::Incoming)
                .find(|e| e.source() == source && *e.weight() == weight)
                .map(|e| e.id())
            {
                self.graph.remove_edge(id);
            }
            self.graph.add_edge(source, new, weight);
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Helpers
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    pub fn next_var(&mut self) -> String {
        let n = self.var_counter;
        self.var_counter += 1;
        format!("v{n}")
    }

    #[inline(always)]
    pub fn register(&mut self, idx: NodeIndex, kind: NodeKind, ty: &Type, op: Option<Operator>) {
        self.types.entry(ty.clone()).or_default().push(idx);
        self.type_kinds.entry(ty.kind()).or_default().push(idx);
        self.nodes.entry(kind).or_default().push(idx);
        if let Some(op) = op {
            self.operators.entry(op).or_default().push(idx);
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // DOT export
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn save_as_dot(&self, path: &Path) {
        let mut out = String::from(
            "digraph {\n  rankdir=BT;\n  node [fontname=monospace shape=box style=\"filled,rounded\" margin=\"0.4,0.2\"];\n"
        );

        for n in self.graph.node_indices() {
            let node = &self.graph[n];
            let _ = writeln!(
                out,
                "  {} [label=\"{}\" fillcolor=\"{}\"]",
                n.index(),
                node,
                node.color()
            );
        }

        for e in self.graph.edge_references() {
            let lbl = match &self.graph[e.source()] {
                Node::Operator { op, .. } if op.is_unary() => "unary".into(),
                Node::Operator { .. } if *e.weight() == 0 => "lhs".into(),
                Node::Operator { .. } => "rhs".into(),
                Node::Variable { .. } => "let".into(),
                Node::Call { .. } => format!("arg{}", e.weight()),
                _ => String::new(),
            };
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
