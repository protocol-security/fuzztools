use crate::circuits::ast::types::TypeKind;

use super::{nodes::Node, operators::Operator, types::Type};
use petgraph::{
    algo::toposort,
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{EdgeRef, IntoEdgeReferences},
    Direction,
};
use std::{collections::HashMap, fmt::Write, io, path::Path};

// ────────────────────────────────────────────────────────────────────────────────
// Forest definition
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct Forest {
    pub(crate) inner: StableDiGraph<Node, usize>,

    pub(crate) types: HashMap<Type, Vec<NodeIndex>>,
    pub(crate) kinds: HashMap<TypeKind, Vec<NodeIndex>>,
    pub(crate) operators: HashMap<Operator, Vec<NodeIndex>>,
    pub(crate) mutables: HashMap<NodeIndex, String>,
    pub(crate) reusables: HashMap<Type, Vec<NodeIndex>>,
    pub(crate) indexables: HashMap<Type, Vec<(NodeIndex, usize)>>,
    pub(crate) tuple_indexables: HashMap<Type, Vec<(NodeIndex, usize)>>,

    pub(crate) return_expr: Option<String>,

    // Helpers
    pub(crate) var_count: usize,
}

// ────────────────────────────────────────────────────────────────────────────────
// Forest definition
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub(crate) fn _input(&mut self, name: String, ty: &Type) -> NodeIndex {
        let idx = self.inner.add_node(Node::Input { name, ty: ty.clone() });
        self.register_reusable(idx, ty);
        self.register(idx, ty);

        idx
    }

    pub(crate) fn literal(&mut self, value: String, ty: &Type) -> NodeIndex {
        let idx = self.inner.add_node(Node::Literal { value, ty: ty.clone() });
        self.register(idx, ty);

        idx
    }

    pub(crate) fn variable(
        &mut self,
        name: String,
        ty: &Type,
        mutable: bool,
        value: NodeIndex,
    ) -> NodeIndex {
        let idx =
            self.inner.add_node(Node::Variable { name: name.clone(), ty: ty.clone(), mutable });
        self.register_reusable(idx, ty);
        self.register(idx, ty);

        self.add_edge(value, idx, 0);

        // We keep track of what variables we can mutate
        if mutable {
            self.mutables.insert(idx, name);
        }

        idx
    }

    pub(crate) fn operator(
        &mut self,
        op: Operator,
        ty: &Type,
        left: NodeIndex,
        right: Option<NodeIndex>,
    ) -> NodeIndex {
        let idx = self.inner.add_node(Node::Operator { op, ty: ty.clone() });
        self.operators.entry(op).or_default().push(idx);
        self.register(idx, ty);

        self.add_edge(left, idx, 0);

        // In unary operations, there is no right operand, so `right` is `None`
        if let Some(right) = right {
            self.add_edge(right, idx, 1);
        }

        idx
    }

    pub(crate) fn index(&mut self, index: usize, ty: &Type, parent: NodeIndex) -> NodeIndex {
        let idx = self.inner.add_node(Node::Index { index, ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(parent, idx, 0);

        idx
    }

    pub(crate) fn tuple_index(&mut self, index: usize, ty: &Type, parent: NodeIndex) -> NodeIndex {
        let idx = self.inner.add_node(Node::TupleIndex { index, ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(parent, idx, 0);

        idx
    }

    pub(crate) fn cast(&mut self, ty: &Type, parent: NodeIndex) -> NodeIndex {
        let idx = self.inner.add_node(Node::Cast { ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(parent, idx, 0);

        idx
    }

    pub(crate) fn assignement(
        &mut self,
        op: Option<Operator>,
        ty: &Type,
        value: NodeIndex,
        variable: NodeIndex,
    ) -> NodeIndex {
        let idx = self.inner.add_node(Node::Assignment { op, ty: ty.clone() });
        self.register_reusable(idx, ty);
        self.register(idx, ty);

        self.inner.add_edge(idx, variable, 0);
        self.add_edge(value, idx, 1);

        // Now, we must shadow the old variable to avoid losing track of the latest state of the
        // mutable variable
        self.unregister(variable, ty);
        self.unregister_reusable(variable, ty);

        // As well as update its entry in `self.mutables`
        let name = self.mutables.remove(&variable).unwrap();
        self.mutables.insert(idx, name);

        idx
    }

    #[inline(always)]
    pub(crate) fn left(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.inner.edges(idx).find(|e| *e.weight() == 0).map(|e| e.target())
    }

    #[inline(always)]
    pub(crate) fn right(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.inner.edges(idx).find(|e| *e.weight() == 1).map(|e| e.target())
    }

    pub(crate) fn swap_edges(&mut self, idx: NodeIndex) {
        let left = self.left(idx);
        let right = self.right(idx);

        if let (Some(l), Some(r)) = (left, right) {
            // Remove existing edges
            let edges: Vec<_> = self.inner.edges(idx).map(|e| e.id()).collect();
            for edge_id in edges {
                self.inner.remove_edge(edge_id);
            }

            // Add swapped edges
            self.inner.add_edge(idx, r, 0);
            self.inner.add_edge(idx, l, 1);
        }
    }

    pub(crate) fn replace_binary(
        &mut self,
        idx: NodeIndex,
        new_left: NodeIndex,
        new_right: NodeIndex,
    ) {
        let old_left = self.left(idx);
        let old_right = self.right(idx);

        // Remove existing edges
        let edges: Vec<_> = self.inner.edges(idx).map(|e| e.id()).collect();
        for edge_id in edges {
            self.inner.remove_edge(edge_id);
        }

        // Add new edges
        self.inner.add_edge(idx, new_left, 0);
        self.inner.add_edge(idx, new_right, 1);

        // Remove old children if they became orphans and aren't reused
        if let Some(old) = old_left {
            if old != new_left && old != new_right {
                self.remove_if_orphan(old);
            }
        }
        if let Some(old) = old_right {
            if old != new_left && old != new_right {
                self.remove_if_orphan(old);
            }
        }
    }

    pub(crate) fn replace_unary(&mut self, idx: NodeIndex, new_child: NodeIndex) {
        let old_left = self.left(idx);
        let old_right = self.right(idx);

        // Remove existing edges
        let edges: Vec<_> = self.inner.edges(idx).map(|e| e.id()).collect();
        for edge_id in edges {
            self.inner.remove_edge(edge_id);
        }

        // Add new edge
        self.inner.add_edge(idx, new_child, 0);

        // Remove old children if they became orphans
        if let Some(old) = old_left {
            if old != new_child {
                self.remove_if_orphan(old);
            }
        }
        if let Some(old) = old_right {
            if old != new_child {
                self.remove_if_orphan(old);
            }
        }
    }

    pub(crate) fn set_op(&mut self, idx: NodeIndex, op: Operator) {
        if let Node::Operator { op: ref mut o, .. } = self.inner[idx] {
            *o = op;
        }
    }

    pub(crate) fn remove_if_orphan(&mut self, idx: NodeIndex) {
        // Don't remove if node doesn't exist
        if self.inner.node_weight(idx).is_none() {
            return;
        }

        // Check if this node has any incoming edges (is used by someone)
        if self.inner.edges_directed(idx, Direction::Incoming).next().is_some() {
            return; // Not an orphan, still in use
        }

        // Collect children before removing
        let children: Vec<NodeIndex> = self.inner.edges(idx).map(|e| e.target()).collect();

        // Remove the node (also removes its edges)
        self.inner.remove_node(idx);

        // Recursively check if children became orphans
        for child in children {
            self.remove_if_orphan(child);
        }
    }

    fn add_edge(&mut self, from: NodeIndex, to: NodeIndex, weight: usize) {
        self.inner.add_edge(to, from, weight);

        let node = self.inner[from].clone();
        if !node.is_single_use() {
            return;
        }

        let ty = node.ty();
        self.unregister(from, &ty);

        if let Node::Operator { op, .. } = node {
            self.operators.get_mut(&op).unwrap().retain(|&x| x != from);
        }
    }

    #[inline(always)]
    fn register(&mut self, idx: NodeIndex, ty: &Type) {
        self.types.entry(ty.clone()).or_default().push(idx);
        self.kinds.entry(ty.kind()).or_default().push(idx);
    }

    #[inline(always)]
    fn unregister(&mut self, idx: NodeIndex, ty: &Type) {
        self.types.get_mut(ty).unwrap().retain(|&x| x != idx);
        self.kinds.get_mut(&ty.kind()).unwrap().retain(|&x| x != idx);
    }

    fn register_reusable(&mut self, idx: NodeIndex, ty: &Type) {
        self.reusables.entry(ty.clone()).or_default().push(idx);

        // Also register in indexables/tuple_indexables based on type structure
        match ty {
            Type::Array(a) if a.size > 0 => {
                let elem_ty = a.ty.as_ref();
                self.indexables.entry(elem_ty.clone()).or_default().push((idx, a.size));
            }
            Type::Slice(s) if s.size > 0 => {
                let elem_ty = s.ty.as_ref();
                self.indexables.entry(elem_ty.clone()).or_default().push((idx, s.size));
            }
            Type::Tuple(t) => {
                for (pos, elem_ty) in t.elements.iter().enumerate() {
                    self.tuple_indexables.entry(elem_ty.clone()).or_default().push((idx, pos));
                }
            }
            _ => {}
        }
    }

    fn unregister_reusable(&mut self, idx: NodeIndex, ty: &Type) {
        self.reusables.get_mut(ty).unwrap().retain(|&x| x != idx);

        match ty {
            Type::Array(a) if a.size > 0 => {
                let elem_ty = a.ty.as_ref();
                self.indexables.get_mut(elem_ty).unwrap().retain(|(x, _)| *x != idx);
            }
            Type::Slice(s) if s.size > 0 => {
                let elem_ty = s.ty.as_ref();
                self.indexables.get_mut(elem_ty).unwrap().retain(|(x, _)| *x != idx);
            }
            Type::Tuple(t) => {
                for elem_ty in &t.elements {
                    self.tuple_indexables.get_mut(elem_ty).unwrap().retain(|(x, _)| *x != idx);
                }
            }
            _ => {}
        }
    }

    #[inline(always)]
    pub(crate) fn get_reusables(&self, ty: &Type) -> &[NodeIndex] {
        match self.reusables.get(ty) {
            Some(v) => v.as_slice(),
            None => &[],
        }
    }

    #[inline(always)]
    pub(crate) fn get_indexables(&self, ty: &Type) -> &[(NodeIndex, usize)] {
        match self.indexables.get(ty) {
            Some(v) => v.as_slice(),
            None => &[],
        }
    }

    #[inline(always)]
    pub(crate) fn get_tuple_indexables(&self, ty: &Type) -> &[(NodeIndex, usize)] {
        match self.tuple_indexables.get(ty) {
            Some(v) => v.as_slice(),
            None => &[],
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub fn format(&self, indent: &str) -> String {
        let mut exprs: HashMap<usize, String> = HashMap::new();
        let mut out = String::new();

        for &idx in toposort(&self.inner, None).unwrap().iter().rev() {
            let left = self.left(idx).map(|i| &exprs[&i.index()]);
            let right = self.right(idx).map(|i| &exprs[&i.index()]);

            let expr = match &self.inner[idx] {
                Node::Input { name, .. } | Node::Literal { value: name, .. } => name.clone(),

                Node::Variable { name, mutable, ty } => {
                    let m = if *mutable { "mut " } else { "" };
                    let _ = writeln!(out, "{indent}let {m}{name}: {ty} = {};", left.unwrap());
                    name.clone()
                }

                Node::Operator { op, .. } => match right {
                    Some(r) => format!("({} {op} {r})", left.unwrap()),
                    None => format!("({op}{})", left.unwrap()),
                },

                Node::Index { index, .. } => format!("{}[{index}]", left.unwrap()),
                Node::TupleIndex { index, .. } => format!("{}.{index}", left.unwrap()),
                Node::Cast { ty, .. } => format!("({} as {ty})", left.unwrap()),

                Node::Assignment { op, .. } => {
                    let op = op.map(|o| o.to_string()).unwrap_or_default();
                    let l = left.unwrap();
                    let _ = writeln!(out, "{indent}{l} {op}= {};", right.unwrap());
                    l.clone()
                }
            };

            exprs.insert(idx.index(), expr);
        }

        if let Some(ret) = &self.return_expr {
            let _ = writeln!(out, "{indent}{ret}");
        }

        out
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// DOT export
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub(crate) fn save_as_dot(&self, path: &Path) -> Result<(), io::Error> {
        let mut out = String::from(
            "digraph {\n  rankdir=BT;\n  node [fontname=monospace shape=box style=\"filled,rounded\" margin=\"0.4,0.2\"];\n"
        );

        for n in self.inner.node_indices() {
            let node = &self.inner[n];
            let _ = writeln!(
                out,
                "  {} [label=\"{}\" fillcolor=\"{}\"]",
                n.index(),
                node,
                node.color()
            );
        }

        for e in self.inner.edge_references() {
            let w = e.weight();
            let label = match &self.inner[e.source()] {
                Node::Operator { .. } => {
                    if *w == 0 {
                        "lhs"
                    } else {
                        "rhs"
                    }
                }
                Node::Variable { .. } => "let",
                Node::Assignment { .. } => {
                    if *w == 0 {
                        "source"
                    } else {
                        "value"
                    }
                }
                _ => "",
            };
            let _ = writeln!(
                out,
                "  {} -> {} [label=\"{}\" dir=back]",
                e.source().index(),
                e.target().index(),
                label
            );
        }

        out.push_str("}\n");
        std::fs::write(path, out)
    }
}
