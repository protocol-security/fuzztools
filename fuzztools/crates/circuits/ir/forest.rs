use crate::circuits::ir::types::TypeKind;

use super::{nodes::Node, operators::Operator, types::Type};
use petgraph::{
    algo::toposort,
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{EdgeRef, IntoEdgeReferences},
};
use std::{collections::HashMap, fmt::Write, io, path::Path};

// ────────────────────────────────────────────────────────────────────────────────
// Forest definition
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct Forest {
    pub inner: StableDiGraph<Node, usize>,

    pub types: HashMap<Type, Vec<NodeIndex>>,
    pub kinds: HashMap<TypeKind, Vec<NodeIndex>>,
    pub mutables: HashMap<NodeIndex, String>,
    pub reusables: HashMap<Type, Vec<NodeIndex>>,
    pub indexables: HashMap<Type, Vec<(NodeIndex, usize)>>,
    pub tuple_indexables: HashMap<Type, Vec<(NodeIndex, usize)>>,
    pub struct_indexables: HashMap<Type, Vec<(NodeIndex, String)>>,

    pub return_expr: Option<String>,

    // Helpers
    pub var_count: usize,
}

// ────────────────────────────────────────────────────────────────────────────────
// Forest definition
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub fn input(&mut self, name: String, ty: &Type) -> NodeIndex {
        let idx = self.inner.add_node(Node::Input { name, ty: ty.clone() });
        self.register_reusable(idx, ty);
        self.register(idx, ty);

        idx
    }

    pub fn literal(&mut self, value: String, ty: &Type) -> NodeIndex {
        let idx = self.inner.add_node(Node::Literal { value, ty: ty.clone() });
        self.register(idx, ty);

        idx
    }

    pub fn variable(
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

    pub fn operator(
        &mut self,
        op: Operator,
        ty: &Type,
        left: NodeIndex,
        right: Option<NodeIndex>,
    ) -> NodeIndex {
        let idx = self.inner.add_node(Node::Operator { op, ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(left, idx, 0);

        // In unary operations, there is no right operand, so `right` is `None`
        if let Some(right) = right {
            self.add_edge(right, idx, 1);
        }

        idx
    }

    pub fn index(&mut self, index: usize, ty: &Type, parent: NodeIndex) -> NodeIndex {
        let idx = self.inner.add_node(Node::Index { index, ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(parent, idx, 0);

        idx
    }

    pub fn tuple_index(&mut self, index: usize, ty: &Type, parent: NodeIndex) -> NodeIndex {
        let idx = self.inner.add_node(Node::TupleIndex { index, ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(parent, idx, 0);

        idx
    }

    pub fn struct_field(&mut self, field: String, ty: &Type, parent: NodeIndex) -> NodeIndex {
        let idx = self.inner.add_node(Node::StructField { field, ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(parent, idx, 0);

        idx
    }

    pub fn cast(&mut self, ty: &Type, parent: NodeIndex) -> NodeIndex {
        let idx = self.inner.add_node(Node::Cast { ty: ty.clone() });
        self.register(idx, ty);

        self.add_edge(parent, idx, 0);

        idx
    }

    pub fn assignement(
        &mut self,
        op: Option<Operator>,
        ty: &Type,
        value: NodeIndex,
        variable: NodeIndex,
    ) -> NodeIndex {
        let name = self.mutables.remove(&variable).unwrap();
        let idx = self.inner.add_node(Node::Assignment { name: name.clone(), op, ty: ty.clone() });
        self.register_reusable(idx, ty);
        self.register(idx, ty);

        self.inner.add_edge(idx, variable, 0);
        self.add_edge(value, idx, 1);

        // Now, we must shadow the old variable to avoid losing track of the latest state of the
        // mutable variable
        self.unregister(variable, ty);
        self.unregister_reusable(variable, ty);

        // As well as update its entry in `self.mutables`
        self.mutables.insert(idx, name);

        idx
    }

    pub fn indexed_assignment(
        &mut self,
        op: Option<Operator>,
        pos: usize,
        elem_ty: &Type,
        value: NodeIndex,
        parent: NodeIndex,
    ) -> NodeIndex {
        let parent_ty = self.ty(parent);
        let name = self.mutables.remove(&parent).unwrap();

        let lhs = self.index(pos, elem_ty, parent);
        let idx =
            self.inner.add_node(Node::Assignment { name: name.clone(), op, ty: parent_ty.clone() });

        self.inner.add_edge(idx, lhs, 0);
        self.add_edge(value, idx, 1);

        // Now, we must shadow the old variable to avoid losing track of the latest state of the
        // mutable variable
        self.unregister(parent, &parent_ty);
        self.unregister_reusable(parent, &parent_ty);

        // The assignment becomes the new "current state" of the parent
        self.register(idx, &parent_ty);
        self.register_reusable(idx, &parent_ty);

        // As well as update its entry in `self.mutables`
        self.mutables.insert(idx, name);

        idx
    }

    pub fn tuple_field_assignment(
        &mut self,
        op: Option<Operator>,
        pos: usize,
        field_ty: &Type,
        value: NodeIndex,
        parent: NodeIndex,
    ) -> NodeIndex {
        let parent_ty = self.ty(parent);
        let name = self.mutables.remove(&parent).unwrap();

        let lhs = self.tuple_index(pos, field_ty, parent);
        let idx =
            self.inner.add_node(Node::Assignment { name: name.clone(), op, ty: parent_ty.clone() });

        self.inner.add_edge(idx, lhs, 0);
        self.add_edge(value, idx, 1);

        // Shadow the parent - it's now stale
        self.unregister(parent, &parent_ty);
        self.unregister_reusable(parent, &parent_ty);

        // Now, we must shadow the old variable to avoid losing track of the latest state of the
        // mutable variable
        self.register(idx, &parent_ty);
        self.register_reusable(idx, &parent_ty);

        // As well as update its entry in `self.mutables`
        self.mutables.insert(idx, name);

        idx
    }

    pub fn struct_field_assignment(
        &mut self,
        op: Option<Operator>,
        field_name: String,
        field_ty: &Type,
        value: NodeIndex,
        parent: NodeIndex,
    ) -> NodeIndex {
        let parent_ty = self.ty(parent);
        let name = self.mutables.remove(&parent).unwrap();

        let lhs = self.struct_field(field_name, field_ty, parent);
        let idx =
            self.inner.add_node(Node::Assignment { name: name.clone(), op, ty: parent_ty.clone() });

        self.inner.add_edge(idx, lhs, 0);
        self.add_edge(value, idx, 1);

        // Shadow the parent - it's now stale
        self.unregister(parent, &parent_ty);
        self.unregister_reusable(parent, &parent_ty);

        // Register the assignment as the new current state
        self.register(idx, &parent_ty);
        self.register_reusable(idx, &parent_ty);

        // Update its entry in `self.mutables`
        self.mutables.insert(idx, name);

        idx
    }

    #[inline(always)]
    pub fn left(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.inner.edges(idx).find(|e| *e.weight() == 0).map(|e| e.target())
    }

    #[inline(always)]
    pub fn right(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.inner.edges(idx).find(|e| *e.weight() == 1).map(|e| e.target())
    }

    #[inline(always)]
    pub fn ty(&self, idx: NodeIndex) -> Type {
        self.inner[idx].ty()
    }

    #[inline(always)]
    pub fn get_reusables(&self, ty: &Type) -> &[NodeIndex] {
        match self.reusables.get(ty) {
            Some(v) => v.as_slice(),
            None => &[],
        }
    }

    #[inline(always)]
    pub fn get_indexables(&self, ty: &Type) -> &[(NodeIndex, usize)] {
        match self.indexables.get(ty) {
            Some(v) => v.as_slice(),
            None => &[],
        }
    }

    #[inline(always)]
    pub fn get_tuple_indexables(&self, ty: &Type) -> &[(NodeIndex, usize)] {
        match self.tuple_indexables.get(ty) {
            Some(v) => v.as_slice(),
            None => &[],
        }
    }

    #[inline(always)]
    pub fn get_struct_indexables(&self, ty: &Type) -> &[(NodeIndex, String)] {
        match self.struct_indexables.get(ty) {
            Some(v) => v.as_slice(),
            None => &[],
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
            Type::Struct(s) => {
                for field in &s.fields {
                    self.struct_indexables
                        .entry(field.ty.clone())
                        .or_default()
                        .push((idx, field.name.clone()));
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
            Type::Struct(s) => {
                for field in &s.fields {
                    self.struct_indexables.get_mut(&field.ty).unwrap().retain(|(x, _)| *x != idx);
                }
            }
            _ => {}
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
                Node::StructField { field, .. } => format!("{}.{field}", left.unwrap()),
                Node::Cast { ty, .. } => format!("({} as {ty})", left.unwrap()),

                Node::Assignment { name, op, .. } => {
                    let op_str = op.map(|o| o.to_string()).unwrap_or_default();
                    let _ =
                        writeln!(out, "{indent}{} {op_str}= {};", left.unwrap(), right.unwrap());
                    name.clone()
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
    pub fn save_as_dot(&self, path: &Path) -> Result<(), io::Error> {
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
