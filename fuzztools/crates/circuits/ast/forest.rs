use super::{
    nodes::{Node, NodeKind},
    operators::Operator,
    types::*,
};
use crate::circuits::{context::Context, scope::Scope};
use petgraph::{
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{EdgeRef, IntoEdgeReferences},
    Direction,
};
use rand::{seq::IteratorRandom, Rng};
use std::{collections::HashMap, fmt::Write, path::Path};

#[derive(Clone, Default)]
pub enum ForestType {
    #[default]
    Main,
    Function,
    Lambda,
    If,
    For,
}

#[derive(Debug, Clone, Default)]
/// This provides a directed graph structure for representing a block of expressions, that
/// is, the body of a function, the body of a loop, etc...
pub struct Forest {
    pub graph: StableDiGraph<Node, usize>,

    // This hashmaps are used to achieve O(1) lookups to interesting data inside `graph`.
    pub types: HashMap<Type, Vec<NodeIndex>>,
    pub type_kinds: HashMap<TypeKind, Vec<NodeIndex>>,
    pub node_kinds: HashMap<NodeKind, Vec<NodeIndex>>,
    pub operators: HashMap<Operator, Vec<NodeIndex>>,
    pub mutables: HashMap<NodeIndex, String>,

    // Tracks callable lambdas: (accessor_expression, params, return_type)
    // Includes direct lambda variables and lambdas nested in structs/arrays/slices/tuples
    pub lambdas: Vec<(String, Lambda, NodeIndex)>,
    // Keeps track of condition expressions being used within if/else if/asserts nodes.
    pub conditions: Vec<NodeIndex>,

    pub var_counter: usize,
    pub depth: usize,

    pub return_expr: Option<String>,
}

impl Forest {
    // ────────────────────────────────────────────────────────────────────────────────
    // Node creation
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    pub fn input(&mut self, random: &mut impl Rng, name: String, ty: Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Input { name: name.clone(), ty: ty.clone() });
        self.add_to_lookups(idx, &ty);

        // To be able to call lambdas within variables, we must locate them. If they are a
        // variable/input, we could always `v()`, but what if we have it within a nested type? Then
        // we keep traversing and building the access on each step, storing it on `self.lambdas`.
        self.register_callable(random, name, &ty, Some(idx));

        idx
    }

    #[inline(always)]
    pub fn literal(&mut self, value: String, ty: Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Literal { value, ty: ty.clone() });
        self.add_to_lookups(idx, &ty);

        idx
    }

    #[inline(always)]
    pub fn variable(
        &mut self,
        random: &mut impl Rng,
        name: String,
        ty: Type,
        mutable: bool,
        shadow: bool,
        value: NodeIndex,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Variable {
            name: name.clone(),
            ty: ty.clone(),
            mutable,
            shadow,
        });
        self.add_edge(idx, value, 0, &ty);
        self.add_to_lookups(idx, &ty);

        // If this is a mutable `Variable`, register it for assignment generation.
        if mutable {
            self.mutables.insert(idx, name.clone());
        }

        // To be able to call lambdas within variables, we must locate them. If they are a
        // variable/input, we could always `v()`, but what if we have it within a nested type? Then
        // we keep traversing and building the access on each step, storing it on `self.lambdas`.
        self.register_callable(random, name, &ty, Some(idx));

        idx
    }

    #[inline(always)]
    pub fn operator(
        &mut self,
        op: Operator,
        ret: Type,
        lhs: NodeIndex,
        rhs: Option<NodeIndex>,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Operator { op, ret: ret.clone() });
        self.add_edge(idx, lhs, 0, &ret);
        self.add_to_lookups(idx, &ret);

        // In unary operations, there is no right operand, so `rhs` is `None`
        if let Some(rhs) = rhs {
            self.add_edge(idx, rhs, 1, &ret);
        }

        idx
    }

    #[inline(always)]
    pub fn index(&mut self, parent: NodeIndex, value: usize, ty: &Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Index { value });
        self.add_edge(idx, parent, 0, &ty);
        self.add_to_lookups(idx, &ty);

        idx
    }

    #[inline(always)]
    pub fn tuple_index(&mut self, parent: NodeIndex, value: usize, ty: &Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::TupleIndex { value });
        self.add_edge(idx, parent, 0, &ty);
        self.add_to_lookups(idx, &ty);

        idx
    }

    #[inline(always)]
    pub fn field_access(&mut self, parent: NodeIndex, name: String, ty: &Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::FieldAccess { name });
        self.add_edge(idx, parent, 0, &ty);
        self.add_to_lookups(idx, &ty);

        idx
    }

    #[inline(always)]
    pub fn call(
        &mut self,
        name: String,
        ret: Type,
        args: Vec<NodeIndex>,
        parent: NodeIndex,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Call { name, ret: ret.clone() });
        self.add_edge(idx, parent, 0, &ret);
        self.add_to_lookups(idx, &ret);

        for (pos, arg) in args.into_iter().enumerate() {
            self.add_edge(idx, arg, pos + 1, &ret);
        }

        idx
    }

    #[inline(always)]
    pub fn cast(&mut self, source: NodeIndex, target: Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Cast { target: target.clone() });
        self.add_edge(idx, source, 0, &target);
        self.add_to_lookups(idx, &target);

        idx
    }

    #[inline(always)]
    pub fn assignment(
        &mut self,
        source: NodeIndex,
        value: NodeIndex,
        op: Option<Operator>,
        ty: &Type,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Assignment { op });
        self.add_edge(idx, value, 1, &ty);
        self.add_to_lookups(idx, &ty);

        // We can't use `self.add_edge()` as assignements are a special case.
        self.graph.add_edge(idx, source, 0);
        self.remove_from_lookups(source, &ty);

        let name = self.mutables.get(&source).unwrap();
        self.mutables.insert(idx, name.clone());
        self.mutables.remove(&source);

        idx
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Graph queries
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    pub fn parent(&self, idx: NodeIndex, w: usize) -> Option<NodeIndex> {
        self.graph.edges(idx).find(|e| *e.weight() == w).map(|e| e.target())
    }

    #[inline(always)]
    pub fn left(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.parent(idx, 0)
    }

    #[inline(always)]
    pub fn right(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.parent(idx, 1)
    }

    #[inline(always)]
    pub fn has_indexable_of_type(&self, ty: &Type) -> bool {
        self.types.iter().any(|(t, nodes)| {
            let is_indexable = match t {
                Type::Array(a) => a.ty.as_ref() == ty && a.size > 0,
                Type::Slice(s) => s.ty.as_ref() == ty && s.size > 0,
                _ => false,
            };
            is_indexable && nodes.iter().any(|&i| self.is_reusable_node(i))
        })
    }

    #[inline(always)]
    pub fn has_tuples_with_type(&self, ty: &Type) -> bool {
        self.types.iter().any(|(t, nodes)| {
            matches!(t, Type::Tuple(tup) if tup.elements.contains(ty)) &&
                nodes.iter().any(|&i| self.is_reusable_node(i))
        })
    }

    #[inline(always)]
    pub fn has_struct_fields_of_type(&self, ty: &Type) -> bool {
        self.types.iter().any(|(t, nodes)| {
            matches!(t, Type::Struct(s) if s.fields.iter().any(|f| f.ty.as_ref() == ty)) &&
                nodes.iter().any(|&i| self.is_reusable_node(i))
        })
    }

    #[inline(always)]
    pub fn has_mutable_variables(&self) -> bool {
        !self.mutables.is_empty()
    }

    #[inline(always)]
    pub fn has_type_kind(&self, kind: TypeKind) -> bool {
        self.type_kinds.contains_key(&kind)
    }

    #[inline(always)]
    pub fn has_integer_types(&self) -> bool {
        self.has_type_kind(TypeKind::Signed) || self.has_type_kind(TypeKind::Unsigned)
    }

    #[inline(always)]
    pub fn has_boolean_types(&self) -> bool {
        self.has_type_kind(TypeKind::Boolean)
    }

    #[inline(always)]
    pub fn has_comparison_operators(&self) -> bool {
        self.operators.keys().any(|op| op.is_comparison())
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Graph mutations
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn swap_operands(&mut self, idx: NodeIndex) {
        let edges: Vec<_> =
            self.graph.edges(idx).map(|e| (e.id(), *e.weight(), e.target())).collect();

        if edges.len() == 2 {
            for (edge, w, target) in edges {
                self.graph.remove_edge(edge);
                self.graph.add_edge(idx, target, 1 - w);
            }
        }
    }

    #[inline(always)]
    pub fn add_operand(&mut self, idx: NodeIndex, w: usize, operand: NodeIndex, ty: &Type) {
        self.add_edge(idx, operand, w, &ty);
    }

    pub fn remove_operand(&mut self, idx: NodeIndex, w: usize) {
        if let Some(edge) = self.graph.edges(idx).find(|e| *e.weight() == w).map(|e| e.id()) {
            let target = self.graph.edge_endpoints(edge).unwrap().1;
            let target_ty = self.ty(target); // Get type before removing edge
            self.graph.remove_edge(edge);

            // Re-enable single-use node if now unused
            if self.is_single_use(target) && self.use_count(target) == 0 {
                self.add_to_lookups(target, &target_ty);
            }
            self.remove_if_orphaned(target);
        }
    }

    pub fn redirect_edges(&mut self, old: NodeIndex, new: NodeIndex) {
        let edges: Vec<_> =
            self.graph.edges(old).map(|e| (e.id(), e.target(), *e.weight())).collect();

        for (edge, target, weight) in edges {
            self.graph.remove_edge(edge);
            self.graph.add_edge(new, target, weight);
        }
    }

    pub fn set_operator(&mut self, idx: NodeIndex, new_op: Operator) {
        if let Node::Operator { op, .. } = &mut self.graph[idx] {
            let old_op = *op;
            *op = new_op;

            if let Some(v) = self.operators.get_mut(&old_op) {
                v.retain(|&x| x != idx);
            }
            self.operators.entry(new_op).or_default().push(idx);
        }
    }

    /// Remove node if it has no outgoing edges (orphaned leaf).
    pub fn remove_if_orphaned(&mut self, idx: NodeIndex) {
        if self.graph.edges(idx).next().is_some() {
            return;
        }
        if self.conditions.contains(&idx) {
            return;
        }
        self.graph.remove_node(idx); // @todo
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Type queries
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn ty(&self, idx: NodeIndex) -> Type {
        match &self.graph[idx] {
            Node::Input { ty, .. } | Node::Literal { ty, .. } | Node::Variable { ty, .. } => {
                ty.clone()
            }
            Node::Operator { ret, .. } | Node::Call { ret, .. } => ret.clone(),
            Node::Index { .. } => match self.ty(self.left(idx).unwrap()) {
                Type::Array(a) => *a.ty,
                Type::Slice(s) => *s.ty,
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
            Node::Cast { target } => target.clone(),
            Node::Assignment { .. } => self.ty(self.left(idx).unwrap()),
            Node::ForLoop { .. } | Node::If { .. } | Node::Assert { .. } => Type::Empty,
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Node classification
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    pub fn is_single_use(&self, idx: NodeIndex) -> bool {
        matches!(
            self.graph.node_weight(idx),
            Some(
                Node::Literal { .. } |
                    Node::Operator { .. } |
                    Node::Index { .. } |
                    Node::TupleIndex { .. } |
                    Node::FieldAccess { .. } |
                    Node::Call { .. } |
                    Node::Cast { .. }
            )
        )
    }

    #[inline(always)]
    pub fn is_reusable_node(&self, idx: NodeIndex) -> bool {
        matches!(
            self.graph.node_weight(idx),
            Some(Node::Variable { .. } | Node::Input { .. } | Node::Assignment { .. })
        )
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Hashmap operations
    // ────────────────────────────────────────────────────────────────────────────────

    fn add_to_lookups(&mut self, idx: NodeIndex, ty: &Type) {
        let node = &self.graph[idx];

        self.types.entry(ty.clone()).or_default().push(idx);
        self.type_kinds.entry(ty.kind()).or_default().push(idx);
        self.node_kinds.entry(node.kind()).or_default().push(idx);

        if let Node::Operator { op, .. } = node {
            self.operators.entry(*op).or_default().push(idx);
        }
    }

    fn remove_from_lookups(&mut self, idx: NodeIndex, ty: &Type) {
        let node = &self.graph[idx];

        if let Some(v) = self.types.get_mut(ty) {
            v.retain(|&x| x != idx);
        }
        if let Some(v) = self.type_kinds.get_mut(&ty.kind()) {
            v.retain(|&x| x != idx);
        }
        if let Some(v) = self.node_kinds.get_mut(&node.kind()) {
            v.retain(|&x| x != idx);
        }

        if let Node::Operator { op, .. } = node {
            if let Some(v) = self.operators.get_mut(op) {
                v.retain(|&x| x != idx);
            }
        }
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Edge management
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    fn add_edge(&mut self, from: NodeIndex, to: NodeIndex, weight: usize, ty: &Type) {
        self.graph.add_edge(from, to, weight);

        if self.is_single_use(to) {
            self.remove_from_lookups(to, &ty);
        }
    }

    #[inline(always)]
    pub fn use_count(&self, idx: NodeIndex) -> usize {
        self.graph.edges_directed(idx, Direction::Incoming).count()
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Helpers
    // ────────────────────────────────────────────────────────────────────────────────

    pub fn set_return_expression(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        if let Some((ret, _)) = &scope.ret {
            let value = self
                .types
                .get(ret)
                .into_iter()
                .flatten()
                .filter(|&&i| matches!(self.graph[i], Node::Variable { .. } | Node::Input { .. }))
                .choose(random)
                .map(|&i| self.get_expr_for_node(i))
                .unwrap_or_else(|| ret.random_value(random, ctx, scope));

            self.return_expr = Some(value);
        }
    }

    fn register_callable(
        &mut self,
        random: &mut impl Rng,
        accessor: String,
        ty: &Type,
        idx: Option<NodeIndex>,
    ) {
        match ty {
            Type::Lambda(lambda) => {
                self.lambdas.push((accessor, lambda.clone(), idx.unwrap()));
            }
            Type::Struct(s) => {
                for field in &s.fields {
                    self.register_callable(
                        random,
                        format!("{}.{}", accessor, field.name), // @todo ask them how to do this cause it reverts
                        &field.ty,
                        idx,
                    );
                }
            }
            Type::Array(arr) if arr.size > 0 => {
                let i = random.random_range(0..arr.size);
                self.register_callable(random, format!("{}[{}]", accessor, i), &arr.ty, idx);
            }
            Type::Slice(slice) if slice.size > 0 => {
                let i = random.random_range(0..slice.size);
                self.register_callable(random, format!("{}[{}]", accessor, i), &slice.ty, idx);
            }
            Type::Tuple(tuple) => {
                for (i, elem_ty) in tuple.elements.iter().enumerate() {
                    self.register_callable(random, format!("{}.{}", accessor, i), elem_ty, idx); // @todo ask them how to do this cause it reverts
                }
            }
            _ => {}
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
            let w = e.weight();
            let label = match &self.graph[e.source()] {
                Node::Operator { op, .. } => {
                    if op.is_unary() {
                        "unary"
                    } else if *w == 0 {
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
                Node::Call { .. } => {
                    let _ = writeln!(
                        out,
                        "  {} -> {} [label=\"arg{}\" dir=back]",
                        e.source().index(),
                        e.target().index(),
                        w
                    );
                    continue;
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
        std::fs::write(path, out).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_forest() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        // Add two literal nodes
        let lhs =
            forest.literal("1".to_string(), Type::Integer(Integer { bits: 32, signed: true }));
        let rhs =
            forest.literal("2".to_string(), Type::Integer(Integer { bits: 32, signed: true }));
        // Add operator node for addition
        let op = forest.operator(
            Operator::Add,
            Type::Integer(Integer { bits: 32, signed: true }),
            lhs,
            Some(rhs),
        );

        // Add variable node
        let var = forest.variable(
            &mut random,
            "result".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
            false,
            false,
            op,
        );

        forest.redirect_edges(op, var);
        forest.remove_if_orphaned(op);

        // Save forest as DOT file
        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_forest.dot"));
    }

    #[test]
    fn test_swap_operands() {
        let mut forest = Forest::default();

        // Add two literal nodes
        let lhs =
            forest.literal("1".to_string(), Type::Integer(Integer { bits: 32, signed: true }));
        let rhs = forest.literal("2".to_string(), Type::Integer(Integer { bits: 8, signed: true }));
        // Add operator node for addition
        let op = forest.operator(
            Operator::Add,
            Type::Integer(Integer { bits: 32, signed: true }),
            lhs,
            Some(rhs),
        );

        assert_eq!(forest.left(op).unwrap(), lhs);
        assert_eq!(forest.right(op).unwrap(), rhs);

        forest.swap_operands(op);

        assert_eq!(forest.left(op).unwrap(), rhs);
        assert_eq!(forest.right(op).unwrap(), lhs);
    }

    #[test]
    fn test_set_operator() {
        let mut forest = Forest::default();

        let lhs =
            forest.literal("1".to_string(), Type::Integer(Integer { bits: 32, signed: true }));
        let rhs =
            forest.literal("2".to_string(), Type::Integer(Integer { bits: 32, signed: true }));
        let op = forest.operator(
            Operator::Add,
            Type::Integer(Integer { bits: 32, signed: true }),
            lhs,
            Some(rhs),
        );

        assert!(forest.operators.get(&Operator::Add).unwrap().contains(&op));

        forest.set_operator(op, Operator::Sub);

        match &forest.graph[op] {
            Node::Operator { op: o, .. } => assert_eq!(*o, Operator::Sub),
            _ => panic!("Expected Operator node"),
        }
        assert!(!forest.operators.get(&Operator::Add).unwrap().contains(&op));
        assert!(forest.operators.get(&Operator::Sub).unwrap().contains(&op));
    }
}
