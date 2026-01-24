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

    // This hashmaps are used to achieve O(1) lookups to interesting data inside the `graph`
    pub types: HashMap<Type, Vec<NodeIndex>>,
    pub type_kinds: HashMap<TypeKind, Vec<NodeIndex>>,

    pub node_kinds: HashMap<NodeKind, Vec<NodeIndex>>,
    pub operators: HashMap<Operator, Vec<NodeIndex>>,

    // Tracks callable lambdas: (accessor_expression, params, return_type)
    // Includes direct lambda variables and lambdas nested in structs/arrays/slices/tuples
    pub lambdas: Vec<(String, Lambda, NodeIndex)>,

    // This hashmap holds which variables from the outer scope (if any) are mutable and can be
    // mutable within this sub-forest
    pub mutables: HashMap<String, NodeIndex>,

    // Keeps track of condition expressions being used within if/else if/asserts
    pub conditions: Vec<NodeIndex>,

    pub var_counter: usize,
    pub depth: usize,

    pub return_expr: Option<String>,
}

impl Forest {
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
    // Node creation
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    pub fn input(&mut self, random: &mut impl Rng, name: String, ty: Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Input { name, ty: ty.clone() });
        self.register(random, idx, NodeKind::Input, &ty, None);

        idx
    }

    #[inline(always)]
    pub fn literal(&mut self, random: &mut impl Rng, value: String, ty: Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Literal { value, ty: ty.clone() });
        self.register(random, idx, NodeKind::Input, &ty, None);

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
        let idx = self.graph.add_node(Node::Variable { name, ty: ty.clone(), mutable, shadow });
        self.register(random, idx, NodeKind::Variable, &ty, None);
        self.graph.add_edge(idx, value, 0);

        idx
    }

    #[inline(always)]
    pub fn operator(
        &mut self,
        random: &mut impl Rng,
        op: Operator,
        ret: Type,
        lhs: NodeIndex,
        rhs: Option<NodeIndex>,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Operator { op, ret: ret.clone() });
        self.register(random, idx, NodeKind::Operator, &ret, Some(op));
        self.graph.add_edge(idx, lhs, 0);

        // In unary operations, there is no right operand, so `rhs` is `None`
        if let Some(rhs) = rhs {
            self.graph.add_edge(idx, rhs, 1);
        }

        idx
    }

    #[inline(always)]
    pub fn index(
        &mut self,
        random: &mut impl Rng,
        parent: NodeIndex,
        value: usize,
        ty: &Type,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Index { value });
        self.register(random, idx, NodeKind::Index, ty, None);
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    #[inline(always)]
    pub fn tuple_index(
        &mut self,
        random: &mut impl Rng,
        parent: NodeIndex,
        value: usize,
        ty: &Type,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::TupleIndex { value });
        self.register(random, idx, NodeKind::TupleIndex, ty, None);
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    #[inline(always)]
    pub fn field_access(
        &mut self,
        random: &mut impl Rng,
        parent: NodeIndex,
        name: String,
        ty: &Type,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::FieldAccess { name });
        self.register(random, idx, NodeKind::FieldAccess, ty, None);
        self.graph.add_edge(idx, parent, 0);

        idx
    }

    #[inline(always)]
    pub fn call(
        &mut self,
        random: &mut impl Rng,
        name: String,
        ret: Type,
        args: Vec<NodeIndex>,
        parent: NodeIndex,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Call { name, ret: ret.clone() });
        self.register(random, idx, NodeKind::Call, &ret, None);
        self.graph.add_edge(idx, parent, 0);

        for (pos, arg) in args.into_iter().enumerate() {
            self.graph.add_edge(idx, arg, pos + 1);
        }

        idx
    }

    #[inline(always)]
    pub fn cast(&mut self, random: &mut impl Rng, source: NodeIndex, target: Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Cast { target: target.clone() });
        self.register(random, idx, NodeKind::Cast, &target, None);
        self.graph.add_edge(idx, source, 0);

        idx
    }

    /// Creates an assignment node.
    /// - `source`: The variable being assigned to (edge 0)
    /// - `value`: The expression to assign (edge 1)
    ///
    /// If `op` is Some, this is a compound assignment (e.g., `x += 5`)
    #[inline(always)]
    pub fn assignment(
        &mut self,
        random: &mut impl Rng,
        source: NodeIndex,
        value: NodeIndex,
        op: Option<Operator>,
        ty: &Type,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Assignment { op });
        self.register(random, idx, NodeKind::Assignment, ty, op);
        self.graph.add_edge(idx, source, 0); // source variable
        self.graph.add_edge(idx, value, 1); // value expression

        idx
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Graph queries
    // ────────────────────────────────────────────────────────────────────────────────

    /// Get the child of the node at `idx` connected via edge with weight `w`.
    #[inline(always)]
    pub fn child(&self, idx: NodeIndex, w: usize) -> Option<NodeIndex> {
        self.graph
            .edges_directed(idx, Direction::Outgoing)
            .find(|e| *e.weight() == w)
            .map(|e| e.target())
    }

    #[inline(always)]
    pub fn left(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.child(idx, 0)
    }

    #[inline(always)]
    pub fn right(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.child(idx, 1)
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

    /// Given a node at position `n`, swap its outgoing edges by inverting its weights (0 ↔ 1).
    /// **NOTE**: does nothing if there are more than two edges.
    pub fn swap_operands(&mut self, idx: NodeIndex) {
        let edges: Vec<_> = self
            .graph
            .edges_directed(idx, Direction::Outgoing)
            .map(|e| (e.id(), *e.weight(), e.target()))
            .collect();

        if edges.len() == 2 {
            for (edge, w, target) in edges {
                self.graph.remove_edge(edge);
                self.graph.add_edge(idx, target, 1 - w);
            }
        }
    }

    #[inline(always)]
    pub fn add_operand(&mut self, idx: NodeIndex, w: usize, operand: NodeIndex) {
        self.graph.add_edge(idx, operand, w);
    }

    pub fn remove_operand(&mut self, n: NodeIndex, pos: usize) {
        if let Some(edge_id) = self
            .graph
            .edges_directed(n, Direction::Outgoing)
            .find(|e| *e.weight() == pos)
            .map(|e| e.id())
        {
            let target = self.graph.edge_endpoints(edge_id).unwrap().1;
            self.graph.remove_edge(edge_id);
            self.remove_if_orphaned(target);
        }
    }

    /// Given a node at position `n` with an outgoing edge whose weight is `w`, reconnect the edge
    /// `w` to `target`, removing the previous destination node if orphaned.
    pub fn set_child(&mut self, idx: NodeIndex, w: usize, target: NodeIndex) {
        if let Some(edge_id) = self
            .graph
            .edges_directed(idx, Direction::Outgoing)
            .find(|e| *e.weight() == w)
            .map(|e| e.id())
        {
            let old_target = self.graph.edge_endpoints(edge_id).unwrap().1;
            self.graph.remove_edge(edge_id);
            self.remove_if_orphaned(old_target);
        }
        self.graph.add_edge(idx, target, w);
    }

    /// Redirect all incoming edges to `old` to `new`. **DOES NOT** remove `old` if orphaned.
    pub fn redirect_edges(&mut self, old: NodeIndex, new: NodeIndex) {
        let edges: Vec<_> = self
            .graph
            .edges_directed(old, Direction::Incoming)
            .map(|e| (e.source(), *e.weight()))
            .collect();

        for (source, weight) in edges {
            let ids: Vec<_> = self
                .graph
                .edges_directed(old, Direction::Incoming)
                .filter(|e| e.source() == source && *e.weight() == weight)
                .map(|e| e.id())
                .collect();

            for id in ids {
                self.graph.remove_edge(id);
            }
            self.graph.add_edge(source, new, weight);
        }
    }

    /// Insert `new` between `parent` and `child`.
    /// Before: parent --[w]--> child.
    /// After:  parent --[w]--> new --[0]--> child.
    pub fn insert_between(&mut self, parent: NodeIndex, w: usize, target: NodeIndex) {
        if let Some(child) = self.child(parent, w) {
            if let Some(edge_id) = self
                .graph
                .edges_directed(parent, Direction::Outgoing)
                .find(|e| *e.weight() == w)
                .map(|e| e.id())
            {
                self.graph.remove_edge(edge_id);
            }
            self.graph.add_edge(parent, target, w);
            self.graph.add_edge(target, child, 0);
        }
    }

    /// Change the inner `Operator` of a `Node::Operator` at position `idx` with a new `Operator`.
    pub fn set_operator(&mut self, idx: NodeIndex, new_op: Operator) {
        if let Node::Operator { op, .. } = &mut self.graph[idx] {
            let old_op = *op;
            *op = new_op;

            // Update the hashmap to keep track of the correct state of the `Node::Operator`
            if let Some(v) = self.operators.get_mut(&old_op) {
                v.retain(|&x| x != idx);
            }

            self.operators.entry(new_op).or_default().push(idx);
        }
    }

    /// Remove node at position `idx` if it has no incoming edges.
    pub fn remove_if_orphaned(&mut self, idx: NodeIndex) {
        if self.graph.edges_directed(idx, Direction::Incoming).next().is_some() {
            return;
        }

        // Check if used as condition in if/else if/asserts, returning as we can not remove it.
        if self.conditions.contains(&idx) {
            return;
        }

        let ty = self.ty(idx);
        let ty_kind = ty.kind();
        let node = &self.graph[idx];
        let node_kind = node.kind();
        let op = match node {
            Node::Operator { op, .. } => Some(*op),
            _ => None,
        };

        // Remove from hashmaps
        if let Some(v) = self.types.get_mut(&ty) {
            v.retain(|&x| x != idx);
        }
        if let Some(v) = self.type_kinds.get_mut(&ty_kind) {
            v.retain(|&x| x != idx);
        }
        if let Some(v) = self.node_kinds.get_mut(&node_kind) {
            v.retain(|&x| x != idx);
        }
        if let Some(op) = op {
            if let Some(v) = self.operators.get_mut(&op) {
                v.retain(|&x| x != idx);
            }
        }

        self.graph.remove_node(idx);
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Helpers
    // ────────────────────────────────────────────────────────────────────────────────

    /// We try to find a variable that matches the return type of this `Forest`. If none is found,
    /// we generate a random expression. @todo
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
                .unwrap_or_else(|| ret.random_value(random, ctx, scope, true));

            self.return_expr = Some(value);
        }
    }

    fn register(
        &mut self,
        random: &mut impl Rng,
        idx: NodeIndex,
        kind: NodeKind,
        ty: &Type,
        op: Option<Operator>,
    ) {
        self.types.entry(ty.clone()).or_default().push(idx);
        self.type_kinds.entry(ty.kind()).or_default().push(idx);
        self.node_kinds.entry(kind).or_default().push(idx);

        if let Some(op) = op {
            self.operators.entry(op).or_default().push(idx);
        }

        // If this is a mutable `Variable`, register it for assignment generation.
        if let Node::Variable { name, mutable: true, .. } = &self.graph[idx] {
            self.mutables.insert(name.clone(), idx);
        }

        // To be able to call lambdas within variables, we must locate them. If they are a
        // variable/input, we could always `v()`, but what if we have it within a nested type? Then
        // we keep traversing and building the access on each step, storing it on `self.lambdas`.
        let name = match &self.graph[idx] {
            Node::Variable { name, .. } | Node::Input { name, .. } => Some(name.clone()),
            _ => None,
        };

        if let Some(name) = name {
            self.register_callable(random, name, ty, Some(idx));
        }
    }

    // We have `idx` as `Option` because we treat top-level functions as `Type::Lambdas`, but as
    // they do not have `NodeIndex` within the `Forest`, we set there to `None` to be able to reuse
    // this in both situations (so `.unwrap()` is safe :P).
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
                    let field_accessor = format!("{}.{}", accessor, field.name);
                    self.register_callable(random, field_accessor, &field.ty, idx);
                }
            }
            Type::Array(arr) => {
                // If empty, skip
                if arr.size == 0 {
                    return;
                }

                // Choose an index randomly
                let index = random.random_range(0..arr.size);
                let elem_accessor = format!("{}[{}]", accessor, index);
                self.register_callable(random, elem_accessor, &arr.ty, idx);
            }
            Type::Slice(slice) => {
                // If empty, skip
                if slice.size == 0 {
                    return;
                }

                // Choose an index randomly
                let index = random.random_range(0..slice.size);
                let elem_accessor = format!("{}[{}]", accessor, index);
                self.register_callable(random, elem_accessor, &slice.ty, idx);
            }
            Type::Tuple(tuple) => {
                for (i, elem_ty) in tuple.elements.iter().enumerate() {
                    let elem_accessor = format!("{}.{}", accessor, i);
                    self.register_callable(random, elem_accessor, elem_ty, idx);
                }
            }
            _ => {}
        }
    }

    #[inline(always)]
    pub fn is_reusable_node(&self, idx: NodeIndex) -> bool {
        matches!(self.graph.node_weight(idx), Some(Node::Variable { .. } | Node::Input { .. }))
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
                    return writeln!(
                        out,
                        "  {} -> {} [label=\"arg{}\" dir=back]",
                        e.source().index(),
                        e.target().index(),
                        w
                    )
                    .unwrap()
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
        let lhs = forest.literal(
            &mut random,
            "1".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        let rhs = forest.literal(
            &mut random,
            "2".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        // Add operator node for addition
        let op = forest.operator(
            &mut random,
            Operator::Add,
            Type::Integer(Integer { bits: 32, signed: true }),
            lhs,
            Some(rhs),
        );

        // Add variable node
        let _ = forest.variable(
            &mut random,
            "result".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
            false,
            false,
            op,
        );

        // Save forest as DOT file
        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_forest.dot"));
    }

    #[test]
    fn test_swap_operands() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        // Add two literal nodes
        let lhs = forest.literal(
            &mut random,
            "1".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        let rhs = forest.literal(
            &mut random,
            "2".to_string(),
            Type::Integer(Integer { bits: 8, signed: true }),
        );
        // Add operator node for addition
        let op = forest.operator(
            &mut random,
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
    fn test_set_operand() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let lhs = forest.literal(
            &mut random,
            "1".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        let rhs = forest.literal(
            &mut random,
            "2".to_string(),
            Type::Integer(Integer { bits: 8, signed: true }),
        );
        let op = forest.operator(
            &mut random,
            Operator::Add,
            Type::Integer(Integer { bits: 32, signed: true }),
            lhs,
            Some(rhs),
        );

        assert_eq!(forest.left(op).unwrap(), lhs);
        assert_eq!(forest.right(op).unwrap(), rhs);

        let new = forest.literal(
            &mut random,
            "3".to_string(),
            Type::Integer(Integer { bits: 1, signed: false }),
        );
        forest.set_child(op, 0, new);

        assert_eq!(forest.left(op).unwrap(), new);
        assert_eq!(forest.right(op).unwrap(), rhs);
    }

    #[test]
    fn test_insert_between() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let lhs = forest.literal(
            &mut random,
            "1".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        let rhs = forest.literal(
            &mut random,
            "2".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        let add = forest.operator(
            &mut random,
            Operator::Add,
            Type::Integer(Integer { bits: 32, signed: true }),
            lhs,
            Some(rhs),
        );
        let var = forest.variable(
            &mut random,
            "x".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
            false,
            false,
            add,
        );

        // Insert negation between var and add
        let neg = forest.graph.add_node(Node::Operator {
            op: Operator::Neg,
            ret: Type::Integer(Integer { bits: 32, signed: true }),
        });
        forest.insert_between(var, 0, neg);

        assert_eq!(forest.left(var).unwrap(), neg);
        assert_eq!(forest.left(neg).unwrap(), add);
    }

    #[test]
    fn test_set_operator() {
        let mut forest = Forest::default();
        let mut random = rand::rng();

        let lhs = forest.literal(
            &mut random,
            "1".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        let rhs = forest.literal(
            &mut random,
            "2".to_string(),
            Type::Integer(Integer { bits: 32, signed: true }),
        );
        let op = forest.operator(
            &mut random,
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
