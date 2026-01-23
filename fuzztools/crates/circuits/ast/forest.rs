use crate::circuits::{
    ast::{
        nodes::{Node, NodeKind},
        operators::Operator,
        types::*,
    },
    context::Context,
    scope::Scope,
};
use petgraph::{
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{EdgeRef, IntoEdgeReferences},
    Direction,
};
use rand::{seq::IteratorRandom, Rng};
use std::{collections::HashMap, fmt::Write, path::Path};

#[derive(Debug, Clone, Default)]
/// This provides a directed graph structure for representing a block of expressions, that
/// is, the body of a function, the body of a loop, etc...
pub struct Forest {
    pub graph: StableDiGraph<Node, usize>,

    // This hashmaps are used to achieve O(1) lookups to interesting data inside the `graph`
    pub types: HashMap<Type, Vec<NodeIndex>>,
    pub type_kinds: HashMap<TypeKind, Vec<NodeIndex>>,

    pub nodes: HashMap<NodeKind, Vec<NodeIndex>>,
    pub operators: HashMap<Operator, Vec<NodeIndex>>,

    // Tracks callable lambdas: (accessor_expression, params, return_type)
    // Includes direct lambda variables and lambdas nested in structs/arrays/slices/tuples
    pub lambdas: Vec<(String, Lambda, NodeIndex)>,

    // Tracks nodes with nested forests: ForLoop and If statements
    // Used by the rewriter to efficiently find nested forests without traversing the graph
    pub nested_forests: Vec<NodeIndex>,

    // This hashmap holds which variables from the outer scope (if any) are mutable and can be
    // mutable within this sub-forest
    pub mutable_refs: HashMap<String, NodeIndex>,

    pub var_counter: usize,
    pub depth: usize,
    pub skip_idle_vars: bool,

    pub return_expr: Option<String>,
}

impl Forest {
    pub fn ty(&self, idx: NodeIndex) -> Type {
        match &self.graph[idx] {
            Node::Input { ty, .. } | Node::Literal { ty, .. } | Node::Variable { ty, .. } => {
                ty.clone()
            }
            // @audit quizás usando esto abajo
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
    pub fn input(&mut self, name: String, ty: Type) -> NodeIndex {
        self.graph.add_node(Node::Input { name, ty })
    }

    #[inline(always)]
    pub fn literal(&mut self, value: String, ty: Type) -> NodeIndex {
        self.graph.add_node(Node::Literal { value, ty })
    }

    #[inline(always)]
    pub fn variable(
        &mut self,
        name: String,
        ty: Type,
        mutable: bool,
        shadow: bool,
        value: NodeIndex,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Variable { name, ty, mutable, shadow });
        self.graph.add_edge(idx, value, 0);

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
    pub fn call(
        &mut self,
        name: String,
        ret: Type,
        args: Vec<NodeIndex>,
        parent: NodeIndex,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Call { name, ret });
        self.graph.add_edge(idx, parent, 0);

        for (pos, arg) in args.into_iter().enumerate() {
            self.graph.add_edge(idx, arg, pos + 1);
        }

        idx
    }

    #[inline(always)]
    pub fn cast(&mut self, source: NodeIndex, target: Type) -> NodeIndex {
        let idx = self.graph.add_node(Node::Cast { target });
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
        source: NodeIndex,
        value: NodeIndex,
        op: Option<Operator>,
    ) -> NodeIndex {
        let idx = self.graph.add_node(Node::Assignment { op });
        self.graph.add_edge(idx, source, 0); // source variable
        self.graph.add_edge(idx, value, 1); // value expression

        idx
    }

    // ────────────────────────────────────────────────────────────────────────────────
    // Graph queries
    // ────────────────────────────────────────────────────────────────────────────────

    /// Get operand at position `pos` (0 = left, 1 = right)
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

    /// Remove a node if nothing points to it (no incoming edges).
    /// Also removes the node from all internal tracking `HashMap`s (types, type kinds, nodes,
    /// operators). Does NOT recursively check if parent nodes become orphaned.
    pub fn remove_if_orphan(&mut self, node: NodeIndex) {
        if self.graph.edges_directed(node, Direction::Incoming).next().is_some() {
            return;
        }

        // Check if this node is used as a condition in any `If` or `Assert`. As we remove dangling
        // nodes when rewriting, we need to check if the node is used as a condition in any `If` or
        // `Assert`, otherwise it will be removed and the formatter will crash
        for idx in self.graph.node_indices() {
            match &self.graph[idx] {
                Node::If { condition, else_ifs, .. } => {
                    if *condition == node {
                        return;
                    }
                    for (cond, _) in else_ifs {
                        if *cond == node {
                            return;
                        }
                    }
                }
                Node::Assert { condition, .. } => {
                    if *condition == node {
                        return;
                    }
                }
                _ => {}
            }
        }

        let n = match self.graph.node_weight(node) {
            Some(n) => n,
            None => return,
        };
        let ty = self.ty(node);
        let ty_kind = ty.kind();
        let node_kind = n.kind();
        let op = match n {
            Node::Operator { op, .. } => Some(*op),
            _ => None,
        };

        // Remove from hashmaps to avoid having dangling nodes
        if let Some(v) = self.types.get_mut(&ty) {
            v.retain(|&x| x != node);
        }
        if let Some(v) = self.type_kinds.get_mut(&ty_kind) {
            v.retain(|&x| x != node);
        }
        if let Some(v) = self.nodes.get_mut(&node_kind) {
            v.retain(|&x| x != node);
        }
        if let Some(op) = op {
            if let Some(v) = self.operators.get_mut(&op) {
                v.retain(|&x| x != node);
            }
        }
        // Remove from nested_forests list if present
        self.nested_forests.retain(|&x| x != node);

        self.graph.remove_node(node);
    }

    /// Swap the operands of a binary node by inverting edge weights (0 ↔ 1)
    /// Only operates on nodes with exactly 2 outgoing edges
    pub fn swap_operands(&mut self, n: NodeIndex) {
        let edges: Vec<_> = self
            .graph
            .edges_directed(n, Direction::Outgoing)
            .map(|e| (e.id(), *e.weight(), e.target()))
            .collect();

        if edges.len() == 2 {
            for (id, w, target) in edges {
                self.graph.remove_edge(id);
                self.graph.add_edge(n, target, 1 - w); // Flip 0 <-> 1
            }
        }
    }

    /// Remove the operand at position `pos` from node `n`
    /// The target operand is removed if it becomes orphaned (no incoming edges)
    /// Does NOT remove `n` itself even if it has no remaining operands
    pub fn remove_operand(&mut self, n: NodeIndex, pos: usize) {
        if let Some(edge_id) = self
            .graph
            .edges_directed(n, Direction::Outgoing)
            .find(|e| *e.weight() == pos)
            .map(|e| e.id())
        {
            let target = self.graph.edge_endpoints(edge_id).unwrap().1;
            self.graph.remove_edge(edge_id);
            self.remove_if_orphan(target);
        }
    }

    /// Replace the operand at position `pos` with `new`
    /// The old operand is removed if it becomes orphaned (no incoming edges)
    pub fn replace_operand(&mut self, n: NodeIndex, pos: usize, new: NodeIndex) {
        self.remove_operand(n, pos);
        self.graph.add_edge(n, new, pos);
    }

    /// Add a new operand edge from `n` to `operand` at position `pos`
    #[inline(always)]
    pub fn add_operand(&mut self, n: NodeIndex, pos: usize, operand: NodeIndex) {
        self.graph.add_edge(n, operand, pos);
    }

    /// Redirect the specified incoming edges from `old` to point to `new` instead
    /// Each entry in `edges` is a (source, weight) pair identifying which edge to redirect
    pub fn redirect_edges(&mut self, old: NodeIndex, new: NodeIndex, edges: &[(NodeIndex, usize)]) {
        for &(source, weight) in edges {
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

    // ────────────────────────────────────────────────────────────────────────────────
    // Helpers
    // ────────────────────────────────────────────────────────────────────────────────

    #[inline(always)]
    pub fn next_var(&mut self) -> String {
        let n = self.var_counter;
        self.var_counter += 1;
        format!("v{n}")
    }

    pub fn register(
        &mut self,
        random: &mut impl Rng,
        idx: NodeIndex,
        kind: NodeKind,
        ty: &Type,
        op: Option<Operator>,
    ) {
        self.types.entry(ty.clone()).or_default().push(idx);
        self.type_kinds.entry(ty.kind()).or_default().push(idx);
        self.nodes.entry(kind).or_default().push(idx);

        if let Some(op) = op {
            self.operators.entry(op).or_default().push(idx);
        }

        // Register callable lambdas for variables/inputs
        if matches!(kind, NodeKind::Variable | NodeKind::Input) {
            if let Some(name) = self.graph.node_weight(idx).and_then(|n| match n {
                Node::Variable { name, .. } | Node::Input { name, .. } => Some(name.clone()),
                _ => None,
            }) {
                self.register_callables(random, &name, ty, Some(idx));
            }
        }
    }

    /// Registers callable lambdas from a type, recursively handling nested types.
    fn register_callables(
        &mut self,
        random: &mut impl Rng,
        accessor: &str,
        ty: &Type,
        idx: Option<NodeIndex>,
    ) {
        match ty {
            Type::Lambda(lambda) => {
                // idx is the top-most node (variable/input) that contains this lambda,
                // either directly or nested within structs/arrays/slices/tuples
                self.lambdas.push((accessor.to_string(), lambda.clone(), idx.unwrap()));
            }
            Type::Struct(s) => {
                for field in &s.fields {
                    let field_accessor = format!("{}.{}", accessor, field.name);
                    self.register_callables(random, &field_accessor, &field.ty, idx);
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
                self.register_callables(random, &elem_accessor, &arr.ty, idx);
            }
            Type::Slice(slice) => {
                // If empty, skip
                if slice.size == 0 {
                    return;
                }

                // Choose an index randomly
                let index = random.random_range(0..slice.size);
                let elem_accessor = format!("{}[{}]", accessor, index);
                self.register_callables(random, &elem_accessor, &slice.ty, idx);
            }
            Type::Tuple(tuple) => {
                for (i, elem_ty) in tuple.elements.iter().enumerate() {
                    let elem_accessor = format!("{}.{}", accessor, i);
                    self.register_callables(random, &elem_accessor, elem_ty, idx);
                }
            }
            _ => {}
        }
    }

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

    pub fn collect_boolean_nodes(&self) -> Vec<NodeIndex> {
        let mut nodes = vec![];

        // Add boolean type nodes, but filter out literals and nodes with complex dependencies
        if let Some(bool_nodes) = self.type_kinds.get(&TypeKind::Boolean) {
            for &idx in bool_nodes {
                // Only include nodes that can be used as standalone conditions:
                // - Variables and Inputs are always valid
                // - Operators (including boolean ops) are valid
                // - FieldAccess, TupleIndex, Index are only valid if they point to a Variable/Input
                match &self.graph[idx] {
                    Node::Variable { .. } | Node::Input { .. } | Node::Operator { .. } => {
                        nodes.push(idx);
                    }
                    Node::FieldAccess { .. } | Node::TupleIndex { .. } | Node::Index { .. } => {
                        // Check if the root (left side) is a Variable or Input
                        if let Some(left) = self.left(idx) {
                            if self.is_variable_or_input(left) {
                                nodes.push(idx);
                            }
                        }
                    }
                    Node::Cast { .. } => {
                        // Casts are valid if they cast from a variable/input
                        if let Some(left) = self.left(idx) {
                            if self.is_variable_or_input(left) {
                                nodes.push(idx);
                            }
                        }
                    }
                    Node::Call { .. } => {
                        // Function calls are valid
                        nodes.push(idx);
                    }
                    _ => {
                        // Skip Literal, Assignment, ForLoop, If, Assert
                    }
                }
            }
        }

        // Add comparison operator results (these are always valid as conditions)
        for (op, indices) in &self.operators {
            if op.is_comparison() {
                nodes.extend(indices.iter().copied());
            }
        }

        nodes
    }

    /// Check if a node is a Variable or Input (recursively follows FieldAccess/TupleIndex/Index)
    fn is_variable_or_input(&self, idx: NodeIndex) -> bool {
        match &self.graph[idx] {
            Node::Variable { .. } | Node::Input { .. } => true,
            Node::FieldAccess { .. } | Node::TupleIndex { .. } | Node::Index { .. } => {
                if let Some(left) = self.left(idx) {
                    self.is_variable_or_input(left)
                } else {
                    false
                }
            }
            _ => false,
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
        let _ = forest.variable(
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
    fn test_replace_operand() {
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

        // Add a new literal node
        let new =
            forest.literal("3".to_string(), Type::Integer(Integer { bits: 1, signed: false }));
        forest.replace_operand(op, 0, new);

        // Only the left operand should be replaced
        assert_eq!(forest.left(op).unwrap(), new);
        assert_eq!(forest.right(op).unwrap(), rhs);
    }

    #[test]
    fn test_remove_operand() {
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

        forest.remove_operand(op, 0);

        assert_eq!(forest.left(op).is_none(), true);
        assert_eq!(forest.right(op).unwrap(), rhs);
    }

    #[test]
    fn test_remove_all_operands() {
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

        forest.remove_operand(op, 0);
        forest.remove_operand(op, 1);

        assert_eq!(forest.left(op).is_none(), true);
        assert_eq!(forest.right(op).is_none(), true);

        // Explicitly remove the operator node if it's now orphaned
        forest.remove_if_orphan(op);

        // Save forest as DOT file
        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_remove_all_operands.dot"));

        // Ensure the operator node is orphaned and so removed
        assert_eq!(forest.graph.node_indices().next().is_none(), true);
    }
}
