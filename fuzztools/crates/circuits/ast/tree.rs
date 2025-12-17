//! This module provides a directed graph structure for representing expressions as nodes
//! connected by edges. Each node is an [`Expr`] (literal, variable, operator, etc.) and edges
//! represent data flow between expressions, for example, they are labeled with a "let" if it is a
//! let statement, with `argX` if it is being used as the Xth argument to a function call, etc...
//!
//! That way I can easily visualize the code block + translate it easily into source code.

use crate::circuits::{
    ast::{operators::Operator, scope::Scope, types::*},
    context::Context,
};
use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::EdgeRef,
    Direction,
};
use rand::{seq::IndexedRandom, Rng};

// ────────────────────────────────────────────────────────────────────────────────
// Expression Node Types
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Expr {
    /// A literal value of the given type, `23`, `false`, `"hello"`, etc...
    Literal(Type),

    /// A variable binding: `let name: ty = <edge 0>`, for example, `let x: u128 = 23`
    Variable { name: String, ty: Type },

    /// Binary operation: `<edge 0> op <edge 1>` with result type `ty`, for example, `23 + 42`
    Binary { op: Operator, ty: Type },

    /// Unary operation: `op <edge 0>` with result type `ty`, for example, `!false`
    Unary { op: Operator, ty: Type },

    /// Array/slice indexing: `<edge 0>[edge_weight]`
    /// The edge weight stores the index value
    Index,

    /// Tuple field access: `<edge 0>.edge_weight`
    /// The edge weight stores the tuple index
    Tuple,

    /// Struct field access: `<edge 0>.field`
    Field { field: String },

    /// Function call: `name(args) -> ret`
    Call { name: String, args: Vec<String>, ret: Type },

    /// Lambda definition: `|params| -> ret_ty { ... }`
    LambdaDef { params: Vec<(String, Type)>, ret_ty: Type },

    /// Lambda invocation: `name(...)` where edges 0..n point to arguments
    LambdaCall { name: String, ret_ty: Type },
}

// ────────────────────────────────────────────────────────────────────────────────
// Expression Graph
// ────────────────────────────────────────────────────────────────────────────────

/// A directed graph of expressions
#[derive(Debug, Clone, Default)]
pub struct ExprGraph {
    pub inner: DiGraph<Expr, usize>,
    pub var_counter: usize,
}

impl ExprGraph {
    pub fn new() -> Self {
        Self { inner: DiGraph::new(), var_counter: 0 }
    }

    /// Generate a fresh variable name (v0, v1, v2, ...)
    pub fn next_var(&mut self) -> String {
        let n = self.var_counter;
        self.var_counter += 1;
        format!("v{n}")
    }

    // ── Type Resolution ──────────────────────────────────────────────────────

    /// Resolve the result type of a node.
    ///
    /// For most nodes, the type is stored directly. For accessors (`Index`, `TupleAt`, `Field`),
    /// the type is derived from the base expression being accessed.
    pub fn ty(&self, idx: NodeIndex) -> Option<Type> {
        match &self.inner[idx] {
            Expr::Literal(ty) |
            Expr::Variable { ty, .. } |
            Expr::Binary { ty, .. } |
            Expr::Unary { ty, .. } => Some(ty.clone()),
            Expr::Call { ret, .. } => Some(ret.clone()),
            Expr::Index => self.index_ty(idx),
            Expr::Tuple => self.tuple_at_ty(idx),
            Expr::Field { field } => self.field_ty(idx, field),
            Expr::LambdaDef { params, ret_ty } => Some(Type::Lambda(Lambda {
                name: String::new(),
                params: params.clone(),
                ret: Box::new(ret_ty.clone()),
            })),
            Expr::LambdaCall { ret_ty, .. } => Some(ret_ty.clone()),
        }
    }

    /// Get the base node (first outgoing edge target) for accessor nodes
    fn base(&self, n: NodeIndex) -> Option<NodeIndex> {
        self.inner.edges_directed(n, Direction::Outgoing).next().map(|e| e.target())
    }

    /// Get the edge weight (index) for the first outgoing edge
    fn edge_idx(&self, n: NodeIndex) -> usize {
        self.inner.edges_directed(n, Direction::Outgoing).next().map(|e| *e.weight()).unwrap_or(0)
    }

    /// Resolve type for `Index` nodes: returns the element type of the array/slice
    fn index_ty(&self, n: NodeIndex) -> Option<Type> {
        let base = self.base(n)?;
        match self.ty(base)? {
            Type::Array(a) => Some((*a.ty).clone()),
            Type::Slice(s) => Some((*s.ty).clone()),
            _ => None,
        }
    }

    /// Resolve type for `TupleAt` nodes: returns the type at the given tuple index
    fn tuple_at_ty(&self, n: NodeIndex) -> Option<Type> {
        let base = self.base(n)?;
        let idx = self.edge_idx(n);
        match self.ty(base)? {
            Type::Tuple(t) => t.inner.get(idx).cloned(),
            _ => None,
        }
    }

    /// Resolve type for `Field` nodes: returns the type of the named struct field
    fn field_ty(&self, n: NodeIndex, field: &str) -> Option<Type> {
        let base = self.base(n)?;
        match self.ty(base)? {
            Type::Struct(s) => s.fields.iter().find(|f| f.name == field).map(|f| (*f.ty).clone()),
            _ => None,
        }
    }

    // ── Constructors ─────────────────────────────────────────────────────────

    /// Add a literal node
    pub fn lit(&mut self, ty: Type) -> NodeIndex {
        self.inner.add_node(Expr::Literal(ty))
    }

    /// Add a variable binding: `let name: ty = value`
    pub fn var(&mut self, name: String, ty: Type, value: NodeIndex) -> NodeIndex {
        let n = self.inner.add_node(Expr::Variable { name, ty });
        self.inner.add_edge(n, value, 0);
        n
    }

    /// Add a binary operation: `l op r` with result type `ty`
    pub fn binop(&mut self, op: Operator, l: NodeIndex, r: NodeIndex, ty: Type) -> NodeIndex {
        let n = self.inner.add_node(Expr::Binary { op, ty });
        self.inner.add_edge(n, l, 0); // left operand
        self.inner.add_edge(n, r, 1); // right operand
        n
    }

    /// Add a unary operation: `op operand` with result type `ty`
    pub fn unary(&mut self, op: Operator, operand: NodeIndex, ty: Type) -> NodeIndex {
        let n = self.inner.add_node(Expr::Unary { op, ty });
        self.inner.add_edge(n, operand, 0);
        n
    }

    /// Add an array/slice index access: `base[idx]`
    pub fn index(&mut self, base: NodeIndex, idx: usize) -> NodeIndex {
        let n = self.inner.add_node(Expr::Index);
        self.inner.add_edge(n, base, idx);
        n
    }

    /// Add a tuple field access: `base.idx`
    pub fn tuple_at(&mut self, base: NodeIndex, idx: usize) -> NodeIndex {
        let n = self.inner.add_node(Expr::Tuple);
        self.inner.add_edge(n, base, idx);
        n
    }

    /// Add a struct field access: `base.field`
    pub fn field(&mut self, base: NodeIndex, field: String) -> NodeIndex {
        let n = self.inner.add_node(Expr::Field { field });
        self.inner.add_edge(n, base, 0);
        n
    }

    /// Add a lambda definition node
    pub fn lambda_def(&mut self, params: Vec<(String, Type)>, ret_ty: Type) -> NodeIndex {
        self.inner.add_node(Expr::LambdaDef { params, ret_ty })
    }

    /// Add a lambda call: `name(args...)` with edges pointing to argument nodes
    pub fn lambda_call(&mut self, name: String, args: Vec<NodeIndex>, ret_ty: Type) -> NodeIndex {
        let n = self.inner.add_node(Expr::LambdaCall { name, ret_ty });
        for (i, arg) in args.into_iter().enumerate() {
            self.inner.add_edge(n, arg, i);
        }
        n
    }

    // ── Queries ──────────────────────────────────────────────────────────────

    /// Get all operand nodes for `n`, sorted by edge weight (position)
    pub fn operands(&self, n: NodeIndex) -> Vec<NodeIndex> {
        let mut e: Vec<_> = self
            .inner
            .edges_directed(n, Direction::Outgoing)
            .map(|e| (*e.weight(), e.target()))
            .collect();
        e.sort_unstable_by_key(|(w, _)| *w);
        e.into_iter().map(|(_, t)| t).collect()
    }

    /// Get the left operand (edge weight 0) of a binary operation
    pub fn left(&self, n: NodeIndex) -> Option<NodeIndex> {
        self.inner
            .edges_directed(n, Direction::Outgoing)
            .find(|e| *e.weight() == 0)
            .map(|e| e.target())
    }

    /// Get the right operand (edge weight 1) of a binary operation
    pub fn right(&self, n: NodeIndex) -> Option<NodeIndex> {
        self.inner
            .edges_directed(n, Direction::Outgoing)
            .find(|e| *e.weight() == 1)
            .map(|e| e.target())
    }

    /// Find all nodes whose type matches the predicate
    fn nodes_matching<F: Fn(&Type) -> bool>(&self, f: F) -> Vec<NodeIndex> {
        self.inner
            .node_indices()
            .filter(|&n| self.ty(n).as_ref().map(&f).unwrap_or(false))
            .collect()
    }

    /// Find all `Var` or `Lit` nodes whose type matches the predicate
    fn vars_or_lits_of<F: Fn(&Type) -> bool>(&self, f: F) -> Vec<NodeIndex> {
        self.inner
            .node_indices()
            .filter(|&n| {
                matches!(self.inner[n], Expr::Variable { .. } | Expr::Literal(_)) &&
                    self.ty(n).as_ref().map(&f).unwrap_or(false)
            })
            .collect()
    }

    // ── Mutations ────────────────────────────────────────────────────────────

    /// Swap the left and right operands of a binary operation
    pub fn swap_operands(&mut self, n: NodeIndex) {
        let edges: Vec<_> = self
            .inner
            .edges_directed(n, Direction::Outgoing)
            .map(|e| (e.id(), *e.weight(), e.target()))
            .collect();
        if edges.len() == 2 {
            for (id, _, _) in &edges {
                self.inner.remove_edge(*id);
            }
            for (_, w, t) in edges {
                self.inner.add_edge(n, t, 1 - w); // flip 0<->1
            }
        }
    }

    /// Replace the operand at position `pos` with a new node
    pub fn replace_operand(&mut self, n: NodeIndex, pos: usize, new: NodeIndex) {
        if let Some(e) =
            self.inner.edges_directed(n, Direction::Outgoing).find(|e| *e.weight() == pos)
        {
            self.inner.remove_edge(e.id());
        }
        self.inner.add_edge(n, new, pos);
    }

    /// Change the operator of a `BinOp` or `UnaryOp` node
    pub fn set_op(&mut self, n: NodeIndex, op: Operator) {
        match &mut self.inner[n] {
            Expr::Binary { op: o, .. } | Expr::Unary { op: o, .. } => *o = op,
            _ => {}
        }
    }

    // ── Random Generation ────────────────────────────────────────────────────

    /// Generate a random expression graph with `count` nodes
    pub fn random(random: &mut impl Rng, ctx: &Context, scope: &Scope, count: usize) -> Self {
        let mut g = Self::new();
        for _ in 0..count {
            g.add_random(random, ctx, scope);
        }
        g
    }

    /// Add a single random node to the graph.
    ///
    /// The choice of node type depends on what's already in the graph:
    /// - Always available: `lit`, `lambda_def`
    /// - If nodes exist: `var`, `unary`
    /// - If specific types exist: operations on those types, accessors
    fn add_random(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        // Scan existing nodes to determine which operations are available
        let (mut has_field, mut has_bool, mut has_int) = (false, false, false);
        let (mut has_arr, mut has_slice, mut has_tup, mut has_struct, mut has_lambda) =
            (false, false, false, false, false);

        for n in self.inner.node_indices() {
            // Only consider "value" nodes (variables, literals, lambda defs)
            if !matches!(
                self.inner[n],
                Expr::Variable { .. } | Expr::Literal(_) | Expr::LambdaDef { .. }
            ) {
                continue;
            }
            if let Some(ty) = self.ty(n) {
                match ty {
                    Type::Field(_) => has_field = true,
                    Type::Boolean(_) => has_bool = true,
                    Type::Integer(_) => has_int = true,
                    Type::Array(_) => has_arr = true,
                    Type::Slice(_) => has_slice = true,
                    Type::Tuple(_) => has_tup = true,
                    Type::Struct(_) => has_struct = true,
                    Type::Lambda(_) => has_lambda = true,
                    _ => {}
                }
            }
        }

        // Build list of available node types based on current graph state
        let has_nodes = self.inner.node_count() > 0;
        let mut choices: Vec<&str> = vec!["lit", "lambda_def"];
        if has_nodes {
            choices.push("var");
            choices.push("unary");
        }
        if has_field {
            choices.push("field_binop");
        }
        if has_bool {
            choices.push("bool_binop");
        }
        if has_int {
            choices.push("int_binop");
        }
        if has_arr {
            choices.push("index_arr");
        }
        if has_slice {
            choices.push("index_slice");
        }
        if has_tup {
            choices.push("tuple_at");
        }
        if has_struct {
            choices.push("field_access");
        }
        if has_lambda {
            choices.push("lambda_call");
        }

        match *choices.choose(random).unwrap() {
            "lit" => {
                self.lit(Type::random(random, ctx, scope, false, false));
            }
            "var" => self.rand_var(random),
            "field_binop" => self.rand_binop(
                random,
                |t| matches!(t, Type::Field(_)),
                Operator::arithmetic_field(),
            ),
            "bool_binop" => self.rand_binop(
                random,
                |t| matches!(t, Type::Boolean(_)),
                Operator::binary_boolean(),
            ),
            "int_binop" => self.rand_int_binop(random),
            "unary" => self.rand_unary(random),
            "index_arr" => self.rand_index(random, true),
            "index_slice" => self.rand_index(random, false),
            "tuple_at" => self.rand_tuple_at(random),
            "field_access" => self.rand_field(random),
            "lambda_def" => self.rand_lambda_def(random, ctx, scope),
            "lambda_call" => self.rand_lambda_call(random, ctx, scope),
            _ => {}
        }
    }

    /// Create a variable binding for a random existing node
    fn rand_var(&mut self, random: &mut impl Rng) {
        let nodes: Vec<_> = self.inner.node_indices().collect();
        if let Some(&v) = nodes.choose(random) {
            if let Some(ty) = self.ty(v) {
                let name = self.next_var();
                self.var(name, ty, v);
            }
        }
    }

    /// Create a binary operation between two nodes matching `pred`
    fn rand_binop<F: Fn(&Type) -> bool>(
        &mut self,
        random: &mut impl Rng,
        pred: F,
        ops: &[Operator],
    ) {
        let nodes = self.nodes_matching(&pred);
        if nodes.len() >= 2 {
            let &l = nodes.choose(random).unwrap();
            let &r = nodes.choose(random).unwrap();
            if let Some(ty) = self.ty(l) {
                if let Some(&op) = ops.choose(random) {
                    self.binop(op, l, r, ty);
                }
            }
        }
    }

    /// Create an integer binary operation (requires matching integer types)
    fn rand_int_binop(&mut self, random: &mut impl Rng) {
        let nodes = self.nodes_matching(|t| matches!(t, Type::Integer(_)));
        if nodes.len() < 2 {
            return;
        }

        let &l = nodes.choose(random).unwrap();
        if let Some(ty) = self.ty(l) {
            // Integer operations require operands of the same type
            let same: Vec<_> =
                nodes.iter().filter(|&&n| self.ty(n).as_ref() == Some(&ty)).copied().collect();

            if same.len() >= 2 {
                let &r = same.choose(random).unwrap();
                let mut ops = Operator::arithmetic_integer().to_vec();
                ops.extend(Operator::binary_integer());
                if let Some(&op) = ops.choose(random) {
                    self.binop(op, l, r, ty);
                }
            }
        }
    }

    /// Create a unary operation on a random node
    fn rand_unary(&mut self, random: &mut impl Rng) {
        let nodes: Vec<_> = self.inner.node_indices().collect();
        if let Some(&n) = nodes.choose(random) {
            if let Some(ty) = self.ty(n) {
                let op = match &ty {
                    Type::Boolean(_) => Operator::Not,
                    Type::Integer(_) => *[Operator::Neg, Operator::Not].choose(random).unwrap(),
                    Type::Field(_) => Operator::Neg,
                    _ => return,
                };
                self.unary(op, n, ty);
            }
        }
    }

    /// Create an index access on a random array or slice
    fn rand_index(&mut self, random: &mut impl Rng, is_array: bool) {
        let nodes = self.vars_or_lits_of(|t| {
            if is_array {
                matches!(t, Type::Array(_))
            } else {
                matches!(t, Type::Slice(_))
            }
        });
        if let Some(&base) = nodes.choose(random) {
            let size = match self.ty(base) {
                Some(Type::Array(a)) => a.size,
                Some(Type::Slice(s)) => s.size,
                _ => return,
            };
            if size > 0 {
                self.index(base, random.random_range(0..size));
            }
        }
    }

    /// Create a tuple field access on a random tuple
    fn rand_tuple_at(&mut self, random: &mut impl Rng) {
        let nodes = self.vars_or_lits_of(|t| matches!(t, Type::Tuple(_)));
        if let Some(&base) = nodes.choose(random) {
            if let Some(Type::Tuple(t)) = self.ty(base) {
                if !t.inner.is_empty() {
                    self.tuple_at(base, random.random_range(0..t.inner.len()));
                }
            }
        }
    }

    /// Create a struct field access on a random struct
    fn rand_field(&mut self, random: &mut impl Rng) {
        let nodes = self.vars_or_lits_of(|t| matches!(t, Type::Struct(_)));
        if let Some(&base) = nodes.choose(random) {
            if let Some(Type::Struct(s)) = self.ty(base) {
                if let Some(f) = s.fields.choose(random) {
                    self.field(base, f.name.clone());
                }
            }
        }
    }

    /// Create a random lambda definition and bind it to a variable
    fn rand_lambda_def(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let param_count = random.random_range(0..ctx.max_function_parameters_count.max(1));

        let params: Vec<(String, Type)> = (0..param_count)
            .map(|i| {
                let ty = Type::random(random, ctx, scope, false, false);
                (format!("p{i}"), ty)
            })
            .collect();

        let ret_ty = Type::random(random, ctx, scope, false, false);
        let lambda_node = self.lambda_def(params.clone(), ret_ty.clone());

        // Bind the lambda to a variable so it can be called later
        let var_name = self.next_var();
        let lambda_ty =
            Type::Lambda(Lambda { name: var_name.clone(), params, ret: Box::new(ret_ty) });
        self.var(var_name, lambda_ty, lambda_node);
    }

    /// Create a call to a random existing lambda
    fn rand_lambda_call(&mut self, random: &mut impl Rng, _ctx: &Context, _scope: &Scope) {
        // Find all variables holding lambdas
        let lambda_vars: Vec<_> = self
            .inner
            .node_indices()
            .filter_map(|n| {
                if let Expr::Variable { name, ty } = &self.inner[n] {
                    if let Type::Lambda(lambda) = ty {
                        return Some((name.clone(), lambda.clone()));
                    }
                }
                None
            })
            .collect();

        let Some((var_name, lambda_ty)) = lambda_vars.choose(random).cloned() else { return };

        // Build argument list by finding compatible nodes or creating literals
        let args: Vec<NodeIndex> = lambda_ty
            .params
            .iter()
            .map(|(_, param_ty)| {
                let candidates: Vec<_> = self
                    .inner
                    .node_indices()
                    .filter(|&n| matches!(self.inner[n], Expr::Variable { .. } | Expr::Literal(_)))
                    .filter(|&n| !matches!(self.ty(n), Some(Type::Lambda(_))))
                    .filter(|&n| {
                        self.ty(n)
                            .map(|t| std::mem::discriminant(&t) == std::mem::discriminant(param_ty))
                            .unwrap_or(false)
                    })
                    .collect();

                if !candidates.is_empty() && random.random_bool(0.6) {
                    *candidates.choose(random).unwrap()
                } else {
                    self.lit(param_ty.clone())
                }
            })
            .collect();

        let ret_ty = (*lambda_ty.ret).clone();
        let call_node = self.lambda_call(var_name, args, ret_ty.clone());

        // Optionally bind the result to a variable
        if random.random_bool(0.5) {
            let var_name = self.next_var();
            self.var(var_name, ret_ty, call_node);
        }
    }

    // ── DOT Export ───────────────────────────────────────────────────────────

    /// Get the label text and fill color for a node in DOT format
    fn dot_node_info(&self, n: NodeIndex) -> (String, &'static str) {
        match &self.inner[n] {
            Expr::Literal(ty) => (ty.to_string(), "#a0d8ef"), // light blue
            Expr::Variable { name, ty } => (format!("{name}: {ty}"), "#98d98e"), // light green
            Expr::Binary { op, .. } => (op.to_string(), "#ffb347"), // orange
            Expr::Unary { op, .. } => (op.to_string(), "#ffd700"), // gold
            Expr::Index | Expr::Tuple | Expr::Field { .. } => {
                (self.ty(n).map(|t| t.to_string()).unwrap_or_default(), "#87ceeb") // sky blue
            }
            Expr::Call { name, args, ret } => {
                (format!("{name}({}): {ret}", args.join(",")), "#ff69b4")
            } // hot pink
            Expr::LambdaDef { params, ret_ty } => {
                let p: Vec<_> = params.iter().map(|(n, t)| format!("{n}: {t}")).collect();
                (format!("|{}| -> {ret_ty}", p.join(", ")), "#e6b3ff") // light purple
            }
            Expr::LambdaCall { name, ret_ty } => (format!("{name}(...) -> {ret_ty}"), "#cc99ff"), /* purple */
        }
    }

    /// Get the edge label for DOT format based on the source node type
    fn dot_edge_label(&self, n: NodeIndex, weight: usize) -> String {
        match &self.inner[n] {
            Expr::Variable { .. } => "let".into(),
            Expr::Binary { .. } => if weight == 0 { "L" } else { "R" }.into(),
            Expr::Index | Expr::Tuple => format!("[{weight}]"),
            Expr::Field { field } => format!(".{field}"),
            Expr::LambdaCall { .. } => format!("arg{weight}"),
            _ => String::new(),
        }
    }

    /// Export the graph to a DOT file for visualization with Graphviz
    pub fn save_dot(&self, path: &str) {
        use std::fmt::Write;
        let mut out = String::with_capacity(self.inner.node_count() * 100);
        out.push_str(
            "digraph {\n  rankdir=BT;\n  node [fontname=monospace shape=box style=\"filled,rounded\" margin=\"0.4,0.2\"];\n",
        );

        for n in self.inner.node_indices() {
            let (lbl, col) = self.dot_node_info(n);
            let _ = writeln!(
                out,
                "  {} [label=\"{}\" fillcolor=\"{col}\"]",
                n.index(),
                lbl.replace('"', "\\\"")
            );
        }

        for e in self.inner.edge_references() {
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

// ────────────────────────────────────────────────────────────────────────────────
// Tests
// ────────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_random_forest() {
        let random = &mut rand::rng();
        let ctx = Context {
            max_inputs_count: 5,
            min_element_count: 2,
            max_element_count: 5,
            min_string_size: 1,
            max_string_size: 5,
            max_expression_depth: 3,
            max_type_depth: 2,
            max_structs_count: 3,
            max_struct_fields_count: 3,
            max_globals_count: 5,
            max_functions_count: 5,
            max_function_parameters_count: 3,
            max_function_return_types_count: 5,
            max_main_expressions_count: 25,
            max_block_expressions_count: 5,
            max_string_hashes_count: 5,
            max_name_characters_count: 5,
            max_small_upper_bound: 5,
            integer_signed_probability: 0.33,
            boundary_value_probability: 0.25,
            small_upper_bound_probability: 0.2,
            exclude_prime_probability: 0.1,
            mutable_probability: 0.4,
            raw_string_probability: 0.2,
            type_depth: 0,
            expression_depth: 0,
            new_variable_probability: 0.5,
            max_if_else_branch_count: 4,
        };

        let mut scope = Scope::new();
        let mut structs: Vec<Struct> = Vec::new();
        for _ in 0..5 {
            let s = Struct::random(random, &ctx, &structs, false);
            structs.push(s);
        }
        scope.structs = structs;

        let g = ExprGraph::random(random, &ctx, &scope, 100);
        println!("Nodes: {}, Edges: {}", g.inner.node_count(), g.inner.edge_count());

        g.save_dot("random_forest.dot");
    }
}
