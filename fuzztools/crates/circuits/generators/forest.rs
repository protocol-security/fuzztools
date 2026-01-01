use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::{Node, NodeKind},
        operators::Operator,
        types::{Array, Integer, Slice, Struct, Tuple, Type, TypeKind},
    },
    context::Context,
    generators::types::TypeLocation,
    scope::Scope,
};
use petgraph::graph::NodeIndex;
use rand::{seq::IndexedRandom, Rng};
use std::collections::HashSet;

// ────────────────────────────────────────────────────────────────────────────────
// Forest generation
// ────────────────────────────────────────────────────────────────────────────────

impl Forest {
    pub fn random(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        self.random_with_bounds(
            random,
            ctx,
            scope,
            ctx.min_expression_count,
            ctx.max_expression_count,
        );
    }

    pub fn random_with_bounds(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        min_expr: usize,
        max_expr: usize,
    ) {
        // Pre-compute function callables (they don't change during forest generation)
        let functions: Vec<(String, Vec<Type>, Type)> = scope
            .functions
            .iter()
            .map(|f| {
                (
                    f.name.clone(),
                    f.params.iter().map(|(_, ty)| ty.clone()).collect(),
                    f.ret.clone().unwrap_or(Type::Empty),
                )
            })
            .collect();

        for _ in 0..random.random_range(min_expr..max_expr) {
            let has_nodes = self.graph.node_count() > 0;
            let mut choices = vec![(NodeKind::Literal, ctx.literal_weight)];

            if has_nodes {
                choices.push((NodeKind::Variable, ctx.variable_weight));
                choices.push((NodeKind::Operator, ctx.operator_weight));
            }
            if self.type_kinds.contains_key(&TypeKind::Array) ||
                self.type_kinds.contains_key(&TypeKind::Slice)
            {
                choices.push((NodeKind::Index, ctx.index_weight));
            }
            if self.type_kinds.contains_key(&TypeKind::Tuple) {
                choices.push((NodeKind::TupleIndex, ctx.tuple_index_weight));
            }
            if self.type_kinds.contains_key(&TypeKind::Struct) {
                choices.push((NodeKind::FieldAccess, ctx.field_access_weight));
            }
            if has_nodes &&
                (!functions.is_empty() || self.type_kinds.contains_key(&TypeKind::Lambda))
            {
                choices.push((NodeKind::Call, ctx.call_weight));
            }
            if self.has_castable_types() {
                choices.push((NodeKind::Cast, ctx.cast_weight));
            }
            if self.has_mutable_variables() {
                choices.push((NodeKind::Assignment, ctx.assignment_weight));
            }
            // Only allow for loops and if statements with one level of nesting
            if self.has_integer_types() && self.depth < 2 {
                choices.push((NodeKind::ForLoop, ctx.for_loop_weight));
            }
            if self.has_boolean_types() && self.depth < 2 {
                choices.push((NodeKind::If, ctx.if_weight));
            }
            if self.has_boolean_types() || self.has_comparison_operators() {
                choices.push((NodeKind::Assert, ctx.assert_weight));
            }

            match choices.choose_weighted(random, |c| c.1).unwrap().0 {
                NodeKind::Literal => self.gen_literal(random, ctx, scope),
                NodeKind::Variable => self.gen_variable(random, ctx),
                NodeKind::Operator => self.gen_operator(random, ctx),
                NodeKind::Index => self.gen_index(random),
                NodeKind::TupleIndex => self.gen_tuple_index(random),
                NodeKind::FieldAccess => self.gen_field_access(random),
                NodeKind::Call => self.gen_call(random, &functions),
                NodeKind::Cast => self.gen_cast(random),
                NodeKind::Assignment => self.gen_assignment(random, ctx),
                NodeKind::ForLoop => self.gen_for_loop(random, ctx, scope),
                NodeKind::If => self.gen_if(random, ctx, scope),
                NodeKind::Assert => self.gen_assert(random, ctx),
                _ => {}
            }
        }

        // This is done to avoid the formatter from cropping the forest
        self.assign_leaf_operators(false);
    }

    fn assign_leaf_operators(&mut self, enforce_mutable: bool) {
        let assigned: HashSet<_> = self
            .nodes
            .get(&NodeKind::Variable)
            .into_iter()
            .flatten()
            .filter_map(|&v| self.left(v))
            .collect();

        let unassigned: Vec<_> = [
            NodeKind::Literal,
            NodeKind::Operator,
            NodeKind::Index,
            NodeKind::TupleIndex,
            NodeKind::FieldAccess,
            NodeKind::Call,
            NodeKind::Cast,
        ]
        .into_iter()
        .flat_map(|k| self.nodes.get(&k).into_iter().flatten().copied())
        .filter(|n| !assigned.contains(n))
        .collect();

        for idx in unassigned {
            let ty = self.ty(idx);
            let name = self.next_var();
            self.variable(name, ty.clone(), enforce_mutable, idx);

            self.register(idx, NodeKind::Variable, &ty, None);
        }
    }

    fn gen_literal(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        let ty = Type::random(random, ctx, scope, TypeLocation::Default);
        let idx = self.literal(ty.random_value(random, ctx, scope), ty.clone());
        self.register(idx, NodeKind::Literal, &ty, None);
    }

    fn gen_variable(&mut self, random: &mut impl Rng, ctx: &Context) {
        // We do this instead of using `node_indices().collect()choose(random)` because the first
        // one is O(n) and the second one is O(1)
        let source = NodeIndex::new(random.random_range(0..self.graph.node_count()));
        let ty = self.ty(source);
        let name = self.next_var();
        let idx =
            self.variable(name, ty.clone(), random.random_bool(ctx.mutable_probability), source);

        self.register(idx, NodeKind::Variable, &ty, None);
    }

    fn gen_operator(&mut self, random: &mut impl Rng, ctx: &Context) {
        // We do this instead of using `node_indices().collect()choose(random)` because the first
        // one is O(n) and the second one is O(1)
        let left = NodeIndex::new(random.random_range(0..self.graph.node_count()));
        let left_ty = self.ty(left);

        let (bin, un) = match left_ty.kind() {
            TypeKind::Field => (Operator::binary_field(), Operator::unary_field()),
            TypeKind::Signed => {
                (Operator::binary_integer_signed(), Operator::unary_integer_signed())
            }
            TypeKind::Unsigned => {
                (Operator::binary_integer_unsigned(), Operator::unary_integer_unsigned())
            }
            TypeKind::Boolean => (Operator::binary_boolean(), Operator::unary_boolean()),
            _ => return,
        };

        let peers = self.types.get(&left_ty).map(|v| v.as_slice()).unwrap_or(&[]);
        let use_unary = random.random_bool(ctx.unary_probability) || peers.len() < 2;

        if use_unary {
            let op = *un.choose(random).unwrap();
            let idx = self.operator(op, left_ty.clone(), left, None);

            self.register(idx, NodeKind::Operator, &left_ty, Some(op));
        } else {
            let rhs = *peers.choose(random).unwrap();
            let op = *bin.choose(random).unwrap();
            let ret = if op.is_comparison() { Type::Boolean } else { left_ty };
            let idx = self.operator(op, ret.clone(), left, Some(rhs));

            self.register(idx, NodeKind::Operator, &ret, Some(op));
        }
    }

    fn gen_index(&mut self, random: &mut impl Rng) {
        let parent = *self
            .type_kinds
            .get(&TypeKind::Array)
            .or_else(|| self.type_kinds.get(&TypeKind::Slice))
            .and_then(|v| v.choose(random))
            .unwrap();

        let (inner, size) = match self.ty(parent) {
            Type::Array(Array { ty, size, .. }) | Type::Slice(Slice { ty, size, .. }) => {
                ((*ty).clone(), size)
            }
            _ => unreachable!(),
        };

        let idx = self.index(parent, random.random_range(0..size));
        self.register(idx, NodeKind::Index, &inner, None);
    }

    fn gen_tuple_index(&mut self, random: &mut impl Rng) {
        let parent = *self.type_kinds.get(&TypeKind::Tuple).and_then(|v| v.choose(random)).unwrap();
        let elements = match self.ty(parent) {
            Type::Tuple(Tuple { elements, .. }) => elements,
            _ => unreachable!(),
        };

        let i = random.random_range(0..elements.len());
        let idx = self.tuple_index(parent, i);
        self.register(idx, NodeKind::TupleIndex, &elements[i], None);
    }

    fn gen_field_access(&mut self, random: &mut impl Rng) {
        let parent =
            *self.type_kinds.get(&TypeKind::Struct).and_then(|v| v.choose(random)).unwrap();
        let fields = match self.ty(parent) {
            Type::Struct(Struct { fields, .. }) => fields,
            _ => unreachable!(),
        };

        let field = fields.choose(random).unwrap();
        let idx = self.field_access(parent, field.name.clone());
        self.register(idx, NodeKind::FieldAccess, &field.ty, None);
    }

    fn gen_call(&mut self, random: &mut impl Rng, functions: &[(String, Vec<Type>, Type)]) {
        let mut callables: Vec<(String, Vec<Type>, Type)> = Vec::new();

        for &lambda_idx in self.type_kinds.get(&TypeKind::Lambda).into_iter().flatten() {
            let Type::Lambda(lambda) = self.ty(lambda_idx) else { continue };

            let name = self
                .nodes
                .get(&NodeKind::Variable)
                .into_iter()
                .flatten()
                .find(|&&var_idx| self.left(var_idx) == Some(lambda_idx))
                .and_then(|&var_idx| match &self.graph[var_idx] {
                    Node::Variable { name, .. } => Some(name.clone()),
                    _ => None,
                });

            // If the lambda variable is not found, skip it
            if name.is_none() {
                continue
            };

            let params: Vec<Type> = lambda.params.iter().map(|(_, ty)| ty.clone()).collect();
            let ret = (*lambda.ret).clone();
            callables.push((name.unwrap(), params, ret));
        }

        // Extend with pre-computed functions
        callables.extend(functions.iter().cloned());

        let Some((name, params, ret)) = callables.choose(random) else { return };
        let Some(args) = params.iter().map(|ty| self.types.get(ty)?.first().copied()).collect()
        else {
            return
        };

        let idx = self.call(name.clone(), ret.clone(), args);
        self.register(idx, NodeKind::Call, ret, None);
    }

    fn gen_cast(&mut self, random: &mut impl Rng) {
        const CASTABLE: [TypeKind; 4] =
            [TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned, TypeKind::Boolean];

        let sources: Vec<_> = CASTABLE
            .into_iter()
            .flat_map(|k| self.type_kinds.get(&k).into_iter().flatten().copied())
            .collect();
        let Some(&source) = sources.choose(random) else { return };
        let src_ty = self.ty(source);

        // Integer targets always valid; Field only from non-signed; Bool only from Bool
        let is_signed = matches!(src_ty, Type::Integer(Integer { signed: true, .. }));
        let targets: Vec<_> = [8, 16, 32, 64]
            .into_iter()
            .flat_map(|b| [Integer { bits: b, signed: true }, Integer { bits: b, signed: false }])
            .map(Type::Integer)
            .chain((!is_signed).then_some(Type::Field))
            .chain(matches!(src_ty, Type::Boolean).then_some(Type::Boolean))
            .filter(|t| *t != src_ty)
            .collect();

        let Some(target) = targets.choose(random).cloned() else { return };
        let idx = self.cast(source, target.clone());
        self.register(idx, NodeKind::Cast, &target, None);
    }

    #[inline(always)]
    fn has_castable_types(&self) -> bool {
        [TypeKind::Field, TypeKind::Signed, TypeKind::Unsigned, TypeKind::Boolean]
            .into_iter()
            .any(|k| self.type_kinds.contains_key(&k))
    }

    #[inline(always)]
    fn has_mutable_variables(&self) -> bool {
        // Check for mutable Variable nodes
        let has_mutable_vars = self.nodes.get(&NodeKind::Variable).map_or(false, |vars| {
            vars.iter().any(|&idx| matches!(&self.graph[idx], Node::Variable { mutable: true, .. }))
        });
        // Also check for mutable refs from parent scope (for loop bodies)
        has_mutable_vars || !self.mutable_refs.is_empty()
    }

    #[inline(always)]
    fn has_integer_types(&self) -> bool {
        self.type_kinds.contains_key(&TypeKind::Signed) ||
            self.type_kinds.contains_key(&TypeKind::Unsigned)
    }

    fn has_boolean_types(&self) -> bool {
        self.type_kinds.contains_key(&TypeKind::Boolean)
    }

    fn has_comparison_operators(&self) -> bool {
        // Check if we have any comparison operators (which produce boolean results)
        self.operators.keys().any(|op| op.is_comparison())
    }

    /// Find the root variable of an access chain (Index, TupleIndex, FieldAccess)
    /// Also returns Input nodes that are mutable refs from parent scope
    fn find_root_variable(&self, idx: NodeIndex) -> Option<NodeIndex> {
        let mut current = idx;
        loop {
            match &self.graph[current] {
                Node::Variable { .. } => return Some(current),
                Node::Input { name, .. } => {
                    // Check if this is a mutable ref from parent scope
                    if self.mutable_refs.contains_key(name) {
                        return Some(current);
                    }
                    return None;
                }
                Node::Index { .. } | Node::TupleIndex { .. } | Node::FieldAccess { .. } => {
                    current = self.left(current)?;
                }
                _ => return None,
            }
        }
    }

    /// Check if a node is rooted in a mutable variable (including mutable refs)
    fn is_rooted_in_mutable(&self, idx: NodeIndex) -> bool {
        self.find_root_variable(idx)
            .map(|root| match &self.graph[root] {
                Node::Variable { mutable: true, .. } => true,
                Node::Input { name, .. } => self.mutable_refs.contains_key(name),
                _ => false,
            })
            .unwrap_or(false)
    }

    fn gen_assignment(&mut self, random: &mut impl Rng, ctx: &Context) {
        // Collect all valid assignment targets:
        // 1. Mutable variables directly
        // 2. Mutable refs from parent scope (for loop bodies)
        // 3. Index/TupleIndex/FieldAccess nodes rooted in mutable variables/refs
        let mut targets: Vec<NodeIndex> = Vec::new();

        // Add mutable variables
        if let Some(vars) = self.nodes.get(&NodeKind::Variable) {
            for &idx in vars {
                if matches!(&self.graph[idx], Node::Variable { mutable: true, .. }) {
                    targets.push(idx);
                }
            }
        }

        // Add mutable refs from parent scope
        for &idx in self.mutable_refs.values() {
            targets.push(idx);
        }

        // Add compound accesses rooted in mutable variables/refs
        for kind in [NodeKind::Index, NodeKind::TupleIndex, NodeKind::FieldAccess] {
            if let Some(nodes) = self.nodes.get(&kind) {
                for &idx in nodes {
                    if self.is_rooted_in_mutable(idx) {
                        targets.push(idx);
                    }
                }
            }
        }

        let Some(&target_idx) = targets.choose(random) else { return };
        let target_ty = self.ty(target_idx);

        // Find a value of the same type to assign
        let Some(candidates) = self.types.get(&target_ty) else { return };
        let Some(&value_idx) = candidates.choose(random) else { return };

        // Don't assign the target to itself
        if value_idx == target_idx {
            return;
        }

        // Optionally use compound assignment if the type supports it
        let compound_op = if random.random_bool(ctx.compound_assignment_probability) {
            self.get_compound_operators(&target_ty).and_then(|ops| ops.choose(random).copied())
        } else {
            None
        };

        let idx = self.assignment(target_idx, value_idx, compound_op);
        self.register(idx, NodeKind::Assignment, &target_ty, None);
    }

    /// Returns compound assignment operators valid for the given type
    fn get_compound_operators(&self, ty: &Type) -> Option<&'static [Operator]> {
        match ty {
            Type::Field => Some(Operator::compound_field()),
            Type::Integer(i) if i.signed => Some(Operator::compound_integer_signed()),
            Type::Integer(_) => Some(Operator::compound_integer_unsigned()),
            Type::Boolean => Some(Operator::compound_boolean()),
            _ => None,
        }
    }

    fn gen_for_loop(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        // Choose a random integer type for the loop variable
        let bits = *[8u8, 16, 32, 64].choose(random).unwrap();
        let signed = random.random_bool(0.5);
        let loop_ty = Type::Integer(Integer { bits, signed });

        // Generate small boundary range values (max 10 iterations)
        // Use small values near 0 for simplicity
        let iterations = random.random_range(1i64..=10);
        let start_val: i64 =
            if signed { random.random_range(-5..=5) } else { random.random_range(0..=5) };
        let end_val = start_val + iterations;

        let ty_suffix = format!("{}{}", if signed { "i" } else { "u" }, bits);
        let start = format!("{}{}", start_val, ty_suffix);
        let end = format!("{}{}", end_val, ty_suffix);

        // Loop variable is always named "tmp"
        let loop_var = "tmp".to_string();

        // Create a sub-forest for the loop body
        // Variable counter continues from parent scope
        // Depth is incremented to prevent nested for loops
        let mut body = Forest::default();
        body.var_counter = self.var_counter;
        body.depth = self.depth + 1;

        // Add the loop variable "tmp" as a non-mutable input to the body forest
        let loop_var_idx = body.input(loop_var.clone(), loop_ty.clone());
        body.register(loop_var_idx, NodeKind::Input, &loop_ty, None);

        // Collect mutable variables from parent forest - they MUST be available for mutation
        if let Some(vars) = self.nodes.get(&NodeKind::Variable) {
            for &idx in vars {
                if let Node::Variable { name, ty, mutable: true } = &self.graph[idx] {
                    // Add as input in body forest for reference
                    let input_idx = body.input(name.clone(), ty.clone());
                    body.register(input_idx, NodeKind::Input, ty, None);
                    // Track as mutable variable reference for assignment targets
                    body.mutable_refs.insert(name.clone(), input_idx);
                }
            }
        }

        // Generate the body forest
        body.random_with_bounds(
            random,
            ctx,
            scope,
            ctx.min_for_loop_body_size,
            ctx.max_for_loop_body_size,
        );

        // Update var counter from body - variables after loop continue from where loop left off
        self.var_counter = body.var_counter;

        // Create the ForLoop node
        let idx = self.graph.add_node(Node::ForLoop {
            var: loop_var,
            ty: loop_ty.clone(),
            start,
            end,
            body: Box::new(body),
        });

        self.nodes.entry(NodeKind::ForLoop).or_default().push(idx);
    }

    fn gen_if(&mut self, random: &mut impl Rng, ctx: &Context, scope: &Scope) {
        // Get a boolean condition from existing nodes
        let Some(bool_nodes) = self.type_kinds.get(&TypeKind::Boolean) else { return };
        let Some(&condition) = bool_nodes.choose(random) else { return };

        // Collect mutable variables info for body creation
        let mutable_vars: Vec<(String, Type)> = self
            .nodes
            .get(&NodeKind::Variable)
            .into_iter()
            .flatten()
            .filter_map(|&idx| {
                if let Node::Variable { name, ty, mutable: true } = &self.graph[idx] {
                    Some((name.clone(), ty.clone()))
                } else {
                    None
                }
            })
            .collect();

        let depth = self.depth;

        // Create the "then" body
        let then_body = self.create_if_body(random, ctx, scope, &mutable_vars, depth);

        // Generate else-if branches
        let else_if_count = random.random_range(ctx.min_else_if_count..=ctx.max_else_if_count);
        let mut else_ifs: Vec<(NodeIndex, Box<Forest>)> = Vec::new();

        for _ in 0..else_if_count {
            // Get another boolean condition
            let Some(bool_nodes) = self.type_kinds.get(&TypeKind::Boolean) else { break };
            let Some(&else_if_cond) = bool_nodes.choose(random) else { break };

            let else_if_body = self.create_if_body(random, ctx, scope, &mutable_vars, depth);
            else_ifs.push((else_if_cond, Box::new(else_if_body)));
        }

        // Optionally generate else branch
        let else_body = if random.random_bool(ctx.else_probability) {
            let body = self.create_if_body(random, ctx, scope, &mutable_vars, depth);
            Some(Box::new(body))
        } else {
            None
        };

        // Create the If node
        let idx = self.graph.add_node(Node::If {
            condition,
            then_body: Box::new(then_body),
            else_ifs,
            else_body,
        });

        self.nodes.entry(NodeKind::If).or_default().push(idx);
    }

    fn create_if_body(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        mutable_vars: &[(String, Type)],
        parent_depth: usize,
    ) -> Forest {
        let mut body = Forest::default();
        body.var_counter = self.var_counter;
        body.depth = parent_depth + 1;

        // Add mutable variables from parent scope
        for (name, ty) in mutable_vars {
            let input_idx = body.input(name.clone(), ty.clone());
            body.register(input_idx, NodeKind::Input, ty, None);
            body.mutable_refs.insert(name.clone(), input_idx);
        }

        body.random_with_bounds(random, ctx, scope, ctx.min_if_body_size, ctx.max_if_body_size);

        // Update parent var counter
        self.var_counter = body.var_counter;

        body
    }

    fn gen_assert(&mut self, random: &mut impl Rng, ctx: &Context) {
        // Collect all valid boolean conditions:
        // 1. Boolean type nodes directly
        // 2. Comparison operator results (which are boolean)
        let mut conditions: Vec<NodeIndex> = Vec::new();

        // Add boolean type nodes
        if let Some(bool_nodes) = self.type_kinds.get(&TypeKind::Boolean) {
            conditions.extend(bool_nodes.iter().copied());
        }

        // Add comparison operator results
        for (op, indices) in &self.operators {
            if op.is_comparison() {
                conditions.extend(indices.iter().copied());
            }
        }

        let Some(&condition) = conditions.choose(random) else { return };

        // Optionally generate a message
        let message = if random.random_bool(ctx.assert_message_probability) {
            Some(format!("\"assertion failed\""))
        } else {
            None
        };

        let idx = self.graph.add_node(Node::Assert { condition, message });
        self.nodes.entry(NodeKind::Assert).or_default().push(idx);
    }
}
