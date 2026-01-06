use std::{fs, process::Command};

use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::{Node, NodeKind},
        types::{Struct, Type, TypeKind},
    },
    context::Context,
    functions::Function,
    generators::types::TypeLocation,
    scope::Scope,
    Circuit,
};
use rand::{seq::IndexedRandom, Rng};

#[derive(Default)]
pub struct CircuitBuilder {}

impl CircuitBuilder {
    /// Create a random Noir circuit with all components
    pub fn generate(&self, random: &mut impl Rng, ctx: &Context) -> (Forest, Scope) {
        let scope = self.create_scope(random, ctx);
        let mut forest = Forest::default();

        // Add inputs to the forest
        for (name, ty) in &scope.inputs {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(idx, NodeKind::Input, ty, None);
        }

        // Add globals to the forest as inputs
        for (name, ty, _) in &scope.globals {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(idx, NodeKind::Input, ty, None);
        }

        forest.random(random, ctx, &scope);

        (forest, scope)
    }

    /// Create a scope with random structs, functions, globals, and inputs
    pub fn create_scope(&self, random: &mut impl Rng, ctx: &Context) -> Scope {
        let mut scope = Scope::default();

        // Create structs (can be used in functions, globals or main)
        let structs_count = random.random_range(ctx.min_struct_count..ctx.max_struct_count);
        for i in 0..structs_count {
            let s = Struct::random(random, ctx, &scope, format!("struct{}", i));
            scope.structs.push(s);
        }

        // Create globals (can be used in functions or main)
        let globals_count = random.random_range(ctx.min_globals_count..ctx.max_globals_count);
        for i in 0..globals_count {
            let ty = Type::random(random, ctx, &scope, TypeLocation::Default);
            let name = format!("global{}", i);
            let value = ty.random_value(random, ctx, &scope);

            scope.globals.push((name, ty, value));
        }

        // Create functions (can be used in other functions or main)
        let function_count = random.random_range(ctx.min_function_count..ctx.max_function_count);
        for i in 0..function_count {
            let mut func = Function::random(random, ctx, &scope, format!("fn{}", i));

            // Add parameters as inputs to the forest
            for (name, ty) in &func.params {
                let idx = func.body.input(name.clone(), ty.clone());
                func.body.register(idx, NodeKind::Input, ty, None);
            }

            // Boost weight of return type so body nodes are more likely to match ret
            let func_ctx = if let Some(ref ret) = func.ret {
                let mut func_ctx = ctx.clone();
                match ret.kind() {
                    TypeKind::Field => func_ctx.field_weight *= 100,
                    TypeKind::Unsigned => func_ctx.unsigned_weight *= 100,
                    TypeKind::Signed => func_ctx.signed_weight *= 100,
                    TypeKind::Boolean => func_ctx.boolean_weight *= 100,
                    TypeKind::String => func_ctx.string_weight *= 100,
                    TypeKind::Array => func_ctx.array_weight *= 100,
                    TypeKind::Slice => func_ctx.slice_weight *= 100,
                    TypeKind::Tuple => func_ctx.tuple_weight *= 100,
                    TypeKind::Struct => func_ctx.struct_weight *= 100,
                    TypeKind::Lambda => func_ctx.lambda_weight *= 100,
                    TypeKind::Empty => func_ctx.empty_weight *= 100,
                }
                func_ctx
            } else {
                ctx.clone()
            };

            func.body.random_with_bounds(
                random,
                &func_ctx,
                &scope,
                func_ctx.min_function_body_size,
                func_ctx.max_function_body_size,
            );

            // Create return expression if function has return type
            // Only consider Variable and Input nodes as return candidates
            func.ret_expr = func.ret.as_ref().map(|ret_ty| {
                let candidates: Vec<_> = func
                    .body
                    .types
                    .get(ret_ty)
                    .into_iter()
                    .flatten()
                    .copied()
                    .filter(|&idx| {
                        matches!(func.body.graph[idx], Node::Variable { .. } | Node::Input { .. })
                    })
                    .collect();
                if let Some(&idx) = candidates.choose(random) {
                    func.body.get_expr_for_node(idx)
                } else {
                    ret_ty.random_value(random, ctx, &scope)
                }
            });

            scope.functions.push(func);
        }

        // Create inputs (can be used in main)
        let input_count = random.random_range(ctx.min_input_count..ctx.max_input_count);
        let inputs = (0..input_count)
            .map(|i| {
                let ty = Type::random(random, ctx, &scope, TypeLocation::Main);
                let name = format!("input{}", i);
                (name, ty)
            })
            .collect::<Vec<(String, Type)>>();
        scope.inputs = inputs;

        scope
    }

    pub fn compile_circuit(circuit: Circuit, name: &str) {
        let tmp_dir = std::env::temp_dir().join("fuzztools");
        if !tmp_dir.exists() {
            fs::create_dir_all(&tmp_dir).unwrap();
        }

        // Create new Nargo project in such location
        let output = Command::new("nargo")
            .args(["new", name])
            .current_dir(&tmp_dir)
            .output()
            .expect("Failed to execute nargo new");

        if !output.status.success() {
            panic!("nargo new failed:\n{}", String::from_utf8_lossy(&output.stderr));
        }

        // Write circuit to the file
        let main_nr = tmp_dir.join(format!("{}/src/main.nr", name));
        fs::write(main_nr, circuit).unwrap();

        // Compile
        let output = Command::new("nargo")
            .args(["compile"])
            .current_dir(&tmp_dir.join(name))
            .output()
            .expect("Failed to execute nargo compile");

        if !output.status.success() {
            panic!(
                "nargo compile failed:\n{}{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
        }

        // Clean up on success
        fs::remove_dir_all(&tmp_dir.join(name)).unwrap();
    }

    pub fn generate_prover_toml(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
    ) -> String {
        let mut lines = Vec::new();
        for (name, ty) in &scope.inputs {
            let value = ty.random_value(random, ctx, scope);
            lines.push(format!("{} = {}", name, value));
        }
        lines.join("\n")
    }

    pub fn format_circuit(&self, forest: &Forest, scope: &Scope) -> Circuit {
        let mut out = String::new();

        // Emit struct definitions
        for s in &scope.structs {
            out.push_str(&format!("{}\n", s));
        }

        // Emit global definitions
        for (name, ty, value) in &scope.globals {
            out.push_str(&format!("global {}: {} = {};\n\n", name, ty, value));
        }

        // Emit function definitions
        for gf in &scope.functions {
            out.push_str(&format!("{}\n\n", gf));
        }

        // Emit main function @todo add public input/output
        let inputs = scope
            .inputs
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty))
            .collect::<Vec<String>>()
            .join(", ");
        out.push_str(&format!("fn main({}) {{\n", inputs));
        out.push_str(&format!("{}", forest));
        out.push_str("}\n");

        out
    }

}
