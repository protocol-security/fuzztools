use std::{collections::HashMap, fs, process::Command};

use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::NodeKind,
        types::{Struct, Type, TypeKind},
    },
    context::Context,
    functions::Function,
    generators::types::TypeLocation,
    scope::Scope,
    Circuit,
};
use rand::Rng;

#[derive(Default)]
pub struct CircuitBuilder {}

impl CircuitBuilder {
    pub fn generate(&self, random: &mut impl Rng, ctx: &Context) -> (Forest, Scope) {
        let scope = self.create_scope(random, ctx);
        let mut forest = Forest::default();

        for (name, ty, _) in &scope.inputs {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(idx, NodeKind::Input, ty, None);
        }
        for (name, ty, _) in &scope.globals {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(idx, NodeKind::Input, ty, None);
        }

        // If main has a return type, boost its weight so that the odds of creating a more "linked"
        // circuit are higher
        let gen_ctx = if let Some((ref ret, _)) = scope.main_return {
            let mut gen_ctx = *ctx;
            match ret.kind() {
                TypeKind::Field => gen_ctx.field_weight *= 100,
                TypeKind::Unsigned => gen_ctx.unsigned_weight *= 100,
                TypeKind::Signed => gen_ctx.signed_weight *= 100,
                TypeKind::Boolean => gen_ctx.boolean_weight *= 100,
                TypeKind::String => gen_ctx.string_weight *= 100,
                TypeKind::Array => gen_ctx.array_weight *= 100,
                TypeKind::Slice => gen_ctx.slice_weight *= 100,
                TypeKind::Tuple => gen_ctx.tuple_weight *= 100,
                TypeKind::Struct => gen_ctx.struct_weight *= 100,
                TypeKind::Lambda => gen_ctx.lambda_weight *= 100,
                TypeKind::Empty => gen_ctx.empty_weight *= 100,
            }
            gen_ctx
        } else {
            *ctx
        };

        forest.random(random, &gen_ctx, &scope);

        (forest, scope)
    }

    /// Create a scope with random structs, functions, globals, and inputs
    pub fn create_scope(&self, random: &mut impl Rng, ctx: &Context) -> Scope {
        let mut scope = Scope::default();

        // Create structs (can be used in functions, globals or main)
        for i in 0..random.random_range(ctx.min_struct_count..ctx.max_struct_count) {
            scope.structs.push(Struct::random(random, ctx, &scope, format!("struct{i}")));
        }

        // Create globals (can be used in functions or main)
        for i in 0..random.random_range(ctx.min_globals_count..ctx.max_globals_count) {
            let ty = Type::random(random, ctx, &scope, TypeLocation::Default);
            let value = ty.random_value(random, ctx, &scope, &HashMap::new());
            scope.globals.push((format!("global{i}"), ty, value));
        }

        // Create functions (can be used in other functions or main)
        for i in 0..random.random_range(ctx.min_function_count..ctx.max_function_count) {
            let mut func = Function::random(random, ctx, &scope, format!("fn{i}"));

            for (name, ty) in &func.params {
                let idx = func.body.input(name.clone(), ty.clone());
                func.body.register(idx, NodeKind::Input, ty, None);
            }

            // If it has a return type, boost its weight so that the odds of creating a more
            // "linked" body are higher
            let func_ctx = if let Some(ref ret) = func.ret {
                let mut func_ctx = *ctx;
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
                *ctx
            };

            func.body.random_with_bounds(
                random,
                &func_ctx,
                &scope,
                func_ctx.min_function_body_size,
                func_ctx.max_function_body_size,
                false,
            );

            func.ret_expr = func.ret.as_ref().map(|ret| {
                func.body
                    .find_return_candidate(random, ret)
                    .unwrap_or_else(|| ret.random_value(random, ctx, &scope, &func.body.exprs))
            });

            scope.functions.push(func);
        }

        // Create inputs (can be used in main) with random visibility
        scope.inputs = (0..random.random_range(ctx.min_input_count..ctx.max_input_count))
            .map(|i| {
                let ty = Type::random(random, ctx, &scope, TypeLocation::Main);
                (format!("input{i}"), ty, random.random_bool(ctx.public_input_probability))
            })
            .collect();

        if random.random_bool(ctx.main_return_probability) {
            let ret = Type::random(random, ctx, &scope, TypeLocation::Main);
            scope.main_return = Some((ret, true));
        }

        scope
    }

    pub fn compile_circuit(circuit: Circuit, name: &str) {
        let tmp_dir = std::env::temp_dir().join("fuzztools");
        fs::create_dir_all(&tmp_dir).unwrap();

        let output = Command::new("nargo")
            .args(["new", name])
            .current_dir(&tmp_dir)
            .output()
            .expect("Failed to execute nargo new");

        if !output.status.success() {
            panic!("nargo new failed:\n{}", String::from_utf8_lossy(&output.stderr));
        }

        // Write circuit to the file
        fs::write(tmp_dir.join(format!("{name}/src/main.nr")), circuit).unwrap();

        // Compile
        let output = Command::new("nargo")
            .args(["compile"])
            .current_dir(tmp_dir.join(name))
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
        fs::remove_dir_all(tmp_dir.join(name)).unwrap();
    }

    pub fn generate_prover_toml(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
    ) -> String {
        scope
            .inputs
            .iter()
            .map(|(name, ty, _)| {
                format!("{name} = {}", ty.random_value(random, ctx, scope, &HashMap::new()))
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn format_circuit(&self, forest: &Forest, scope: &Scope) -> Circuit {
        let mut out = String::new();

        for s in &scope.structs {
            out.push_str(&format!("{s}\n"));
        }

        for (name, ty, value) in &scope.globals {
            out.push_str(&format!("global {name}: {ty} = {value};\n\n"));
        }

        for func in &scope.functions {
            out.push_str(&format!("{func}\n\n"));
        }

        let inputs = scope
            .inputs
            .iter()
            .map(
                |(name, ty, is_pub)| {
                    if *is_pub {
                        format!("{name}: pub {ty}")
                    } else {
                        format!("{name}: {ty}")
                    }
                },
            )
            .collect::<Vec<_>>()
            .join(", ");

        let ret_sig = match &scope.main_return {
            Some((ty, true)) => format!(" -> pub {ty}"),
            Some((ty, false)) => format!(" -> {ty}"),
            None => String::new(),
        };

        out.push_str(&format!("fn main({inputs}){ret_sig} {{\n"));
        out.push_str(&forest.format_with_indent("    "));

        if let Some((ret, _)) = &scope.main_return {
            let ret_expr =
                forest.find_return_candidate(&mut rand::rng(), ret).unwrap_or_else(|| {
                    ret.random_value(&mut rand::rng(), &Context::default(), scope, &forest.exprs)
                });
            out.push_str(&format!("    {ret_expr}\n"));
        }

        out.push_str("}\n");
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn setup_project(dir: &std::path::Path, code: &str) -> Result<(), String> {
        fs::create_dir_all(dir).map_err(|e| e.to_string())?;
        fs::write(
            dir.join("Nargo.toml"),
            "[package]\nname = \"circuit\"\ntype = \"bin\"\nauthors = [\"\"]\n\n[dependencies]\n",
        )
        .map_err(|e| e.to_string())?;

        let src_dir = dir.join("src");
        fs::create_dir_all(&src_dir).map_err(|e| e.to_string())?;
        fs::write(src_dir.join("main.nr"), code).map_err(|e| e.to_string())
    }

    fn compile_project(dir: &std::path::Path) -> Result<(), String> {
        let output = Command::new("nargo")
            .args(["check", "--silence-warnings"])
            .current_dir(dir)
            .output()
            .map_err(|e| format!("Failed to spawn nargo: {e}"))?;

        if output.status.success() {
            Ok(())
        } else {
            Err(String::from_utf8_lossy(&output.stderr).to_string())
        }
    }

    #[test]
    fn test_generate_and_compile_circuit() {
        let ctx = Context::default();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();

        let (forest, scope) = builder.generate(&mut random, &ctx);
        let code = builder.format_circuit(&forest, &scope);

        println!("Circuit code:\n{code}");

        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let project_dir = temp_dir.path().join("circuit");

        setup_project(&project_dir, &code).expect("Failed to setup project");

        if let Err(e) = compile_project(&project_dir) {
            panic!("Circuit failed to compile:\n{e}");
        }
    }
}
