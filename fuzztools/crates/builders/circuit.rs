//! Implements `CircuitBuilder`, whose task is to create and format **VALID** Noir circuits.

use crate::circuits::{
    ast::{
        forest::Forest,
        types::{Struct, Type, TypeKind},
    },
    context::Context,
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
            forest.input(random, name.clone(), ty.clone());
        }
        for (name, ty, _) in &scope.globals {
            forest.input(random, name.clone(), ty.clone());
        }

        // If main has a return type, boost its weight so that the odds of creating a more "linked"
        // circuit are higher
        let gen_ctx = if let Some((ref ret, _)) = scope.ret {
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

        forest.random(random, &gen_ctx, &scope, true);

        (forest, scope)
    }

    pub fn format_circuit(&self, scope: &Scope, forest: &Forest) -> Circuit {
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

        let ret_sig = match &scope.ret {
            Some((ty, true)) => format!(" -> pub {ty}"),
            Some((ty, false)) => format!(" -> {ty}"),
            None => String::new(),
        };

        out.push_str(&format!("fn main({inputs}){ret_sig} {{\n"));
        out.push_str(&forest.format_with_indent("    "));
        out.push_str("}\n");

        out
    }

    #[inline(always)]
    pub fn generate_prover_toml(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
    ) -> String {
        scope
            .inputs
            .iter()
            .map(|(name, ty, _)| format!("{name} = {}", ty.random_value(random, ctx, scope, false)))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Create a scope with random structs, functions, globals, and inputs
    pub(crate) fn create_scope(&self, random: &mut impl Rng, ctx: &Context) -> Scope {
        let mut scope = Scope::default();

        // Create structs (can be used in functions, globals or main)
        for i in 0..random.random_range(ctx.min_struct_count..=ctx.max_struct_count) {
            scope.structs.push(Struct::random(random, ctx, &scope, format!("struct{i}")));
        }

        // Create globals (can be used in functions or main)
        for i in 0..random.random_range(ctx.min_globals_count..=ctx.max_globals_count) {
            let ty = Type::random(random, ctx, &scope, TypeLocation::Default);
            let value = ty.random_value(random, ctx, &scope, true);
            scope.globals.push((format!("global{i}"), ty, value));
        }

        // Create functions (can be used in other functions or main)
        // @todo I removed function calls and only rely on lambdas, maybe change in the future
        /*
        for i in 0..random.random_range(ctx.min_function_count..=ctx.max_function_count) {
            let mut func = Function::random(random, ctx, &scope, format!("fn{i}"));

            for (name, ty) in &func.params {
                let idx = func.body.input(name.clone(), ty.clone());
                func.body.register(random, idx, NodeKind::Input, ty, None);
            }

            func.body.random_with_bounds(
                random,
                ctx,
                &scope,
                ctx.min_function_body_size,
                ctx.max_function_body_size,
                false,
            );

            scope.functions.push(func);
        }
        */

        // Create inputs (can be used in main) with random visibility
        scope.inputs = (0..random.random_range(ctx.min_input_count..=ctx.max_input_count))
            .map(|i| {
                let ty = Type::random(random, ctx, &scope, TypeLocation::Main);
                (format!("input{i}"), ty, random.random_bool(ctx.public_input_probability))
            })
            .collect();

        if random.random_bool(ctx.main_return_probability) {
            let ret = Type::random(random, ctx, &scope, TypeLocation::Main);
            scope.ret = Some((ret, true));
        }

        scope
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, process::Command};

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
        let ctx =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();

        let (forest, scope) = builder.generate(&mut random, &ctx);
        let code = builder.format_circuit(&scope, &forest);

        println!("Circuit code:\n{code}");

        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let project_dir = temp_dir.path().join("circuit");

        setup_project(&project_dir, &code).expect("Failed to setup project");

        if let Err(e) = compile_project(&project_dir) {
            panic!("Circuit failed to compile:\n{e}");
        }
    }
}
