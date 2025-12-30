use crate::circuits::{
    ast::{
        forest::Forest,
        nodes::NodeKind,
        types::{Struct, Type},
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
    /// Create a random Noir circuit
    pub fn circuit(&self, random: &mut impl Rng, ctx: &Context) -> Forest {
        let scope = self.create_scope(random, ctx);

        let mut forest = Forest::default();

        // Add inputs to the forest
        for (name, ty) in &scope.inputs {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(idx, NodeKind::Input, ty, None);
        }

        // Add globals to the forest as inputs @todo variables?
        for (name, ty, _) in &scope.globals {
            let idx = forest.input(name.clone(), ty.clone());
            forest.register(idx, NodeKind::Input, ty, None);
        }

        forest.random(random, ctx, &scope);
        forest
    }

    pub fn format_circuit(&self, scope: &Scope, forest: &Forest) -> Circuit {
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

        // Emit main function
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

    /// Generate Prover.toml content with random values for each input
    pub fn generate_prover_toml(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
    ) -> String {
        let mut lines = Vec::new();
        for (name, ty) in &scope.inputs {
            let value = ty.random_value(random, ctx);
            lines.push(format!("{} = {}", name, value));
        }
        lines.join("\n")
    }

    /// Create a scope with random structs, functions, globals, and inputs
    pub fn create_scope(&self, random: &mut impl Rng, ctx: &Context) -> Scope {
        let mut scope = Scope::default();

        // Create structs (can be used in functions, globals or main)
        let structs_count = random.random_range(ctx.min_struct_count..ctx.max_struct_count);
        let structs = (0..structs_count)
            .map(|i| Struct::random(random, ctx, &scope, format!("s{}", i)))
            .collect();
        scope.structs = structs;

        // Create globals (can be used in functions or main)
        let globals_count = random.random_range(ctx.min_globals_count..ctx.max_globals_count);
        let globals = (0..globals_count)
            .map(|i| {
                let ty = Type::random(random, ctx, &scope, TypeLocation::Default);
                let name = format!("g{}", i);
                (name, ty.clone(), ty.random_value(random, ctx))
            })
            .collect::<Vec<(String, Type, String)>>();
        scope.globals = globals;

        // Create functions (can be used in other functions or main)
        let function_count = random.random_range(ctx.min_function_count..ctx.max_function_count);
        for i in 0..function_count {
            let mut func = Function::random(random, ctx, &scope, format!("fn{}", i));

            // Add parameters as inputs to the forest
            for (name, ty) in &func.params {
                let idx = func.body.input(name.clone(), ty.clone());
                func.body.register(idx, NodeKind::Input, ty, None);
            }

            func.body.random(random, ctx, &scope);

            // Create return expression if function has return type
            func.ret_expr = func.ret.as_ref().map(|ret_ty| {
                let candidates: Vec<_> =
                    func.body.types.get(ret_ty).into_iter().flatten().copied().collect();
                if let Some(&idx) = candidates.choose(random) {
                    func.body.get_expr_for_node(idx)
                } else {
                    ret_ty.random_value(random, ctx)
                }
            });

            scope.functions.push(func);
        }

        // Create inputs (can be used in main)
        let input_count = random.random_range(ctx.min_input_count..ctx.max_input_count);
        let inputs = (0..input_count)
            .map(|i| {
                let ty = Type::random(random, ctx, &scope, TypeLocation::Main);
                let name = format!("i{}", i);
                (name, ty)
            })
            .collect::<Vec<(String, Type)>>();
        scope.inputs = inputs;

        scope
    }
}
