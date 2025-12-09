pub mod rules;
pub mod context;
pub mod types;
pub mod expressions;
pub mod operators;
pub mod misc;

use context::Context;
use misc::*;
use rand::Rng;
use types::*;

pub struct Circuit;

impl Circuit {
    pub fn random(random: &mut impl Rng, ctx: &Context) -> String {
        let ctx = ctx.top();
        let mut code = String::new();

        // Generate structs
        let size = random.random_range(0..ctx.max_structs_count);
        let mut structs = Vec::with_capacity(size);
        for _ in 0..size {
            let s = Struct::random(random, &ctx, &structs);
            let fields = s
                .fields
                .iter()
                .map(|f| format!("    {}{}: {},", f.visibility, f.name, f.ty))
                .collect::<Vec<_>>()
                .join("\n");
            code.push_str(format!("struct {} {{\n{}\n}}\n\n", s.name, fields).as_str());
            structs.push(s);
        }

        // Generate globals using as reference types the structs
        let size = random.random_range(0..ctx.max_globals_count);
        let mut globals = Vec::with_capacity(size);
        for _ in 0..size {
            let g = Global::random(random, &ctx, &structs);
            code.push_str(
                format!("global {}: {} = {};\n\n", g.name, g.ty, g.ty.random_value(random, &ctx))
                    .as_str(),
            );
            globals.push(g);
        }

        // @todo generate functions

        // Generate main inputs
        let size = random.random_range(0..ctx.max_inputs_count);
        let mut inputs = Vec::with_capacity(size);
        for _ in 0..size {
            let i = Input::random(random, &ctx.entrypoint(), &structs);
            inputs.push(i);
        }

        // Generate main function
        let size = random.random_range(0..ctx.max_main_expressions_count);
        let mut statements: Vec<String> = Vec::with_capacity(size);

        let inputs_str =
            inputs.iter().map(|i| format!("{}: {}", i.name, i.ty)).collect::<Vec<_>>().join(", ");
        code.push_str(
            format!("fn main({}) {{\n{}\n}}", inputs_str, statements.join("\n")).as_str(),
        );

        code
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_random_circuit() {
        let mut random = rand::thread_rng();
        let ctx = Context {
            max_inputs_count: 10,
            min_element_count: 0,
            max_element_count: 10,
            min_string_size: 0,
            max_string_size: 10,
            max_expression_depth: 5,
            max_type_depth: 5,
            max_structs_count: 10,
            max_struct_fields_count: 5,
            max_globals_count: 10,
            max_functions_count: 10,
            max_function_parameters_count: 10,
            max_function_return_types_count: 10,
            max_main_expressions_count: 10,
            max_block_expressions_count: 10,
            max_string_hashes_count: 10,
            max_name_characters_count: 10,
            max_small_upper_bound: 10,
            integer_signed_probability: 0.5,
            boundary_value_probability: 0.5,
            small_upper_bound_probability: 0.5,
            exclude_prime_probability: 0.5,
            mutable_probability: 0.5,
            raw_string_probability: 0.5,
            allow_slices: true,
            allow_references: true,
            allow_structs: true,
            filter_entrypoint_structs: true,
            type_depth: 0,
            expression_depth: 0,
        };

        let circuit = Circuit::random(&mut random, &ctx);
        println!("{}", circuit);
    }
}
