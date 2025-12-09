pub mod context;
pub mod expressions;
pub mod misc;
pub mod operators;
pub mod rules;
pub mod types;

use crate::utils::{random_string, RandomChoice};
use context::Context;
use expressions::{ExprType, FunctionDef, Scope, Statement, Variable};
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
                format!(
                    "global {}: {} = {};\n\n",
                    g.name,
                    g.ty,
                    g.ty.random_value(random, &ctx)
                )
                .as_str(),
            );
            globals.push(g);
        }

        // Generate helper functions (defined outside main, callable from main)
        // Similar to structs: each function can only call previously defined functions
        // to avoid circular dependencies. Function N can call functions [0..N-1].
        let func_count = random.random_range(0..ctx.max_functions_count);
        let mut functions: Vec<FunctionDef> = Vec::with_capacity(func_count);

        for _ in 0..func_count {
            // Pass only previously defined functions - the new function can call these
            let (func_def, func_code) = Self::generate_function(random, &ctx, &functions);
            code.push_str(&func_code);
            code.push_str("\n\n");
            // Add to list AFTER generation so it can't call itself
            functions.push(func_def);
        }

        // Generate main inputs (public inputs to the circuit)
        let size = random.random_range(1..=ctx.max_inputs_count);
        let mut inputs = Vec::with_capacity(size);
        for _ in 0..size {
            let i = Input::random(random, &ctx.entrypoint(), &structs);
            inputs.push(i);
        }

        // Create scope with main inputs as variables (available throughout main body)
        let mut scope = Scope::new();
        for input in &inputs {
            if let Some(expr_type) = Self::type_to_expr_type(&input.ty) {
                scope.add(Variable {
                    name: input.name.clone(),
                    ty: expr_type,
                    mutable: false, // main inputs are immutable
                });
            }
        }

        // Generate main function body statements
        // These can use: inputs, locally defined variables, and call helper functions
        let size = random.random_range(1..=ctx.max_main_expressions_count);
        let mut statements: Vec<String> = Vec::with_capacity(size);
        for _ in 0..size {
            let stmt = Statement::random(random, &ctx, &mut scope, &functions, 0);
            // Use format_indented(1) since we're inside main function body
            statements.push(format!("    {}", stmt.format_indented(1)));
        }

        // Assemble main function
        let inputs_str = inputs
            .iter()
            .map(|i| format!("{}: {}", i.name, i.ty))
            .collect::<Vec<_>>()
            .join(", ");
        code.push_str(
            format!("fn main({}) {{\n{}\n}}", inputs_str, statements.join("\n")).as_str(),
        );

        code
    }

    /// Generate a helper function with its own parameters and body.
    /// Returns the FunctionDef (for type-safe calls) and the generated code string.
    fn generate_function(
        random: &mut impl Rng,
        ctx: &Context,
        existing_functions: &[FunctionDef],
    ) -> (FunctionDef, String) {
        let name = random_string(random, ctx.max_name_characters_count);

        // Generate function parameters (1 to max)
        let param_count = random.random_range(1..=ctx.max_function_parameters_count);
        let mut params: Vec<(String, ExprType)> = Vec::with_capacity(param_count);

        for _ in 0..param_count {
            let param_name = random_string(random, ctx.max_name_characters_count);
            let param_type = Self::random_expr_type(random);
            params.push((param_name, param_type));
        }

        // Generate return type (functions return a simple type)
        let return_type = Self::random_expr_type(random);

        // Create FunctionDef for type tracking
        let func_def = FunctionDef {
            name: name.clone(),
            params: params.clone(),
            return_type: return_type.clone(),
        };

        // Create scope with function parameters
        let mut scope = Scope::new();
        for (param_name, param_type) in &params {
            scope.add(Variable {
                name: param_name.clone(),
                ty: param_type.clone(),
                mutable: false,
            });
        }

        // Generate function body statements
        let body_size = random.random_range(1..=ctx.max_block_expressions_count);
        let mut statements: Vec<String> = Vec::with_capacity(body_size);

        for _ in 0..body_size {
            let stmt = Statement::random(random, ctx, &mut scope, existing_functions, 0);
            // Use format_indented(1) since we're inside function body
            statements.push(format!("    {}", stmt.format_indented(1)));
        }

        // Generate return expression (must match return type)
        let ret_expr =
            expressions::Expr::random(random, ctx, return_type.clone(), &scope, existing_functions);
        statements.push(format!("    {}", ret_expr));

        // Format parameters
        let params_str: Vec<_> = params
            .iter()
            .map(|(n, t)| format!("{}: {}", n, t))
            .collect();

        // Assemble function code
        let func_code = format!(
            "fn {}({}) -> {} {{\n{}\n}}",
            name,
            params_str.join(", "),
            return_type,
            statements.join("\n")
        );

        (func_def, func_code)
    }

    /// Generate a random simple expression type (Field, Integer, or Boolean)
    fn random_expr_type(random: &mut impl Rng) -> ExprType {
        match random.random_range(0..3) {
            0 => ExprType::Field,
            1 => {
                let bits = *random.choice(&[8u8, 16, 32, 64]);
                let signed = random.random_bool(0.5);
                ExprType::Integer { bits, signed }
            }
            _ => ExprType::Boolean,
        }
    }

    /// Convert a Type to an ExprType for use in expression generation.
    /// Returns None for types that can't be used in arithmetic/boolean expressions.
    fn type_to_expr_type(ty: &Type) -> Option<ExprType> {
        match ty {
            Type::Field(_) => Some(ExprType::Field),
            Type::Integer(i) => Some(ExprType::Integer {
                bits: i.bits,
                signed: i.signed,
            }),
            Type::Boolean(_) => Some(ExprType::Boolean),
            // Complex types like arrays, tuples, structs, etc. don't map to simple ExprType
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_random_circuit() {
        let mut random = rand::rng();
        let ctx = Context {
            max_inputs_count: 5,
            min_element_count: 1,
            max_element_count: 10,
            min_string_size: 1,
            max_string_size: 10,
            max_expression_depth: 3,
            max_type_depth: 5,
            max_structs_count: 5,
            max_struct_fields_count: 5,
            max_globals_count: 5,
            max_functions_count: 5,
            max_function_parameters_count: 5,
            max_function_return_types_count: 5,
            max_main_expressions_count: 5,
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
            allow_slices: true,
            allow_references: true,
            allow_structs: true,
            filter_public_input_structs: false,
            type_depth: 0,
            expression_depth: 0,
        };

        let circuit = Circuit::random(&mut random, &ctx);
        println!("{}", circuit);
    }
}
