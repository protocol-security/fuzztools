use crate::circuits::{
    context::Context,
    expressions::{Expr, ExprType, FunctionDef, Scope, Statement, Variable},
    misc::{random_string, Global, Input},
    types::*,
    Circuit,
};
use rand::{seq::IndexedRandom, Rng};
use std::fmt::Write;

///  Handles the logic of creating **VALID** Noir circuits
pub struct CircuitBuilder {}

impl CircuitBuilder {
    /// Creates a random Noir circuit
    pub fn circuit(&self, random: &mut impl Rng, ctx: &Context) -> Circuit {
        let mut circuit = Circuit::new();

        let top_ctx = ctx.top();
        let entrypoint_ctx = ctx.entrypoint();
        let statement_ctx = ctx.statement();

        // First, we create all top level components, without they intrinsic values/statements
        let structs = self.random_structs(random, &top_ctx);
        let globals = self.random_globals(random, &top_ctx, &structs);
        let functions = self.random_functions(random, &top_ctx);
        let inputs = self.random_inputs(random, &entrypoint_ctx, &structs);

        // Then, we format each component into the output code, filling the values/statements
        // randomly
        self.write_structs(&structs, &mut circuit);
        self.write_globals(random, &top_ctx, &globals, &mut circuit);
        self.write_functions(random, &statement_ctx, &functions, &mut circuit);
        self.write_main(random, &statement_ctx, &inputs, &functions, &mut circuit);

        circuit
    }
}

// ============================================================================
// Random Type Generation
// ============================================================================

impl CircuitBuilder {
    /// Generates a random expression type: Field, Integer, or Boolean @todo extend to support more
    /// types
    fn random_type(random: &mut impl Rng) -> ExprType {
        match random.random_range(0..3) {
            0 => ExprType::Field,
            1 => {
                let signed = random.random_bool(0.5);
                let bits = if signed {
                    *[8u8, 16, 32, 64].choose(random).unwrap()
                } else {
                    *[1u8, 8, 16, 32, 64].choose(random).unwrap()
                };
                ExprType::Integer { bits, signed }
            }
            _ => ExprType::Boolean,
        }
    }

    /// This is used in functions to determine the return type. If this returns `None`, the function
    /// will not have a return type
    fn random_return_type(random: &mut impl Rng) -> Option<ExprType> {
        // @todo make this a vec of values? and pub¿?
        if random.random_bool(0.75) {
            // @todo make this configurable
            Some(Self::random_type(random))
        } else {
            None
        }
    }

    /// Converts a full Type to a simplified ExprType for expression generation
    /// Returns None for complex types (arrays, tuples, structs, etc.) @todo extend to support more
    /// types
    fn to_expr_type(ty: &Type) -> Option<ExprType> {
        match ty {
            Type::Field(_) => Some(ExprType::Field),
            Type::Integer(i) => Some(ExprType::Integer { bits: i.bits, signed: i.signed }),
            Type::Boolean(_) => Some(ExprType::Boolean),
            _ => None,
        }
    }
}

// ============================================================================
// Component Generation
// ============================================================================

impl CircuitBuilder {
    fn random_structs(&self, random: &mut impl Rng, ctx: &Context) -> Vec<Struct> {
        let count = random.random_range(0..ctx.max_structs_count);
        let mut structs = Vec::with_capacity(count);

        // Each struct can reference previously defined structs, that's why we pass a reference to
        // `structs`
        for _ in 0..count {
            structs.push(Struct::random(random, ctx, &structs));
        }

        structs
    }

    fn random_globals(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        structs: &[Struct],
    ) -> Vec<Global> {
        let count = random.random_range(0..ctx.max_globals_count);
        let mut globals = Vec::with_capacity(count);

        for _ in 0..count {
            // Globals can be typed as primitive types, structs or functions, so we pass them as
            // inputs @todo extend to support more types
            globals.push(Global::random(random, ctx, structs));
        }

        globals
    }

    fn random_functions(&self, random: &mut impl Rng, ctx: &Context) -> Vec<FunctionDef> {
        let count = random.random_range(0..ctx.max_functions_count);
        let mut functions = Vec::with_capacity(count);

        for _ in 0..count {
            let name = random_string(random, ctx.max_name_characters_count);

            let inputs_count = random.random_range(0..=ctx.max_function_parameters_count);
            let mut inputs = Vec::with_capacity(inputs_count);

            for _ in 0..inputs_count {
                let name = random_string(random, ctx.max_name_characters_count);
                let ty = Self::random_type(random); // @todo should allow to pass structs or functions as inputs
                inputs.push((name, ty));
            }

            // If `None` is returned, when formatted we strip the `-> $TYPE` signature from the
            // function signature
            let return_type = Self::random_return_type(random);

            functions.push(FunctionDef { name, inputs, return_type });
        }

        functions
    }

    fn random_inputs(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        structs: &[Struct],
    ) -> Vec<Input> {
        let count = random.random_range(1..=ctx.max_inputs_count); // @todo should allow to pass 0 inputs
        let mut inputs = Vec::with_capacity(count);

        for _ in 0..count {
            // We pass a different context as Noir has some rules on what can be passed as inputs to
            // the main function
            inputs.push(Input::random(random, &ctx, structs));
        }

        inputs
    }
}

// ============================================================================
// Circuit Formatting
// ============================================================================

impl CircuitBuilder {
    /// Formats a given struct as follows:
    /// ```noir
    /// struct $NAME {
    ///     $VISIBILITY_1 $FIELD_1: $TYPE,
    ///     $VISIBILITY_2 $FIELD_2: $TYPE,
    ///     ...
    /// }
    /// ```
    ///
    /// By default we treat all structs as private @todo
    fn write_structs(&self, structs: &[Struct], circuit: &mut String) {
        for s in structs {
            let _ = writeln!(circuit, "struct {} {{", s.name);
            for field in &s.fields {
                let _ = writeln!(circuit, "    {}{}: {},", field.visibility, field.name, field.ty);
            }
            circuit.push_str("}\n\n");
        }
    }

    /// Formats a given global as follows:
    /// ```noir
    /// global $NAME: $TYPE = $VALUE;
    /// ```
    fn write_globals(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        globals: &[Global],
        circuit: &mut String,
    ) {
        for global in globals {
            let value = global.ty.random_value(random, ctx);
            let _ = writeln!(circuit, "global {}: {} = {};\n", global.name, global.ty, value);
        }
    }

    /// Formats a given function as follows:
    /// ```noir
    /// fn $NAME($PARAMS) (-> $RETURN_TYPE)? {
    ///     $STATEMENTS
    ///
    ///     ($RETURN_EXPRESSION)?
    /// }
    /// ```
    ///
    /// By default we treat all functions as private @todo
    fn write_functions(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        functions: &[FunctionDef],
        circuit: &mut String,
    ) {
        for (index, func) in functions.iter().enumerate() {
            // Build scope from function parameters, so that we can reference them within the
            // function statements
            let mut scope = Scope::new();
            for (name, ty) in &func.inputs {
                scope.add(Variable {
                    name: name.clone(),
                    ty: ty.clone(),
                    mutable: false, // @todo
                });
            }

            // Only call functions defined before this one, to avoid circular dependencies
            let callable_functions = &functions[..index];

            // Generate body statements
            let statement_count = random.random_range(0..=ctx.max_block_expressions_count);
            let mut body = Vec::with_capacity(statement_count + 1);

            if statement_count > 0 {
                for _ in 0..statement_count {
                    let stmt = Statement::random(random, ctx, &mut scope, callable_functions, 0);
                    body.push(format!("    {}", stmt.format_indented(1)));
                }
            }

            // Add return expression if needed
            if let Some(ref return_type) = func.return_type {
                let expr =
                    Expr::random(random, ctx, return_type.clone(), &scope, callable_functions);
                body.push(format!("    {}", expr));
            }

            let body = body.join("\n");

            // Format function signature
            let params = func
                .inputs
                .iter()
                .map(|(name, ty)| format!("{name}: {ty}"))
                .collect::<Vec<_>>()
                .join(", ");

            let return_sig = match &func.return_type {
                Some(ty) => format!(" -> {ty}"),
                None => String::new(),
            };

            let _ =
                writeln!(circuit, "fn {}({}){} {{\n{}\n}}\n", func.name, params, return_sig, body);
        }
    }

    fn write_main(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        inputs: &[Input],
        functions: &[FunctionDef],
        circuit: &mut String,
    ) {
        // Build scope from main inputs
        let mut scope = Scope::new();
        for input in inputs {
            if let Some(ty) = Self::to_expr_type(&input.ty) {
                scope.add(Variable { name: input.name.clone(), ty, mutable: false });
            }
        }

        // Generate body statements
        let statement_count = random.random_range(0..=ctx.max_main_expressions_count);
        let mut body = Vec::with_capacity(statement_count);
        if statement_count > 0 {
            for _ in 0..statement_count {
                let stmt = Statement::random(random, ctx, &mut scope, functions, 0);
                body.push(format!("    {}", stmt.format_indented(1)));
            }
        }

        // Format main function
        let params = inputs
            .iter()
            .map(|input| format!("{}: {}", input.name, input.ty))
            .collect::<Vec<_>>()
            .join(", ");

        let _ = write!(circuit, "fn main({}) {{\n{}\n}}", params, body.join("\n"));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circuit_builder() {
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
            allow_slices: true,
            allow_references: true,
            allow_structs: true,
            filter_public_input_structs: false,
            type_depth: 0,
            expression_depth: 0,
        };

        let builder = CircuitBuilder {};
        let circuit = builder.circuit(&mut random, &ctx);
        println!("{}", circuit);
    }
}
