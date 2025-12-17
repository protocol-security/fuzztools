use crate::circuits::{
    context::Context,
    scope::{Scope, Variable},
    expressions::Expression,
    statement::{Statement, Block},
    misc::{random_string, Global, Input},
    types::*,
    Circuit,
};
use rand::{seq::IndexedRandom, Rng};
use std::fmt::Write;

// ============================================================================
// Function Definition
// ============================================================================

/// A function that can be called in the circuit
#[derive(Clone)]
pub struct FunctionDef {
    pub name: String,
    pub inputs: Vec<(String, Type)>,
    pub return_type: Option<Type>,
}

// ============================================================================
// Name Generators
// ============================================================================

/// Tracks counters for consistent naming
#[derive(Default)]
struct NameGenerator {
    struct_count: usize,
    global_count: usize,
    function_count: usize,
    variable_count: usize,
}

impl NameGenerator {
    fn next_struct(&mut self) -> String {
        let name = format!("struct_{}", self.struct_count);
        self.struct_count += 1;
        name
    }

    fn next_global(&mut self) -> String {
        let name = format!("global_{}", self.global_count);
        self.global_count += 1;
        name
    }

    fn next_function(&mut self) -> String {
        let name = format!("function_{}", self.function_count);
        self.function_count += 1;
        name
    }

    fn next_variable(&mut self) -> String {
        let name = format!("variable_{}", self.variable_count);
        self.variable_count += 1;
        name
    }

    fn next_argument(&mut self, index: usize) -> String {
        format!("argument_{}", index)
    }
}

// ============================================================================
// Circuit Builder
// ============================================================================

/// Handles the logic of creating **VALID** Noir circuits
pub struct CircuitBuilder {
    names: NameGenerator,
}

impl CircuitBuilder {
    pub fn new() -> Self {
        Self { names: NameGenerator::default() }
    }

    /// Creates a random Noir circuit
    pub fn circuit(&mut self, random: &mut impl Rng, ctx: &Context) -> Circuit {
        let mut circuit = Circuit::new();

        let top_ctx = ctx.top();
        let entrypoint_ctx = ctx.entrypoint();
        let statement_ctx = ctx.statement();

        // First, create all top level components
        let structs = self.random_structs(random, &top_ctx);
        let globals = self.random_globals(random, &top_ctx, &structs);
        let functions = self.random_functions(random, &top_ctx);
        let inputs = self.random_inputs(random, &entrypoint_ctx, &structs);

        // Then, format each component into the output code
        self.write_structs(&structs, &mut circuit);
        self.write_globals(random, &top_ctx, &globals, &mut circuit);
        self.write_functions(random, &statement_ctx, &functions, &globals, &mut circuit);
        self.write_main(random, &statement_ctx, &inputs, &functions, &globals, &mut circuit);

        circuit
    }
}

impl Default for CircuitBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Component Generation
// ============================================================================

impl CircuitBuilder {
    fn random_structs(&mut self, random: &mut impl Rng, ctx: &Context) -> Vec<Struct> {
        let count = random.random_range(0..ctx.max_structs_count);
        let mut structs = Vec::with_capacity(count);

        for _ in 0..count {
            let name = self.names.next_struct();
            let field_count = random.random_range(1..=ctx.max_struct_fields_count);
            let fields: Vec<StructField> = (0..field_count)
                .map(|i| {
                    let field_name = format!("field_{}", i);
                    let ty = Box::new(Self::random_primitive_type(random, ctx));
                    let visibility = *[Visibility::Public, Visibility::Private].choose(random).unwrap();
                    StructField { name: field_name, ty, visibility }
                })
                .collect();

            structs.push(Struct { name, fields });
        }

        structs
    }

    fn random_globals(&mut self, random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Vec<Global> {
        let count = random.random_range(0..ctx.max_globals_count);
        (0..count)
            .map(|_| {
                let name = self.names.next_global();
                let ty = Type::random(random, ctx, structs);
                Global { name, ty }
            })
            .collect()
    }

    fn random_functions(&mut self, random: &mut impl Rng, ctx: &Context) -> Vec<FunctionDef> {
        let count = random.random_range(0..ctx.max_functions_count);
        let mut functions = Vec::with_capacity(count);

        for _ in 0..count {
            let name = self.names.next_function();
            let inputs_count = random.random_range(0..=ctx.max_function_parameters_count);
            let inputs: Vec<_> = (0..inputs_count)
                .map(|i| {
                    let arg_name = self.names.next_argument(i);
                    let ty = Self::random_primitive_type(random, ctx);
                    (arg_name, ty)
                })
                .collect();

            let return_type = if random.random_bool(0.75) {
                Some(Self::random_primitive_type(random, ctx))
            } else {
                None
            };

            functions.push(FunctionDef { name, inputs, return_type });
        }

        functions
    }

    fn random_inputs(&mut self, random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Vec<Input> {
        let count = random.random_range(1..=ctx.max_inputs_count);
        (0..count)
            .map(|i| {
                let name = self.names.next_argument(i);
                let ty = Type::random(random, &ctx.entrypoint(), structs);
                Input { name, ty }
            })
            .collect()
    }

    /// Generate a random primitive type for function params/returns
    fn random_primitive_type(random: &mut impl Rng, ctx: &Context) -> Type {
        match random.random_range(0..3) {
            0 => Type::Field,
            1 => Type::Integer(Integer::random(random, ctx)),
            2 => Type::Boolean,
            _ => unreachable!(),
        }
    }
}

// ============================================================================
// Circuit Formatting
// ============================================================================

impl CircuitBuilder {
    fn write_structs(&self, structs: &[Struct], circuit: &mut String) {
        for s in structs {
            let _ = writeln!(circuit, "struct {} {{", s.name);
            for field in &s.fields {
                let _ = writeln!(circuit, "    {}{}: {},", field.visibility, field.name, field.ty);
            }
            circuit.push_str("}\n\n");
        }
    }

    fn write_globals(&self, random: &mut impl Rng, ctx: &Context, globals: &[Global], circuit: &mut String) {
        for global in globals {
            let value = global.ty.random_value(random, ctx);
            let _ = writeln!(circuit, "global {}: {} = {};\n", global.name, global.ty, value);
        }
    }

    fn write_functions(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        functions: &[FunctionDef],
        globals: &[Global],
        circuit: &mut String,
    ) {
        for (index, func) in functions.iter().enumerate() {
            // Build scope from function parameters AND globals
            let mut scope = Scope::new();

            // Add globals to scope (they can be referenced in functions)
            for global in globals {
                scope.add(Variable {
                    name: global.name.clone(),
                    ty: global.ty.clone(),
                    mutable: false, // globals are immutable
                });
            }

            // Add function parameters
            for (name, ty) in &func.inputs {
                scope.add(Variable {
                    name: name.clone(),
                    ty: ty.clone(),
                    mutable: false,
                });
            }

            // Only call functions defined before this one (to avoid circular dependencies)
            let callable_functions = &functions[..index];

            // Generate body statements
            let statement_count = random.random_range(0..=ctx.max_block_expressions_count);
            let mut body = Vec::with_capacity(statement_count + 1);

            for _ in 0..statement_count {
                let stmt = self.random_statement_with_calls(random, ctx, &mut scope, callable_functions, 0);
                body.push(format!("    {}", stmt.format_indented(1)));
            }

            // Add return expression if needed
            if let Some(ref return_type) = func.return_type {
                let expr = Expression::random(random, ctx, return_type.clone(), &scope);
                body.push(format!("    {}", expr));
            }

            // Format function signature
            let params = func.inputs
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, ty))
                .collect::<Vec<_>>()
                .join(", ");

            let return_sig = match &func.return_type {
                Some(ty) => format!(" -> {}", ty),
                None => String::new(),
            };

            let _ = writeln!(circuit, "fn {}({}){} {{\n{}\n}}\n", func.name, params, return_sig, body.join("\n"));
        }
    }

    fn write_main(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        inputs: &[Input],
        functions: &[FunctionDef],
        globals: &[Global],
        circuit: &mut String,
    ) {
        // Build scope from main inputs AND globals
        let mut scope = Scope::new();

        // Add globals to scope
        for global in globals {
            scope.add(Variable {
                name: global.name.clone(),
                ty: global.ty.clone(),
                mutable: false,
            });
        }

        // Add main inputs
        for input in inputs {
            scope.add(Variable {
                name: input.name.clone(),
                ty: input.ty.clone(),
                mutable: false,
            });
        }

        // Generate body statements
        let statement_count = random.random_range(0..=ctx.max_main_expressions_count);
        let mut body = Vec::with_capacity(statement_count);

        for _ in 0..statement_count {
            let stmt = self.random_statement_with_calls(random, ctx, &mut scope, functions, 0);
            body.push(format!("    {}", stmt.format_indented(1)));
        }

        // Format main function
        let params = inputs
            .iter()
            .map(|input| format!("{}: {}", input.name, input.ty))
            .collect::<Vec<_>>()
            .join(", ");

        let _ = write!(circuit, "fn main({}) {{\n{}\n}}", params, body.join("\n"));
    }

    /// Generate a statement that may include function calls
    fn random_statement_with_calls(
        &mut self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &mut Scope,
        functions: &[FunctionDef],
        depth: usize,
    ) -> Statement {
        let max_depth_reached = depth >= ctx.max_expression_depth;

        let mut choices: Vec<&str> = vec!["let", "assert"];

        if !scope.mutable_variables().is_empty() {
            choices.push("assign");
            choices.push("compound_assign");
        }

        // Add function call and lambda options
        if !functions.is_empty() {
            choices.push("call");
        }
        choices.push("lambda");

        if !max_depth_reached {
            choices.push("if");
            choices.push("for");
        }

        match *choices.choose(random).unwrap() {
            "let" => self.random_let_with_name(random, ctx, scope),
            "assign" => self.random_assign(random, ctx, scope),
            "compound_assign" => self.random_compound_assign(random, ctx, scope),
            "assert" => self.random_assert(random, ctx, scope),
            "call" => self.random_call_statement(random, ctx, scope, functions),
            "lambda" => self.random_lambda(random, ctx, scope),
            "if" => self.random_if(random, ctx, scope, depth),
            "for" => self.random_for(random, ctx, scope, depth),
            _ => self.random_let_with_name(random, ctx, scope),
        }
    }

    fn random_let_with_name(&mut self, random: &mut impl Rng, ctx: &Context, scope: &mut Scope) -> Statement {
        let name = self.names.next_variable();
        let mutable = random.random_bool(ctx.mutable_probability);
        let ty = Type::random(random, ctx, &[]);
        let expr = Expression::random(random, ctx, ty.clone(), scope);

        scope.add(Variable { name: name.clone(), ty: ty.clone(), mutable });

        Statement::Let { name, ty, expr, mutable }
    }

    fn random_assign(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> Statement {
        let mutable_vars = scope.mutable_variables();
        if mutable_vars.is_empty() {
            return Statement::Assert {
                condition: Expression::random_boolean(random, ctx, scope),
                message: None,
            };
        }

        let var = *mutable_vars.choose(random).unwrap();
        let expr = Expression::random(random, ctx, var.ty.clone(), scope);

        Statement::Assign { name: var.name.clone(), expr }
    }

    fn random_compound_assign(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> Statement {
        use crate::circuits::operators::Operator;

        let mutable_vars: Vec<_> = scope.mutable_variables()
            .into_iter()
            .filter(|v| matches!(v.ty, Type::Field | Type::Integer(_)))
            .collect();

        if mutable_vars.is_empty() {
            return Statement::Assert {
                condition: Expression::random_boolean(random, ctx, scope),
                message: None,
            };
        }

        let var = *mutable_vars.choose(random).unwrap();
        let expr = Expression::random(random, ctx, var.ty.clone(), scope);

        let op = match &var.ty {
            Type::Field => *Operator::arithmetic_field().choose(random).unwrap(),
            Type::Integer(_) => *Operator::arithmetic_integer().choose(random).unwrap(),
            _ => unreachable!(),
        };

        Statement::CompoundAssign { name: var.name.clone(), op, expr }
    }

    fn random_assert(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> Statement {
        let condition = Expression::random_boolean(random, ctx, scope);
        let message = if random.random_bool(0.5) {
            Some(random_string(random, ctx.max_name_characters_count))
        } else {
            None
        };

        Statement::Assert { condition, message }
    }

    fn random_call_statement(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        functions: &[FunctionDef],
    ) -> Statement {
        if functions.is_empty() {
            return self.random_assert(random, ctx, scope);
        }

        let func = functions.choose(random).unwrap();

        // Generate arguments for the function
        let args: Vec<Expression> = func.inputs
            .iter()
            .map(|(_, ty)| Expression::random(random, ctx, ty.clone(), scope))
            .collect();

        // Create a function call expression
        let call_expr = Expression::Call {
            func: func.name.clone(),
            args,
        };

        // If function returns a value, assign it to a variable
        if let Some(ref return_type) = func.return_type {
            Statement::Let {
                name: format!("result_{}", random.random_range(0..1000u32)),
                ty: return_type.clone(),
                expr: call_expr,
                mutable: false,
            }
        } else {
            // Just call the function as an expression statement
            Statement::Expression(call_expr)
        }
    }

    fn random_lambda(&mut self, random: &mut impl Rng, ctx: &Context, scope: &mut Scope) -> Statement {
        let name = self.names.next_variable();

        let param_count = random.random_range(0..=ctx.max_function_parameters_count);
        let params: Vec<(String, Type)> = (0..param_count)
            .map(|i| {
                let pname = format!("p{}", i);
                let pty = Self::random_primitive_type(random, ctx);
                (pname, pty)
            })
            .collect();

        // Create a temporary scope with lambda parameters
        let mut lambda_scope = scope.clone();
        lambda_scope.push();
        for (pname, pty) in &params {
            lambda_scope.add(Variable { name: pname.clone(), ty: pty.clone(), mutable: false });
        }

        // Generate return type and body
        let ret_type = Self::random_primitive_type(random, ctx);
        let body = Expression::random(random, ctx, ret_type.clone(), &lambda_scope);

        // Add lambda to outer scope (as a variable with lambda "type")
        // Note: In Noir, lambdas are first-class values
        scope.add(Variable { name: name.clone(), ty: ret_type, mutable: false });

        Statement::Lambda { name, params, body }
    }

    fn random_if(&mut self, random: &mut impl Rng, ctx: &Context, scope: &mut Scope, depth: usize) -> Statement {
        let branch_count = random.random_range(1..ctx.max_if_else_branch_count);
        let mut branches = Vec::with_capacity(branch_count);

        for _ in 0..branch_count {
            let condition = Expression::random_boolean(random, ctx, scope);
            let body = self.random_block(random, ctx, scope, depth + 1);
            branches.push((condition, body));
        }

        let else_block = if random.random_bool(0.5) {
            Some(self.random_block(random, ctx, scope, depth + 1))
        } else {
            None
        };

        Statement::If { branches, else_block }
    }

    fn random_for(&mut self, random: &mut impl Rng, ctx: &Context, scope: &mut Scope, depth: usize) -> Statement {
        let var = self.names.next_variable();

        let (bits, signed) = *INTEGER_PARAMS.choose(random).unwrap();
        let iter_type = Type::Integer(Integer { signed, bits });

        let start = Expression::random(random, ctx, iter_type.clone(), scope);
        let end = Expression::random(random, ctx, iter_type.clone(), scope);

        let mut body_scope = scope.clone();
        body_scope.push();
        body_scope.add(Variable {
            name: var.clone(),
            ty: iter_type,
            mutable: false,
        });

        let body = self.random_block(random, ctx, &mut body_scope, depth + 1);

        Statement::For { var, start, end, body }
    }

    fn random_block(&mut self, random: &mut impl Rng, ctx: &Context, outer_scope: &Scope, depth: usize) -> Block {
        let mut scope = outer_scope.clone();
        scope.push();

        let count = random.random_range(1..=ctx.max_block_expressions_count);
        let statements = (0..count)
            .map(|_| Statement::random(random, ctx, &mut scope, depth))
            .collect();

        Block { statements }
    }
}

// ============================================================================
// Tests
// ============================================================================

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
            allow_structs: true,
            filter_entrypoint_structs: false,
            type_depth: 0,
            expression_depth: 0,
            new_variable_probability: 0.5,
            max_if_else_branch_count: 4,
        };

        let mut builder = CircuitBuilder::new();
        let circuit = builder.circuit(&mut random, &ctx);
        println!("{}", circuit);
    }
}
