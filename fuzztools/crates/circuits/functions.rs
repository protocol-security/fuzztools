use crate::circuits::{
    ast::{forest::Forest, types::Type},
    context::Context,
    generators::types::TypeLocation,
    scope::Scope,
};
use rand::Rng;

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret: Type,
    pub body: Forest,
}

impl Function {
    pub fn random(random: &mut impl Rng, ctx: &Context, scope: &Scope, name: String) -> Self {
        // First, generate function arguments
        let count = random
            .random_range(ctx.min_function_parameters_count..=ctx.max_function_parameters_count);
        let params: Vec<_> = (0..count)
            .map(|i| (format!("p{}", i), Type::random(random, ctx, scope, TypeLocation::Default)))
            .collect();

        // Generate return type
        let ret = Type::random(random, ctx, scope, TypeLocation::Default);

        // Then, bias generation towards them
        let mut bias: Vec<_> = params.iter().map(|(_, t)| t.clone()).collect();
        bias.push(ret.clone());

        let mut scope = scope.clone();
        scope.ret = Some((ret.clone(), false));
        scope.inputs = params.iter().cloned().map(|(n, t)| (n, t, false)).collect();
        scope.type_bias = Scope::compute_type_bias(&bias);

        let mut body = Forest::default();

        // Make arguments as inputs into the forest
        for (n, t) in &params {
            body.input(random, n.clone(), t.clone());
        }

        // Generate
        body.random_with_bounds(
            random,
            ctx,
            &scope,
            ctx.min_function_body_size,
            ctx.max_function_body_size,
            false,
        );

        body.set_return_expression(random, ctx, &scope);

        Self { name, params, ret, body }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params =
            self.params.iter().map(|(n, t)| format!("{}: {}", n, t)).collect::<Vec<_>>().join(", ");

        writeln!(f, "fn {}({}) -> {} {{", self.name, params, self.ret)?;
        writeln!(f, "{}", self.body.format_with_indent("    "))?;
        writeln!(f, "}}")
    }
}
