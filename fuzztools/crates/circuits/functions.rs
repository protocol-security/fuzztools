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
    pub ret: Option<Type>,
    pub body: Forest,
    pub ret_expr: Option<String>,
}

impl Function {
    pub fn random(random: &mut impl Rng, ctx: &Context, scope: &Scope, name: String) -> Self {
        let param_count = random
            .random_range(ctx.min_function_parameters_count..ctx.max_function_parameters_count);
        let params: Vec<(String, Type)> = (0..param_count)
            .map(|i| (format!("p{}", i), Type::random(random, ctx, scope, TypeLocation::Default)))
            .collect();

        let ret = random
            .random_bool(ctx.function_return_probability)
            .then(|| Type::random(random, ctx, scope, TypeLocation::Default));

        Self { name, params, ret, body: Forest::default(), ret_expr: None }
    }

    pub fn signature(&self) -> String {
        let params =
            self.params.iter().map(|(n, t)| format!("{}: {}", n, t)).collect::<Vec<_>>().join(", ");
        let ret = self.ret.as_ref().map(|t| format!(" -> {}", t)).unwrap_or_default();
        format!("fn {}({}){}", self.name, params, ret)
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.signature())?;
        write!(f, "{}", self.body)?;
        if let Some(ret_expr) = &self.ret_expr {
            writeln!(f, "    {}", ret_expr)?;
        }
        writeln!(f, "}}")
    }
}
