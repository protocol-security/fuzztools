use super::{
    types::{Type, TypeContext},
    Visibility,
};
use crate::{
    compilers::{config::Config, globals::Global, structs::Struct},
    utils::random_string,
};
use rand::Rng;

pub type Parameter = (String, Type);
pub type ReturnType = Type;

pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    // pub expressions: Vec<Expression>,
    pub is_unconstrained: bool,
    pub is_oracle: bool,
}

impl Function {
    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        functions: &[Function],
        structs: &[Struct],
        globals: &[Global],
    ) -> Self {
        let name = random_string(random, config.name_size);

        let parameters_count = random.random_range(0..config.max_function_parameters_count);
        let parameters = (0..parameters_count)
            .map(|_| {
                (
                    random_string(random, config.name_size),
                    Type::random(random, config, ctx, structs),
                )
            })
            .collect();

        let return_type = Type::random(random, config, ctx, structs); // @todo possibly return function type instead like a lambda

        let is_unconstrained = random.random_bool(config.is_unconstrained_probability);
        let is_oracle = random.random_bool(config.is_oracle_probability);

        // let expressions = Vec::new();
        // @todo add expressions

        Self { name, parameters, return_type, is_unconstrained, is_oracle }
    }

    pub fn type_definition(&self) -> String {
        let parameters = self
            .parameters
            .iter()
            .map(|(name, r#type)| format!("{}: {}", name, r#type))
            .collect::<Vec<_>>()
            .join(", ");

        // @todo add is_unconstrained and is_oracle
        format!(
            // @todo return type "{}fn {}({}) -> {} {{\n{}\n}}",
            "fn {}({}) {{\n\n}}",
            self.name,
            parameters,
            // @todo return type self.return_type,
            // self.expressions.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n")
        )
    }
}
