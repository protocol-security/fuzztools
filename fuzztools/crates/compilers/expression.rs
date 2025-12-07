use super::{functions::Function, inputs::Input};
use crate::{
    compilers::{config::Config, globals::Global, structs::Struct, types::Type},
    utils::random_string,
};
use rand::Rng;

pub enum Expression {
    Assignment(Assignment),
    Assertion(Assertion),
    ForLoop(ForLoop),
    IfElse(IfElse),
    // @todo unconstrained can use while and loop as well as continue and break
    Lambda(Lambda),
}

impl Expression {
    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        depth: usize,
        functions: &[Function],
        structs: &[Struct],
        globals: &[Global],
        inputs: &[Input],
        local_variables: &mut Vec<(String, Type)>,
    ) -> Self {
        Self {}
    }

    pub fn to_string(&self) -> String {
        "".to_string()
    }
}
