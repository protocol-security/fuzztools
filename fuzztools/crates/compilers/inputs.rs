use super::{
    structs::Struct,
    types::{Type, TypeContext},
};
use crate::{compilers::config::Config, utils::random_string};
use rand::Rng;
use std::fmt;

pub struct Input {
    pub name: String,
    pub r#type: Type,
}

impl Input {
    pub fn random(random: &mut impl Rng, config: &Config, structs: &[Struct]) -> Self {
        let ctx = TypeContext::entrypoint();
        Self {
            name: random_string(random, config.max_string_size),
            r#type: Type::random(random, config, &ctx, structs),
        }
    }
}

impl fmt::Display for Input {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.r#type)
    }
}
