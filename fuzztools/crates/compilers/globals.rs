use super::{
    structs::Struct,
    types::{Type, TypeContext},
};
use crate::{compilers::config::Config, utils::random_string};
use rand::Rng;
use std::fmt;

pub struct Global {
    pub name: String,
    pub r#type: Type,
}

impl Global {
    pub fn random(random: &mut impl Rng, config: &Config, structs: &[Struct]) -> Self {
        let ctx = TypeContext::top_level();
        Self {
            name: random_string(random, config.max_string_size),
            r#type: Type::random(random, config, &ctx, structs),
        }
    }
}

impl fmt::Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "global {}: {}", self.name, self.r#type)
    }
}
