use crate::{
    circuits::{
        context::Context,
        types::{Struct, Type},
    },
    utils::random_string,
};
use rand::Rng;

pub struct Global {
    pub name: String,
    pub ty: Type,
}

impl Global {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            ty: Type::random(random, ctx, structs),
        }
    }
}

pub struct Input {
    pub name: String,
    pub ty: Type,
}

impl Input {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            ty: Type::random(random, ctx, structs),
        }
    }
}
