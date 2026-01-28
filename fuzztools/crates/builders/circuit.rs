//! Implements `CircuitBuilder`, whose task is to create and format **VALID** Noir circuits.

use crate::circuits::{
    context::Context,
    ir::{Forest, Struct},
};
use rand::Rng;

#[derive(Default)]
pub struct CircuitBuilder {}

impl CircuitBuilder {
    pub fn generate(&self, random: &mut impl Rng, ctx: &Context) -> Forest {
        let mut structs = Vec::new();
        for i in 0..random.random_range(ctx.min_struct_count..=ctx.max_struct_count) {
            structs.push(Struct::random(random, ctx, &structs, format!("struct{i}")));
        }

        let mut forest = Forest::default();
        forest.random(random, ctx, &structs);

        forest
    }
}
