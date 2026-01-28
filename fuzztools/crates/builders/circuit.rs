//! Implements `CircuitBuilder`, whose task is to create and format **VALID** Noir circuits.

use crate::circuits::{context::Context, ir::Forest};
use rand::Rng;

#[derive(Default)]
pub struct CircuitBuilder {}

impl CircuitBuilder {
    pub fn generate(&self, random: &mut impl Rng, ctx: &Context) -> Forest {
        let mut forest = Forest::default();

        forest.random(random, ctx);

        forest
    }
}
