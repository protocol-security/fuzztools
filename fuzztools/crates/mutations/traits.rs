//! Core traits for mutation-based fuzzing.

use rand::Rng;

/// Core trait for types that can be mutated during fuzzing.
///
/// Returns `true` if the parent should set this field to `None`.
pub trait Mutable {
    /// Applies a random mutation to the value.
    fn mutate(&mut self, random: &mut impl Rng) -> bool;
}

/// Trait for generating random instances of a type.
pub trait Random {
    /// Returns a random value of the type
    fn random(random: &mut impl Rng) -> Self;
}

/// Mutation operations using known interesting values.
pub(crate) trait InterestingMutations {
    /// Sets the value to a known interesting edge case.
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool;
}

pub(crate) trait Phantom {}
