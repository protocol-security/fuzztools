use rand::Rng;

/// Core trait for types that can be mutated during fuzzing.
///
/// Returns `true` if the mutation should propagate to the parent (e.g., setting an `Option` to
/// `None`).
pub trait Mutable {
    /// Applies a random mutation to the value.
    fn mutate(&mut self, random: &mut impl Rng) -> bool;
}

/// Trait for generating random instances of a type.
pub trait Random {
    /// Returns a random value of the type
    fn random(random: &mut impl Rng) -> Self;
}

pub(crate) trait Phantom {}

pub(crate) trait UintMutations {
    fn flip_bit(&mut self, random: &mut impl Rng);
    /// Adds a random value with saturation.
    fn add(&mut self, random: &mut impl Rng);
    /// Increments by one with saturation.
    fn add_one(&mut self);
    /// Adds a random value modulo another random value.
    fn add_mod(&mut self, random: &mut impl Rng);
    /// Subtracts a random value with saturation.
    fn sub(&mut self, random: &mut impl Rng);
    /// Decrements by one with saturation.
    fn sub_one(&mut self);
    /// Subtracts a random value modulo another random value.
    fn sub_mod(&mut self, random: &mut impl Rng);
    /// Multiplies by a random value with saturation.
    fn mul(&mut self, random: &mut impl Rng);
    /// Multiplies by a random value modulo another random value.
    fn mul_mod(&mut self, random: &mut impl Rng);
    /// Divides by a random non-zero value with saturation.
    fn div(&mut self, random: &mut impl Rng);
    /// Computes remainder with a random non-zero divisor.
    fn remainder(&mut self, random: &mut impl Rng);
    /// Sets the value to zero.
    fn set_zero(&mut self);
    /// Sets the value to one.
    fn set_one(&mut self);
    /// Sets the value to the maximum for the type.
    fn set_max(&mut self);
    /// Sets the value to a random value.
    fn set_random(&mut self, random: &mut impl Rng);
    /// XORs with a random value.
    fn xor(&mut self, random: &mut impl Rng);
    /// ANDs with a random value.
    fn and(&mut self, random: &mut impl Rng);
    /// ORs with a random value.
    fn or(&mut self, random: &mut impl Rng);
    /// Applies bitwise NOT.
    fn not(&mut self);
    /// Rotates bits left by a random amount.
    fn rotate_left(&mut self, random: &mut impl Rng);
    /// Rotates bits right by a random amount.
    fn rotate_right(&mut self, random: &mut impl Rng);
    /// Shifts bits left by a random amount.
    fn shift_left(&mut self, random: &mut impl Rng);
    /// Shifts bits right by a random amount.
    fn shift_right(&mut self, random: &mut impl Rng);
    /// Reverses all bits.
    fn reverse_bits(&mut self);
    /// Swaps adjacent bit pairs.
    fn swap_adjacent_bits(&mut self);
    /// Shuffles bytes using Fisher-Yates algorithm.
    fn fisher_yates_shuffle(&mut self, random: &mut impl Rng);
    /// Converts to Gray code encoding.
    fn gray_code_encoding(&mut self);
}

/// Mutation operations using known interesting values.
pub trait InterestingMutations {
    /// Sets the value to a known interesting edge case.
    fn set_interesting(&mut self, random: &mut impl Rng);
}

/// Mutation operations for byte sequences.
pub trait BytesMutations {
    /// Appends a random byte.
    fn random_byte_push(&mut self, random: &mut impl Rng);
    /// Duplicates a random byte and appends it.
    fn byte_clone(&mut self, random: &mut impl Rng);
    /// Removes a random byte.
    fn byte_remove(&mut self, random: &mut impl Rng);
    /// Swaps two random bytes.
    fn byte_swap(&mut self, random: &mut impl Rng);
    /// Mutates a random byte.
    fn byte_mutate(&mut self, random: &mut impl Rng);

    /// Sets all bytes to zero.
    fn set_all_zero(&mut self);
    /// Sets all bytes to one.
    fn set_all_one(&mut self);
    /// Sets all bytes to 0xFF.
    fn set_all_max(&mut self);
    /// Sets bytes to alternating 0x00 and 0xFF pattern.
    fn set_all_pattern(&mut self);
    /// Fills with random bytes.
    fn set_all_random(&mut self, random: &mut impl Rng);

    /// Rotates bytes left by a random amount.
    fn rotate_left_by_n(&mut self, random: &mut impl Rng);
    /// Rotates bytes right by a random amount.
    fn rotate_right_by_n(&mut self, random: &mut impl Rng);
    /// Shuffles all bytes randomly.
    fn shuffle_array(&mut self, random: &mut impl Rng);

    /// Reverses the byte order.
    fn reverse_array(&mut self);

    /// Duplicates a random slice and appends it.
    fn slice_clone(&mut self, random: &mut impl Rng);
    /// Swaps two random slices.
    fn slice_swap(&mut self, random: &mut impl Rng);
    /// Replaces a random slice with invalid UTF-8 sequences.
    fn slice_swap_with_invalid_utf8(&mut self, random: &mut impl Rng);
    /// Mutates a random slice.
    fn slice_mutate(&mut self, random: &mut impl Rng);
}

/// Mutation operations for generic arrays.
pub trait ArrayMutations {
    /// Swaps two random elements.
    fn value_swap(&mut self, random: &mut impl Rng);
    /// Mutates a random element.
    fn value_mutate(&mut self, random: &mut impl Rng);

    /// Rotates elements left by a random amount.
    fn rotate_left_by_n(&mut self, random: &mut impl Rng);
    /// Rotates elements right by a random amount.
    fn rotate_right_by_n(&mut self, random: &mut impl Rng);
    /// Shuffles all elements randomly.
    fn shuffle_array(&mut self, random: &mut impl Rng);

    /// Reverses the element order.
    fn reverse_array(&mut self);

    /// Swaps two random slices.
    fn slice_swap(&mut self, random: &mut impl Rng);
    /// Mutates all elements in a random slice.
    fn slice_mutate(&mut self, random: &mut impl Rng);
}
