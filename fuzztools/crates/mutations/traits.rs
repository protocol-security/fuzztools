use rand::Rng;

/// This traits implements the `mutate` method for a given type
pub trait Mutable {
    /// Applies a random mutation to the type, ensuring it will always output a different value
    fn mutate(&mut self, random: &mut impl Rng) -> bool;
}

pub(crate) trait UintMutations {
    fn flip_bit(&mut self, random: &mut impl Rng);
    fn add(&mut self, random: &mut impl Rng);
    fn add_one(&mut self);
    fn add_mod(&mut self, random: &mut impl Rng);
    fn sub(&mut self, random: &mut impl Rng);
    fn sub_one(&mut self);
    fn sub_mod(&mut self, random: &mut impl Rng);
    fn mul(&mut self, random: &mut impl Rng);
    fn mul_mod(&mut self, random: &mut impl Rng);
    fn div(&mut self, random: &mut impl Rng);
    fn remainder(&mut self, random: &mut impl Rng);
    fn set_zero(&mut self);
    fn set_one(&mut self);
    fn set_max(&mut self);
    fn set_random(&mut self, random: &mut impl Rng);
    fn xor(&mut self, random: &mut impl Rng);
    fn and(&mut self, random: &mut impl Rng);
    fn or(&mut self, random: &mut impl Rng);
    fn not(&mut self);
    fn rotate_left(&mut self, random: &mut impl Rng);
    fn rotate_right(&mut self, random: &mut impl Rng);
    fn shift_left(&mut self, random: &mut impl Rng);
    fn shift_right(&mut self, random: &mut impl Rng);
    fn reverse_bits(&mut self);
}

pub(crate) trait UintInteresting {
    fn set_interesting(&mut self, random: &mut impl Rng);
}

pub(crate) trait ArrayMutations {
    fn byte_clone(&mut self, random: &mut impl Rng);
    fn byte_remove(&mut self, random: &mut impl Rng);
    fn byte_swap(&mut self, random: &mut impl Rng);
    fn byte_mutate(&mut self, random: &mut impl Rng);

    fn set_all_zero(&mut self);
    fn set_all_one(&mut self);
    fn set_all_max(&mut self);
    fn set_all_pattern(&mut self);
    fn set_all_random(&mut self, random: &mut impl Rng);

    fn rotate_left_by_n(&mut self, random: &mut impl Rng);
    fn rotate_right_by_n(&mut self, random: &mut impl Rng);
    fn shuffle_array(&mut self, random: &mut impl Rng);

    fn reverse_array(&mut self);

    fn slice_clone(&mut self, random: &mut impl Rng);
    fn slice_swap(&mut self, random: &mut impl Rng);
    fn slice_swap_with_invalid_utf8(&mut self, random: &mut impl Rng);
    fn slice_mutate(&mut self, random: &mut impl Rng);
}

pub(crate) trait FixedArrayMutations {
    fn byte_swap(&mut self, random: &mut impl Rng);
    fn byte_mutate(&mut self, random: &mut impl Rng);

    fn set_all_zero(&mut self);
    fn set_all_one(&mut self);
    fn set_all_max(&mut self);
    fn set_all_pattern(&mut self);
    fn set_all_random(&mut self, random: &mut impl Rng);

    fn rotate_left_by_n(&mut self, random: &mut impl Rng);
    fn rotate_right_by_n(&mut self, random: &mut impl Rng);
    fn shuffle_array(&mut self, random: &mut impl Rng);

    fn reverse_array(&mut self);

    fn slice_swap(&mut self, random: &mut impl Rng);
    fn slice_swap_with_invalid_utf8(&mut self, random: &mut impl Rng);
    fn slice_mutate(&mut self, random: &mut impl Rng);
}

pub(crate) trait ArrayInteresting {
    fn set_interesting_u8(&mut self, random: &mut impl Rng);
    fn set_interesting_u8_be(&mut self, random: &mut impl Rng);
    fn set_interesting_u16_le(&mut self, random: &mut impl Rng);
    fn set_interesting_u16_be(&mut self, random: &mut impl Rng);
    fn set_interesting_u32_le(&mut self, random: &mut impl Rng);
    fn set_interesting_u32_be(&mut self, random: &mut impl Rng);
    fn set_slice_with_invalid_utf8(&mut self, random: &mut impl Rng);
}

pub(crate) trait TransactionMutations {
    fn mutate_access_list(&mut self, random: &mut impl Rng);
    fn mutate_blob_versioned_hashes(&mut self, random: &mut impl Rng);
    fn mutate_authorization_list(&mut self, random: &mut impl Rng);
    fn mutate_calldata(&mut self, random: &mut impl Rng);
}
