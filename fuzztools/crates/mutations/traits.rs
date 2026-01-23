use rand::{seq::IndexedRandom, Rng};

pub trait Mutable {
    /// - If this is a basic type, mutates it according to the mutations defined in this crate.
    /// - If it is a struct, mutates each of its fields, calling `mutate` on each of them.
    ///
    /// It also returns a boolean indicating that, in case this is an `Option<T>`, the caller must
    /// set it to `None` if `true`.
    fn mutate(&mut self, random: &mut impl Rng) -> bool;
}

pub trait Random {
    /// - If this is a basic type, returns a random intance of itself.
    /// - If it is a struct, calls `random` on each of its fields.
    fn random(random: &mut impl Rng) -> Self;
}

pub(crate) trait InterestingMutations {
    /// Returns an interesting intance of itself.
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool;
}

/// This is a placeholder to differenciate between `Vec<numeric>` and `Vec<T>`
pub(crate) trait Phantom {}

// ────────────────────────────────────────────────────────────────────────────────
// Handy macros to auto implement stuff
// ────────────────────────────────────────────────────────────────────────────────

define_mutation!(VecMutations {
    Push,
    Remove,
    Replace,
    Swap,
    Default,
    Shuffle,
    Reverse,
    Duplicate,
    Mutate,
    MutateAll,
    SetNone,
});

define_mutation!(SliceMutations { Replace, Swap, Shuffle, Reverse, Mutate, MutateAll, SetNone });

define_mutation!(ArrayMutations {
    ValueSwap,
    ValueMutate,
    RotateLeftByN,
    RotateRightByN,
    ShuffleArray,
    ReverseArray,
    SliceSwap,
    SliceMutate,
    SetNone,
});

define_mutation!(BytesMutations {
    RandomBytePush,
    ByteClone,
    ByteRemove,
    ByteSwap,
    ByteMutate,
    SetAllZero,
    SetAllOne,
    SetAllMax,
    SetAllPattern,
    SetAllRandom,
    ShuffleArray,
    RotateLeftByN,
    RotateRightByN,
    ReverseArray,
    SliceClone,
    SliceSwap,
    SliceSwapWithInvalidUtf8,
    SliceMutate,
    SetInteresting,
    SetNone,
});

define_mutation!(UintMutations {
    FlipBit,
    Add,
    AddOne,
    AddMod,
    Sub,
    SubOne,
    SubMod,
    Mul,
    MulMod,
    Div,
    Remainder,
    SetZero,
    SetOne,
    SetMax,
    SetRandom,
    Xor,
    And,
    Or,
    Not,
    RotateLeft,
    RotateRight,
    ShiftLeft,
    ShiftRight,
    ReverseBits,
    SetInteresting,
    SwapAdjacentBits,
    FisherYatesShuffle,
    GrayCodeEncoding,
    SetNone,
});

define_mutation!(BytesSetInterestingMutations {
    SetInterestingU8,
    SetInterestingU8ReverseBits,
    SetInterestingU16LE,
    SetInterestingU16BE,
    SetInterestingU32LE,
    SetInterestingU32BE,
    SetInvalidUtf8,
    SetNone,
});

define_mutation!(AccessListMutations {
    SetNone,
    Clear,
    AddRandomEntry,
    RemoveEntry,
    SwapEntries,
    ReplaceEntry,
    AddEntryWithInterestingStorageKeys,
    MutateEntry,
});

define_mutation!(VecAuthorizationMutations {
    SetNone,
    Clear,
    AddRandomAuthorization,
    RemoveEntry,
    SwapEntries,
    ReplaceEntry,
    AddAuthorizationWithInterestingValues,
    MutateAddress,
    MutateChainId,
    MutateNonce,
    MutateEntry,
});
