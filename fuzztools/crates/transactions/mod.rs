//! # Transactions
//!
//! This module contains the `Transaction` and `SignedTransaction` types. The interesting one is the
//! `Transaction` type, which is a wrapper around all possible fields an Ethereum transaction can
//! have (without taking into account EIP-4844 sidecars, which I don't think I will be using for
//! now). This is mainly used in `rakoon` for fuzzing transactions, as the idea is to send malformed
//! transactions like a `Legacy` one with blob hashes or EIP-7702 with `gas_price` instead of
//! `EIP-1559`'s. I could not do it via `alloy`'s types, so I had to implement it myself. Implements
//! the usual RLP encoding, decoding and signing hash methods, so they work out of the box **AND**
//! are equivalent to the ones in `alloy` (if not please lmk but I am 99% sure they are).

pub mod transaction;
mod signed;

pub use signed::SignedTransaction;
pub use transaction::Transaction;

/// RLP encodes an `Option` field.
#[macro_export]
macro_rules! encode_field {
    ($field:expr, $out:expr) => {
        if let Some(value) = &$field {
            value.encode($out);
        }
    };
}

/// Gets the RLP length of an `Option` field.
#[macro_export]
macro_rules! field_len {
    ($field:expr) => {
        if let Some(value) = &$field {
            value.length()
        } else {
            0
        }
    };
}
