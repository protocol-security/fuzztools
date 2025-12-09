# Fuzztools

This crate implements many types as well as stuff I will be using in my fuzzers. The reason why is because I am lazy as fuck and do not wanna copy-paste the same code over and over again :P

## Modules

- [`blockchain`](#blockchain) - Implements the consensus and execution spec types and constants.
- [`builders`](#builders) - Handles the logic of creating **VALID** types according to protocol rules.
- [`circuits`](#circuits) - Implements stuff to fuzz circuits, currently only Noir.
- [`evm`](#evm) - WIP
- [`math`](#math) - Math utilities for fuzzing (field elements, weighted selection, etc.).
- [`mutations`](#mutations) - Implements the `Mutable` trait for various types.
- [`net`](#net) - WIP.
- [`rpc`](#rpc) - WIP.
- [`transactions`](#transactions) - Implements the `Transaction` and `SignedTransaction` types.
- [`utils`](#utils) - Some stuff I do not know where to put like `FastPrivateKeySigner`.

## Example

The idea is to make it as easy as possible to create fuzzers that are 1) fast 2) easy to maintain and 3) actually useful. If you do not trust me, I already found many bugs in Ethereum clients, so check'em out [here](https://nethoxa.github.io/bug-bounties).

---

## Builders

This module contains the next builders:

- `TransactionBuilder` - Handles the logic of creating **VALID** transactions per mempool rules.

Moreover, it also contains a set of pre-deployed contracts as well as some optimizations to speed up stuff like an `RpcCache` that updates itself every time a new block is received.

### Cache

This submodule contains some structs that are used to cache data:

- `RpcCache` - Stores the `gas_price` and `max_priority_fee` for the current block as they are constant per block.

---

## Circuits

Implements random circuit generation for fuzzing Noir circuits. Generates valid Noir programs with:
- Random structs and globals
- Helper functions with proper scoping (no circular dependencies)
- Main function with public inputs
- Type-safe expression generation

---

## EVM

WIP

---

## Mutations

Mutation primitives for fuzzing.

Provides the `Mutable` trait and implementations for common types including primitives, collections, and Alloy types for Ethereum fuzzing.

### Supported Types

- Alloy's `Address`, `Bytes`, `FixedBytes<N>`, `U256`, `Authorization`, `AccessList`, `Vec<Authorization>` and `Vec<SignedAuthorization>`.
- `[T; N]` and `[u8; N]` arrays.
- `Vec<T>` and `Vec<u8>`.
- `u8`, `u16`, `u32`, `u64`, `u128`.

---

## Net

WIP

---

## RPC

WIP

---

## Transactions

This module contains the `Transaction` and `SignedTransaction` types. The interesting one is the `Transaction` type, which is a wrapper around all possible fields an Ethereum transaction can have (without taking into account EIP-4844 sidecars, which I don't think I will be using for now).

This is mainly used in `rakoon` for fuzzing transactions, as the idea is to send malformed transactions like a `Legacy` one with blob hashes or EIP-7702 with `gas_price` instead of `EIP-1559`'s. I could not do it via `alloy`'s types, so I had to implement it myself. Implements the usual RLP encoding, decoding and signing hash methods, so they work out of the box **AND** are equivalent to the ones in `alloy` (if not please lmk but I am 99% sure they are).

---

## Utils

This module contains utility functions and types that are used throughout all my fuzzers.

### Submodules

- `fast_signer` - Implements a secp256k1 signer that is faster than alloy's one by 20-30%.
- `random` - Implements the `choice` function for any `Rng` type, allowing to pick a random element from a non-empty slice.

