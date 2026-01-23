# Fuzztools

This crate implements many types as well as stuff I will be using in my fuzzers. The reason why is because I am lazy as fuck and do not wanna copy-paste the same code over and over again :P

## Modules

- [`blockchain`](#blockchain) - Implements the consensus and execution spec types and constants.
- [`builders`](#builders) - Handles the logic of creating **VALID** instances of *to-be-fuzzed* types.
- [`circuits`](#circuits) - Implements the Noir IR as well as stuff to create random circuits.
- [`mutations`](#mutations) - Implements the `Mutable` trait for various types.
- [`rpc`](#rpc) - Implements a blazingly fast `RpcClient` to send batched JSON-RPC requests.
- [`transactions`](#transactions) - Implements the `Transaction` and `SignedTransaction` types.

## Builders

- `TransactionBuilder` - Handles the logic of creating **VALID** transactions per mempool rules.
- `CircuitBuilder` - Handles the logic of creating **VALID** Noir circuits.

It also contains a set of Solidity contracts to test on-chain, like sending transactions that `selfdestruct` contracts mid-construction and some other stuff.

## Circuits

Implements the Noir IR to generate random circuits. I do follow a different approach against usual compiler fuzzers, whereas they use linear ASTs (`Vec<Expr>` and so) I use directed acyclic graphs. The benefit is that i can create very complex expresions in `O(1)` time by referencing index nodes rather than nested expressions within elements from the AST. This crate implements to a rewritter based on [circuzz](https://github.com/Rigorous-Software-Engineering/circuzz) that applies random equivalence-preserving mutations to the graph, so that it maintains the same "input-output map".

## Mutations

Implements the `Mutable` trait for the next types:

- Alloy's `Address`, `Bytes`, `FixedBytes<N>`, `U256`, `Authorization`, `AccessList`, `Vec<Authorization>` and `Vec<SignedAuthorization>`
- `[T; N]` and `[u8; N]` arrays
- `Vec<T>` and `Vec<u8>`
- `bool`, `u8`, `u16`, `u32`, `u64`, `u128`

## Rpc

Implements a blazingly fast `RpcClient` to send batched JSON-RPC requests.

## Transactions

This module contains the `Transaction` and `SignedTransaction` types. The interesting one is the `Transaction` type, which is a wrapper around all possible fields an Ethereum transaction can have (without taking into account EIP-4844 sidecars, which I don't think I will be using for now).

This is mainly used in `rakoon` for fuzzing transactions, as the idea is to send malformed transactions like a `Legacy` one with blob hashes or EIP-7702 with `gas_price` instead of `EIP-1559`'s. I could not do it via `alloy`'s types, so I had to implement it myself. Implements the usual RLP encoding, decoding and signing hash methods, so they work out of the box **AND** are equivalent to the ones in `alloy` (if not please lmk but I am 99% sure they are).
