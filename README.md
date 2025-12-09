# Fuzztools

The main target of this repo is to have in one place a set of utilities to build fuzzers that are 1) fast 2) easy to maintain and 3) actually useful finding bugs.

- [fuzztools](./fuzztools/): a set of crates implementing common stuff to be used among all fuzzers
    - [blockchain](./fuzztools/crates/blockchain/): consensus and execution spec types and constants
    - [builders](./fuzztools/crates/builders/): logic for creating **VALID** types according to protocol rules
    - [circuits](./fuzztools/crates/circuits/): logic for creating **VALID** Noir circuits
    - [evm](./fuzztools/crates/evm/): (WIP)
    - [math](./fuzztools/crates/math/): math utilities (field elements, weighted selection, etc.)
    - [mutations](./fuzztools/crates/mutations/): `Mutable` trait implementation for base types
    - [net](./fuzztools/crates/net/): (WIP)
    - [rpc](./fuzztools/crates/rpc/): (WIP)
    - [transactions](./fuzztools/crates/transactions/): `Transaction` and `SignedTransaction` types
    - [utils](./fuzztools/crates/utils/): miscellaneous utilities like `FastPrivateKeySigner` or `choice` function for array types
- [mutable](./mutable/): macro that implements automatically the `Mutable` trait for arbitrary structs
- [noiruzz](./noiruzz/): Noir fuzzer
- [rakoon](./rakoon/): transaction fuzzer for the Ethereum protocol 