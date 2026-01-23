## Fuzztools

This crate implements many types as well as stuff I will be using in my fuzzers. The reason why is because I am lazy as fuck and do not wanna copy-paste the same code over and over again :P

### Modules

- [`blockchain`](#blockchain) - Implements the consensus and execution spec types and constants.
- [`builders`](#builders) - Handles the logic of creating **VALID** instances of *to-be-fuzzed* types.
- [`circuits`](#circuits) - Implements the Noir IR as well as stuff to create random circuits.
- [`mutations`](#mutations) - Implements the `Mutable` trait for various types.
- [`rpc`](#rpc) - Implements a blazingly fast `RpcClient` to send batched JSON-RPC requests.
- [`transactions`](#transactions) - Implements the `Transaction` and `SignedTransaction` types.

## Mutable

This crate implements the `Mutable` derive macro, that implements `Mutable` automatically for arbitrary structs.

```rs
#[derive(Mutable)]
struct Payload {
    a: u64,
    b: u64
}

fn main() {
    let mut base = Payload {
        a: 3,
        b: 5
    };
    let mut random = SmallRng::seed_from_u64(0);

    loop {
        base.mutate(&mut random);

        // Check your target condition or send the payload
        if base.a + base.b == 5 {
            panic!("POC");
        }
    }
}
```

## Noiruzz

Metamorphic fuzzer for the Noir compiler. What it does is creates an AST representing a circuit, apply equivalence operations on top (which by definition mean the program output does not change) and checks whether it returns something different or if it returns a non-expected error. It was inspired in [circuzz](https://github.com/Rigorous-Software-Engineering/circuzz/tree/main), so kudos to the team :)

For anyone interested in the bugs it has found, you can check [my submissions](https://github.com/noir-lang/noir/issues?q=is%3Aissue%20author%3Anethoxa) to the Noir repo.

## Rakoon

A transaction fuzzer for the Ethereum Protocol. Huge thanks to [Marius van der Wijden](https://github.com/MariusVanDerWijden) for building [tx-fuzz](https://github.com/MariusVanDerWijden/tx-fuzz), which I used as reference in many parts of this project, as well as to the [alloy team](https://github.com/alloy-rs), as I leveraged heavily on them to build this.

### Hall of fame 🏆
- Crash in anvil -> [PR link](https://github.com/foundry-rs/foundry/issues/10444)
- Crash in anvil -> [PR link](https://github.com/foundry-rs/foundry/issues/11435)
- Crash in go-ethereum -> [PR link](https://github.com/ethereum/go-ethereum/pull/32544)
- Crash in reth -> `pending...`

## Ideas

- Maybe, to add more mutations/entropy, we could have like a LLM moddel running and we ask queries on the fly? like, given this DSL, generate a different mutation from the already generated ones
- Fix the `@todo`s around the codebase
- Noiruzz:
    - Add support for unstable features like ownership reference, oracle...
    - Make it possible to increase `max_rewrites_count` without blowing up memory
    - Restore usage of top level functions
