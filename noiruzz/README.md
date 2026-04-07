# Noiruzz

![noiruzz](https://github.com/user-attachments/assets/31235d95-9855-402a-8679-f96b800f1de6)

Metamorphic fuzzer for the Noir compiler. What it does is creates an AST representing a circuit, apply equivalence operations on top (which by definition mean the program output does not change) and checks whether it returns something different or if it returns a non-expected error.

## Hall of fame 🏆

- Error nodes triggering a compiler bug -> [PR link](https://github.com/noir-lang/noir/issues/10824)
- Crash -> [PR link](https://github.com/noir-lang/noir/issues/11326)
- Crash -> [PR link](https://github.com/noir-lang/noir/issues/11250)
- OOB XOR output at compile time -> [PR link](https://github.com/noir-lang/noir/issues/11249)
- Nested lambdas can't be accessed -> [PR link](https://github.com/noir-lang/noir/issues/11325)

For more low/info bugs, check my [submissions](https://github.com/noir-lang/noir/issues?q=is%3Aissue%20author%3Anethoxa).

## Benchmarks

On an AMD Ryzen 9950x with 64GB RAM:

```sh
cargo run --release -p noiruzz -- --workers 12
```

and using the default [config](../configs/noiruzz.json), has a throughput of `~80` circuits per second through the whole lifecycle, that is, generation, rewrite, compilation, execution, proving and verification (with an average CPU usage of `53%`).

**NOTE**: On very fast machines, power scheduler does not correctly compute usage, so it is better to pass `--always-run-later-stages`.
