# Rakoon

![Rakoon](../imgs/rakoon.png)

This folder holds `rakoon`, a blazingly-fast transaction fuzzer for the Ethereum Protocol. Huge thanks to [Marius van der Wijden](https://github.com/MariusVanDerWijden) for building [tx-fuzz](https://github.com/MariusVanDerWijden/tx-fuzz), which I used as reference in many parts of this project, as well as to the [alloy team](https://github.com/alloy-rs), as I leveraged heavily on them to build this.

**NOTE**: blob transactions are not supported as those will go in another, more dedicated fuzzer.

## Hall of fame 🏆
- Crash in anvil -> [PR link](https://github.com/foundry-rs/foundry/issues/10444)
- Crash in anvil -> [PR link](https://github.com/foundry-rs/foundry/issues/11435)
- Crash in go-ethereum -> [PR link](https://github.com/ethereum/go-ethereum/pull/32544)
- Crash in reth -> `pending...`

## Benchmark

Against a local geth node via IPC:

```bash
geth --dev --dev.period 12 --ipcpath /tmp/geth.ipc
```

- Fuzzing enabled:
    - Legacy: `28000`
    - EIP-2930: `28000`
    - EIP-1559: `30800`
    - EIP-7702: `11200`
- Fuzzing disabled:
    - Legacy: `25200`
    - EIP-2930: `23800`
    - EIP-1559: `22400`
    - EIP-7702: `8400`

quantities in transactions per second running on a MacBook Pro 2023, M3 Max and 36 GB of RAM. About increasing this numbers, the bottleneck is RPC requests, so there is not much one can do other than increase the threading of the system to scale this shit up.