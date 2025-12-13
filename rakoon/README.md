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

Against a local geth node:

```bash
geth --dev \
  --dev.period 12 \
  --datadir ./tmp/geth-db \
  --http \
  --http.addr "127.0.0.1" \
  --http.port 8545 \
  --http.api "eth,net,web3,debug"
```

| Transaction Type | Fuzzing Enabled |    Total TXs Sent   | Time Spent  |  Average TPS  |
|------------------|-----------------|---------------------|-------------|---------------|
| Legacy           | Yes             | 4,530,824           | 30s         | 151,027       |
| EIP-2930         | Yes             | 4,659,408           | 30s         | 155,313       |
| EIP-1559         | Yes             | 5,111,835           | 30s         | 170,394       |
| EIP-7702         | Yes             | 4,043,620           | 30s         | 143,787       |
|                  |                 |                     |             |               |
| Legacy           | No              | 4,746,413           | 30s         | 158,213       |
| EIP-2930         | No              | 5,129,023           | 30s         | 170,967       |
| EIP-1559         | No              | 4,633,435           | 30s         | 154,447       |
| EIP-7702         | No              | 4,505,711           | 30s         | 150,190       |

running on a MacBook Pro 2023, M3 Max and 36 GB of RAM.