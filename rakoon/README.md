# Rakoon

![Rakoon](../imgs/rakoon.png)

This folder holds `rakoon`, a transaction fuzzer for the Ethereum Protocol. Huge thanks to [Marius van der Wijden](https://github.com/MariusVanDerWijden) for building [tx-fuzz](https://github.com/MariusVanDerWijden/tx-fuzz), which I used as reference in many parts of this project, as well as to the [alloy team](https://github.com/alloy-rs), as I leveraged heavily on them to build this.

**NOTE**: blob transactions are not supported as those will go in another, more dedicated fuzzer.

## Hall of fame 🏆
- Crash in anvil -> [PR link](https://github.com/foundry-rs/foundry/issues/10444)
- Crash in anvil -> [PR link](https://github.com/foundry-rs/foundry/issues/11435)
- Crash in go-ethereum -> [PR link](https://github.com/ethereum/go-ethereum/pull/32544)
- Crash in reth -> `pending...`

## Fast commands

If you wanna create a kurtosis testnet, modify [network_params.yaml](../network_params.yaml) and run:

```sh
kurtosis run --enclave testnet github.com/ethpandaops/ethereum-package --args-file ./network_params.yaml
```

To stop it and remove its files:

```sh
kurtosis enclave stop testnet && kurtosis enclave rm testnet --force
```

You will need to choose one of the default keys shown in the output and put it under the `--key` flag, as well as you must choose the `rpc` endpoint of the client you wanna fuzz.

For a local geth testnet with the `--dev` flag:

```sh
geth --dev \
		--dev.period 12 \
		--datadir ./tmp/geth-db \
		--http \
		--http.addr "127.0.0.1" \
		--http.port 8545 \
		--http.api "eth"
```

use the next command to run `rakoon`:

```sh
cargo run --release --package rakoon -- --key 0xb71c71a67e1177ad4e901695e1b4b9ee17ae16c6668d313eac2f96dbcda3f291
```

and for a local reth instance with the `--dev` flag:

```sh
	reth node \
		--dev \
		--dev.block-time 12s \
		--datadir ./tmp/reth-db
    --rpc.max-connections max
```

use the next command to run `rakoon`:

```sh
cargo run --release --package rakoon -- --key 0xdbda1821b80551c9d65939329250298aa3472ba22feea921c0cf5d620ea67b97
```

To see more flags, run:

```sh
cargo run --release --package rakoon -- --help
```

with output:

```sh
Transaction Fuzzer for the Ethereum Protocol

Usage: rakoon [OPTIONS] --key <KEY>

Options:
      --tx-type <TX_TYPE>              Transaction type to fuzz (legacy, eip2930, eip1559, eip7702) [default: eip7702]
      --key <KEY>                      Private key for signing transactions
      --seed <SEED>                    Seed for the random generator [default: 0]
      --url <URL>                      URL to send transactions to [default: http://127.0.0.1:8545]
      --fuzzing                        Wether to mutate txs or not before sending them
      --poll-interval <POLL_INTERVAL>  Poll interval to query gas prices (in seconds) [default: 2]
      --workers <WORKERS>              Number of concurrent RPC batches [default: 50]
      --batch-size <BATCH_SIZE>        Number of txs per batch [default: 100]
  -h, --help                           Print help
```
