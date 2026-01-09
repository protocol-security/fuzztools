init:
	git submodule update --init --recursive

make pull:
	git stash
	git pull
	git stash pop

# These are commands for rakoon, used for testing/running
run-testnet:
	kurtosis run --enclave testnet github.com/ethpandaops/ethereum-package --args-file ./network_params.yaml

stop-testnet:
	kurtosis enclave stop testnet

rm-testnet:
	kurtosis enclave rm testnet --force

geth-testnet:
	geth --dev \
		--dev.period 12 \
		--datadir ./tmp/geth-db \
		--http \
		--http.addr "127.0.0.1" \
		--http.port 8545 \
		--http.api "eth,net,web3,debug"

reth-testnet:
	reth node \
		--dev \
		--dev.block-time 12s \
		--datadir ./tmp/reth-db \

# These are commands for building
build:
	MALLOC_CONF="thp:always,metadata_thp:always" cargo build --release

optimize:
	cargo pgo build
	cargo pgo optimize