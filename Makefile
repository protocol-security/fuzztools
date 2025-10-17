init:
	git submodule update --init --recursive

# These are commands for rakoon, used for testing/running
run-testnet:
	kurtosis run --enclave testnet github.com/ethpandaops/ethereum-package --args-file ./network_params.yaml

stop-testnet:
	kurtosis enclave stop testnet

rm-testnet:
	kurtosis enclave rm testnet --force

geth-testnet:
	geth --dev --dev.period 12 --ipcpath /tmp/geth.ipc --datadir ./tmp/geth-db
