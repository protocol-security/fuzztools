rm-kurtosis:
	kurtosis enclave rm testnet --force

run-kurtosis:
	kurtosis run --enclave testnet github.com/ethpandaops/ethereum-package --args-file ./network_params.yaml

geth-dev:
	geth --dev --dev.period 12 --ipcpath /tmp/geth.ipc --datadir ./geth-db