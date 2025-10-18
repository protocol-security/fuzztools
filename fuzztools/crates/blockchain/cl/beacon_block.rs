use super::types::*;
use super::constants::*;
use alloy::primitives::FixedBytes;

pub struct BeaconBlock {
    slot: Slot,
    proposer_index: ValidatorIndex,
    parent_root: Root,
    state_root: Root,
    body: BeaconBlockBody,
}

/// Implements the `BeaconBlockBody` from the specs
pub struct BeaconBlockBody {
    randao_reveal: BlsSignature,
    eth1_data: Eth1Data,
    graffiti: FixedBytes<32>,
    proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS],
    attestations: [Attestation[MAX_VALIDATORS_PER_SLOT, MAX_COMMITTEES_PER_SLOT]; MAX_ATTESTATIONS],
    deposits: [Deposit; MAX_DEPOSITS],
    voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS],
    sync_aggregate: SyncAggregate[SYNC_COMMITTEE_SIZE],
    execution_payload: ExecutionPayload[
        BYTES_PER_LOGS_BLOOM;
        MAX_EXTRA_DATA_BYTES;
        MAX_BYTES_PER_TRANSACTION;
        MAX_TRANSACTIONS_PER_PAYLOAD;
        MAX_WITHDRAWALS_PER_PAYLOAD;
        MAX_DEPOSIT_RECEIPTS_PER_PAYLOAD;
        MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD;
    ],
    bls_to_execution_changes: [SignedBlsToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES],
    blob_kzg_commitments: [KzgCommitment; MAX_BLOB_COMMITMENTS_PER_BLOCK],
    consolidations: [SignedConsolidation; MAX_CONSOLIDATIONS],
}

pub struct BeaconBlockHeader {
    slot: Slot,
    proposer_index: ValidatorIndex,
    parent_root: Root,
    state_root: Root,
    body_root: Root,
}