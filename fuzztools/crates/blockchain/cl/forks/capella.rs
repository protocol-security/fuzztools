use alloy::primitives::{FixedBytes, U256};
use super::phase0::*;
use super::altair::*;
use super::bellatrix::*;

pub(crate) type WithdrawalIndex = u64;

pub(crate) const DOMAIN_BLS_TO_EXECUTION_CHANGE: DomainType = FixedBytes([0x0A, 0x00, 0x00, 0x00]);

pub(crate) const MAX_BLS_TO_EXECUTION_CHANGES: u64 = 16;
pub(crate) const MAX_WITHDRAWALS_PER_PAYLOAD: u64 = 16;
pub(crate) const MAX_VALIDATORS_PER_WITHDRAWALS_SWEEP: u64 = 16_384;

pub(crate) struct Withdrawal {
    index: WithdrawalIndex,
    validator_index: ValidatorIndex,
    address: ExecutionAddress,
    amount: Gwei,
}

pub(crate) struct BLSToExecutionChange {
    validator_index: ValidatorIndex,
    from_bls_pubkey: BLSPubkey,
    to_execution_address: ExecutionAddress,
}

pub(crate) struct SignedBLSToExecutionChange {
    message: BLSToExecutionChange,
    signature: BLSSignature,
}

pub(crate) struct HistoricalSummary {
    block_summary_root: Root,
    state_summary_root: Root,
}

pub(crate) struct ExecutionPayload {
    parent_hash: Hash32,
    fee_recipient: ExecutionAddress,
    state_root: FixedBytes<32>,
    receipts_root: FixedBytes<32>,
    logs_bloom: [u8; BYTES_PER_LOGS_BLOOM],
    prev_randao: FixedBytes<32>,
    block_number: u64,
    gas_limit: u64,
    gas_used: u64,
    timestamp: u64,
    extra_data: [u8; MAX_EXTRA_DATA_BYTES],
    base_fee_per_gas: U256,
    block_hash: Hash32,
    transactions: [Transaction; MAX_TRANSACTIONS_PER_PAYLOAD as usize],
    // [New in Capella]
    withdrawals: [Withdrawal; MAX_WITHDRAWALS_PER_PAYLOAD as usize],
}

pub(crate) struct ExecutionPayloadHeader {
    parent_hash: Hash32,
    fee_recipient: ExecutionAddress,
    state_root: FixedBytes<32>,
    receipts_root: FixedBytes<32>,
    logs_bloom: [u8; BYTES_PER_LOGS_BLOOM],
    prev_randao: FixedBytes<32>,
    block_number: u64,
    gas_limit: u64,
    gas_used: u64,
    timestamp: u64,
    extra_data: [u8; MAX_EXTRA_DATA_BYTES],
    base_fee_per_gas: U256,
    block_hash: Hash32,
    transactions_root: Root,
    // [New in Capella]
    withdrawals_root: Root,
}

pub(crate) struct BeaconBlockBody {
    randao_reveal: BLSSignature,
    eth1_data: Eth1Data,
    graffiti: FixedBytes<32>,
    proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS as usize],
    attestations: [Attestation; MAX_ATTESTATIONS as usize],
    deposits: [Deposit; MAX_DEPOSITS as usize],
    voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize],
    sync_aggregate: SyncAggregate,
    execution_payload: ExecutionPayload,
    // [New in Capella]
    bls_to_execution_changes: [SignedBLSToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES as usize],
}

pub(crate) struct BeaconState {
    genesis_time: u64,
    genesis_validators_root: Root,
    slot: Slot,
    fork: Fork,
    latest_block_header: BeaconBlockHeader,
    block_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize],
    state_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize],
    historical_roots: [Root; HISTORICAL_ROOTS_LIMIT as usize],
    eth1_data: Eth1Data,
    eth1_data_votes: [Eth1Data; (EPOCHS_PER_ETH1_VOTING_PERIOD * SLOTS_PER_EPOCH) as usize],
    eth1_deposit_index: u64,
    validators: [Validator; VALIDATOR_REGISTRY_LIMIT as usize],
    balances: [Gwei; VALIDATOR_REGISTRY_LIMIT as usize],
    randao_mixes: [FixedBytes<32>; EPOCHS_PER_HISTORICAL_VECTOR as usize],
    slashings: [Gwei; EPOCHS_PER_SLASHINGS_VECTOR as usize],
    previous_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    current_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    justification_bits: [bool; JUSTIFICATION_BITS_LENGTH],
    previous_justified_checkpoint: Checkpoint,
    current_justified_checkpoint: Checkpoint,
    finalized_checkpoint: Checkpoint,
    inactivity_scores: [u64; VALIDATOR_REGISTRY_LIMIT as usize],
    current_sync_committee: SyncCommittee,
    next_sync_committee: SyncCommittee,
    // [Modified in Capella]
    latest_execution_payload_header: ExecutionPayloadHeader,
    // [New in Capella]
    next_withdrawal_index: WithdrawalIndex,
    // [New in Capella]
    next_withdrawal_validator_index: ValidatorIndex,
    // [New in Capella]
    historical_summaries: [HistoricalSummary; HISTORICAL_ROOTS_LIMIT as usize],
}

