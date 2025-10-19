use alloy::primitives::{FixedBytes, U256};
use super::phase0::*;
use super::altair::*;

pub type Transaction = [u8; MAX_BYTES_PER_TRANSACTION as usize];
pub type ExecutionAddress = FixedBytes<20>;

pub const INACTIVITY_PENALTY_QUOTIENT_BELLATRIX: u64 = 16_777_216;
pub const MIN_SLASHING_PENALTY_QUOTIENT_BELLATRIX: u64 = 32;
pub const PROPORTIONAL_SLASHING_MULTIPLIER_BELLATRIX: u64 = 3;

pub const MAX_BYTES_PER_TRANSACTION: u64 = 1_073_741_824;
pub const MAX_TRANSACTIONS_PER_PAYLOAD: u64 = 1_048_576;
pub const BYTES_PER_LOGS_BLOOM: usize = 256;
pub const MAX_EXTRA_DATA_BYTES: usize = 32;

pub const TERMINAL_TOTAL_DIFFICULTY: u128 = 58_750_000_000_000_000_000_000;
pub const TERMINAL_BLOCK_HASH: Hash32 = FixedBytes([0; 32]);
pub const TERMINAL_BLOCK_HASH_ACTIVATION_EPOCH: Epoch = FAR_FUTURE_EPOCH;

pub struct BeaconBlockBody {
    randao_reveal: BLSSignature,
    eth1_data: Eth1Data,
    graffiti: FixedBytes<32>,
    proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS as usize],
    attestations: [Attestation; MAX_ATTESTATIONS as usize],
    deposits: [Deposit; MAX_DEPOSITS as usize],
    voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize],
    sync_aggregate: SyncAggregate,
    // [New in Bellatrix]
    execution_payload: ExecutionPayload,
}

pub struct BeaconState {
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
    // [New in Bellatrix]
    latest_execution_payload_header: ExecutionPayloadHeader,
}

pub struct ExecutionPayload {
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
}

pub struct ExecutionPayloadHeader {
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
}

