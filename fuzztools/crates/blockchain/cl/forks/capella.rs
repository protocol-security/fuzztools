use alloy::primitives::{FixedBytes, U256};
use super::phase0::*;
use super::altair::*;
use super::bellatrix::*;

pub type WithdrawalIndex = u64;

pub const DOMAIN_BLS_TO_EXECUTION_CHANGE: DomainType = FixedBytes([0x0A, 0x00, 0x00, 0x00]);

pub const MAX_BLS_TO_EXECUTION_CHANGES: u64 = 16;
pub const MAX_WITHDRAWALS_PER_PAYLOAD: u64 = 16;
pub const MAX_VALIDATORS_PER_WITHDRAWALS_SWEEP: u64 = 16_384;

pub struct Withdrawal {
    pub index: WithdrawalIndex,
    pub validator_index: ValidatorIndex,
    pub address: ExecutionAddress,
    pub amount: Gwei,
}

pub struct BLSToExecutionChange {
    pub validator_index: ValidatorIndex,
    pub from_bls_pubkey: BLSPubkey,
    pub to_execution_address: ExecutionAddress,
}

pub struct SignedBLSToExecutionChange {
    pub message: BLSToExecutionChange,
    pub signature: BLSSignature,
}

pub struct HistoricalSummary {
    pub block_summary_root: Root,
    pub state_summary_root: Root,
}

pub struct ExecutionPayload {
    pub parent_hash: Hash32,
    pub fee_recipient: ExecutionAddress,
    pub state_root: FixedBytes<32>,
    pub receipts_root: FixedBytes<32>,
    pub logs_bloom: [u8; BYTES_PER_LOGS_BLOOM],
    pub prev_randao: FixedBytes<32>,
    pub block_number: u64,
    pub gas_limit: u64,
    pub gas_used: u64,
    pub timestamp: u64,
    pub extra_data: [u8; MAX_EXTRA_DATA_BYTES],
    pub base_fee_per_gas: U256,
    pub block_hash: Hash32,
    pub transactions: [Transaction; MAX_TRANSACTIONS_PER_PAYLOAD as usize],
    // [New in Capella]
    pub withdrawals: [Withdrawal; MAX_WITHDRAWALS_PER_PAYLOAD as usize],
}

pub struct ExecutionPayloadHeader {
    pub parent_hash: Hash32,
    pub fee_recipient: ExecutionAddress,
    pub state_root: FixedBytes<32>,
    pub receipts_root: FixedBytes<32>,
    pub logs_bloom: [u8; BYTES_PER_LOGS_BLOOM],
    pub prev_randao: FixedBytes<32>,
    pub block_number: u64,
    pub gas_limit: u64,
    pub gas_used: u64,
    pub timestamp: u64,
    pub extra_data: [u8; MAX_EXTRA_DATA_BYTES],
    pub base_fee_per_gas: U256,
    pub block_hash: Hash32,
    pub transactions_root: Root,
    // [New in Capella]
    pub withdrawals_root: Root,
}

pub struct BeaconBlockBody {
    pub randao_reveal: BLSSignature,
    pub eth1_data: Eth1Data,
    pub graffiti: FixedBytes<32>,
    pub proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    pub attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS as usize],
    pub attestations: [Attestation; MAX_ATTESTATIONS as usize],
    pub deposits: [Deposit; MAX_DEPOSITS as usize],
    pub voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize],
    pub sync_aggregate: SyncAggregate,
    pub execution_payload: ExecutionPayload,
    // [New in Capella]
    pub bls_to_execution_changes: [SignedBLSToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES as usize],
}

pub struct BeaconState {
    pub genesis_time: u64,
    pub genesis_validators_root: Root,
    pub slot: Slot,
    pub fork: Fork,
    pub latest_block_header: BeaconBlockHeader,
    pub block_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize],
    pub state_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize],
    pub historical_roots: [Root; HISTORICAL_ROOTS_LIMIT as usize],
    pub eth1_data: Eth1Data,
    pub eth1_data_votes: [Eth1Data; (EPOCHS_PER_ETH1_VOTING_PERIOD * SLOTS_PER_EPOCH) as usize],
    pub eth1_deposit_index: u64,
    pub validators: [Validator; VALIDATOR_REGISTRY_LIMIT as usize],
    pub balances: [Gwei; VALIDATOR_REGISTRY_LIMIT as usize],
    pub randao_mixes: [FixedBytes<32>; EPOCHS_PER_HISTORICAL_VECTOR as usize],
    pub slashings: [Gwei; EPOCHS_PER_SLASHINGS_VECTOR as usize],
    pub previous_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    pub current_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    pub justification_bits: [u8;JUSTIFICATION_BITS_LENGTH],
    pub previous_justified_checkpoint: Checkpoint,
    pub current_justified_checkpoint: Checkpoint,
    pub finalized_checkpoint: Checkpoint,
    pub inactivity_scores: [u64; VALIDATOR_REGISTRY_LIMIT as usize],
    pub current_sync_committee: SyncCommittee,
    pub next_sync_committee: SyncCommittee,
    // [Modified in Capella]
    pub latest_execution_payload_header: ExecutionPayloadHeader,
    // [New in Capella]
    pub next_withdrawal_index: WithdrawalIndex,
    // [New in Capella]
    pub next_withdrawal_validator_index: ValidatorIndex,
    // [New in Capella]
    pub historical_summaries: [HistoricalSummary; HISTORICAL_ROOTS_LIMIT as usize],
}
