//! Bellatrix consensus layer types and constants.

use super::{altair::*, phase0::*};
use crate::mutations::Mutable;
use alloy::primitives::{FixedBytes, U256};
use mutable::Mutable;
use rand::Rng;

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

#[derive(Copy, Clone, Mutable)]
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
    // [New in Bellatrix]
    pub execution_payload: ExecutionPayload,
}

#[derive(Copy, Clone, Mutable)]
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
    pub justification_bits: [u8; JUSTIFICATION_BITS_LENGTH],
    pub previous_justified_checkpoint: Checkpoint,
    pub current_justified_checkpoint: Checkpoint,
    pub finalized_checkpoint: Checkpoint,
    pub inactivity_scores: [u64; VALIDATOR_REGISTRY_LIMIT as usize],
    pub current_sync_committee: SyncCommittee,
    pub next_sync_committee: SyncCommittee,
    // [New in Bellatrix]
    pub latest_execution_payload_header: ExecutionPayloadHeader,
}

#[derive(Copy, Clone, Mutable)]
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
}

#[derive(Copy, Clone, Mutable)]
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
}
