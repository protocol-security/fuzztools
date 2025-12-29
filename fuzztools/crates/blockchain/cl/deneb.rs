//! Deneb consensus layer types and constants.

use super::{altair::*, bellatrix::*, capella::*, phase0::*};
use crate::mutations::Mutable;
use alloy::primitives::{FixedBytes, U256};
use mutable::Mutable;
use rand::Rng;
pub type VersionedHash = FixedBytes<32>;
pub type BlobIndex = u64;
pub type KZGCommitment = FixedBytes<48>;

pub const VERSIONED_HASH_VERSION_KZG: FixedBytes<1> = FixedBytes([0x01]);

pub const MAX_BLOB_COMMITMENTS_PER_BLOCK: u64 = 4096;
pub const MAX_BLOBS_PER_BLOCK: u64 = 6;
pub const MAX_PER_EPOCH_ACTIVATION_CHURN_LIMIT: u64 = 8;

#[derive(Clone, Copy, Mutable)]
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
    // [Modified in Deneb:EIP4844]
    pub execution_payload: ExecutionPayload,
    pub bls_to_execution_changes:
        [SignedBLSToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES as usize],
    // [New in Deneb:EIP4844]
    pub blob_kzg_commitments: [KZGCommitment; MAX_BLOB_COMMITMENTS_PER_BLOCK as usize],
}

#[derive(Clone, Copy, Mutable)]
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
    pub withdrawals: [Withdrawal; MAX_WITHDRAWALS_PER_PAYLOAD as usize],
    // [New in Deneb:EIP4844]
    pub blob_gas_used: u64,
    // [New in Deneb:EIP4844]
    pub excess_blob_gas: u64,
}

#[derive(Clone, Copy, Mutable)]
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
    pub withdrawals_root: Root,
    // [New in Deneb:EIP4844]
    pub blob_gas_used: u64,
    // [New in Deneb:EIP4844]
    pub excess_blob_gas: u64,
}

#[derive(Clone, Copy, Mutable)]
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
    // [Modified in Deneb:EIP4844]
    pub latest_execution_payload_header: ExecutionPayloadHeader,
    pub next_withdrawal_index: WithdrawalIndex,
    pub next_withdrawal_validator_index: ValidatorIndex,
    pub historical_summaries: [HistoricalSummary; HISTORICAL_ROOTS_LIMIT as usize],
}
