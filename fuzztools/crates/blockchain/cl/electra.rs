//! Electra consensus layer types and constants.

use super::{
    altair::*,
    bellatrix::ExecutionAddress,
    capella::*,
    deneb::{
        ExecutionPayload, ExecutionPayloadHeader, KZGCommitment, MAX_BLOB_COMMITMENTS_PER_BLOCK,
    },
    phase0::*,
};
use crate::mutations::Mutable;
use alloy::primitives::FixedBytes;
use mutable::Mutable;
use rand::Rng;

pub const UNSET_DEPOSIT_REQUESTS_START_INDEX: u64 = u64::MAX;
pub const FULL_EXIT_REQUEST_AMOUNT: u64 = 0;

pub const COMPOUNDING_WITHDRAWAL_PREFIX: FixedBytes<1> = FixedBytes([0x02]);

pub const DEPOSIT_REQUEST_TYPE: FixedBytes<1> = FixedBytes([0x00]);
pub const WITHDRAWAL_REQUEST_TYPE: FixedBytes<1> = FixedBytes([0x01]);
pub const CONSOLIDATION_REQUEST_TYPE: FixedBytes<1> = FixedBytes([0x02]);

pub const MIN_ACTIVATION_BALANCE: Gwei = 32_000_000_000;
pub const MAX_EFFECTIVE_BALANCE_ELECTRA: Gwei = 2_048_000_000_000;

pub const MIN_SLASHING_PENALTY_QUOTIENT_ELECTRA: u64 = 4_096;
pub const WHISTLEBLOWER_REWARD_QUOTIENT_ELECTRA: u64 = 4_096;

pub const PENDING_DEPOSITS_LIMIT: u64 = 134_217_728;
pub const PENDING_PARTIAL_WITHDRAWALS_LIMIT: u64 = 134_217_728;
pub const PENDING_CONSOLIDATIONS_LIMIT: u64 = 262_144;

pub const MAX_ATTESTER_SLASHINGS_ELECTRA: u64 = 1;
pub const MAX_ATTESTATIONS_ELECTRA: u64 = 8;

pub const MAX_DEPOSIT_REQUESTS_PER_PAYLOAD: u64 = 8_192;
pub const MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD: u64 = 16;
pub const MAX_CONSOLIDATION_REQUESTS_PER_PAYLOAD: u64 = 2;

pub const MAX_PENDING_PARTIALS_PER_WITHDRAWALS_SWEEP: u64 = 8;

pub const MAX_PENDING_DEPOSITS_PER_EPOCH: u64 = 16;

pub const MAX_BLOBS_PER_BLOCK_ELECTRA: u64 = 9;

pub const MIN_PER_EPOCH_CHURN_LIMIT_ELECTRA: Gwei = 128_000_000_000;
pub const MAX_PER_EPOCH_ACTIVATION_EXIT_CHURN_LIMIT: Gwei = 256_000_000_000;

#[derive(Mutable)]
pub struct PendingDeposit {
    pub pubkey: BLSPubkey,
    pub withdrawal_credentials: FixedBytes<32>,
    pub amount: Gwei,
    pub signature: BLSSignature,
    pub slot: Slot,
}

#[derive(Mutable)]
pub struct PendingPartialWithdrawal {
    pub validator_index: ValidatorIndex,
    pub amount: Gwei,
    pub withdrawable_epoch: Epoch,
}

#[derive(Mutable)]
pub struct PendingConsolidation {
    pub source_index: ValidatorIndex,
    pub target_index: ValidatorIndex,
}

#[derive(Mutable)]
pub struct DepositRequest {
    pub pubkey: BLSPubkey,
    pub withdrawal_credentials: FixedBytes<32>,
    pub amount: Gwei,
    pub signature: BLSSignature,
    pub index: u64,
}

#[derive(Mutable)]
pub struct WithdrawalRequest {
    pub source_address: ExecutionAddress,
    pub validator_pubkey: BLSPubkey,
    pub amount: Gwei,
}

#[derive(Mutable)]
pub struct ConsolidationRequest {
    pub source_address: ExecutionAddress,
    pub source_pubkey: BLSPubkey,
    pub target_pubkey: BLSPubkey,
}

#[derive(Mutable)]
pub struct ExecutionRequests {
    // [New in Electra:EIP6110]
    pub deposits: [DepositRequest; MAX_DEPOSIT_REQUESTS_PER_PAYLOAD as usize],
    // [New in Electra:EIP7002:EIP7251]
    pub withdrawals: [WithdrawalRequest; MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD as usize],
    // [New in Electra:EIP7251]
    pub consolidations: [ConsolidationRequest; MAX_CONSOLIDATION_REQUESTS_PER_PAYLOAD as usize],
}

#[derive(Mutable)]
pub struct SingleAttestation {
    pub committee_index: CommitteeIndex,
    pub attester_index: ValidatorIndex,
    pub data: AttestationData,
    pub signature: BLSSignature,
}

#[derive(Mutable)]
pub struct AttesterSlashing {
    // [Modified in Electra:EIP7549]
    pub attestation_1: IndexedAttestation,
    // [Modified in Electra:EIP7549]
    pub attestation_2: IndexedAttestation,
}

#[derive(Mutable)]
pub struct BeaconBlockBody {
    pub randao_reveal: BLSSignature,
    pub eth1_data: Eth1Data,
    pub graffiti: FixedBytes<32>,
    pub proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    // [Modified in Electra:EIP7549]
    pub attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS_ELECTRA as usize],
    // [Modified in Electra:EIP7549]
    pub attestations: [Attestation; MAX_ATTESTATIONS_ELECTRA as usize],
    pub deposits: [Deposit; MAX_DEPOSITS as usize],
    pub voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize],
    pub sync_aggregate: SyncAggregate,
    pub execution_payload: ExecutionPayload,
    pub bls_to_execution_changes:
        [SignedBLSToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES as usize],
    pub blob_kzg_commitments: [KZGCommitment; MAX_BLOB_COMMITMENTS_PER_BLOCK as usize],
    // [New in Electra]
    pub execution_requests: ExecutionRequests,
}

#[derive(Mutable)]
pub struct Attestation {
    // [Modified in Electra:EIP7549]
    pub aggregation_bits: [u8; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize],
    pub data: AttestationData,
    pub signature: BLSSignature,
    // [New in Electra:EIP7549]
    pub committee_bits: [u8; MAX_COMMITTEES_PER_SLOT as usize],
}

#[derive(Mutable)]
pub struct IndexedAttestation {
    // [Modified in Electra:EIP7549]
    pub attesting_indices:
        [ValidatorIndex; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize],
    pub data: AttestationData,
    pub signature: BLSSignature,
}

#[derive(Mutable)]
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
    pub latest_execution_payload_header: ExecutionPayloadHeader,
    pub next_withdrawal_index: WithdrawalIndex,
    pub next_withdrawal_validator_index: ValidatorIndex,
    pub historical_summaries: [HistoricalSummary; HISTORICAL_ROOTS_LIMIT as usize],
    // [New in Electra:EIP6110]
    pub deposit_requests_start_index: u64,
    // [New in Electra:EIP7251]
    pub deposit_balance_to_consume: Gwei,
    // [New in Electra:EIP7251]
    pub exit_balance_to_consume: Gwei,
    // [New in Electra:EIP7251]
    pub earliest_exit_epoch: Epoch,
    // [New in Electra:EIP7251]
    pub consolidation_balance_to_consume: Gwei,
    // [New in Electra:EIP7251]
    pub earliest_consolidation_epoch: Epoch,
    // [New in Electra:EIP7251]
    pub pending_deposits: [PendingDeposit; PENDING_DEPOSITS_LIMIT as usize],
    // [New in Electra:EIP7251]
    pub pending_partial_withdrawals:
        [PendingPartialWithdrawal; PENDING_PARTIAL_WITHDRAWALS_LIMIT as usize],
    // [New in Electra:EIP7251]
    pub pending_consolidations: [PendingConsolidation; PENDING_CONSOLIDATIONS_LIMIT as usize],
}
