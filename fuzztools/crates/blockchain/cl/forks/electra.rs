use alloy::primitives::FixedBytes;
use super::phase0::*;
use super::altair::*;
use super::bellatrix::ExecutionAddress;
use super::capella::*;
use super::deneb::{ExecutionPayload, ExecutionPayloadHeader, KZGCommitment, MAX_BLOB_COMMITMENTS_PER_BLOCK};

pub(crate) const UNSET_DEPOSIT_REQUESTS_START_INDEX: u64 = u64::MAX;
pub(crate) const FULL_EXIT_REQUEST_AMOUNT: u64 = 0;

pub(crate) const COMPOUNDING_WITHDRAWAL_PREFIX: FixedBytes<1> = FixedBytes([0x02]);

pub(crate) const DEPOSIT_REQUEST_TYPE: FixedBytes<1> = FixedBytes([0x00]);
pub(crate) const WITHDRAWAL_REQUEST_TYPE: FixedBytes<1> = FixedBytes([0x01]);
pub(crate) const CONSOLIDATION_REQUEST_TYPE: FixedBytes<1> = FixedBytes([0x02]);

pub(crate) const MIN_ACTIVATION_BALANCE: Gwei = 32_000_000_000;
pub(crate) const MAX_EFFECTIVE_BALANCE_ELECTRA: Gwei = 2_048_000_000_000;

pub(crate) const MIN_SLASHING_PENALTY_QUOTIENT_ELECTRA: u64 = 4_096;
pub(crate) const WHISTLEBLOWER_REWARD_QUOTIENT_ELECTRA: u64 = 4_096;

pub(crate) const PENDING_DEPOSITS_LIMIT: u64 = 134_217_728;
pub(crate) const PENDING_PARTIAL_WITHDRAWALS_LIMIT: u64 = 134_217_728;
pub(crate) const PENDING_CONSOLIDATIONS_LIMIT: u64 = 262_144;

pub(crate) const MAX_ATTESTER_SLASHINGS_ELECTRA: u64 = 1;
pub(crate) const MAX_ATTESTATIONS_ELECTRA: u64 = 8;

pub(crate) const MAX_DEPOSIT_REQUESTS_PER_PAYLOAD: u64 = 8_192;
pub(crate) const MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD: u64 = 16;
pub(crate) const MAX_CONSOLIDATION_REQUESTS_PER_PAYLOAD: u64 = 2;

pub(crate) const MAX_PENDING_PARTIALS_PER_WITHDRAWALS_SWEEP: u64 = 8;

pub(crate) const MAX_PENDING_DEPOSITS_PER_EPOCH: u64 = 16;

pub(crate) const MAX_BLOBS_PER_BLOCK_ELECTRA: u64 = 9;

pub(crate) const MIN_PER_EPOCH_CHURN_LIMIT_ELECTRA: Gwei = 128_000_000_000;
pub(crate) const MAX_PER_EPOCH_ACTIVATION_EXIT_CHURN_LIMIT: Gwei = 256_000_000_000;

pub(crate) struct PendingDeposit {
    pubkey: BLSPubkey,
    withdrawal_credentials: FixedBytes<32>,
    amount: Gwei,
    signature: BLSSignature,
    slot: Slot,
}

pub(crate) struct PendingPartialWithdrawal {
    validator_index: ValidatorIndex,
    amount: Gwei,
    withdrawable_epoch: Epoch,
}

pub(crate) struct PendingConsolidation {
    source_index: ValidatorIndex,
    target_index: ValidatorIndex,
}

pub(crate) struct DepositRequest {
    pubkey: BLSPubkey,
    withdrawal_credentials: FixedBytes<32>,
    amount: Gwei,
    signature: BLSSignature,
    index: u64,
}

pub(crate) struct WithdrawalRequest {
    source_address: ExecutionAddress,
    validator_pubkey: BLSPubkey,
    amount: Gwei,
}

pub(crate) struct ConsolidationRequest {
    source_address: ExecutionAddress,
    source_pubkey: BLSPubkey,
    target_pubkey: BLSPubkey,
}

pub(crate) struct ExecutionRequests {
    // [New in Electra:EIP6110]
    deposits: [DepositRequest; MAX_DEPOSIT_REQUESTS_PER_PAYLOAD as usize],
    // [New in Electra:EIP7002:EIP7251]
    withdrawals: [WithdrawalRequest; MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD as usize],
    // [New in Electra:EIP7251]
    consolidations: [ConsolidationRequest; MAX_CONSOLIDATION_REQUESTS_PER_PAYLOAD as usize],
}

pub(crate) struct SingleAttestation {
    committee_index: CommitteeIndex,
    attester_index: ValidatorIndex,
    data: AttestationData,
    signature: BLSSignature,
}

pub(crate) struct AttesterSlashing {
    // [Modified in Electra:EIP7549]
    attestation_1: IndexedAttestation,
    // [Modified in Electra:EIP7549]
    attestation_2: IndexedAttestation,
}

pub(crate) struct BeaconBlockBody {
    randao_reveal: BLSSignature,
    eth1_data: Eth1Data,
    graffiti: FixedBytes<32>,
    proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    // [Modified in Electra:EIP7549]
    attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS_ELECTRA as usize],
    // [Modified in Electra:EIP7549]
    attestations: [Attestation; MAX_ATTESTATIONS_ELECTRA as usize],
    deposits: [Deposit; MAX_DEPOSITS as usize],
    voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize],
    sync_aggregate: SyncAggregate,
    execution_payload: ExecutionPayload,
    bls_to_execution_changes: [SignedBLSToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES as usize],
    blob_kzg_commitments: [KZGCommitment; MAX_BLOB_COMMITMENTS_PER_BLOCK as usize],
    // [New in Electra]
    execution_requests: ExecutionRequests,
}

pub(crate) struct Attestation {
    // [Modified in Electra:EIP7549]
    aggregation_bits: [bool; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize],
    data: AttestationData,
    signature: BLSSignature,
    // [New in Electra:EIP7549]
    committee_bits: [bool; MAX_COMMITTEES_PER_SLOT as usize],
}

pub(crate) struct IndexedAttestation {
    // [Modified in Electra:EIP7549]
    attesting_indices: [ValidatorIndex; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize],
    data: AttestationData,
    signature: BLSSignature,
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
    latest_execution_payload_header: ExecutionPayloadHeader,
    next_withdrawal_index: WithdrawalIndex,
    next_withdrawal_validator_index: ValidatorIndex,
    historical_summaries: [HistoricalSummary; HISTORICAL_ROOTS_LIMIT as usize],
    // [New in Electra:EIP6110]
    deposit_requests_start_index: u64,
    // [New in Electra:EIP7251]
    deposit_balance_to_consume: Gwei,
    // [New in Electra:EIP7251]
    exit_balance_to_consume: Gwei,
    // [New in Electra:EIP7251]
    earliest_exit_epoch: Epoch,
    // [New in Electra:EIP7251]
    consolidation_balance_to_consume: Gwei,
    // [New in Electra:EIP7251]
    earliest_consolidation_epoch: Epoch,
    // [New in Electra:EIP7251]
    pending_deposits: [PendingDeposit; PENDING_DEPOSITS_LIMIT as usize],
    // [New in Electra:EIP7251]
    pending_partial_withdrawals: [PendingPartialWithdrawal; PENDING_PARTIAL_WITHDRAWALS_LIMIT as usize],
    // [New in Electra:EIP7251]
    pending_consolidations: [PendingConsolidation; PENDING_CONSOLIDATIONS_LIMIT as usize],
}
