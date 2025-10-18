use alloy::primitives::FixedBytes;
use crate::blockchain::cl::forks::capella::{HistoricalSummary, WithdrawalIndex};
use crate::blockchain::cl::forks::deneb::ExecutionPayloadHeader;
use super::phase0::*;
use super::altair::*;
use super::electra::*;

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
    latest_execution_payload_header: ExecutionPayloadHeader,
    next_withdrawal_index: WithdrawalIndex,
    next_withdrawal_validator_index: ValidatorIndex,
    historical_summaries: [HistoricalSummary; HISTORICAL_ROOTS_LIMIT as usize],
    deposit_requests_start_index: u64,
    deposit_balance_to_consume: Gwei,
    exit_balance_to_consume: Gwei,
    earliest_exit_epoch: Epoch,
    consolidation_balance_to_consume: Gwei,
    earliest_consolidation_epoch: Epoch,
    pending_deposits: [PendingDeposit; PENDING_DEPOSITS_LIMIT as usize],
    pending_partial_withdrawals: [PendingPartialWithdrawal; PENDING_PARTIAL_WITHDRAWALS_LIMIT as usize],
    pending_consolidations: [PendingConsolidation; PENDING_CONSOLIDATIONS_LIMIT as usize],
    // [New in Fulu:EIP7917]
    proposer_lookahead: [ValidatorIndex; ((MIN_SEED_LOOKAHEAD + 1) * SLOTS_PER_EPOCH) as usize],
}
