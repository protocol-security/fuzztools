//! Fulu consensus layer types and constants.

use super::{altair::*, capella::*, electra::*, phase0::*};
use crate::mutations::Mutable;
use alloy::primitives::FixedBytes;
use mutable::Mutable;
use rand::Rng;

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
    pub deposit_requests_start_index: u64,
    pub deposit_balance_to_consume: Gwei,
    pub exit_balance_to_consume: Gwei,
    pub earliest_exit_epoch: Epoch,
    pub consolidation_balance_to_consume: Gwei,
    pub earliest_consolidation_epoch: Epoch,
    pub pending_deposits: [PendingDeposit; PENDING_DEPOSITS_LIMIT as usize],
    pub pending_partial_withdrawals:
        [PendingPartialWithdrawal; PENDING_PARTIAL_WITHDRAWALS_LIMIT as usize],
    pub pending_consolidations: [PendingConsolidation; PENDING_CONSOLIDATIONS_LIMIT as usize],
    // [New in Fulu:EIP7917]
    pub proposer_lookahead: [ValidatorIndex; ((MIN_SEED_LOOKAHEAD + 1) * SLOTS_PER_EPOCH) as usize],
}
