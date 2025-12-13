//! Altair consensus layer types and constants.

use super::phase0::*;
use crate::mutations::Mutable;
use alloy::primitives::FixedBytes;
use mutable::Mutable;
use rand::Rng;

pub type ParticipationFlags = u8;

pub const TIMELY_SOURCE_FLAG_INDEX: u64 = 0;
pub const TIMELY_TARGET_FLAG_INDEX: u64 = 1;
pub const TIMELY_HEAD_FLAG_INDEX: u64 = 2;

pub const TIMELY_SOURCE_WEIGHT: u64 = 14;
pub const TIMELY_TARGET_WEIGHT: u64 = 26;
pub const TIMELY_HEAD_WEIGHT: u64 = 14;
pub const SYNC_REWARD_WEIGHT: u64 = 2;
pub const PROPOSER_WEIGHT: u64 = 8;
pub const WEIGHT_DENOMINATOR: u64 = 64;

pub const DOMAIN_SYNC_COMMITTEE: DomainType = FixedBytes([0x07, 0x00, 0x00, 0x00]);
pub const DOMAIN_SYNC_COMMITTEE_SELECTION_PROOF: DomainType = FixedBytes([0x08, 0x00, 0x00, 0x00]);
pub const DOMAIN_CONTRIBUTION_AND_PROOF: DomainType = FixedBytes([0x09, 0x00, 0x00, 0x00]);

pub const PARTICIPATION_FLAG_WEIGHTS: [u64; 3] =
    [TIMELY_SOURCE_WEIGHT, TIMELY_TARGET_WEIGHT, TIMELY_HEAD_WEIGHT];

pub const INACTIVITY_PENALTY_QUOTIENT_ALTAIR: u64 = 50_331_648;
pub const MIN_SLASHING_PENALTY_QUOTIENT_ALTAIR: u64 = 64;
pub const PROPORTIONAL_SLASHING_MULTIPLIER_ALTAIR: u64 = 2;

pub const SYNC_COMMITTEE_SIZE: u64 = 512;
pub const EPOCHS_PER_SYNC_COMMITTEE_PERIOD: u64 = 256;

pub const INACTIVITY_SCORE_BIAS: u64 = 4;
pub const INACTIVITY_SCORE_RECOVERY_RATE: u64 = 16;

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
    // [New in Altair]
    pub sync_aggregate: SyncAggregate,
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
    // [Modified in Altair]
    pub previous_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    // [Modified in Altair]
    pub current_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    pub justification_bits: [u8; JUSTIFICATION_BITS_LENGTH],
    pub previous_justified_checkpoint: Checkpoint,
    pub current_justified_checkpoint: Checkpoint,
    pub finalized_checkpoint: Checkpoint,
    // [New in Altair]
    pub inactivity_scores: [u64; VALIDATOR_REGISTRY_LIMIT as usize],
    // [New in Altair]
    pub current_sync_committee: SyncCommittee,
    // [New in Altair]
    pub next_sync_committee: SyncCommittee,
}

#[derive(Copy, Clone, Mutable)]
pub struct SyncAggregate {
    pub sync_committee_bits: [u8; SYNC_COMMITTEE_SIZE as usize],
    pub sync_committee_signature: BLSSignature,
}

#[derive(Copy, Clone, Mutable)]
pub struct SyncCommittee {
    pub pubkeys: [BLSPubkey; SYNC_COMMITTEE_SIZE as usize],
    pub aggregate_pubkey: BLSPubkey,
}
