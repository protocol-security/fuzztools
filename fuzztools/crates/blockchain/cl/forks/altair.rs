use alloy::primitives::FixedBytes;
use super::phase0::*;

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

pub const PARTICIPATION_FLAG_WEIGHTS: [u64; 3] = [TIMELY_SOURCE_WEIGHT, TIMELY_TARGET_WEIGHT, TIMELY_HEAD_WEIGHT];

pub const INACTIVITY_PENALTY_QUOTIENT_ALTAIR: u64 = 50_331_648;
pub const MIN_SLASHING_PENALTY_QUOTIENT_ALTAIR: u64 = 64;
pub const PROPORTIONAL_SLASHING_MULTIPLIER_ALTAIR: u64 = 2;

pub const SYNC_COMMITTEE_SIZE: u64 = 512;
pub const EPOCHS_PER_SYNC_COMMITTEE_PERIOD: u64 = 256;

pub const INACTIVITY_SCORE_BIAS: u64 = 4;
pub const INACTIVITY_SCORE_RECOVERY_RATE: u64 = 16;

pub struct BeaconBlockBody {
    randao_reveal: BLSSignature,
    eth1_data: Eth1Data,
    graffiti: FixedBytes<32>,
    proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS as usize],
    attestations: [Attestation; MAX_ATTESTATIONS as usize],
    deposits: [Deposit; MAX_DEPOSITS as usize],
    voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize],
    // [New in Altair]
    sync_aggregate: SyncAggregate,
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
    // [Modified in Altair]
    previous_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    // [Modified in Altair]
    current_epoch_participation: [ParticipationFlags; VALIDATOR_REGISTRY_LIMIT as usize],
    justification_bits: [bool; JUSTIFICATION_BITS_LENGTH],
    previous_justified_checkpoint: Checkpoint,
    current_justified_checkpoint: Checkpoint,
    finalized_checkpoint: Checkpoint,
    // [New in Altair]
    inactivity_scores: [u64; VALIDATOR_REGISTRY_LIMIT as usize],
    // [New in Altair]
    current_sync_committee: SyncCommittee,
    // [New in Altair]
    next_sync_committee: SyncCommittee,
}

pub struct SyncAggregate {
    sync_committee_bits: [bool; SYNC_COMMITTEE_SIZE as usize],
    sync_committee_signature: BLSSignature,
}

pub struct SyncCommittee {
    pubkeys: [BLSPubkey; SYNC_COMMITTEE_SIZE as usize],
    aggregate_pubkey: BLSPubkey,
}
