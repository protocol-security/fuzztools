use alloy::primitives::FixedBytes;

pub(crate) type Slot = u64;
pub(crate) type Epoch = u64;
pub(crate) type CommitteeIndex = u64;
pub(crate) type ValidatorIndex = u64;
pub(crate) type Gwei = u64;
pub(crate) type Root = FixedBytes<32>;
pub(crate) type Hash32 = FixedBytes<32>;
pub(crate) type Version = FixedBytes<4>;
pub(crate) type DomainType = FixedBytes<4>;
pub(crate) type ForkDigest = FixedBytes<4>;
pub(crate) type Domain = FixedBytes<32>;
pub(crate) type BLSPubkey = FixedBytes<48>;
pub(crate) type BLSSignature = FixedBytes<96>;

pub(crate) const UINT64_MAX: u64 = u64::MAX;
pub(crate) const UINT64_MAX_SQRT: u64 = 4294967295;
pub(crate) const GENESIS_SLOT: Slot = 0;
pub(crate) const GENESIS_EPOCH: Epoch = 0;
pub(crate) const FAR_FUTURE_EPOCH: Epoch = u64::MAX;
pub(crate) const BASE_REWARDS_PER_EPOCH: u64 = 4;
pub(crate) const DEPOSIT_CONTRACT_TREE_DEPTH: usize = 32;
pub(crate) const JUSTIFICATION_BITS_LENGTH: usize = 4;
pub(crate) const ENDIANNESS: &str = "little";
pub(crate) const BLS_WITHDRAWAL_PREFIX: FixedBytes<1> = FixedBytes([0]);
pub(crate) const ETH1_ADDRESS_WITHDRAWAL_PREFIX: FixedBytes<1> = FixedBytes([0x01]);

// @audit revisit cause little endian or shit
pub(crate) const DOMAIN_BEACON_PROPOSER: DomainType = FixedBytes([0x00, 0x00, 0x00, 0x00]);
pub(crate) const DOMAIN_BEACON_ATTESTER: DomainType = FixedBytes([0x01, 0x00, 0x00, 0x00]);
pub(crate) const DOMAIN_RANDAO: DomainType = FixedBytes([0x02, 0x00, 0x00, 0x00]);
pub(crate) const DOMAIN_DEPOSIT: DomainType = FixedBytes([0x03, 0x00, 0x00, 0x00]);
pub(crate) const DOMAIN_VOLUNTARY_EXIT: DomainType = FixedBytes([0x04, 0x00, 0x00, 0x00]);
pub(crate) const DOMAIN_SELECTION_PROOF: DomainType = FixedBytes([0x05, 0x00, 0x00, 0x00]);
pub(crate) const DOMAIN_AGGREGATE_AND_PROOF: DomainType = FixedBytes([0x06, 0x00, 0x00, 0x00]);
pub(crate) const DOMAIN_APPLICATION_MASK: DomainType = FixedBytes([0x00, 0x00, 0x00, 0x01]);
pub(crate) const MAX_COMMITTEES_PER_SLOT: u64 = 64;
pub(crate) const TARGET_COMMITTEE_SIZE: u64 = 128;
pub(crate) const MAX_VALIDATORS_PER_COMMITTEE: usize = 2048;
pub(crate) const SHUFFLE_ROUND_COUNT: u64 = 90;
pub(crate) const HYSTERESIS_QUOTIENT: u64 = 4;
pub(crate) const HYSTERESIS_DOWNWARD_MULTIPLIER: u64 = 1;
pub(crate) const HYSTERESIS_UPWARD_MULTIPLIER: u64 = 5;

pub(crate) const MIN_DEPOSIT_AMOUNT: Gwei = 1_000_000_000;
pub(crate) const MAX_EFFECTIVE_BALANCE: Gwei = 32_000_000_000;
pub(crate) const EFFECTIVE_BALANCE_INCREMENT: Gwei = 1_000_000_000;


pub(crate) const MIN_ATTESTATION_INCLUSION_DELAY: u64 = 1;
pub(crate) const SLOTS_PER_EPOCH: u64 = 32;
pub(crate) const MIN_SEED_LOOKAHEAD: u64 = 1;
pub(crate) const MAX_SEED_LOOKAHEAD: u64 = 4;
pub(crate) const MIN_EPOCHS_TO_INACTIVITY_PENALTY: u64 = 4;
pub(crate) const EPOCHS_PER_ETH1_VOTING_PERIOD: u64 = 64;
pub(crate) const SLOTS_PER_HISTORICAL_ROOT: u64 = 8_192;


pub(crate) const EPOCHS_PER_HISTORICAL_VECTOR: u64 = 65_536;
pub(crate) const EPOCHS_PER_SLASHINGS_VECTOR: u64 = 8_192;
pub(crate) const HISTORICAL_ROOTS_LIMIT: u64 = 16_777_216;
pub(crate) const VALIDATOR_REGISTRY_LIMIT: u64 = 1_099_511_627_776;

pub(crate) const BASE_REWARD_FACTOR: u64 = 64;
pub(crate) const WHISTLEBLOWER_REWARD_QUOTIENT: u64 = 512;
pub(crate) const PROPOSER_REWARD_QUOTIENT: u64 = 8;
pub(crate) const INACTIVITY_PENALTY_QUOTIENT: u64 = 67_108_864;
pub(crate) const MIN_SLASHING_PENALTY_QUOTIENT: u64 = 128;
pub(crate) const PROPORTIONAL_SLASHING_MULTIPLIER: u64 = 1;


pub(crate) const MAX_PROPOSER_SLASHINGS: u64 = 16;
pub(crate) const MAX_ATTESTER_SLASHINGS: u64 = 2;
pub(crate) const MAX_ATTESTATIONS: u64 = 128;
pub(crate) const MAX_DEPOSITS: u64 = 16;
pub(crate) const MAX_VOLUNTARY_EXITS: u64 = 16;


pub(crate) const MIN_GENESIS_ACTIVE_VALIDATOR_COUNT: u64 = 16_384;
pub(crate) const MIN_GENESIS_TIME: u64 = 1_606_824_000;
pub(crate) const GENESIS_FORK_VERSION: Version = FixedBytes([0x00, 0x00, 0x00, 0x00]);
pub(crate) const GENESIS_DELAY: u64 = 604_800;


pub(crate) const SLOT_DURATION_MS: u64 = 12000;
pub(crate) const SECONDS_PER_ETH1_BLOCK: u64 = 14;
pub(crate) const MIN_VALIDATOR_WITHDRAWABILITY_DELAY: u64 = 256;
pub(crate) const SHARD_COMMITTEE_PERIOD: u64 = 256;
pub(crate) const ETH1_FOLLOW_DISTANCE: u64 = 2_048;


pub(crate) const EJECTION_BALANCE: Gwei = 16_000_000_000;
pub(crate) const MIN_PER_EPOCH_CHURN_LIMIT: u64 = 4;
pub(crate) const CHURN_LIMIT_QUOTIENT: u64 = 65_536;

pub(crate) struct Fork {
    previous_version: Version,
    current_version: Version,
    epoch: Epoch
}

pub(crate) struct ForkData {
    current_version: Version,
    genesis_validators_root: Root
}

pub(crate) struct Checkpoint {
    epoch: Epoch,
    root: Root
}

pub(crate) struct Validator {
    pubkey: BLSPubkey,
    withdrawal_credentials: FixedBytes<32>,
    effective_balance: Gwei,
    slashed: bool,
    activation_eligibility_epoch: Epoch,
    activation_epoch: Epoch,
    exit_epoch: Epoch,
    withdrawable_epoch: Epoch
}

pub(crate) struct AttestationData {
    slot: Slot,
    index: CommitteeIndex,
    beacon_block_root: Root,
    source: Checkpoint,
    target: Checkpoint
}

pub(crate) struct IndexedAttestation {
    attesting_indices: [ValidatorIndex; MAX_VALIDATORS_PER_COMMITTEE],
    data: AttestationData,
    signature: BLSSignature
}

pub(crate) struct PendingAttestation {
    aggregation_bits: [bool; MAX_VALIDATORS_PER_COMMITTEE],
    data: AttestationData,
    inclusion_delay: Slot,
    proposer_index: ValidatorIndex
}

pub(crate) struct Eth1Data {
    deposit_root: Root,
    deposit_count: u64,
    block_hash: Hash32
}

pub(crate) struct HistoricalBatch {
    block_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize],
    state_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize]
}

pub(crate) struct DepositMessage {
    pubkey: BLSPubkey,
    withdrawal_credentials: FixedBytes<32>,
    amount: Gwei
}

pub(crate) struct DepositData {
    pubkey: BLSPubkey,
    withdrawal_credentials: FixedBytes<32>,
    amount: Gwei,
    signature: BLSSignature
}

pub(crate) struct BeaconBlockHeader {
    slot: Slot,
    proposer_index: ValidatorIndex,
    parent_root: Root,
    state_root: Root,
    body_root: Root
}

pub(crate) struct SigningData {
    object_root: Root,
    domain: Domain
}

pub(crate) struct ProposerSlashing {
    signed_header_1: SignedBeaconBlockHeader,
    signed_header_2: SignedBeaconBlockHeader
}

pub(crate) struct AttesterSlashing {
    attestation_1: IndexedAttestation,
    attestation_2: IndexedAttestation
}

pub(crate) struct Attestation {
    aggregation_bits: [bool; MAX_VALIDATORS_PER_COMMITTEE],
    data: AttestationData,
    signature: BLSSignature
}

pub(crate) struct Deposit {
    proof: [FixedBytes<32>; DEPOSIT_CONTRACT_TREE_DEPTH + 1],
    data: DepositData
}

pub(crate) struct VoluntaryExit {
    epoch: Epoch,
    validator_index: ValidatorIndex
}

pub(crate) struct BeaconBlockBody {
    randao_reveal: BLSSignature,
    eth1_data: Eth1Data,
    graffiti: FixedBytes<32>,
    proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS as usize],
    attestations: [Attestation; MAX_ATTESTATIONS as usize],
    deposits: [Deposit; MAX_DEPOSITS as usize],
    voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize]
}

pub(crate) struct BeaconBlock {
    slot: Slot,
    proposer_index: ValidatorIndex,
    parent_root: Root,
    state_root: Root,
    body: BeaconBlockBody,
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
    previous_epoch_attestations: [PendingAttestation; (MAX_ATTESTATIONS * SLOTS_PER_EPOCH) as usize],
    current_epoch_attestations: [PendingAttestation; (MAX_ATTESTATIONS * SLOTS_PER_EPOCH) as usize],
    justification_bits: [bool; JUSTIFICATION_BITS_LENGTH],
    previous_justified_checkpoint: Checkpoint,
    current_justified_checkpoint: Checkpoint,
    finalized_checkpoint: Checkpoint
}

pub(crate) struct SignedVoluntaryExit {
    message: VoluntaryExit,
    signature: BLSSignature,
}

pub(crate) struct SignedBeaconBlock {
    message: BeaconBlock,
    signature: BLSSignature,
}

pub(crate) struct SignedBeaconBlockHeader {
    message: BeaconBlockHeader,
    signature: BLSSignature,
}