use alloy::primitives::FixedBytes;

pub type Slot = u64;
pub type Epoch = u64;
pub type CommitteeIndex = u64;
pub type ValidatorIndex = u64;
pub type Gwei = u64;
pub type Root = FixedBytes<32>;
pub type Hash32 = FixedBytes<32>;
pub type Version = FixedBytes<4>;
pub type DomainType = FixedBytes<4>;
pub type ForkDigest = FixedBytes<4>;
pub type Domain = FixedBytes<32>;
pub type BLSPubkey = FixedBytes<48>;
pub type BLSSignature = FixedBytes<96>;

pub const UINT64_MAX: u64 = u64::MAX;
pub const UINT64_MAX_SQRT: u64 = 4294967295;
pub const GENESIS_SLOT: Slot = 0;
pub const GENESIS_EPOCH: Epoch = 0;
pub const FAR_FUTURE_EPOCH: Epoch = u64::MAX;
pub const BASE_REWARDS_PER_EPOCH: u64 = 4;
pub const DEPOSIT_CONTRACT_TREE_DEPTH: usize = 32;
pub const JUSTIFICATION_BITS_LENGTH: usize = 4;
pub const ENDIANNESS: &str = "little";
pub const BLS_WITHDRAWAL_PREFIX: FixedBytes<1> = FixedBytes([0]);
pub const ETH1_ADDRESS_WITHDRAWAL_PREFIX: FixedBytes<1> = FixedBytes([0x01]);

// @audit revisit cause little endian or shit
pub const DOMAIN_BEACON_PROPOSER: DomainType = FixedBytes([0x00, 0x00, 0x00, 0x00]);
pub const DOMAIN_BEACON_ATTESTER: DomainType = FixedBytes([0x01, 0x00, 0x00, 0x00]);
pub const DOMAIN_RANDAO: DomainType = FixedBytes([0x02, 0x00, 0x00, 0x00]);
pub const DOMAIN_DEPOSIT: DomainType = FixedBytes([0x03, 0x00, 0x00, 0x00]);
pub const DOMAIN_VOLUNTARY_EXIT: DomainType = FixedBytes([0x04, 0x00, 0x00, 0x00]);
pub const DOMAIN_SELECTION_PROOF: DomainType = FixedBytes([0x05, 0x00, 0x00, 0x00]);
pub const DOMAIN_AGGREGATE_AND_PROOF: DomainType = FixedBytes([0x06, 0x00, 0x00, 0x00]);
pub const DOMAIN_APPLICATION_MASK: DomainType = FixedBytes([0x00, 0x00, 0x00, 0x01]);
pub const MAX_COMMITTEES_PER_SLOT: u64 = 64;
pub const TARGET_COMMITTEE_SIZE: u64 = 128;
pub const MAX_VALIDATORS_PER_COMMITTEE: usize = 2048;
pub const SHUFFLE_ROUND_COUNT: u64 = 90;
pub const HYSTERESIS_QUOTIENT: u64 = 4;
pub const HYSTERESIS_DOWNWARD_MULTIPLIER: u64 = 1;
pub const HYSTERESIS_UPWARD_MULTIPLIER: u64 = 5;

pub const MIN_DEPOSIT_AMOUNT: Gwei = 1_000_000_000;
pub const MAX_EFFECTIVE_BALANCE: Gwei = 32_000_000_000;
pub const EFFECTIVE_BALANCE_INCREMENT: Gwei = 1_000_000_000;


pub const MIN_ATTESTATION_INCLUSION_DELAY: u64 = 1;
pub const SLOTS_PER_EPOCH: u64 = 32;
pub const MIN_SEED_LOOKAHEAD: u64 = 1;
pub const MAX_SEED_LOOKAHEAD: u64 = 4;
pub const MIN_EPOCHS_TO_INACTIVITY_PENALTY: u64 = 4;
pub const EPOCHS_PER_ETH1_VOTING_PERIOD: u64 = 64;
pub const SLOTS_PER_HISTORICAL_ROOT: u64 = 8_192;


pub const EPOCHS_PER_HISTORICAL_VECTOR: u64 = 65_536;
pub const EPOCHS_PER_SLASHINGS_VECTOR: u64 = 8_192;
pub const HISTORICAL_ROOTS_LIMIT: u64 = 16_777_216;
pub const VALIDATOR_REGISTRY_LIMIT: u64 = 1_099_511_627_776;

pub const BASE_REWARD_FACTOR: u64 = 64;
pub const WHISTLEBLOWER_REWARD_QUOTIENT: u64 = 512;
pub const PROPOSER_REWARD_QUOTIENT: u64 = 8;
pub const INACTIVITY_PENALTY_QUOTIENT: u64 = 67_108_864;
pub const MIN_SLASHING_PENALTY_QUOTIENT: u64 = 128;
pub const PROPORTIONAL_SLASHING_MULTIPLIER: u64 = 1;


pub const MAX_PROPOSER_SLASHINGS: u64 = 16;
pub const MAX_ATTESTER_SLASHINGS: u64 = 2;
pub const MAX_ATTESTATIONS: u64 = 128;
pub const MAX_DEPOSITS: u64 = 16;
pub const MAX_VOLUNTARY_EXITS: u64 = 16;


pub const MIN_GENESIS_ACTIVE_VALIDATOR_COUNT: u64 = 16_384;
pub const MIN_GENESIS_TIME: u64 = 1_606_824_000;
pub const GENESIS_FORK_VERSION: Version = FixedBytes([0x00, 0x00, 0x00, 0x00]);
pub const GENESIS_DELAY: u64 = 604_800;


pub const SLOT_DURATION_MS: u64 = 12000;
pub const SECONDS_PER_ETH1_BLOCK: u64 = 14;
pub const MIN_VALIDATOR_WITHDRAWABILITY_DELAY: u64 = 256;
pub const SHARD_COMMITTEE_PERIOD: u64 = 256;
pub const ETH1_FOLLOW_DISTANCE: u64 = 2_048;


pub const EJECTION_BALANCE: Gwei = 16_000_000_000;
pub const MIN_PER_EPOCH_CHURN_LIMIT: u64 = 4;
pub const CHURN_LIMIT_QUOTIENT: u64 = 65_536;

#[derive(Copy, Clone)]
pub struct Fork {
    pub previous_version: Version,
    pub current_version: Version,
    pub epoch: Epoch
}

#[derive(Copy, Clone)]
pub struct ForkData {
    pub current_version: Version,
    pub genesis_validators_root: Root
}

#[derive(Copy, Clone)]
pub struct Checkpoint {
    pub epoch: Epoch,
    pub root: Root
}

#[derive(Copy, Clone)]
pub struct Validator {
    pub pubkey: BLSPubkey,
    pub withdrawal_credentials: FixedBytes<32>,
    pub effective_balance: Gwei,
    pub slashed: bool,
    pub activation_eligibility_epoch: Epoch,
    pub activation_epoch: Epoch,
    pub exit_epoch: Epoch,
    pub withdrawable_epoch: Epoch
}

#[derive(Copy, Clone)]
pub struct AttestationData {
    pub slot: Slot,
    pub index: CommitteeIndex,
    pub beacon_block_root: Root,
    pub source: Checkpoint,
    pub target: Checkpoint
}

#[derive(Copy, Clone)]
pub struct IndexedAttestation {
    pub attesting_indices: [ValidatorIndex; MAX_VALIDATORS_PER_COMMITTEE],
    pub data: AttestationData,
    pub signature: BLSSignature
}

#[derive(Copy, Clone)]
pub struct PendingAttestation {
    pub aggregation_bits: [u8;MAX_VALIDATORS_PER_COMMITTEE],
    pub data: AttestationData,
    pub inclusion_delay: Slot,
    pub proposer_index: ValidatorIndex
}

#[derive(Copy, Clone)]
pub struct Eth1Data {
    pub deposit_root: Root,
    pub deposit_count: u64,
    pub block_hash: Hash32
}

#[derive(Copy, Clone)]
pub struct HistoricalBatch {
    pub block_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize],
    pub state_roots: [Root; SLOTS_PER_HISTORICAL_ROOT as usize]
}

#[derive(Copy, Clone)]
pub struct DepositMessage {
    pub pubkey: BLSPubkey,
    pub withdrawal_credentials: FixedBytes<32>,
    pub amount: Gwei
}

#[derive(Copy, Clone)]
pub struct DepositData {
    pub pubkey: BLSPubkey,
    pub withdrawal_credentials: FixedBytes<32>,
    pub amount: Gwei,
    pub signature: BLSSignature
}

#[derive(Copy, Clone)]
pub struct BeaconBlockHeader {
    pub slot: Slot,
    pub proposer_index: ValidatorIndex,
    pub parent_root: Root,
    pub state_root: Root,
    pub body_root: Root
}

#[derive(Copy, Clone)]
pub struct SigningData {
    pub object_root: Root,
    pub domain: Domain
}

#[derive(Copy, Clone)]
pub struct ProposerSlashing {
    pub signed_header_1: SignedBeaconBlockHeader,
    pub signed_header_2: SignedBeaconBlockHeader
}

#[derive(Copy, Clone)]
pub struct AttesterSlashing {
    pub attestation_1: IndexedAttestation,
    pub attestation_2: IndexedAttestation
}

#[derive(Copy, Clone)]
pub struct Attestation {
    pub aggregation_bits: [u8;MAX_VALIDATORS_PER_COMMITTEE],
    pub data: AttestationData,
    pub signature: BLSSignature
}

#[derive(Copy, Clone)]
pub struct Deposit {
    pub proof: [FixedBytes<32>; DEPOSIT_CONTRACT_TREE_DEPTH + 1],
    pub data: DepositData
}

#[derive(Copy, Clone)]
pub struct VoluntaryExit {
    pub epoch: Epoch,
    pub validator_index: ValidatorIndex
}

#[derive(Copy, Clone)]
pub struct BeaconBlockBody {
    pub randao_reveal: BLSSignature,
    pub eth1_data: Eth1Data,
    pub graffiti: FixedBytes<32>,
    pub proposer_slashings: [ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize],
    pub attester_slashings: [AttesterSlashing; MAX_ATTESTER_SLASHINGS as usize],
    pub attestations: [Attestation; MAX_ATTESTATIONS as usize],
    pub deposits: [Deposit; MAX_DEPOSITS as usize],
    pub voluntary_exits: [SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize]
}

#[derive(Copy, Clone)]
pub struct BeaconBlock {
    pub slot: Slot,
    pub proposer_index: ValidatorIndex,
    pub parent_root: Root,
    pub state_root: Root,
    pub body: BeaconBlockBody,
}

#[derive(Copy, Clone)]
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
    pub previous_epoch_attestations: [PendingAttestation; (MAX_ATTESTATIONS * SLOTS_PER_EPOCH) as usize],
    pub current_epoch_attestations: [PendingAttestation; (MAX_ATTESTATIONS * SLOTS_PER_EPOCH) as usize],
    pub justification_bits: [u8;JUSTIFICATION_BITS_LENGTH],
    pub previous_justified_checkpoint: Checkpoint,
    pub current_justified_checkpoint: Checkpoint,
    pub finalized_checkpoint: Checkpoint
}

#[derive(Copy, Clone)]
pub struct SignedVoluntaryExit {
    pub message: VoluntaryExit,
    pub signature: BLSSignature,
}

#[derive(Copy, Clone)]
pub struct SignedBeaconBlock {
    pub message: BeaconBlock,
    pub signature: BLSSignature,
}

#[derive(Copy, Clone)]
pub struct SignedBeaconBlockHeader {
    pub message: BeaconBlockHeader,
    pub signature: BLSSignature,
}