use super::traits::{Mutable, ArrayMutations, FixedArrayMutations};
use crate::blockchain::cl::forks::{altair::SyncAggregate, capella::{BLSToExecutionChange, SignedBLSToExecutionChange}, deneb::{ExecutionPayload, KZGCommitment}, electra::{Attestation, AttesterSlashing, ExecutionRequests, IndexedAttestation, WithdrawalRequest}, phase0::{AttestationData, BeaconBlockHeader, Checkpoint, Deposit, DepositData, Eth1Data, ProposerSlashing, SignedBeaconBlockHeader, SignedVoluntaryExit, VoluntaryExit}};
use alloy::eips::{eip6110::DepositRequest, eip7251::ConsolidationRequest};
use rand::Rng;

impl Mutable for Eth1Data {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.deposit_root.mutate(random),
            1 => self.deposit_count.mutate(random),
            2 => self.block_hash.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl BeaconBlockHeader {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=4) {
            0 => self.slot.mutate(random),
            1 => self.proposer_index.mutate(random),
            2 => self.parent_root.mutate(random),
            3 => self.state_root.mutate(random),
            4 => self.body_root.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for SignedBeaconBlockHeader {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.message.mutate(random),
            1 => self.signature.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for ProposerSlashing {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.signed_header_1.mutate(random),
            1 => self.signed_header_2.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for Checkpoint {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.epoch.mutate(random),
            1 => self.root.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for AttestationData {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=3) {
            0 => self.slot.mutate(random),
            1 => self.index.mutate(random),
            2 => self.beacon_block_root.mutate(random),
            3 => self.source.mutate(random),
            4 => self.target.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for IndexedAttestation {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.attesting_indices.mutate(random),
            1 => self.data.mutate(random),
            2 => self.signature.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for AttesterSlashing {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.attestation_1.mutate(random),
            1 => self.attestation_2.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for Attestation {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=3) {
            0 => self.aggregation_bits.mutate(random),
            1 => self.data.mutate(random),
            2 => self.signature.mutate(random),
            3 => self.committee_bits.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for DepositData {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=3) {
            0 => self.pubkey.mutate(random),
            1 => self.withdrawal_credentials.mutate(random),
            2 => self.amount.mutate(random),
            3 => self.signature.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for Deposit {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.proof.mutate(random),
            1 => self.data.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for VoluntaryExit {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.epoch.mutate(random),
            1 => self.validator_index.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for SignedVoluntaryExit {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.message.mutate(random),
            1 => self.signature.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for SyncAggregate {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.sync_committee_bits.mutate(random),
            1 => self.sync_committee_signature.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for ExecutionPayload {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=16) {
            0 => self.parent_hash.mutate(random),
            1 => self.fee_recipient.mutate(random),
            2 => self.state_root.mutate(random),
            3 => self.receipts_root.mutate(random),
            4 => self.logs_bloom.mutate(random),
            5 => self.prev_randao.mutate(random),
            6 => self.block_number.mutate(random),
            7 => self.gas_limit.mutate(random),
            8 => self.gas_used.mutate(random),
            9 => self.timestamp.mutate(random),
            10 => self.extra_data.mutate(random),
            11 => self.base_fee_per_gas.mutate(random),
            12 => self.block_hash.mutate(random),
            13 => self.transactions.mutate(random),
            14 => self.withdrawals.mutate(random),
            15 => self.blob_gas_used.mutate(random),
            16 => self.excess_blob_gas.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for BLSToExecutionChange {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.validator_index.mutate(random),
            1 => self.from_bls_pubkey.mutate(random),
            2 => self.to_execution_address.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for SignedBLSToExecutionChange {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.message.mutate(random),
            1 => self.signature.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for DepositRequest {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=4) {
            0 => self.pubkey.mutate(random),
            1 => self.withdrawal_credentials.mutate(random),
            2 => self.amount.mutate(random),
            3 => self.signature.mutate(random),
            4 => self.index.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for WithdrawalRequest {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.source_address.mutate(random),
            1 => self.validator_pubkey.mutate(random),
            2 => self.amount.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for ConsolidationRequest {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.source_address.mutate(random),
            1 => self.source_pubkey.mutate(random),
            2 => self.target_pubkey.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for ExecutionRequests {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.deposits.mutate(random),
            1 => self.withdrawals.mutate(random),
            2 => self.consolidations.mutate(random),
            _ => unreachable!(),
        }
    }
}