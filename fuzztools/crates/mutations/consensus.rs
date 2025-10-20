use super::traits::Mutable;
use crate::blockchain::cl::{
    forks::{
        altair::{SyncAggregate, SyncCommittee},
        capella::{
            BLSToExecutionChange, HistoricalSummary, SignedBLSToExecutionChange, Withdrawal,
        },
        deneb::{ExecutionPayload, ExecutionPayloadHeader},
        electra::{
            Attestation, AttesterSlashing, ConsolidationRequest, DepositRequest, ExecutionRequests,
            IndexedAttestation, PendingConsolidation, PendingDeposit, PendingPartialWithdrawal,
            WithdrawalRequest,
        },
        phase0::{
            AttestationData, BeaconBlockHeader, Checkpoint, Deposit, DepositData, Eth1Data, Fork,
            ProposerSlashing, SignedBeaconBlockHeader, SignedVoluntaryExit, Validator,
            VoluntaryExit,
        },
    },
    BeaconBlockBody, BeaconState,
};
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
            0 => {
                let mut aggregation_bits = self.aggregation_bits.to_vec();
                aggregation_bits.mutate(random);
                aggregation_bits.resize(self.aggregation_bits.len(), 0);
                self.aggregation_bits.copy_from_slice(&aggregation_bits);
                false
            },
            1 => self.data.mutate(random),
            2 => {
                let mut signature = self.signature.to_vec();
                signature.mutate(random);
                signature.resize(self.signature.len(), 0);
                self.signature.copy_from_slice(&signature);
                false
            },
            3 => {
                let mut committee_bits = self.committee_bits.to_vec();
                committee_bits.mutate(random);
                committee_bits.resize(self.committee_bits.len(), 0);
                self.committee_bits.copy_from_slice(&committee_bits);
                false
            },
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
            0 => {
                let mut sync_committee_bits = self.sync_committee_bits.to_vec();
                sync_committee_bits.mutate(random);
                sync_committee_bits.resize(self.sync_committee_bits.len(), 0);
                self.sync_committee_bits.copy_from_slice(&sync_committee_bits);
                false
            },
            1 => self.sync_committee_signature.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for Withdrawal {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=3) {
            0 => self.index.mutate(random),
            1 => self.validator_index.mutate(random),
            2 => self.address.mutate(random),
            3 => self.amount.mutate(random),
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
            4 => {
                let mut logs_bloom = self.logs_bloom.to_vec();
                logs_bloom.mutate(random);
                logs_bloom.resize(self.logs_bloom.len(), 0);
                self.logs_bloom.copy_from_slice(&logs_bloom);
                false
            },
            5 => self.prev_randao.mutate(random),
            6 => self.block_number.mutate(random),
            7 => self.gas_limit.mutate(random),
            8 => self.gas_used.mutate(random),
            9 => self.timestamp.mutate(random),
            10 => {
                let mut extra_data = self.extra_data.to_vec();
                extra_data.mutate(random);
                extra_data.resize(self.extra_data.len(), 0);
                self.extra_data.copy_from_slice(&extra_data);
                false
            },
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

impl Mutable for BeaconBlockBody {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=12) {
            0 => self.randao_reveal.mutate(random),
            1 => self.eth1_data.mutate(random),
            2 => self.graffiti.mutate(random),
            3 => self.proposer_slashings.mutate(random),
            4 => self.attester_slashings.mutate(random),
            5 => self.attestations.mutate(random),
            6 => self.deposits.mutate(random),
            7 => self.voluntary_exits.mutate(random),
            8 => self.sync_aggregate.mutate(random),
            9 => self.execution_payload.mutate(random),
            10 => self.bls_to_execution_changes.mutate(random),
            11 => self.blob_kzg_commitments.mutate(random),
            12 => self.execution_requests.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for Fork {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.previous_version.mutate(random),
            1 => self.current_version.mutate(random),
            2 => self.epoch.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for Validator {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=7) {
            0 => self.pubkey.mutate(random),
            1 => self.withdrawal_credentials.mutate(random),
            2 => self.effective_balance.mutate(random),
            3 => {
                self.slashed = !self.slashed;
                false
            },
            4 => self.activation_eligibility_epoch.mutate(random),
            5 => self.activation_epoch.mutate(random),
            6 => self.exit_epoch.mutate(random),
            7 => self.withdrawable_epoch.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for SyncCommittee {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.pubkeys.mutate(random),
            1 => self.aggregate_pubkey.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for ExecutionPayloadHeader {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=16) {
            0 => self.parent_hash.mutate(random),
            1 => self.fee_recipient.mutate(random),
            2 => self.state_root.mutate(random),
            3 => self.receipts_root.mutate(random),
            4 => {
                let mut logs_bloom = self.logs_bloom.to_vec();
                logs_bloom.mutate(random);
                logs_bloom.resize(self.logs_bloom.len(), 0);
                self.logs_bloom.copy_from_slice(&logs_bloom);
                false
            },
            5 => self.prev_randao.mutate(random),
            6 => self.block_number.mutate(random),
            7 => self.gas_limit.mutate(random),
            8 => self.gas_used.mutate(random),
            9 => self.timestamp.mutate(random),
            10 => {
                let mut extra_data = self.extra_data.to_vec();
                extra_data.mutate(random);
                extra_data.resize(self.extra_data.len(), 0);
                self.extra_data.copy_from_slice(&extra_data);
                false
            },
            11 => self.base_fee_per_gas.mutate(random),
            12 => self.block_hash.mutate(random),
            13 => self.transactions_root.mutate(random),
            14 => self.withdrawals_root.mutate(random),
            15 => self.blob_gas_used.mutate(random),
            16 => self.excess_blob_gas.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for HistoricalSummary {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.block_summary_root.mutate(random),
            1 => self.state_summary_root.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for PendingDeposit {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=4) {
            0 => self.pubkey.mutate(random),
            1 => self.withdrawal_credentials.mutate(random),
            2 => self.amount.mutate(random),
            3 => self.signature.mutate(random),
            4 => self.slot.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for PendingPartialWithdrawal {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.validator_index.mutate(random),
            1 => self.amount.mutate(random),
            2 => self.withdrawable_epoch.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for PendingConsolidation {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => self.source_index.mutate(random),
            1 => self.target_index.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for BeaconState {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=37) {
            0 => self.genesis_time.mutate(random),
            1 => self.genesis_validators_root.mutate(random),
            2 => self.slot.mutate(random),
            3 => self.fork.mutate(random),
            4 => self.latest_block_header.mutate(random),
            5 => self.block_roots.mutate(random),
            6 => self.state_roots.mutate(random),
            7 => self.historical_roots.mutate(random),
            8 => self.eth1_data.mutate(random),
            9 => self.eth1_data_votes.mutate(random),
            10 => self.eth1_deposit_index.mutate(random),
            11 => self.validators.mutate(random),
            12 => self.balances.mutate(random),
            13 => self.randao_mixes.mutate(random),
            14 => self.slashings.mutate(random),
            15 => self.previous_epoch_participation.mutate(random),
            16 => self.current_epoch_participation.mutate(random),
            17 => {
                let mut justification_bits = self.justification_bits.to_vec();
                justification_bits.mutate(random);
                justification_bits.resize(self.justification_bits.len(), 0);
                self.justification_bits.copy_from_slice(&justification_bits);
                false
            },
            18 => self.previous_justified_checkpoint.mutate(random),
            19 => self.current_justified_checkpoint.mutate(random),
            20 => self.finalized_checkpoint.mutate(random),
            21 => self.inactivity_scores.mutate(random),
            22 => self.current_sync_committee.mutate(random),
            23 => self.next_sync_committee.mutate(random),
            24 => self.latest_execution_payload_header.mutate(random),
            25 => self.next_withdrawal_index.mutate(random),
            26 => self.next_withdrawal_validator_index.mutate(random),
            27 => self.historical_summaries.mutate(random),
            28 => self.deposit_requests_start_index.mutate(random),
            29 => self.deposit_balance_to_consume.mutate(random),
            30 => self.exit_balance_to_consume.mutate(random),
            31 => self.earliest_exit_epoch.mutate(random),
            32 => self.consolidation_balance_to_consume.mutate(random),
            33 => self.earliest_consolidation_epoch.mutate(random),
            34 => self.pending_deposits.mutate(random),
            35 => self.pending_partial_withdrawals.mutate(random),
            36 => self.pending_consolidations.mutate(random),
            37 => self.proposer_lookahead.mutate(random),
            _ => unreachable!(),
        }
    }
}
