use crate::{
    blockchain::cl::forks::{
        bellatrix::{Transaction, MAX_BYTES_PER_TRANSACTION, MAX_TRANSACTIONS_PER_PAYLOAD},
        capella::{
            SignedBLSToExecutionChange, Withdrawal, MAX_BLS_TO_EXECUTION_CHANGES,
            MAX_WITHDRAWALS_PER_PAYLOAD,
        },
        deneb::{KZGCommitment, MAX_BLOB_COMMITMENTS_PER_BLOCK},
        electra::{
            Attestation, AttesterSlashing, ConsolidationRequest, DepositRequest, WithdrawalRequest,
            MAX_ATTESTATIONS_ELECTRA, MAX_ATTESTER_SLASHINGS_ELECTRA,
            MAX_CONSOLIDATION_REQUESTS_PER_PAYLOAD, MAX_DEPOSIT_REQUESTS_PER_PAYLOAD,
            MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD,
        },
        phase0::{
            Deposit, ProposerSlashing, SignedVoluntaryExit, DEPOSIT_CONTRACT_TREE_DEPTH,
            MAX_COMMITTEES_PER_SLOT, MAX_DEPOSITS, MAX_PROPOSER_SLASHINGS,
            MAX_VALIDATORS_PER_COMMITTEE, MAX_VOLUNTARY_EXITS,
        },
    },
    mutations::traits::{ArrayMutations, Mutable},
};
use alloy::primitives::FixedBytes;
use rand::{seq::SliceRandom, Rng};

macro_rules! check_not_empty {
    ($bytes:ident) => {
        if $bytes.is_empty() {
            return;
        }
    };
}

macro_rules! check_not_smaller {
    ($bytes:ident, $n:expr) => {
        if $bytes.len() < $n {
            return;
        }
    };
}

macro_rules! impl_mutate {
    ($type:ty) => {
        impl Mutable for $type {
            #[inline(always)]
            fn mutate(&mut self, random: &mut impl Rng) -> bool {
                match random.random_range(0..=7) {
                    0 => self.value_swap(random),
                    1 => self.value_mutate(random),
                    2 => self.rotate_left_by_n(random),
                    3 => self.rotate_right_by_n(random),
                    4 => self.shuffle_array(random),
                    5 => self.reverse_array(),
                    6 => self.slice_swap(random),
                    7 => self.slice_mutate(random),
                    _ => unreachable!(),
                }

                false
            }
        }
    };
}

macro_rules! impl_mutations {
    ($type:ty) => {
        impl ArrayMutations for $type {
            #[inline(always)]
            fn value_swap(&mut self, random: &mut impl Rng) {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());
                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }

            #[inline(always)]
            fn value_mutate(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }

            #[inline(always)]
            fn rotate_left_by_n(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_left(positions);
            }

            #[inline(always)]
            fn rotate_right_by_n(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_right(positions);
            }

            #[inline(always)]
            fn shuffle_array(&mut self, random: &mut impl Rng) {
                // No need to check as it is a NOOP if empty
                self.shuffle(random);
            }

            #[inline(always)]
            fn reverse_array(&mut self) {
                check_not_empty!(self);
                self.reverse();
            }

            #[inline(always)]
            fn slice_swap(&mut self, random: &mut impl Rng) {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());

                if idx1 != idx2 {
                    let max_idx = idx1.max(idx2);
                    let len = random.random_range(0..=self.len() - max_idx);

                    let slice1 = self[idx1..idx1 + len].to_vec();
                    let slice2 = self[idx2..idx2 + len].to_vec();

                    self[idx1..idx1 + len].copy_from_slice(&slice2);
                    self[idx2..idx2 + len].copy_from_slice(&slice1);
                }
            }

            #[inline(always)]
            fn slice_mutate(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let len = random.random_range(0..self.len() - idx);

                for i in idx..idx + len {
                    self[i].mutate(random);
                }
            }
        }
    };
}

impl_mutations!([u64; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize]);
impl_mutate!([u64; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize]);

impl_mutations!([FixedBytes<32>; DEPOSIT_CONTRACT_TREE_DEPTH + 1]);
impl_mutate!([FixedBytes<32>; DEPOSIT_CONTRACT_TREE_DEPTH + 1]);

impl_mutations!([u8; MAX_BYTES_PER_TRANSACTION as usize]);
impl_mutate!([u8; MAX_BYTES_PER_TRANSACTION as usize]);

impl_mutations!([Transaction; MAX_TRANSACTIONS_PER_PAYLOAD as usize]);
impl_mutate!([Transaction; MAX_TRANSACTIONS_PER_PAYLOAD as usize]);

impl_mutations!([Withdrawal; MAX_WITHDRAWALS_PER_PAYLOAD as usize]);
impl_mutate!([Withdrawal; MAX_WITHDRAWALS_PER_PAYLOAD as usize]);

impl_mutations!([DepositRequest; MAX_DEPOSIT_REQUESTS_PER_PAYLOAD as usize]);
impl_mutate!([DepositRequest; MAX_DEPOSIT_REQUESTS_PER_PAYLOAD as usize]);

impl_mutations!([WithdrawalRequest; MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD as usize]);
impl_mutate!([WithdrawalRequest; MAX_WITHDRAWAL_REQUESTS_PER_PAYLOAD as usize]);

impl_mutations!([ConsolidationRequest; MAX_CONSOLIDATION_REQUESTS_PER_PAYLOAD as usize]);
impl_mutate!([ConsolidationRequest; MAX_CONSOLIDATION_REQUESTS_PER_PAYLOAD as usize]);

impl_mutations!([KZGCommitment; MAX_BLOB_COMMITMENTS_PER_BLOCK as usize]);
impl_mutate!([KZGCommitment; MAX_BLOB_COMMITMENTS_PER_BLOCK as usize]);

impl_mutations!([ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize]);
impl_mutate!([ProposerSlashing; MAX_PROPOSER_SLASHINGS as usize]);

impl_mutations!([AttesterSlashing; MAX_ATTESTER_SLASHINGS_ELECTRA as usize]);
impl_mutate!([AttesterSlashing; MAX_ATTESTER_SLASHINGS_ELECTRA as usize]);

impl_mutations!([Attestation; MAX_ATTESTATIONS_ELECTRA as usize]);
impl_mutate!([Attestation; MAX_ATTESTATIONS_ELECTRA as usize]);

impl_mutations!([Deposit; MAX_DEPOSITS as usize]);
impl_mutate!([Deposit; MAX_DEPOSITS as usize]);

impl_mutations!([SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize]);
impl_mutate!([SignedVoluntaryExit; MAX_VOLUNTARY_EXITS as usize]);

impl_mutations!([SignedBLSToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES as usize]);
impl_mutate!([SignedBLSToExecutionChange; MAX_BLS_TO_EXECUTION_CHANGES as usize]);

impl_mutations!([u8; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize]);
impl_mutate!([u8; MAX_VALIDATORS_PER_COMMITTEE * MAX_COMMITTEES_PER_SLOT as usize]);

impl_mutations!([u8; MAX_COMMITTEES_PER_SLOT as usize]);
impl_mutate!([u8; MAX_COMMITTEES_PER_SLOT as usize]);
