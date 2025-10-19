//! Mutations for BeaconState and BeaconBlockBody

use super::traits::{BeaconBlockBodyMutations, BeaconStateMutations, Mutable};
use crate::blockchain::cl::{BeaconBlockBody, BeaconState};
use rand::Rng;

impl Mutable for BeaconState {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.mutate_pending_deposits(random),
            1 => self.mutate_pending_partial_withdrawals(random),
            2 => self.mutate_pending_consolidations(random),
            _ => unreachable!(),
        }

        false
    }
}

impl Mutable for BeaconBlockBody {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            0 => self.mutate_attestations(random),
            1 => self.mutate_deposits(random),
            2 => self.mutate_execution_requests(random),
            _ => unreachable!(),
        }

        false
    }
}

impl BeaconStateMutations for BeaconState {
    fn mutate_pending_deposits(&mut self, random: &mut impl Rng) {
        todo!("Implement pending deposits mutations")
    }

    fn mutate_pending_partial_withdrawals(&mut self, random: &mut impl Rng) {
        todo!("Implement pending partial withdrawals mutations")
    }

    fn mutate_pending_consolidations(&mut self, random: &mut impl Rng) {
        todo!("Implement pending consolidations mutations")
    }
}

impl BeaconBlockBodyMutations for BeaconBlockBody {
    fn mutate_attestations(&mut self, random: &mut impl Rng) {
        todo!("Implement attestations mutations")
    }

    fn mutate_deposits(&mut self, random: &mut impl Rng) {
        todo!("Implement deposits mutations")
    }

    fn mutate_execution_requests(&mut self, random: &mut impl Rng) {
        todo!("Implement execution requests mutations")
    }
}
