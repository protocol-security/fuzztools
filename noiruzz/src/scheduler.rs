use std::{
    sync::atomic::{AtomicU64, Ordering},
    time::Duration,
};

pub(crate) struct PowerScheduler {
    t1: AtomicU64,
    t2: AtomicU64,
    target_ratio: f64,
}

impl PowerScheduler {
    pub(crate) const fn new(target_ratio: f64) -> Self {
        Self { t1: AtomicU64::new(0), t2: AtomicU64::new(0), target_ratio }
    }

    /// Add time spent on initial stages
    pub(crate) fn add_t1(&self, duration: Duration) {
        self.t1.fetch_add(duration.as_nanos() as u64, Ordering::Relaxed);
    }

    /// Add time spent on later stages
    pub(crate) fn add_t2(&self, duration: Duration) {
        self.t2.fetch_add(duration.as_nanos() as u64, Ordering::Relaxed);
    }

    /// Check if we should run the later (slower) stages based on current ratio
    pub(crate) fn should_run_later_stages(&self) -> bool {
        let t1 = self.t1.load(Ordering::Relaxed) as f64;
        let t2 = self.t2.load(Ordering::Relaxed) as f64;

        // Avoid division by zero - always run later stages initially
        if t1 + t2 == 0.0 {
            return true;
        }

        let current_ratio = t2 / (t1 + t2);
        current_ratio < self.target_ratio
    }

    /// Get current ratio for display
    pub(crate) fn current_ratio(&self) -> f64 {
        let t1 = self.t1.load(Ordering::Relaxed) as f64;
        let t2 = self.t2.load(Ordering::Relaxed) as f64;

        if t1 + t2 == 0.0 {
            0.0
        } else {
            t2 / (t1 + t2)
        }
    }
}
