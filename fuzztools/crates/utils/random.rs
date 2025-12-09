use rand::Rng;

/// A trait for types that can pick a random item from a slice.
pub trait RandomChoice {
    /// Picks one random element from a slice.
    /// Returns `None` if the slice is empty.
    /// **NOTE**: This function does not check if the slice is empty. If the slice is empty, it will
    /// panic.
    fn choice<'a, T>(&mut self, items: &'a [T]) -> &'a T;
}

impl<R: Rng + ?Sized> RandomChoice for R {
    fn choice<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        let idx = self.random_range(0..items.len());
        &items[idx]
    }
}
