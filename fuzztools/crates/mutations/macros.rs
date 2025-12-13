//! Macros for checking if an array or vector is empty or smaller than a given value.

#[macro_export]
macro_rules! check_not_empty {
    ($val:expr) => {
        if $val.is_empty() {
            return false;
        }
    };
}

#[macro_export]
macro_rules! check_not_smaller {
    ($val:expr, $n:expr) => {
        if $val.len() < $n {
            return false;
        }
    };
}
