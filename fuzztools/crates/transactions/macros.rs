//! Macros for RLP encoding transaction fields.

/// RLP encodes an `Option` field.
#[macro_export]
macro_rules! encode_field {
    ($field:expr, $out:expr) => {
        if let Some(value) = &$field {
            value.encode($out);
        }
    };
}

/// Gets the RLP length of an `Option` field.
#[macro_export]
macro_rules! field_len {
    ($field:expr) => {
        if let Some(value) = &$field {
            value.length()
        } else {
            0
        }
    };
}
