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

#[macro_export]
macro_rules! define_mutation {
    ($name:ident { $($variant:ident),+ $(,)? }) => {
        #[derive(Clone, Copy)]
        pub(crate) enum $name { $($variant),+ }

        impl $name {
            const ALL: &[Self] = &[$(Self::$variant),+];

            #[inline(always)]
            pub(crate) fn random(random: &mut impl Rng) -> Self {
                *Self::ALL.choose(random).unwrap()
            }
        }
    };
}
