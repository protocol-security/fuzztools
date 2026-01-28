use crate::circuits::{
    context::Context,
    utils::{bernoulli, random_field_element},
};
use rand::{seq::IndexedRandom, Rng};

// ────────────────────────────────────────────────────────────────────────────────
// Type definitions
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Field,
    Signed,
    Unsigned,
    Bool,
    Array,
    Slice,
    Tuple,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Field,
    Integer(Integer),
    Bool,
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Integer {
    pub bits: u32,
    pub signed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Array {
    pub ty: Box<Type>,
    pub size: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Slice {
    pub ty: Box<Type>,
    pub size: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub elements: Vec<Type>,
}

// ────────────────────────────────────────────────────────────────────────────────
// Type implementations
// ────────────────────────────────────────────────────────────────────────────────

impl Type {
    #[inline(always)]
    pub const fn kind(&self) -> TypeKind {
        match self {
            Self::Field => TypeKind::Field,
            Self::Integer(i) if i.signed => TypeKind::Signed,
            Self::Integer(_) => TypeKind::Unsigned,
            Self::Bool => TypeKind::Bool,
            Self::Array(_) => TypeKind::Array,
            Self::Slice(_) => TypeKind::Slice,
            Self::Tuple(_) => TypeKind::Tuple,
        }
    }

    #[inline(always)]
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        match self {
            Self::Field => random_field_element(random, ctx, "bn254", true),
            Self::Integer(i) => i.random_value(random, ctx),
            Self::Bool => random.random_bool(0.5).to_string(),
            Self::Array(a) => a.random_value(random, ctx),
            Self::Slice(s) => s.random_value(random, ctx),
            Self::Tuple(t) => t.random_value(random, ctx),
        }
    }

    pub fn random_castable_source(random: &mut impl Rng, target: &Type) -> Self {
        match (target, random.random_bool(0.5)) {
            (Self::Field, true) => Self::Bool,
            (Self::Field, false) => Self::Integer(Integer::random(random, false)),
            (Self::Integer(_), true) => Self::Field,
            (Self::Integer(_), false) => {
                let signed = random.random_bool(0.5);

                Self::Integer(Integer::random(random, signed))
            }
            _ => unreachable!(),
        }
    }

    /// - Primitive types: `Field`, `Integer`, `Bool`.
    /// - Non-empty arrays.
    /// - Tuples with a size greater than 1.
    ///
    /// Slices are invalid.
    #[inline(always)]
    pub fn is_valid_public_input(&self) -> bool {
        match self {
            Self::Field | Self::Integer(_) | Self::Bool => true,
            Self::Array(a) => a.size > 0 && a.ty.is_valid_public_input(),
            Self::Tuple(t) => {
                t.elements.len() > 1 && t.elements.iter().all(|e| e.is_valid_public_input())
            }
            Self::Slice(_) => false,
        }
    }

    #[inline(always)]
    pub const fn is_primitive(&self) -> bool {
        matches!(self, Self::Field | Self::Integer(_) | Self::Bool)
    }

    #[inline(always)]
    pub const fn is_numeric(&self) -> bool {
        matches!(self, Self::Field | Self::Integer(_))
    }

    #[inline(always)]
    pub const fn is_field(&self) -> bool {
        matches!(self, Self::Field)
    }

    #[inline(always)]
    pub const fn is_integer(&self) -> bool {
        matches!(self, Self::Integer(_))
    }

    #[inline(always)]
    pub const fn is_signed(&self) -> bool {
        matches!(self, Self::Integer(i) if i.signed)
    }

    #[inline(always)]
    pub const fn is_unsigned(&self) -> bool {
        matches!(self, Self::Integer(i) if !i.signed)
    }

    #[inline(always)]
    pub const fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }
}

impl Integer {
    const SIGNED_BITS: [u32; 4] = [8, 16, 32, 64];
    const UNSIGNED_BITS: [u32; 6] = [1, 8, 16, 32, 64, 128];

    /// Returns a random `Integer` with the given signedness.
    pub fn random(random: &mut impl Rng, signed: bool) -> Self {
        let bits = if signed {
            *Self::SIGNED_BITS.choose(random).unwrap()
        } else {
            *Self::UNSIGNED_BITS.choose(random).unwrap()
        };

        Self { bits, signed }
    }

    fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let ty = format!("{}{}", if self.signed { "i" } else { "u" }, self.bits);

        // This is a helper macro, dont mind it
        macro_rules! pick {
            ($min:expr, $max:expr, $boundaries:expr) => {
                if bernoulli(random, ctx.boundary_value_probability) {
                    *$boundaries.choose(random).unwrap()
                } else if bernoulli(random, ctx.small_value_probability) {
                    let small_limit = (ctx.max_small_value as u128).min($max as u128);
                    random.random_range(0..=small_limit as _)
                } else {
                    random.random_range($min..=$max)
                }
            };
        }

        // Short circuit if `u1`
        if self.bits == 1 {
            return format!("{}{}", random.random_bool(0.5) as u8, ty);
        }

        let value = if self.signed {
            let half = 1i128.checked_shl(self.bits - 1);
            let (min, max) = half.map(|h| (-h, h - 1)).unwrap_or((i128::MIN, i128::MAX));

            pick!(min, max, [0, 1, -1, min, max]).to_string()
        } else {
            let max = 1u128.checked_shl(self.bits).map(|m| m - 1).unwrap_or(u128::MAX);

            pick!(0, max, [0, 1, max]).to_string()
        };

        format!("{}{}", value, ty)
    }
}

impl Array {
    fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = (0..self.size).map(|_| self.ty.random_value(random, ctx)).collect();

        format!("[{}]", elems.join(", "))
    }
}

impl Slice {
    fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = (0..self.size).map(|_| self.ty.random_value(random, ctx)).collect();

        format!("&[{}]", elems.join(", "))
    }
}

impl Tuple {
    fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> =
            self.elements.iter().map(|elem_ty| elem_ty.random_value(random, ctx)).collect();

        format!("({})", elems.join(", "))
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Field => f.write_str("Field"),
            Self::Bool => f.write_str("bool"),
            Self::Integer(i) => write!(f, "{}{}", if i.signed { "i" } else { "u" }, i.bits),
            Self::Array(a) => write!(f, "[{}; {}]", a.ty, a.size),
            Self::Slice(s) => write!(f, "[{}]", s.ty),
            Self::Tuple(t) => {
                let elems = t.elements.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({elems})")
            }
        }
    }
}
