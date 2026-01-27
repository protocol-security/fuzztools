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
    pub(crate) bits: u32,
    pub(crate) signed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Array {
    pub(crate) ty: Box<Type>,
    pub(crate) size: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Slice {
    pub(crate) ty: Box<Type>,
    pub(crate) size: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub(crate) elements: Vec<Type>,
}

// ────────────────────────────────────────────────────────────────────────────────
// Type implementations
// ────────────────────────────────────────────────────────────────────────────────

impl Type {
    #[inline(always)]
    pub const fn kind(&self) -> TypeKind {
        match self {
            Type::Field => TypeKind::Field,
            Type::Integer(i) if i.signed => TypeKind::Signed,
            Type::Integer(_) => TypeKind::Unsigned,
            Type::Bool => TypeKind::Bool,
            Type::Array(_) => TypeKind::Array,
            Type::Slice(_) => TypeKind::Slice,
            Type::Tuple(_) => TypeKind::Tuple,
        }
    }

    #[inline(always)]
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        match self {
            Type::Field => random_field_element(random, ctx, "bn254", true),
            Type::Integer(i) => i.random_value(random, ctx),
            Type::Bool => random.random_bool(0.5).to_string(),
            Type::Array(a) => a.random_value(random, ctx),
            Type::Slice(s) => s.random_value(random, ctx),
            Type::Tuple(t) => t.random_value(random, ctx),
        }
    }

    /// - Primitive types: `Field`, `Integer`, `Bool`.
    /// - Non-empty arrays.
    /// - Tuples with a size greater than 1.
    ///
    /// Slices are invalid.
    #[inline(always)]
    pub(crate) fn is_valid_public_input(&self) -> bool {
        match self {
            Type::Field | Type::Integer(_) | Type::Bool => true,
            Type::Array(a) => a.size > 0 && a.ty.is_valid_public_input(),
            Type::Tuple(t) => {
                t.elements.len() > 1 && t.elements.iter().all(|e| e.is_valid_public_input())
            }
            Type::Slice(_) => false,
        }
    }

    #[inline(always)]
    pub(crate) const fn is_primitive(&self) -> bool {
        matches!(self, Type::Field | Type::Integer(_) | Type::Bool)
    }

    #[inline(always)]
    pub(crate) const fn is_numeric(&self) -> bool {
        matches!(self, Type::Field | Type::Integer(_))
    }

    #[inline(always)]
    pub(crate) const fn is_integer(&self) -> bool {
        matches!(self, Type::Integer(_))
    }

    #[inline(always)]
    pub(crate) const fn is_signed(&self) -> bool {
        matches!(self, Type::Integer(i) if i.signed)
    }

    #[inline(always)]
    pub(crate) const fn is_unsigned(&self) -> bool {
        matches!(self, Type::Integer(i) if !i.signed)
    }

    #[inline(always)]
    pub(crate) const fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }
}

impl Integer {
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
