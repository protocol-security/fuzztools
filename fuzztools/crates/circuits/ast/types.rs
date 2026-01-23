use crate::circuits::{
    ast::forest::Forest,
    context::Context,
    scope::Scope,
    utils::{bernoulli, random_field_element, random_string},
};
use rand::{seq::IndexedRandom, Rng};

// ────────────────────────────────────────────────────────────────────────────────
// Type definitions
// ────────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Field,
    Unsigned,
    Signed,
    Boolean,
    String,
    Array,
    Slice,
    Tuple,
    Struct,
    Lambda,
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Field,
    Integer(Integer),
    Boolean,
    String(StringType),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Struct(Struct),
    Lambda(Lambda),
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Integer {
    pub bits: u32,
    pub signed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringType {
    pub size: usize,
    pub is_raw: bool,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: String,
    pub ty: Box<Type>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    PublicCrate,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lambda {
    pub params: Vec<(String, Type)>,
    pub ret: Box<Type>,
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
            Type::Boolean => TypeKind::Boolean,
            Type::String(_) => TypeKind::String,
            Type::Array(_) => TypeKind::Array,
            Type::Slice(_) => TypeKind::Slice,
            Type::Tuple(_) => TypeKind::Tuple,
            Type::Struct(_) => TypeKind::Struct,
            Type::Lambda(_) => TypeKind::Lambda,
            Type::Empty => TypeKind::Empty,
        }
    }

    #[inline(always)]
    pub const fn is_signed(&self) -> bool {
        matches!(self.kind(), TypeKind::Signed)
    }

    #[inline(always)]
    pub const fn is_unsigned(&self) -> bool {
        matches!(self.kind(), TypeKind::Unsigned)
    }

    #[inline(always)]
    pub const fn can_be_mutable(&self) -> bool {
        !matches!(self, Type::Lambda(_))
    }

    #[inline(always)]
    pub fn random_value(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        with_suffix: bool,
    ) -> String {
        match self {
            Type::Field => format!(
                "{}{}",
                random_field_element(random, ctx, "bn254"),
                if with_suffix { "Field" } else { "" }
            ),
            Type::Integer(i) => i.random_value(random, ctx, with_suffix),
            Type::Boolean => random.random_bool(0.5).to_string(),
            Type::String(s) => s.random_value(random, ctx),
            Type::Array(a) => a.random_value(random, ctx, scope, with_suffix),
            Type::Slice(s) => s.random_value(random, ctx, scope, with_suffix),
            Type::Tuple(t) => t.random_value(random, ctx, scope, with_suffix),
            Type::Struct(s) => s.random_value(random, ctx, scope, with_suffix),
            Type::Lambda(l) => l.random_value(random, ctx, scope),
            Type::Empty => "()".to_string(),
        }
    }

    /// According to the Noir IR spec, the following types are valid public inputs:
    /// - Primitive types: `Field`, `Integer`, `Boolean`
    /// - Strings with a non-zero size
    /// - Arrays with a non-zero size and a valid sub-type
    /// - Tuples with a size greater than 1 (as otherwise they collapse to a single type) and valid
    ///   sub-types
    /// - Structs where all fields are valid public inputs
    ///
    /// The rest are all invalid as public inputs.
    #[inline(always)]
    pub fn is_valid_public_input(&self) -> bool {
        match self {
            Type::Field | Type::Integer(_) | Type::Boolean => true,
            Type::String(s) => s.size > 0,
            Type::Array(a) => a.size > 0 && a.ty.is_valid_public_input(),
            Type::Tuple(t) => {
                t.elements.len() > 1 && t.elements.iter().all(|e| e.is_valid_public_input())
            }
            Type::Struct(s) => s.fields.iter().all(|f| f.ty.is_valid_public_input()),
            Type::Slice(_) | Type::Lambda(_) | Type::Empty => false,
        }
    }

    #[inline(always)]
    pub fn has_slice(&self) -> bool {
        match self {
            Type::Field |
            Type::Integer(_) |
            Type::Boolean |
            Type::String(_) |
            Type::Lambda(_) |
            Type::Empty => false,
            Type::Slice(_) => true,
            Type::Array(a) => a.ty.has_slice(),
            Type::Tuple(t) => t.elements.iter().any(|e| e.has_slice()),
            Type::Struct(s) => s.fields.iter().any(|f| f.ty.has_slice()),
        }
    }

    /// Returns true if this type contains any zero-sized arrays or slices.
    /// Zero-sized arrays/slices in on-the-fly literals cause type inference failures in Noir.
    #[inline(always)]
    pub fn has_zero_sized(&self) -> bool {
        match self {
            Type::Array(a) => a.size == 0 || a.ty.has_zero_sized(),
            Type::Slice(s) => s.size == 0 || s.ty.has_zero_sized(),
            Type::Tuple(t) => t.elements.iter().any(|e| e.has_zero_sized()),
            Type::Struct(s) => s.fields.iter().any(|f| f.ty.has_zero_sized()),
            _ => false,
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Primitive types
// ────────────────────────────────────────────────────────────────────────────────

impl Integer {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, with_suffix: bool) -> String {
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

        format!("{}{}", value, if with_suffix { ty } else { String::new() })
    }
}

impl StringType {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let value = random_string(random, self.size, self.is_raw);

        if self.is_raw {
            let hash_count = random.random_range(0..=ctx.max_hashes_count);
            let hashes = "#".repeat(hash_count);
            format!("r{hashes}\"{value}\"{hashes}")
        } else {
            format!("\"{value}\"")
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Complex types
// ────────────────────────────────────────────────────────────────────────────────

impl Array {
    pub fn random_value(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        with_suffix: bool,
    ) -> String {
        let elems: Vec<_> =
            (0..self.size).map(|_| self.ty.random_value(random, ctx, scope, with_suffix)).collect();

        format!("[{}]", elems.join(", "))
    }
}

impl Slice {
    pub fn random_value(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        with_suffix: bool,
    ) -> String {
        let elems: Vec<_> =
            (0..self.size).map(|_| self.ty.random_value(random, ctx, scope, with_suffix)).collect();

        format!("&[{}]", elems.join(", "))
    }
}

impl Tuple {
    pub fn random_value(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        with_suffix: bool,
    ) -> String {
        let elems: Vec<_> = self
            .elements
            .iter()
            .map(|elem_ty| elem_ty.random_value(random, ctx, scope, with_suffix))
            .collect();

        format!("({})", elems.join(", "))
    }
}

impl Struct {
    pub fn random_value(
        &self,
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        with_suffix: bool,
    ) -> String {
        let fields: Vec<_> = self
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, f.ty.random_value(random, ctx, scope, with_suffix)))
            .collect();

        format!("{} {{ {} }}", self.name, fields.join(", "))
    }
}

impl Lambda {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> String {
        // Compute bias from param types and return type
        let mut bias = self.params.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();
        bias.push((*self.ret).clone());

        let mut scope = scope.clone();
        scope.lambda_depth += 1;
        scope.ret = Some(((*self.ret).clone(), false));
        scope.type_bias = Scope::compute_type_bias(&bias);
        scope.inputs = self.params.iter().cloned().map(|(name, ty)| (name, ty, false)).collect();

        let mut body = Forest::default();
        self.params.iter().for_each(|(n, t)| {
            body.input(random, n.clone(), t.clone());
        });

        body.random_with_bounds(
            random,
            ctx,
            &scope,
            ctx.min_lambda_body_size,
            ctx.max_lambda_body_size,
            false,
        );

        body.set_return_expression(random, ctx, &scope);

        let indent = "    ".repeat(scope.lambda_depth);
        let params =
            self.params.iter().map(|(n, t)| format!("{n}: {t}")).collect::<Vec<_>>().join(", ");

        format!(
            "|{params}| -> {} {{\n{}{indent}}}",
            self.ret,
            body.format_with_indent(&format!("{indent}    "))
        )
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Field => f.write_str("Field"),
            Self::Boolean => f.write_str("bool"),
            Self::Empty => f.write_str("()"),
            Self::Integer(i) => write!(f, "{}{}", if i.signed { "i" } else { "u" }, i.bits),
            Self::String(s) => write!(f, "str<{}>", s.size),
            Self::Array(a) => write!(f, "[{}; {}]", a.ty, a.size),
            Self::Slice(s) => write!(f, "[{}]", s.ty),
            Self::Struct(s) => f.write_str(&s.name),
            Self::Tuple(t) => {
                let elems = t.elements.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({elems})")
            }
            Self::Lambda(l) => {
                let args =
                    l.params.iter().map(|(_, t)| t.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "fn({args}) -> {}", l.ret)
            }
        }
    }
}

impl std::fmt::Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {} {{", self.name)?;

        for field in &self.fields {
            writeln!(f, "    {}: {},", field.name, field.ty)?;
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}
