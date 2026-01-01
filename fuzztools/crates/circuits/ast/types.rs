//! Implements the Noir IR types

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
    // @todo Reference
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
    pub bits: u8,
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
    pub fn kind(&self) -> TypeKind {
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

    pub fn is_signed(&self) -> bool {
        matches!(self.kind(), TypeKind::Signed)
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(self.kind(), TypeKind::Unsigned)
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> String {
        match self {
            Type::Field => format!("{}Field", random_field_element(random, ctx, "bn254")),
            Type::Integer(i) => i.random_value(random, ctx),
            Type::Boolean => random.random_bool(0.5).to_string(),
            Type::String(s) => s.random_value(random),
            Type::Array(a) => a.random_value(random, ctx, scope),
            Type::Slice(s) => s.random_value(random, ctx, scope),
            Type::Tuple(t) => t.random_value(random, ctx, scope),
            Type::Struct(s) => s.random_value(random, ctx, scope),
            Type::Lambda(l) => l.random_value(random, ctx, scope),
            Type::Empty => "()".to_string(),
        }
    }

    /// According to the Noir IR spec, the following types are valid public inputs:
    /// - Primitive types: `Field`, `Integer`, `Boolean`
    /// - Strings with a non-zero size
    /// - Arrays with a non-zero size and a valid sub-type
    /// - Tuples with a size greater than 1 (as otherwise they collapse to a single type) and a
    ///   valid sub-type
    /// - Structs where all fields are valid public inputs @todo they allow empty structs wtf
    ///
    /// The rest are all invalid as public inputs.
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

    pub fn allows_slice(&self) -> bool {
        match self {
            Type::Field |
            Type::Integer(_) |
            Type::Boolean |
            Type::String(_) |
            Type::Lambda(_) |
            Type::Empty => false,
            Type::Slice(_) => true,
            Type::Array(a) => a.ty.allows_slice(),
            Type::Tuple(t) => t.elements.iter().any(|e| e.allows_slice()),
            Type::Struct(s) => s.fields.iter().any(|f| f.ty.allows_slice()),
        }
    }

    /// Returns whether this type can be declared as mutable.
    /// Lambda and Empty types cannot be mutable.
    pub fn can_be_mutable(&self) -> bool {
        !matches!(self, Type::Lambda(_) | Type::Empty)
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Primitive types
// ────────────────────────────────────────────────────────────────────────────────

impl Integer {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let ty = format!("{}{}", if self.signed { "i" } else { "u" }, self.bits);

        // Special cases
        if self.bits == 1 {
            return format!("{}{}", if random.random_bool(0.5) { "1" } else { "0" }, ty);
        }

        if self.bits == 128 {
            return format!("{}{}", random.random_range(0..=u128::MAX), ty);
        }

        let value = if self.signed {
            let half = 1i128 << (self.bits - 1);
            let (min, max) = (-half, half - 1);

            if bernoulli(random, ctx.boundary_value_probability) {
                *[0, 1, -1, min, max].choose(random).unwrap()
            } else {
                random.random_range(min..=max)
            }
        } else {
            let max = (1 << self.bits) - 1;

            if bernoulli(random, ctx.boundary_value_probability) {
                *[0, 1, max].choose(random).unwrap()
            } else {
                random.random_range(0..=max)
            }
        };

        format!("{}{}", value, ty)
    }
}

impl StringType {
    pub fn random_value(&self, random: &mut impl Rng) -> String {
        let value = random_string(random, self.size);

        // @todo # and escape characters
        if self.is_raw {
            format!("r\"{value}\"")
        } else {
            format!("\"{value}\"")
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Complex types
// ────────────────────────────────────────────────────────────────────────────────

impl Array {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> String {
        let elems: Vec<_> =
            (0..self.size).map(|_| self.ty.random_value(random, ctx, scope)).collect();

        format!("[{}]", elems.join(", "))
    }
}

impl Slice {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> String {
        let elems: Vec<_> =
            (0..self.size).map(|_| self.ty.random_value(random, ctx, scope)).collect();

        format!("&[{}]", elems.join(", "))
    }
}

impl Tuple {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> String {
        let elems: Vec<_> =
            self.elements.iter().map(|e| e.random_value(random, ctx, scope)).collect();

        format!("({})", elems.join(", "))
    }
}

impl Struct {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> String {
        let fields: Vec<_> = self
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, f.ty.random_value(random, ctx, scope)))
            .collect();

        format!("{} {{ {} }}", self.name, fields.join(", "))
    }
}

impl Lambda {
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context, scope: &Scope) -> String {
        let mut out = String::new();
        let mut lambda_scope = scope.clone();
        lambda_scope.inputs = self.params.clone();

        let mut body = Forest::default();
        for param in self.params.iter() {
            body.input(param.0.clone(), param.1.clone());
        }

        body.random_with_bounds(
            random,
            ctx,
            &lambda_scope,
            ctx.min_lambda_body_size,
            ctx.max_lambda_body_size,
        );
        let body_string = body.to_string();

        out.push_str(
            format!(
                "|{}|",
                self.params
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .as_str(),
        );

        if matches!(*self.ret, Type::Empty) {
            out.push_str(" {\n");
            out.push_str(&body_string);
            out.push_str("}\n");
        } else {
            out.push_str(format!(" -> {} {{\n", self.ret).as_str());
            let candidates: Vec<_> =
                body.types.get(&self.ret).into_iter().flatten().copied().collect();
            let ret_expr = if let Some(&idx) = candidates.choose(random) {
                body.get_expr_for_node(idx)
            } else {
                self.ret.random_value(random, ctx, &lambda_scope)
            };
            out.push_str(&body_string);
            out.push_str(format!("\n{}\n}}", ret_expr).as_str());
        }

        out
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Display
// ────────────────────────────────────────────────────────────────────────────────

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Field => f.write_str("Field"),
            Type::Integer(i) => write!(f, "{}{}", if i.signed { "i" } else { "u" }, i.bits),
            Type::Boolean => f.write_str("bool"),
            Type::String(s) => write!(f, "str<{}>", s.size),
            Type::Array(a) => write!(f, "[{}; {}]", a.ty, a.size),
            Type::Slice(s) => write!(f, "[{}]", s.ty),
            Type::Tuple(t) => write!(
                f,
                "({})",
                t.elements.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
            ),
            Type::Struct(s) => write!(f, "{}", s.name),
            Type::Lambda(l) => write!(
                f,
                "fn({}) -> {}",
                l.params.iter().map(|(_, ty)| ty.to_string()).collect::<Vec<_>>().join(", "),
                l.ret
            ),
            Type::Empty => f.write_str("()"),
        }
    }
}

impl std::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => f.write_str("pub "),
            Visibility::PublicCrate => f.write_str("pub(crate) "),
            Visibility::Private => f.write_str(""),
        }
    }
}

impl std::fmt::Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {} {{", self.name)?;

        for field in self.fields.iter() {
            writeln!(f, "    {}{}: {},", field.visibility, field.name, field.ty)?;
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}
