use super::context::Context;
use crate::{
    math::{bernoulli, random_field_element},
    utils::{random_string, RandomChoice},
};
use rand::Rng;

// ------------------------------------------------------------
// Type Definitions
// ------------------------------------------------------------

#[derive(Clone)]
pub enum Type {
    Field(Field),
    Integer(Integer),
    Boolean(Boolean),
    String(StringType),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Struct(Struct),
    Reference(Reference),
}

#[derive(Clone)]
pub struct Field {
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Integer {
    pub bits: u8,
    pub signed: bool,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Boolean {
    pub mutable: bool,
}

#[derive(Clone)]
pub struct StringType {
    pub size: usize,
    pub hashes: usize,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Array {
    pub ty: Box<Type>,
    pub size: usize,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Slice {
    pub ty: Box<Type>,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Tuple {
    pub inner: Vec<Type>,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Box<Type>,
    pub visibility: Visibility,
}

#[derive(Clone, Copy)]
pub enum Visibility {
    Public,
    PublicCrate,
    Private,
}

#[derive(Clone)]
pub struct Reference {
    pub ty: Box<Type>,
}

// ------------------------------------------------------------
// Methods Lists
// ------------------------------------------------------------

const FIELD_METHODS: &[&str] = &[
    "to_le_bits",
    "to_be_bits",
    "to_le_bytes",
    "to_be_bytes",
    "pow_32",
    "assert_max_bit_size",
    "sgn0",
    "lt",
    "wrapping_add",
    "wrapping_sub",
    "wrapping_mul",
];

const INTEGER_METHODS: &[&str] = &["wrapping_add", "wrapping_sub", "wrapping_mul"];

const STRING_METHODS: &[&str] = &["as_bytes"];

const ARRAY_METHODS: &[&str] = &[
    "all",
    "any",
    "as_slice",
    "concat",
    "fold",
    "for_each",
    "for_eachi",
    "len",
    "map",
    "mapi",
    "reduce",
    "sort",
    "sort_via",
    "cmp",
    "eq",
];

const SLICE_METHODS: &[&str] = &[
    "all",
    "any",
    "append",
    "as_array",
    "filter",
    "fold",
    "for_each",
    "for_eachi",
    "insert",
    "join",
    "len",
    "map",
    "mapi",
    "pop_back",
    "pop_front",
    "push_back",
    "push_front",
    "reduce",
    "remove",
    "cmp",
    "eq",
    "sort",
    "sort_via",
    "reverse",
];

const TUPLE_METHODS: &[&str] = &["cmp", "eq"];

const STRUCT_METHODS: &[&str] = &["cmp", "eq"];

const INTEGER_PARAMS: [(u8, bool); 10] = [
    (1, false),
    (8, false),
    (16, false),
    (32, false),
    (64, false),
    (128, false),
    (8, true),
    (16, true),
    (32, true),
    (64, true),
];

const VISIBILITIES: [Visibility; 3] =
    [Visibility::Private, Visibility::PublicCrate, Visibility::Public];

// ------------------------------------------------------------
// Type Implementation
// ------------------------------------------------------------

impl Type {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        if ctx.type_depth > ctx.max_type_depth {
            return Type::Field(Field::random(random, ctx));
        }

        let valid_structs: Vec<&Struct> = if ctx.filter_entrypoint_structs {
            structs.iter().filter(|s| s.fields.iter().all(|f| f.ty.is_valid_entrypoint())).collect()
        } else {
            structs.iter().collect()
        };

        let mut available_types: Vec<&str> =
            vec!["Field", "Integer", "Boolean", "String", "Array", "Tuple"];

        if ctx.allow_slices {
            available_types.push("Slice");
        }
        if ctx.allow_references {
            available_types.push("Reference");
        }
        if ctx.allow_structs && !valid_structs.is_empty() {
            available_types.push("Struct");
        }

        match *random.choice(&available_types) {
            "Field" => Type::Field(Field::random(random, ctx)),
            "Integer" => Type::Integer(Integer::random(random, ctx)),
            "Boolean" => Type::Boolean(Boolean::random(random, ctx)),
            "String" => Type::String(StringType::random(random, ctx)),
            "Array" => Type::Array(Array::random(random, ctx, structs)),
            "Slice" => Type::Slice(Slice::random(random, ctx, structs)),
            "Tuple" => Type::Tuple(Tuple::random(random, ctx, structs)),
            "Struct" => Type::Struct((*random.choice(&valid_structs)).clone()),
            "Reference" => Type::Reference(Reference::random(random, ctx, structs)),
            _ => unreachable!(),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        match self {
            Type::Field(f) => f.random_value(random, ctx),
            Type::Integer(i) => i.random_value(random, ctx),
            Type::Boolean(b) => b.random_value(random),
            Type::String(s) => s.random_value(random),
            Type::Array(a) => a.random_value(random, ctx),
            Type::Slice(s) => s.random_value(random, ctx),
            Type::Tuple(t) => t.random_value(random, ctx),
            Type::Struct(s) => s.random_value(random, ctx),
            Type::Reference(r) => r.random_value(random, ctx),
        }
    }

    pub fn random_method(&self, random: &mut impl Rng) -> Option<&'static str> {
        match self {
            Type::Field(_) => Some(*random.choice(FIELD_METHODS)),
            Type::Integer(_) => Some(*random.choice(INTEGER_METHODS)),
            Type::Boolean(_) => None,
            Type::String(_) => Some(*random.choice(STRING_METHODS)),
            Type::Array(_) => Some(*random.choice(ARRAY_METHODS)),
            Type::Slice(_) => Some(*random.choice(SLICE_METHODS)),
            Type::Tuple(_) => Some(*random.choice(TUPLE_METHODS)),
            Type::Struct(_) => Some(*random.choice(STRUCT_METHODS)),
            Type::Reference(_) => None,
        }
    }

    pub fn is_valid_entrypoint(&self) -> bool {
        match self {
            Type::Field(_) | Type::Integer(_) | Type::Boolean(_) => true,
            Type::String(s) => s.size > 0,
            Type::Array(a) => a.size > 0 && a.ty.is_valid_entrypoint(),
            Type::Slice(_) | Type::Reference(_) => false,
            Type::Tuple(t) => {
                !t.inner.is_empty() && t.inner.iter().all(|e| e.is_valid_entrypoint())
            },
            Type::Struct(s) => s.fields.iter().all(|f| f.ty.is_valid_entrypoint()),
        }
    }
}

// ------------------------------------------------------------
// Primitive Types
// ------------------------------------------------------------

impl Field {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        Self { mutable: random.random_bool(ctx.mutable_probability) }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let exclude_prime = random.random_bool(ctx.exclude_prime_probability);
        random_field_element(
            "bn254",
            random,
            exclude_prime,
            ctx.boundary_value_probability,
            ctx.small_upper_bound_probability,
            ctx.max_small_upper_bound,
        )
        .to_string()
    }
}

impl Integer {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        let (bits, signed) = *random.choice(&INTEGER_PARAMS);
        Self { bits, signed, mutable: random.random_bool(ctx.mutable_probability) }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        if self.bits == 1 {
            return if random.random_bool(0.5) { "1" } else { "0" }.into();
        }

        if self.signed {
            let half = 1i128 << (self.bits - 1);
            let (min, max) = (-half, half - 1);

            let value = if bernoulli(ctx.boundary_value_probability, random) {
                *random.choice(&[0, 1, -1, min, max])
            } else {
                random.random_range(min..=max)
            };

            if value < 0 {
                format!("-{}", value.unsigned_abs())
            } else {
                value.to_string()
            }
        } else {
            let max = if self.bits == 128 { u128::MAX } else { (1u128 << self.bits) - 1 };

            let value = if bernoulli(ctx.boundary_value_probability, random) {
                *random.choice(&[0, 1, max])
            } else {
                random.random_range(0..=max)
            };

            value.to_string()
        }
    }
}

impl Boolean {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        Self { mutable: random.random_bool(ctx.mutable_probability) }
    }

    #[inline]
    pub fn random_value(&self, random: &mut impl Rng) -> String {
        if random.random_bool(0.5) { "true" } else { "false" }.into()
    }
}

impl StringType {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context) -> Self {
        Self {
            size: random.random_range(ctx.min_string_size..ctx.max_string_size),
            hashes: if random.random_bool(ctx.raw_string_probability) {
                random.random_range(0..ctx.max_string_hashes_count)
            } else {
                0
            },
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng) -> String {
        let value = random_string(random, self.size);
        if self.hashes > 0 {
            let hashes = "#".repeat(self.hashes);
            format!("r{hashes}\"{value}\"{hashes}")
        } else {
            format!("r\"{value}\"")
        }
    }
}

// ------------------------------------------------------------
// Compound Types
// ------------------------------------------------------------

impl Array {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner = ctx.inner_type();
        Self {
            ty: Box::new(Type::random(random, &inner, structs)),
            size: random.random_range(ctx.min_element_count..ctx.max_element_count),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = (0..self.size).map(|_| self.ty.random_value(random, ctx)).collect();
        format!("[{}]", elems.join(", "))
    }
}

impl Slice {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner = ctx.inner_type();
        Self {
            ty: Box::new(Type::random(random, &inner, structs)),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let size = random.random_range(0..ctx.max_element_count);
        let elems: Vec<_> = (0..size).map(|_| self.ty.random_value(random, ctx)).collect();
        format!("[{}]", elems.join(", "))
    }
}

impl Tuple {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner_ctx = ctx.inner_type();
        let size = random.random_range(ctx.min_element_count..ctx.max_element_count);
        Self {
            inner: (0..size).map(|_| Type::random(random, &inner_ctx, structs)).collect(),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let elems: Vec<_> = self.inner.iter().map(|e| e.random_value(random, ctx)).collect();
        format!("({})", elems.join(", "))
    }
}

impl Struct {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            fields: (0..ctx.max_struct_fields_count)
                .map(|_| StructField::random(random, &ctx.inner_type(), structs))
                .collect(),
            mutable: random.random_bool(ctx.mutable_probability),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        let fields: Vec<_> = self
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, f.ty.random_value(random, ctx)))
            .collect();
        format!("{} {{ {} }}", self.name, fields.join(", "))
    }
}

impl StructField {
    #[inline]
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            ty: Box::new(Type::random(random, ctx, structs)),
            visibility: *random.choice(&VISIBILITIES),
        }
    }
}

impl Reference {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        let inner = ctx.inner_type();
        Self { ty: Box::new(Type::random(random, &inner, structs)) }
    }

    #[inline]
    pub fn random_value(&self, random: &mut impl Rng, ctx: &Context) -> String {
        format!("&{}", self.ty.random_value(random, ctx))
    }
}

// ------------------------------------------------------------
// Display
// ------------------------------------------------------------

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Field(_) => f.write_str("Field"),
            Type::Integer(i) => write!(f, "{}{}", if i.signed { "i" } else { "u" }, i.bits),
            Type::Boolean(_) => f.write_str("bool"),
            Type::String(s) => write!(f, "str<{}>", s.size),
            Type::Array(a) => write!(f, "[{}; {}]", a.ty, a.size),
            Type::Slice(s) => write!(f, "[{}]", s.ty),
            Type::Tuple(t) => {
                let types: Vec<_> = t.inner.iter().map(ToString::to_string).collect();
                write!(f, "({})", types.join(", "))
            },
            Type::Struct(s) => f.write_str(&s.name),
            Type::Reference(r) => write!(f, "&{}", r.ty),
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
