use super::{config::Config, structs::Struct};
use crate::{
    math::{bernoulli, random_field_element},
    utils::{random_string, RandomChoice},
};
use rand::Rng;
use std::fmt;

/// Context that guides type generation, filtering what types can be generated
/// and controlling constraints like minimum sizes.
#[derive(Clone, Debug)]
pub struct TypeContext {
    /// Whether slices are allowed
    pub allow_slices: bool,
    /// Whether references are allowed
    pub allow_references: bool,
    /// Whether structs are allowed
    pub allow_structs: bool,
    /// Minimum array size (1 = no empty arrays)
    pub min_array_size: usize,
    /// Minimum string size (1 = no empty strings)
    pub min_string_size: usize,
    /// Minimum tuple size
    pub min_tuple_size: usize,
    /// Minimum struct field count (1 = no empty structs)
    pub min_struct_fields: usize,
    /// Whether to filter structs to only entrypoint-valid ones
    pub filter_entrypoint_structs: bool,
    /// Current type depth
    pub depth: usize,
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::top_level()
    }
}

impl TypeContext {
    /// Default context for top-level types (allows everything)
    pub fn top_level() -> Self {
        Self {
            allow_slices: true,
            allow_references: false,
            allow_structs: true,
            min_array_size: 0,
            min_string_size: 0,
            min_tuple_size: 1,
            min_struct_fields: 0,
            filter_entrypoint_structs: false,
            depth: 0,
        }
    }

    /// Context for inner types (inside arrays, tuples, etc.)
    /// Slices are not allowed at inner level.
    pub fn inner(&self) -> Self {
        Self {
            allow_slices: false,
            depth: self.depth + 1,
            ..*self
        }
    }

    /// Context for entry point types (main, test, contract, fuzz functions).
    /// Cannot contain: slices, references, empty arrays, empty strings.
    /// Structs are allowed but filtered to only those that are entrypoint-valid.
    pub fn entrypoint() -> Self {
        Self {
            allow_slices: false,
            allow_references: false,
            allow_structs: true, // Structs allowed, but filtered to entrypoint-valid ones
            min_array_size: 1,
            min_string_size: 1,
            min_tuple_size: 1,
            min_struct_fields: 1,
            filter_entrypoint_structs: true,
            depth: 0,
        }
    }

    /// Build inner context for entrypoint types (propagates all restrictions)
    pub fn entrypoint_inner(&self) -> Self {
        Self {
            allow_slices: false,
            allow_structs: false,
            depth: self.depth + 1,
            ..*self
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Field(Field),
    Integer(Integer),
    Boolean(Boolean),
    String(StringLiteral),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Reference(Reference),
    Struct(Struct),
}

#[derive(Clone, Debug)]
pub struct Field {
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Integer {
    pub bits: u8,
    pub signed: bool,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Boolean {
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct StringLiteral {
    pub size: usize,
    pub raw_hashes: Option<usize>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Array {
    pub r#type: Box<Type>,
    pub size: usize,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Slice {
    pub r#type: Box<Type>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Tuple {
    pub inner: Vec<Type>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Reference {
    pub r#type: Box<Type>,
}

impl Type {
    /// Generate a random type guided by the given context.
    ///
    /// The context controls:
    /// - Which types are allowed (slices, references, structs)
    /// - Minimum sizes for arrays, strings, tuples
    /// - Current recursion depth
    /// - Whether to filter structs to only entrypoint-valid ones
    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        structs: &[Struct],
    ) -> Self {
        if ctx.depth > config.max_type_depth {
            return Type::Field(Field::random(random, config));
        }

        // Filter structs if needed for entrypoint context
        let valid_structs: Vec<&Struct> = if ctx.filter_entrypoint_structs {
            structs.iter().filter(|s| s.is_entrypoint_valid()).collect()
        } else {
            structs.iter().collect()
        };

        // Build available types based on context
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
            "Field" => Type::Field(Field::random(random, config)),
            "Integer" => Type::Integer(Integer::random(random, config)),
            "Boolean" => Type::Boolean(Boolean::random(random, config)),
            "String" => Type::String(StringLiteral::random(random, config, ctx)),
            "Array" => Type::Array(Array::random(random, config, ctx, structs)),
            "Slice" => Type::Slice(Slice::random(random, config, ctx, structs)),
            "Tuple" => Type::Tuple(Tuple::random(random, config, ctx, structs)),
            "Struct" => Type::Struct((*random.choice(&valid_structs)).clone()),
            "Reference" => Type::Reference(Reference::random(random, config, ctx, structs)),
            _ => unreachable!(),
        }
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        match self {
            Type::Field(f) => f.random_value(random, config),
            Type::Integer(i) => i.random_value(random, config),
            Type::Boolean(b) => b.random_value(random),
            Type::String(s) => s.random_value(random, config),
            Type::Array(a) => a.random_value(random, config),
            Type::Slice(s) => s.random_value(random, config),
            Type::Tuple(t) => t.random_value(random, config),
            Type::Reference(r) => r.random_value(random, config),
            Type::Struct(s) => s.random_value(random, config),
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Type::Field(f) => f.mutable,
            Type::Integer(i) => i.mutable,
            Type::Boolean(b) => b.mutable,
            Type::String(s) => s.mutable,
            Type::Array(a) => a.mutable,
            Type::Slice(s) => s.mutable,
            Type::Tuple(t) => t.mutable,
            Type::Reference(r) => r.r#type.is_mutable(),
            Type::Struct(s) => s.mutable,
        }
    }

    /// Check if this type is valid for entry points (main, test, contract, fuzz functions).
    /// Invalid types: slices, references, empty arrays, empty strings, or any type containing them.
    pub fn is_entrypoint_valid(&self) -> bool {
        match self {
            Type::Field(_) | Type::Integer(_) | Type::Boolean(_) => true,
            Type::String(s) => s.size > 0,
            Type::Array(a) => a.size > 0 && a.r#type.is_entrypoint_valid(),
            Type::Slice(_) => false,
            Type::Tuple(t) => !t.inner.is_empty() && t.inner.iter().all(|t| t.is_entrypoint_valid()),
            Type::Reference(_) => false,
            Type::Struct(s) => s.is_entrypoint_valid(),
        }
    }
}

impl Field {
    const METHODS: [&str; 11] = [
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

    pub fn random(random: &mut impl Rng, config: &Config) -> Self {
        let mutable = random.random_bool(config.mutable_probability);
        Self { mutable }
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        let exclude_prime = random.random_bool(config.exclude_prime_probability);
        let value = random_field_element(
            "bn254",
            random,
            exclude_prime,
            config.boundary_value_probability,
            config.small_upper_bound_probability,
            config.small_upper_bound,
        );
        value.to_string()
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

impl Integer {
    const METHODS: [&str; 3] = ["wrapping_add", "wrapping_sub", "wrapping_mul"];
    const PARAMS: [(u8, bool); 10] = [
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

    fn bounds(bits: u8, signed: bool) -> (i128, u128) {
        if signed {
            let val = 1i128 << (bits - 1);
            (-val, (val - 1) as u128)
        } else if bits == 128 {
            (0, u128::MAX)
        } else {
            (0, (1u128 << bits) - 1)
        }
    }

    pub fn random(random: &mut impl Rng, config: &Config) -> Self {
        let (bits, signed) = *random.choice(&Self::PARAMS);
        let mutable = random.random_bool(config.mutable_probability);
        Self { bits, signed, mutable }
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        let (min, max) = Self::bounds(self.bits, self.signed);

        if max == 0 {
            return if random.random_bool(0.5) { "1".to_string() } else { "0".to_string() };
        }

        if self.signed {
            let val = if bernoulli(config.boundary_value_probability, random) {
                *random.choice(&[0, 1, -1, min, max as i128])
            } else {
                random.random_range(min..=max as i128)
            };
            if val < 0 {
                format!("-{}", val.unsigned_abs())
            } else {
                format!("{}", val)
            }
        } else {
            let val = if bernoulli(config.boundary_value_probability, random) {
                *random.choice(&[0, 1, max])
            } else {
                random.random_range(0..=max)
            };
            format!("{}", val)
        }
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

impl Boolean {
    pub fn random(random: &mut impl Rng, config: &Config) -> Self {
        let mutable = random.random_bool(config.mutable_probability);
        Self { mutable }
    }

    pub fn random_value(&self, random: &mut impl Rng) -> String {
        format!("{}", random.random_bool(0.5))
    }
}

impl StringLiteral {
    const METHODS: [&str; 1] = ["as_bytes"];

    pub fn random(random: &mut impl Rng, config: &Config, ctx: &TypeContext) -> Self {
        let size = random.random_range(ctx.min_string_size..config.max_string_size);
        let raw_hashes = if random.random_bool(config.raw_string_probability) {
            Some(random.random_range(0..config.max_raw_string_hashes_count))
        } else {
            None
        };
        let mutable = random.random_bool(config.mutable_probability);
        Self { size, raw_hashes, mutable }
    }

    pub fn random_value(&self, random: &mut impl Rng, _config: &Config) -> String {
        let value = random_string(random, self.size);
        match self.raw_hashes {
            Some(n) => {
                let hashes = "#".repeat(n);
                format!("r{0}\"{1}\"{0}", hashes, value)
            },
            None => format!("{:?}", value),
        }
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

impl Array {
    const METHODS: [&str; 15] = [
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

    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        structs: &[Struct],
    ) -> Self {
        let inner_ctx = ctx.inner();
        let r#type = Type::random(random, config, &inner_ctx, structs);
        let size = random.random_range(ctx.min_array_size..config.max_element_count);
        let mutable = random.random_bool(config.mutable_probability);
        Self { r#type: Box::new(r#type), size, mutable }
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        let elems: Vec<_> =
            (0..self.size).map(|_| self.r#type.random_value(random, config)).collect();
        format!("[{}]", elems.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

impl Slice {
    const METHODS: [&str; 24] = [
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

    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        structs: &[Struct],
    ) -> Self {
        let inner_ctx = ctx.inner();
        let r#type = Type::random(random, config, &inner_ctx, structs);
        let mutable = random.random_bool(config.mutable_probability);
        Self { r#type: Box::new(r#type), mutable }
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        let size = random.random_range(0..config.max_element_count);
        let elems: Vec<_> = (0..size).map(|_| self.r#type.random_value(random, config)).collect();
        format!("&[{}]", elems.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

impl Tuple {
    const METHODS: [&str; 2] = ["cmp", "eq"];

    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        structs: &[Struct],
    ) -> Self {
        let inner_ctx = ctx.inner();
        let size = random.random_range(ctx.min_tuple_size..config.max_element_count);
        let inner =
            (0..size).map(|_| Type::random(random, config, &inner_ctx, structs)).collect();
        let mutable = random.random_bool(config.mutable_probability);
        Self { inner, mutable }
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        let elems: Vec<_> = self.inner.iter().map(|t| t.random_value(random, config)).collect();
        format!("({})", elems.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

impl Reference {
    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        structs: &[Struct],
    ) -> Self {
        let inner_ctx = ctx.inner();
        let r#type = Type::random(random, config, &inner_ctx, structs);
        Self { r#type: Box::new(r#type) }
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        format!("&{}", self.r#type.random_value(random, config))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Field(_) => write!(f, "Field"),
            Type::Integer(i) => write!(f, "{}{}", if i.signed { "i" } else { "u" }, i.bits),
            Type::Boolean(_) => write!(f, "bool"),
            Type::String(s) => write!(f, "str<{}>", s.size),
            Type::Array(a) => write!(f, "[{}; {}]", a.r#type, a.size),
            Type::Slice(s) => write!(f, "[{}]", s.r#type),
            Type::Tuple(t) => {
                let types: Vec<_> = t.inner.iter().map(|e| e.to_string()).collect();
                write!(f, "({})", types.join(", "))
            },
            Type::Struct(s) => write!(f, "{}", s.name),
            Type::Reference(r) => write!(f, "&{}", r.r#type),
        }
    }
}
