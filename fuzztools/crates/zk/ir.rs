use super::config::Config;
use crate::{
    math::{bernoulli, random_field_element},
    utils::{random_string, RandomChoice},
};
use alloy::primitives::U256;
use rand::Rng;
use std::fmt;

// ============================================================================
// Type Definitions
// ============================================================================

#[derive(Clone)]
pub enum Type {
    Field(Field),
    Integer(Integer),
    Boolean(Boolean),
    String(StringLiteral),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Struct(Struct),
    Reference(Reference),
}

#[derive(Clone)]
pub struct Field {
    pub inner: U256,
}

#[derive(Clone)]
pub struct Integer {
    pub inner: U256,
    pub bits: u8,
    pub signed: bool,
    pub is_negative: bool,
}

#[derive(Clone)]
pub struct Boolean {
    pub inner: bool,
}

#[derive(Clone)]
pub struct StringLiteral {
    pub inner: String,
    pub size: usize,
    pub raw_hashes: Option<usize>,
    // @audit maybe add a flag for escape sequences or unicode characters
}

#[derive(Clone)]
pub struct Array {
    pub r#type: Box<Type>,
    pub size: usize,
    pub inner: Vec<Type>,
}

#[derive(Clone)]
pub struct Slice {
    pub r#type: Box<Type>,
    pub inner: Vec<Type>,
}

#[derive(Clone)]
pub struct Tuple {
    pub inner: Vec<Type>,
}

#[derive(Clone, Copy)]
pub enum Visibility {
    Private,
    PubCrate,
    Public,
}

#[derive(Clone)]
pub struct StructField {
    pub name: String,
    pub r#type: Box<Type>,
    pub visibility: Visibility,
}

#[derive(Clone)]
pub struct Struct {
    pub name: String,
    pub visibility: Visibility,
    pub inner: Vec<StructField>,
}

#[derive(Clone)]
pub struct Reference {
    pub r#type: Box<Type>,
    pub mutable: bool,
}

// ----------------------------------------------------------------------------
// Type Context - Controls which types can be generated
// ----------------------------------------------------------------------------

/// Types allowed at top level (can include Slice)
pub const TOP_TYPES: [&'static str; 7] = [
    "Field",
    "Integer",
    "Boolean",
    "String",
    "Array",
    "Slice",
    "Tuple",
];

/// Types allowed inside arrays/slices (no nested slices, no random structs)
pub const INNER_TYPES: [&'static str; 5] = [
    "Field",
    "Integer",
    "Boolean",
    "String",
    "Array",
];


// ============================================================================
// Type Implementation
// ============================================================================

impl Type {
    /// Generate a random type.
    /// - When `inner=true`, uses INNER_TYPES (no slices, no structs) to prevent nested slices
    /// - When a template is provided, generates a value matching that type exactly
    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        depth: u32,
        template: Option<&Self>,
        structs: &[Struct],
    ) -> Self {
        Self::random_impl(random, config, depth, template, structs, false)
    }

    /// Generate a random type suitable for array/slice elements (no nested slices)
    pub fn random_inner(
        random: &mut impl Rng,
        config: &Config,
        depth: u32,
        template: Option<&Self>,
        structs: &[Struct],
    ) -> Self {
        Self::random_impl(random, config, depth, template, structs, true)
    }

    fn random_impl(
        random: &mut impl Rng,
        config: &Config,
        depth: u32,
        template: Option<&Self>,
        structs: &[Struct],
        inner: bool,
    ) -> Self {
        if depth > config.max_type_depth {
            return Type::Field(Field::random(random, config));
        }

        if let Some(t) = template {
            // When a template is provided, generate a value matching EXACTLY that type
            match t {
                Type::Field(_) => Type::Field(Field::random(random, config)),
                Type::Boolean(_) => Type::Boolean(Boolean::random(random)),
                Type::Integer(i) => Type::Integer(Integer::random(random, config, Some((i.bits, i.signed)))),
                Type::String(s) => Type::String(StringLiteral::random(random, config, Some(s.size))),
                Type::Array(a) => Type::Array(Array::from_template(random, config, depth, a)),
                Type::Slice(s) => Type::Slice(Slice::from_template(random, config, depth, s)),
                Type::Tuple(t) => Type::Tuple(Tuple::from_template(random, config, depth, t)),
                Type::Struct(s) => Type::Struct(Struct::from_template(random, config, s)),
                Type::Reference(r) => Type::Reference(Reference::from_template(random, config, depth, r)),
            }
        } else {
            // Choose type list based on context (inner types exclude slices to prevent nesting)
            let types = if inner { &INNER_TYPES[..] } else { &TOP_TYPES[..] };
            match *random.choice(types) {
                "Field" => Type::Field(Field::random(random, config)),
                "Integer" => Type::Integer(Integer::random(random, config, None)),
                "Boolean" => Type::Boolean(Boolean::random(random)),
                "String" => Type::String(StringLiteral::random(random, config, None)),
                "Array" => Type::Array(Array::random(random, config, depth, structs, None)),
                "Slice" => Type::Slice(Slice::random(random, config, depth, structs)),
                "Tuple" => Type::Tuple(Tuple::random(random, config, depth, structs)),
                _ => Type::Field(Field::random(random, config)), // Fallback
            }
        }
    }

    pub fn value_literal(&self, random: &mut impl Rng) -> String {
        match self {
            Type::Field(f) => f.value_literal(),
            Type::Integer(i) => i.value_literal(),
            Type::Boolean(b) => b.value_literal(),
            Type::String(s) => s.value_literal(),
            Type::Array(a) => a.value_literal(random),
            Type::Slice(s) => s.value_literal(random),
            Type::Tuple(t) => t.value_literal(random),
            Type::Struct(s) => s.value_literal(random),
            Type::Reference(r) => r.value_literal(random),
        }
    }
}

// ============================================================================
// Field
// ============================================================================

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
        let exclude_prime = random.random_bool(config.prime_exclusion_probability);
        let inner = random_field_element(
            "bn254",
            random,
            exclude_prime,
            config.boundary_value_probability,
            config.small_upper_bound_probability,
            config.small_upper_bound,
        );
        Self { inner }
    }

    pub fn value_literal(&self) -> String {
        self.inner.to_string()
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

// ============================================================================
// Integer
// ============================================================================

impl Integer {
    const METHODS: [&str; 3] = ["wrapping_add", "wrapping_sub", "wrapping_mul"];
    const PARAMS: [(u8, bool); 10] = [
        (1, false), (8, false), (16, false), (32, false), (64, false), (128, false),
        (8, true), (16, true), (32, true), (64, true),
    ];

    // @todo test this for all types
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

    pub fn random(random: &mut impl Rng, config: &Config, params: Option<(u8, bool)>) -> Self {
        let (bits, signed) = params.unwrap_or_else(|| *random.choice(&Self::PARAMS));
        let (min, max) = Self::bounds(bits, signed);
        
        // Handle `u1` case
        if max == 0 {
            let inner = U256::from(random.random_bool(0.5));
            return Self { inner, bits, signed, is_negative: false };
        }

        let (inner, is_negative) = if signed {
            let val = if bernoulli(config.boundary_value_probability, random) {
                *random.choice(&[0, 1, -1, min, max as i128])
            } else {
                random.random_range(min..=max as i128)
            };
            (U256::from(val.unsigned_abs()), val < 0)
        } else {
            let val = if bernoulli(config.boundary_value_probability, random) {
                *random.choice(&[0, 1, max])
            } else {
                random.random_range(0..=max)
            };
            (U256::from(val), false)
        };

        Self { inner, bits, signed, is_negative }
    }

    pub fn value_literal(&self) -> String {
        if self.is_negative {
            format!("-{}", self.inner)
        } else {
            format!("{}", self.inner)
        }
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

// ============================================================================
// Boolean
// ============================================================================

impl Boolean {
    pub fn random(random: &mut impl Rng) -> Self {
        Self { inner: random.random_bool(0.5) }
    }

    pub fn value_literal(&self) -> String {
        format!("{}", self.inner)
    }
}

// ============================================================================
// StringLiteral
// ============================================================================

impl StringLiteral {
    const METHODS: [&str; 1] = ["as_bytes"];

    pub fn random(random: &mut impl Rng, config: &Config, params: Option<usize>) -> Self {
        let size = params.unwrap_or_else(|| random.random_range(0..config.max_string_size) as usize);
        let inner = random_string(random, size);
        let raw_hashes = if random.random_bool(config.raw_string_probability) {
            Some(random.random_range(0..config.max_hash_count) as usize)
        } else {
            None
        };
        Self { inner, size, raw_hashes }
    }

    pub fn value_literal(&self) -> String {
        match self.raw_hashes {
            Some(n) => {
                let hashes = "#".repeat(n);
                format!("r{0}\"{1}\"{0}", hashes, self.inner)
            },
            None => format!("{:?}", self.inner),
        }
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

// ============================================================================
// Array
// ============================================================================

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
        depth: u32,
        structs: &[Struct],
        params: Option<usize>,
    ) -> Self {
        // Use random_inner to prevent nested slices
        let r#type = Type::random_inner(random, config, depth + 1, None, structs);
        let size = params.unwrap_or_else(|| random.random_range(0..config.max_element_count) as usize);
        let inner = (0..size)
            .map(|_| Type::random_inner(random, config, depth + 1, Some(&r#type), structs))
            .collect();
        Self { r#type: Box::new(r#type), size, inner }
    }

    /// Create an array matching the template's type exactly
    pub fn from_template(random: &mut impl Rng, config: &Config, depth: u32, template: &Array) -> Self {
        let inner = (0..template.size)
            .map(|_| Type::random_inner(random, config, depth + 1, Some(&template.r#type), &[]))
            .collect();
        Self { r#type: template.r#type.clone(), size: template.size, inner }
    }

    pub fn value_literal(&self, random: &mut impl Rng) -> String {
        let elems: Vec<_> = self.inner.iter().map(|t| t.value_literal(random)).collect();
        format!("[{}]", elems.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

// ============================================================================
// Slice
// ============================================================================

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
        depth: u32,
        structs: &[Struct],
    ) -> Self {
        // Use random_inner to prevent nested slices
        let r#type = Type::random_inner(random, config, depth + 1, None, structs);
        let size = random.random_range(0..config.max_element_count) as usize;
        let inner = (0..size)
            .map(|_| Type::random_inner(random, config, depth + 1, Some(&r#type), structs))
            .collect();
        Self { r#type: Box::new(r#type), inner }
    }

    /// Create a slice matching the template's type exactly
    pub fn from_template(random: &mut impl Rng, config: &Config, depth: u32, template: &Slice) -> Self {
        let size = random.random_range(0..config.max_element_count) as usize;
        let inner = (0..size)
            .map(|_| Type::random_inner(random, config, depth + 1, Some(&template.r#type), &[]))
            .collect();
        Self { r#type: template.r#type.clone(), inner }
    }

    pub fn value_literal(&self, random: &mut impl Rng) -> String {
        let elems: Vec<_> = self.inner.iter().map(|t| t.value_literal(random)).collect();
        format!("&[{}]", elems.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

// ============================================================================
// Tuple
// ============================================================================

impl Tuple {
    const METHODS: [&str; 2] = ["cmp", "eq"];

    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        depth: u32,
        structs: &[Struct],
    ) -> Self {
        let size = random.random_range(1..config.max_element_count) as usize;
        // Use random_inner to avoid undefined struct types and nested slices in tuples
        let inner = (0..size)
            .map(|_| Type::random_inner(random, config, depth + 1, None, structs))
            .collect();
        Self { inner }
    }

    /// Create a tuple matching the template's element types exactly
    pub fn from_template(random: &mut impl Rng, config: &Config, depth: u32, template: &Tuple) -> Self {
        let inner = template.inner.iter()
            .map(|elem_type| Type::random_inner(random, config, depth + 1, Some(elem_type), &[]))
            .collect();
        Self { inner }
    }

    pub fn value_literal(&self, random: &mut impl Rng) -> String {
        let elems: Vec<_> = self.inner.iter().map(|t| t.value_literal(random)).collect();
        format!("({})", elems.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

// ============================================================================
// Visibility
// ============================================================================

impl Visibility {
    const ALL: [Self; 3] = [Self::Private, Self::PubCrate, Self::Public];

    pub fn random(random: &mut impl Rng) -> Self {
        *random.choice(&Self::ALL)
    }
}

// ============================================================================
// Struct
// ============================================================================

impl StructField {
    pub fn random(random: &mut impl Rng, config: &Config, depth: u32, structs: &[Struct]) -> Self {
        let name = random_string(random, config.max_string_size as usize);
        let visibility = Visibility::random(random);
        // Use random_inner to avoid undefined struct types in fields
        let r#type = Type::random_inner(random, config, depth + 1, None, structs);
        Self { name, r#type: Box::new(r#type), visibility }
    }

    pub fn value_literal(&self, random: &mut impl Rng) -> String {
        format!("{} {}: {}", self.visibility, self.name, self.r#type.value_literal(random))
    }
}

impl Struct {
    const METHODS: [&str; 2] = ["cmp", "eq"];

    pub fn random(random: &mut impl Rng, config: &Config, depth: u32, structs: &[Struct]) -> Self {
        let name = random_string(random, config.max_string_size as usize);
        let visibility = Visibility::random(random);
        let size = random.random_range(0..config.max_element_count) as usize;
        let inner = (0..size)
            .map(|_| StructField::random(random, config, depth + 1, structs))
            .collect();
        Self { name, visibility, inner }
    }

    /// Create a struct instance matching the template (same name, same fields, new values)
    pub fn from_template(random: &mut impl Rng, config: &Config, template: &Struct) -> Self {
        let inner = template.inner.iter()
            .map(|f| StructField {
                name: f.name.clone(),
                visibility: f.visibility,
                r#type: Box::new(Type::random(random, config, 0, Some(&f.r#type), &[])),
            })
            .collect();
        Self { name: template.name.clone(), visibility: template.visibility, inner }
    }

    /// Generates the struct type definition (e.g., `struct Foo { x: Field, y: bool }`)
    pub fn type_definition(&self) -> String {
        let fields: Vec<_> = self
            .inner
            .iter()
            .map(|f| format!("    {}{}: {},", f.visibility, f.name, f.r#type))
            .collect();
        format!("struct {} {{\n{}\n}}", self.name, fields.join("\n"))
    }

    /// Generates a struct instantiation expression (e.g., `Foo { x: 123, y: true }`)
    pub fn value_literal(&self, random: &mut impl Rng) -> String {
        let fields: Vec<_> = self
            .inner
            .iter()
            .map(|f| format!("{}: {}", f.name, f.r#type.value_literal(random)))
            .collect();
        format!("{} {{ {} }}", self.name, fields.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }
}

// ============================================================================
// Reference
// ============================================================================

impl Reference {
    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        depth: u32,
        structs: &[Struct],
    ) -> Self {
        // Use random_inner to avoid undefined struct types
        let r#type = Type::random_inner(random, config, depth + 1, None, structs);
        Self { r#type: Box::new(r#type), mutable: random.random_bool(0.5) }
    }

    /// Create a reference matching the template's type exactly
    pub fn from_template(random: &mut impl Rng, config: &Config, depth: u32, template: &Reference) -> Self {
        let r#type = Type::random_inner(random, config, depth + 1, Some(&template.r#type), &[]);
        Self { r#type: Box::new(r#type), mutable: template.mutable }
    }

    pub fn value_literal(&self, random: &mut impl Rng) -> String {
        let prefix = if self.mutable { "&mut " } else { "&" };
        format!("{}{}", prefix, self.r#type.value_literal(random))
    }
}

// ============================================================================
// Display Implementations
// ============================================================================

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
            Type::Reference(r) => {
                if r.mutable {
                    write!(f, "&mut {}", r.r#type)
                } else {
                    write!(f, "&{}", r.r#type)
                }
            },
        }
    }
}


impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Visibility::Private => Ok(()),
            Visibility::PubCrate => write!(f, "pub(crate) "),
            Visibility::Public => write!(f, "pub "),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn single_run() {
    let mut random = rand::rng();
    let config = Config {
        max_element_count: 10,
        max_string_size: 10,
        max_hash_count: 5,
        max_type_depth: 3,
        raw_string_probability: 0.3,
        ..Config::default()
    };

    let struct_count = random.random_range(1..config.max_element_count) as usize;
    let mut structs = Vec::new();
    for _ in 0..struct_count {
        structs.push(Struct::random(&mut random, &config, 0, &structs));
    }

    let mut code = String::new();

    for def in &structs {
        code.push_str(&def.type_definition());
        code.push_str("\n\n");
    }

    code.push_str("fn main() {\n");

    let stmt_count = random.random_range(5..20) as usize;
    for _ in 0..stmt_count {
        let t = Type::random(&mut random, &config, 0, None, &structs);
        code.push_str(&format!(
            "    let {}: {} = {};\n",
            random_string(&mut random, 10),
            t,
            t.value_literal(&mut random)
        ));
    }

    code.push_str("}\n");

    println!("{}", code);
}

#[test]
fn test_types() {
    use std::{fs, path::Path, process::Command};

    let mut random = rand::rng();
    let config = Config {
        max_element_count: 10,
        max_string_size: 10,
        max_hash_count: 5,
        max_type_depth: 3,
        raw_string_probability: 0.3,
        ..Config::default()
    };

    let temp_dir = Path::new("temp_noir_test");

    if temp_dir.exists() {
        fs::remove_dir_all(temp_dir).expect("Failed to remove existing temp directory");
    }

    let output = Command::new("nargo")
        .args(["new", "temp_noir_test"])
        .output()
        .expect("Failed to execute nargo new");

    if !output.status.success() {
        panic!("nargo new failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    let src_file = temp_dir.join("src").join("main.nr");
    let (mut successes, mut failures) = (0, 0);

    for iteration in 0..500 {
        let struct_count = random.random_range(1..config.max_element_count) as usize;
        let mut structs = Vec::new();
        for _ in 0..struct_count {
            structs.push(Struct::random(&mut random, &config, 0, &structs));
        }

        let mut code = String::new();

        for def in &structs {
            code.push_str(&def.type_definition());
            code.push_str("\n\n");
        }

        code.push_str("fn main() {\n");

        let stmt_count = random.random_range(5..50) as usize;
        for _ in 0..stmt_count {
            let t = Type::random(&mut random, &config, 0, None, &structs);
            code.push_str(&format!(
                "    let {}: {} = {};\n",
                random_string(&mut random, 10),
                t,
                t.value_literal(&mut random)
            ));
        }

        code.push_str("}\n");

        fs::write(&src_file, &code).expect("Failed to write main.nr");

        let output = Command::new("nargo")
            .args(["compile"])
            .current_dir(temp_dir)
            .output()
            .expect("Failed to execute nargo compile");

        if output.status.success() {
            successes += 1;
            println!("Iteration {}: OK", iteration + 1);
        } else {
            failures += 1;
            println!("Iteration {}: FAILED", iteration + 1);
            println!("Error:\n{}", String::from_utf8_lossy(&output.stderr));
            println!("Generated code:\n{}", code);
            println!("---");
        }
    }

    fs::remove_dir_all(temp_dir).expect("Failed to remove temp directory");

    println!("\n=== Summary ===");
    println!("Successes: {successes}");
    println!("Failures: {failures}");

    if failures > 0 {
        panic!("{failures} out of 500 iterations failed to compile");
    }
}
