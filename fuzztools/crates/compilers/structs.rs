use super::{
    config::Config,
    types::{Type, TypeContext},
    Visibility,
};
use crate::utils::{random_string, RandomChoice};
use rand::Rng;

#[derive(Clone, Debug)]
pub struct StructField {
    pub name: String,
    pub r#type: Box<Type>,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub visibility: Visibility,
    pub fields: Vec<StructField>,
    pub mutable: bool,
}

impl StructField {
    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        structs: &[Struct],
    ) -> Self {
        let name = random_string(random, config.max_string_size);
        let visibility = Visibility::random(random);
        let inner_ctx = ctx.inner();
        let r#type = Type::random(random, config, &inner_ctx, structs);
        Self { name, r#type: Box::new(r#type), visibility }
    }
}

impl Struct {
    const METHODS: [&str; 2] = ["cmp", "eq"];

    pub fn random(
        random: &mut impl Rng,
        config: &Config,
        ctx: &TypeContext,
        structs: &[Struct],
    ) -> Self {
        let name = random_string(random, config.max_string_size);
        let visibility = Visibility::random(random);
        let size = random.random_range(ctx.min_struct_fields..config.max_element_count);
        let inner_ctx = ctx.inner();
        let fields =
            (0..size).map(|_| StructField::random(random, config, &inner_ctx, structs)).collect();
        let mutable = random.random_bool(config.mutable_probability);
        Self { name, visibility, fields, mutable }
    }

    pub fn from_template(random: &mut impl Rng, config: &Config, template: &Struct) -> Self {
        let fields = template
            .fields
            .iter()
            .map(|f| StructField {
                name: f.name.clone(),
                visibility: f.visibility,
                r#type: f.r#type.clone(),
            })
            .collect();
        let mutable = random.random_bool(config.mutable_probability);
        Self { name: template.name.clone(), visibility: template.visibility, fields, mutable }
    }

    pub fn type_definition(&self) -> String {
        let field_strs: Vec<_> = self
            .fields
            .iter()
            .map(|f| format!("    {}{}: {},", f.visibility, f.name, f.r#type))
            .collect();
        format!("struct {} {{\n{}\n}}", self.name, field_strs.join("\n"))
    }

    pub fn random_value(&self, random: &mut impl Rng, config: &Config) -> String {
        let field_strs: Vec<_> = self
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, f.r#type.random_value(random, config)))
            .collect();
        format!("{} {{ {} }}", self.name, field_strs.join(", "))
    }

    pub fn random_method(random: &mut impl Rng) -> &'static str {
        *random.choice(&Self::METHODS)
    }

    /// Check if this struct is valid for entry points.
    /// A struct is valid if all fields are entrypoint-valid.
    pub fn is_entrypoint_valid(&self) -> bool {
        self.fields.iter().all(|f| f.r#type.is_entrypoint_valid())
    }
}
