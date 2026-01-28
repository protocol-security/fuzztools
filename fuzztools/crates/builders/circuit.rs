use crate::circuits::{
    context::Context,
    generators::types::TypeLocation,
    ir::{Forest, Struct, Type},
};
use rand::Rng;

#[derive(Default)]
pub struct CircuitBuilder {}

pub struct Circuit {
    pub forest: Forest,
    pub inputs: Vec<(String, Type)>,
    pub ret: Option<Type>,
}

impl CircuitBuilder {
    pub fn generate(&self, random: &mut impl Rng, ctx: &Context) -> Circuit {
        let mut structs = Vec::new();
        let mut forest = Forest::default();
        let mut inputs = Vec::new();

        for i in 0..random.random_range(ctx.min_struct_count..=ctx.max_struct_count) {
            let name = format!("struct{i}");
            structs.push(Struct::random(random, ctx, &structs, name));
        }

        for i in 0..random.random_range(ctx.min_input_count..ctx.max_input_count) {
            let name = format!("input{i}");
            let ty = Type::random(random, ctx, &structs, TypeLocation::Main);

            inputs.push((name.clone(), ty.clone()));
            forest.input(name, &ty);
        }

        // Decide if we have a return type
        let ret = if random.random_bool(ctx.main_return_probability) {
            Some(Type::random(random, ctx, &structs, TypeLocation::Main))
        } else {
            None
        };

        forest.random(random, ctx, &structs, ret.as_ref());

        Circuit { forest, inputs, ret }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn test_random_forest() {
        let ctx: &Context =
            &serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let mut random = rand::rng();
        let builder = CircuitBuilder::default();
        let circuit = builder.generate(&mut random, ctx);

        let inputs = circuit
            .inputs
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty))
            .collect::<Vec<String>>()
            .join(", ");

        let ret_ty = circuit.ret.as_ref().map(|ty| format!(" -> pub {ty}")).unwrap_or_default();

        println!("fn main({inputs}){ret_ty} {{\n{}}}", circuit.forest.format("    "));

        circuit
            .forest
            .save_as_dot(&std::env::current_dir().unwrap().join("test_random_forest.dot"))
            .unwrap();
    }
}
