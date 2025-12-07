pub mod types;
pub mod functions;
pub mod config;
// pub mod expression;
pub mod structs;
pub mod globals;
pub mod inputs;
pub mod rules;

use crate::{
    compilers::{
        config::Config,
        functions::Function,
        globals::Global,
        structs::{Struct, StructField},
        types::TypeContext,
    },
    utils::RandomChoice,
};
// use expression::Expression;
use inputs::Input;
use rand::Rng;
use std::fmt;

/*
{
    "operators": {
        "relation_operators" : ["<", ">", "<=", ">=", "==", "!="],
        "unary_operators" : ["!"],
        "binary_operators" : ["+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>"],
        "assign_operators" : ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="]
    },
}
*/

// @audit type alias exist by default
// @audit enum are available but under -Zenum the same as -Zownership

#[derive(Clone, Copy, Debug)]
pub enum Visibility {
    Private,
    PubCrate,
    Public,
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

impl Visibility {
    const ALL: [Self; 3] = [Self::Private, Self::PubCrate, Self::Public];

    pub fn random(random: &mut impl Rng) -> Self {
        *random.choice(&Self::ALL)
    }
}
    
pub struct Program {
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
    pub globals: Vec<Global>,
    pub inputs: Vec<Input>,
    // pub expressions: Vec<Expression>,
}

impl Program {
    pub fn random(random: &mut impl Rng, config: &Config) -> String {
        let structs_count = random.random_range(0..config.max_structs_count);
        let globals_count = random.random_range(0..config.max_globals_count);
        let functions_count = random.random_range(0..config.max_functions_count);
        let inputs_count = random.random_range(0..config.max_inputs_count);
        let main_expressions_count = random.random_range(0..config.max_main_expressions_count);

        let mut code = String::new();

        let mut structs = Vec::new();
        let mut globals = Vec::new();
        let mut functions = Vec::new();
        let mut inputs = Vec::new();
        // let mut expressions = Vec::new();
        // let mut local_variables = Vec::new();

        // Use top-level context for structs (allows all types)
        let top_ctx = TypeContext::top_level();
        for _ in 1..structs_count {
            structs.push(Struct::random(random, config, &top_ctx, &structs));
            code.push_str(&structs.last().unwrap().type_definition());
            code.push_str("\n\n");
        }

        for _ in 1..globals_count {
            globals.push(Global::random(random, config, &structs));
            code.push_str(&globals.last().unwrap().to_string());
            code.push_str(format!(" = {};", globals.last().unwrap().r#type.random_value(random, config)).as_str());
            code.push_str("\n\n");
        }

        for _ in 1..functions_count {
            functions.push(Function::random(random, config, &top_ctx, &functions, &structs, &globals));
            code.push_str(&functions.last().unwrap().type_definition());
            code.push_str("\n\n");
        }

        for _ in 1..inputs_count {
            inputs.push(Input::random(random, config, &structs));
        }

        /*for _ in 1..main_expressions_count {
            expressions.push(Expression::random(
                random,
                config,
                0,
                &mut functions,
                &structs,
                &globals,
                &inputs,
                &mut local_variables,
            ));
        }*/

        code.push_str(&format!(
            "fn main({}) {{\n\n}}",
            inputs.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(", "),
            // expressions.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n")
        ));

        code
    }
}
