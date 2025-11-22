use super::config::Config;
use crate::{
    utils::{random_id, random_name},
    zk::ir::IRNode,
};
use rand::Rng;

pub struct Circuit {
    pub name: String,
    pub inputs: Vec<String>,
    pub body: Vec<IRNode>,
    pub constraints: Vec<IRNode>,
}

impl Circuit {
    pub fn new(random: &mut impl Rng, config: &Config) -> Self {
        let name = format!("circuit_{}", random_id(random, 32));
        let input_count = random.random_range(0..config.max_input_count);

        let inputs: Vec<String> = (0..input_count).map(|i| format!("in{}", i)).collect();

        Self { name, inputs, body: Vec::new(), constraints: Vec::new() }
    }

    pub fn run(&mut self, random: &mut impl Rng, config: &Config) {
        let mut intermediate_nodes: Vec<IRNode> =
            Vec::with_capacity(config.max_node_count as usize);
        let mut depth = 0;

        for _ in 0..config.max_node_count {}
    }
    // fn create_node(&mut self, random: &mut impl Rng, config: &Config, depth: u64) -> IRNode {
    // if depth >= config.max_expression_depth {
    // return IRNode::Integer { value: U256::from(random.random_range(0..config.max_value)) };
    // }
    //
    //
    //
    // }
}
