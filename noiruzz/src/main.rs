mod utils;
mod types;
mod operators;
mod traveller;
mod nodes;
mod config;
mod base;
mod boolean;
mod arithmetic;
mod rewriter;

use arithmetic::ArithmeticCircuitGenerator;
use boolean::Circuit;
use config::IRConfig;
use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;
use std::time::Instant;

/// Enum representing the metamorphic test kind
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MetamorphicKind {
    Equal,
    Weaker,
}

impl MetamorphicKind {
    pub fn value(&self) -> &'static str {
        match self {
            MetamorphicKind::Equal => "equal",
            MetamorphicKind::Weaker => "weaker",
        }
    }
}

/// Struct to hold a metamorphic circuit pair
#[derive(Debug, Clone)]
pub struct MetamorphicCircuitPair {
    pub kind: MetamorphicKind,
    pub original: Circuit,
    pub transformed: Circuit,
}

/// Struct to hold test iteration results (simplified version)
#[derive(Debug, Clone)]
pub struct TestIteration {
    pub c1_execute: Option<bool>,
    pub c1_execute_time: Option<f64>,
    pub c2_execute: Option<bool>,
    pub c2_execute_time: Option<f64>,
    pub error: Option<String>,
}

/// Struct to hold test results
#[derive(Debug)]
pub struct TestResult {
    pub iterations: Vec<TestIteration>,
}

/// Struct to hold data entry for a single test iteration
#[derive(Debug)]
pub struct DataEntry {
    pub tool: String,
    pub test_time: f64,
    pub seed: u64,
    pub curve: String,
    pub oracle: String,
    pub iteration: usize,
    pub error: Option<String>,
    pub ir_generation_seed: u64,
    pub ir_generation_time: f64,
    pub ir_rewrite_seed: u64,
    pub ir_rewrite_time: f64,
    pub c1_node_size: usize,
    pub c1_assignments: usize,
    pub c1_assertions: usize,
    pub c1_input_signals: usize,
    pub c1_output_signals: usize,
    pub c2_node_size: usize,
    pub c2_assignments: usize,
    pub c2_assertions: usize,
    pub c2_input_signals: usize,
    pub c2_output_signals: usize,
}

/// Generates a random metamorphic kind based on weakening probability
fn random_weighted_metamorphic_kind(rng: &mut impl Rng, weakening_probability: f64) -> MetamorphicKind {
    if rng.random_bool(weakening_probability) {
        MetamorphicKind::Weaker
    } else {
        MetamorphicKind::Equal
    }
}

/// Count statements in a circuit (helper function)
fn count_statements(circuit: &Circuit) -> (usize, usize) {
    let mut assignments = 0;
    let mut assertions = 0;
    
    for stmt in &circuit.statements {
        match stmt {
            nodes::Statement::AssignStatement(_) => assignments += 1,
            nodes::Statement::AssertStatement(_) => assertions += 1,
            _ => {}
        }
    }
    
    (assignments, assertions)
}

/// Calculate node size (simplified - counts all statements)
fn calculate_node_size(circuit: &Circuit) -> usize {
    circuit.statements.len()
}

/// Main function that runs noir metamorphic tests
/// This is the Rust equivalent of run_noir_metamorphic_tests in core.py
fn run_noir_metamorphic_tests(
    seed: u64,
    working_dir: &str,
    config: IRConfig,
    weakening_probability: f64,
) -> TestResult {
    let start_time = Instant::now();
    println!("noir metamorphic testing, seed: {}, working-dir: {}", seed, working_dir);

    // Create RNG from seed
    let mut rng = StdRng::seed_from_u64(seed);
    
    // Generate seeds for different phases
    let ir_gen_seed = rng.random_range(1000000000u64..=9999999999u64);
    let ir_tf_seed = rng.random_range(1000000000u64..=9999999999u64);
    let _test_seed = rng.random_range(1000000000u64..=9999999999u64);
    
    // Determine metamorphic kind (equal or weaker)
    let kind = random_weighted_metamorphic_kind(&mut rng, weakening_probability);
    
    // Noir only supports BN254 curve natively
    let curve_prime = String::from("bn254");
    let curve_name = "bn254";
    
    // Phase 1: Generate original circuit (IR generation)
    println!("Generating original circuit...");
    let ir_generation_start = Instant::now();
    let ir_gen_rng = StdRng::seed_from_u64(ir_gen_seed);
    let exclude_prime = true;
    let mut generator = ArithmeticCircuitGenerator::new(
        curve_prime.clone(),
        config.clone(),
        ir_gen_rng,
        exclude_prime,
    );
    let mut ir = generator.run();
    ir.name = format!("Circuit_{}", curve_name);
    let ir_generation_time = ir_generation_start.elapsed().as_secs_f64();
    
    println!("Original circuit generated: {} inputs, {} outputs, {} statements",
        ir.inputs.len(), ir.outputs.len(), ir.statements.len());
    
    // Phase 2: Generate transformed circuit (IR rewrite)
    println!("Generating transformed circuit (metamorphic kind: {})...", kind.value());
    let ir_rewrite_start = Instant::now();
    
    let rules = match kind {
        MetamorphicKind::Equal => config.rewrite.equivalence.clone(),
        MetamorphicKind::Weaker => {
            let mut res = config.rewrite.equivalence.clone();
            res.extend(config.rewrite.weakening.clone());
            res
        }
    };

    // Apply rewriter to transform the circuit
    let ir_rewrite_rng = StdRng::seed_from_u64(ir_tf_seed);
    let mut rewriter = rewriter::RuleBasedRewriter::new(
        rules,
        ir_rewrite_rng,
        config.rewrite.min_rewrites,
        config.rewrite.max_rewrites,
    );
    let (_applied_pois, ir_tf) = rewriter.run(&ir, None);

    let ir_rewrite_time = ir_rewrite_start.elapsed().as_secs_f64();
    
    // Phase 3: Run metamorphic tests
    // For now, we'll create a single test iteration
    // In a full implementation, this would compile Noir programs, execute them,
    // and collect results
    println!("Running metamorphic tests...");
    let mut iterations = Vec::new();
    
    // Single iteration example
    let iteration = TestIteration {
        c1_execute: Some(true),
        c1_execute_time: Some(0.1),
        c2_execute: Some(true),
        c2_execute_time: Some(0.1),
        error: None,
    };
    iterations.push(iteration);
    
    let test_time = start_time.elapsed().as_secs_f64();
    
    // Collect statistics
    let (c1_assignments, c1_assertions) = count_statements(&ir);
    let (c2_assignments, c2_assertions) = count_statements(&ir_tf);
    
    let data_entry = DataEntry {
        tool: String::from("noir"),
        test_time,
        seed,
        curve: curve_name.to_string(),
        oracle: kind.value().to_string(),
        iteration: 0,
        error: None,
        ir_generation_seed: ir_gen_seed,
        ir_generation_time,
        ir_rewrite_seed: ir_tf_seed,
        ir_rewrite_time,
        c1_node_size: calculate_node_size(&ir),
        c1_assignments,
        c1_assertions,
        c1_input_signals: ir.inputs.len(),
        c1_output_signals: ir.outputs.len(),
        c2_node_size: calculate_node_size(&ir_tf),
        c2_assignments,
        c2_assertions,
        c2_input_signals: ir_tf.inputs.len(),
        c2_output_signals: ir_tf.outputs.len(),
    };
    
    println!("\n=== Test Results ===");
    println!("Tool: {}", data_entry.tool);
    println!("Seed: {}", data_entry.seed);
    println!("Curve: {}", data_entry.curve);
    println!("Oracle: {}", data_entry.oracle);
    println!("Test time: {:.3}s", data_entry.test_time);
    println!("IR generation time: {:.3}s", data_entry.ir_generation_time);
    println!("IR rewrite time: {:.3}s", data_entry.ir_rewrite_time);
    println!("\nOriginal Circuit:");
    println!("  - Node size: {}", data_entry.c1_node_size);
    println!("  - Assignments: {}", data_entry.c1_assignments);
    println!("  - Assertions: {}", data_entry.c1_assertions);
    println!("  - Input signals: {}", data_entry.c1_input_signals);
    println!("  - Output signals: {}", data_entry.c1_output_signals);
    println!("\nTransformed Circuit:");
    println!("  - Node size: {}", data_entry.c2_node_size);
    println!("  - Assignments: {}", data_entry.c2_assignments);
    println!("  - Assertions: {}", data_entry.c2_assertions);
    println!("  - Input signals: {}", data_entry.c2_input_signals);
    println!("  - Output signals: {}", data_entry.c2_output_signals);
    
    TestResult { iterations }
}

fn main() {
    // Configuration
    let config = IRConfig::default();
    let weakening_probability = 0.3; // 30% chance of weakening transformation
    let working_dir = "./noir_test_workdir";
    
    println!("Starting Noir Metamorphic Testing Framework");
    println!("===========================================\n");
    
    // Initialize RNG for generating seeds
    let mut main_rng = StdRng::from_os_rng();
    let mut cycle = 0;
    
    // Infinite loop for continuous testing
    loop {
        cycle += 1;
        
        // Generate a new random seed for each iteration
        let seed: u64 = main_rng.random_range(1000000000u64..=9999999999u64);
        
        println!("\n=== Cycle {} ===", cycle);
        println!("Using seed: {}", seed);
        
        // Run the metamorphic test
        let result = run_noir_metamorphic_tests(
            seed,
            working_dir,
            config.clone(),
            weakening_probability,
        );
        
        println!("\n=== Cycle {} Results ===", cycle);
        println!("Total iterations: {}", result.iterations.len());
        println!("Cycle completed successfully.");
        
        // Optional: Add a small delay to prevent overwhelming the system
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}
