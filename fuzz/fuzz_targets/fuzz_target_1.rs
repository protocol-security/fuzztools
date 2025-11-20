#![no_main]

use libfuzzer_sys::fuzz_target;
use noiruzz::{circuit::Circuit, config::Config};
use rand::{rngs::StdRng, SeedableRng};
use std::{fs, sync::OnceLock};

static CONFIG: OnceLock<Config> = OnceLock::new();

fn get_config() -> &'static Config {
    CONFIG.get_or_init(|| {
        let config_path = "configs/noiruzz.json";
        let config_content = fs::read_to_string(config_path).expect("Failed to read config file");
        serde_json::from_str(&config_content).expect("Failed to parse config file")
    })
}

fuzz_target!(|data: &[u8]| {
    // Use the fuzzer input as a seed for deterministic randomness
    if data.len() < 8 {
        return;
    }

    let seed = u64::from_le_bytes([
        data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
    ]);

    let mut rng = StdRng::seed_from_u64(seed);
    let config = get_config();

    assert!(config.operators.assign_operators.len() > 0, "assign operators are empty");
    assert!(config.operators.relation_operators.len() > 0, "relation operators are empty");
    assert!(config.operators.unary_operators.len() > 0, "unary operators are empty");
    assert!(config.operators.binary_operators.len() > 0, "binary operators are empty");

    // Generate a circuit using the seeded RNG
    let _circuit = Circuit::new(&mut rng, config);
});
