mod config;
mod circuit;
mod nodes;
mod traveller;
mod types;
mod rewriter;

use anyhow::Result;
use circuit::Circuit;
use clap::Parser;
use config::Config;
use fuzztools::math::bernoulli;
use rand::{rngs::SmallRng, SeedableRng};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::fs;
use types::MetamorphicKind;

mod utils;

#[derive(Parser)]
#[command(name = "noiruzz", about = "Metamorphic fuzzer for the Noir compiler", author = "nethoxa")]
struct Cli {
    #[arg(long, help = "Seed for the random generator", default_value = "0")]
    seed: u64,

    #[arg(long, help = "Path to the config file (default: ./configs/noiruzz.json)")]
    config: Option<String>,

    #[arg(long, help = "Path to the crash report directory (default: ./out)")]
    crash_report_dir: Option<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut random = SmallRng::seed_from_u64(cli.seed);
    let crash_report_dir = if let Some(crash_report_dir) = cli.crash_report_dir {
        crash_report_dir
    } else {
        "./out".to_string()
    };

    let config = if let Some(config_path) = cli.config {
        let config_content = fs::read_to_string(config_path).unwrap();
        serde_json::from_str(&config_content).unwrap()
    } else {
        let config_content = fs::read_to_string("./configs/noiruzz.json").unwrap();
        serde_json::from_str(&config_content).unwrap()
    };

    loop {
        let circuit = Circuit::new(&mut random, &config);

        let metamorphic_kind = if bernoulli(config.rewrite.weakening_probability, &mut random) {
            MetamorphicKind::Equal
        } else {
            MetamorphicKind::Weaker
        };
    }

    Ok(())
}
