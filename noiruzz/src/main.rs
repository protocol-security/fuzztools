use anyhow::Result;
use clap::Parser;
use fuzztools::circuits::context::Context;
use rand::{rngs::SmallRng, SeedableRng};
use std::{fs, path::Path};

mod constants;
mod app;
use crate::app::App;
use constants::{GREEN, HEADER, RED, RESET};

#[derive(Parser)]
#[command(name = "noiruzz", about = "Metamorphic fuzzer for the Noir compiler", author = "nethoxa")]
struct Cli {
    #[arg(long, default_value = "0", help = "Seed for the random generator")]
    seed: u64,

    #[arg(long, help = "Path to the config file")]
    config: Option<String>,

    #[arg(long, help = "Path to the crash report directory")]
    crash_report_dir: Option<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    let crash_dir = cli.crash_report_dir.unwrap_or_else(|| "./tests/noiruzz/out".into());
    fs::create_dir_all(&crash_dir)?;

    let config_path = cli.config.unwrap_or_else(|| "./configs/noiruzz.json".into());
    if !Path::new(&config_path).exists() {
        return Err(anyhow::anyhow!("Config file not found: {}", config_path));
    }

    let config: Context = serde_json::from_str(&fs::read_to_string(&config_path)?)?;
    if config.max_type_depth > 4 {
        // @todo edit this
        return Err(anyhow::anyhow!(
            "Max type depth {} risks type explosion",
            config.max_type_depth
        ));
    }

    let prelude = format!(
        "{HEADER}\n{GREEN}INFO{RESET}      Seed:              {RED}{}{RESET}\n\
         {GREEN}INFO{RESET}      Config:            {RED}{}{RESET}\n\
         {GREEN}INFO{RESET}      Crash report dir:  {RED}{}{RESET}\n\n",
        cli.seed, config_path, crash_dir
    );

    // Create the application
    let mut app = App::new(config, prelude, crash_dir).await?;

    // Run the application
    let mut random = SmallRng::seed_from_u64(cli.seed);
    let result = app.run(&mut random).await;
    if let Err(e) = result {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
