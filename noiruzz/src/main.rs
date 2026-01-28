use anyhow::Result;
use clap::Parser;
use fuzztools::circuits::context::Context;
use rand::{rngs::SmallRng, SeedableRng};
use std::{fs, path::Path};

mod app;
mod commands;
mod constants;
mod scheduler;
use crate::app::App;
use constants::{GREEN, HEADER, RED, RESET};

#[derive(Parser)]
#[command(name = "noiruzz", about = "Metamorphic fuzzer for the Noir compiler", author = "nethoxa")]
struct Cli {
    #[arg(long, default_value = "0", help = "Seed for the random generator")]
    seed: u64,

    #[arg(long, default_value = "10", help = "Number of executions per circuit")]
    executions: usize,

    #[arg(long, help = "Path to the config file")]
    config: Option<String>,

    #[arg(long, help = "Path to the crash directory")]
    crash_dir: Option<String>,

    #[arg(
        long,
        default_value = "0.5",
        help = "Target ratio for power schedule (T2/(T1+T2) < ratio to run prover/verifier stages)"
    )]
    target_ratio: f64,

    #[arg(long, default_value = "4", help = "Number of concurrent workers")]
    workers: usize,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    let crash_dir = cli.crash_dir.unwrap_or_else(|| "./crashes/".into());
    fs::create_dir_all(&crash_dir)?;

    let config_path = cli.config.unwrap_or_else(|| "./configs/noiruzz.json".into());
    if !Path::new(&config_path).exists() {
        return Err(anyhow::anyhow!("Config file not found: {}", config_path));
    }

    let ctx: Context = serde_json::from_str(&fs::read_to_string(&config_path)?)?;
    let prelude = format!(
        "{HEADER}\n{GREEN}INFO{RESET}      Seed:       {RED}{}{RESET}\n\
         {GREEN}INFO{RESET}      Executions: {RED}{}{RESET}\n\
         {GREEN}INFO{RESET}      Config:     {RED}{}{RESET}\n\
         {GREEN}INFO{RESET}      Crash dir:  {RED}{}{RESET}\n\
         {GREEN}INFO{RESET}      Target p:   {RED}{}{RESET}\n\
         {GREEN}INFO{RESET}      Workers:    {RED}{}{RESET}\n\n",
        cli.seed, cli.executions, config_path, crash_dir, cli.target_ratio, cli.workers,
    );

    let mut app = App::new(ctx, cli.executions, prelude, crash_dir, cli.target_ratio, cli.workers)?;
    let mut random = SmallRng::seed_from_u64(cli.seed);

    if let Err(e) = app.run(&mut random).await {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
