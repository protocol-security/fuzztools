mod app;
mod constants;
mod context;

use std::{fs, path::Path};

use anyhow::Result;
use app::App;
use clap::Parser;
use constants::{GREEN, HEADER, RED, RESET};
use context::Config;
use rand::{rngs::SmallRng, SeedableRng};

#[derive(Parser)]
#[command(
    name = "rakoon",
    about = "Transaction Fuzzer for the Ethereum Protocol",
    author = "nethoxa"
)]
struct Cli {
    #[arg(long, help = "Transaction type to fuzz (legacy, eip2930, eip1559, eip7702)")]
    tx_type: String,

    #[arg(long, help = "Private key for signing transactions")]
    key: String,

    #[arg(long, help = "Seed for the random generator", default_value_t = 0)]
    seed: u64,

    #[arg(long, help = "URL to send transactions to", default_value = "http://127.0.0.1:8545")]
    url: String,

    #[arg(long, help = "Wether to mutate txs or not before sending them", default_value_t = true)]
    fuzzing: bool,

    #[arg(long, help = "Time to wait between tx batches (in ms)", default_value_t = 0)]
    sleep: u64,

    #[arg(long, help = "Path to the config file", default_value = "./configs/rakoon.json")]
    config: String,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut random = SmallRng::seed_from_u64(cli.seed);

    if !Path::new(&cli.config).exists() {
        return Err(anyhow::anyhow!("Config file not found: {}", cli.config));
    }

    let config: Config = serde_json::from_str(&fs::read_to_string(&cli.config)?)?;
    let header = format!(
        "{HEADER}\n\
        {GREEN}INFO{RESET}      Tx type:         {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Key:             {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Seed:            {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Url:             {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Fuzzing enabled: {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Sleep time:      {RED}{}s{RESET}\n\
        {GREEN}INFO{RESET}      Config:          {RED}{}{RESET}\n\n",
        cli.tx_type, cli.key, cli.seed, cli.url, cli.fuzzing, cli.sleep, cli.config
    );

    // Run the application
    let mut app =
        App::new(cli.tx_type, cli.key, cli.url, cli.fuzzing, cli.sleep, header, config).await?;

    let result = app.run(&mut random).await;
    if let Err(e) = result {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
