mod constants;
mod app;

use crate::app::App;
use anyhow::Result;
use clap::Parser;
use constants::{TransactionType, GREEN, HEADER, RED, RESET};
use rand::{rngs::SmallRng, SeedableRng};

#[derive(Parser)]
#[command(
    name = "rakoon",
    about = "Transaction Fuzzer for the Ethereum Protocol",
    author = "nethoxa"
)]
struct Cli {
    #[arg(long, help = "Transaction type to fuzz", value_enum)]
    tx_type: TransactionType,

    #[arg(long, help = "Private key for signing transactions")]
    key: String,

    #[arg(long, help = "Seed for the random generator")]
    seed: u64,

    #[arg(long, help = "URL to send transactions to (supports IPC, WS and HTTP)")]
    url: String,

    #[arg(long, help = "If false, it spams VALID transactions")]
    fuzzing: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    let prelude = format!(
        "{HEADER}\n{GREEN}INFO{RESET}      URL:                    \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Key:                    \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Seed:                   \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Type:                   \
         {RED}{:?}{RESET}\n{GREEN}INFO{RESET}      Fuzzing enabled:        {RED}{}{RESET}\n\n",
        cli.url,
        cli.key,
        cli.seed,
        cli.tx_type,
        cli.fuzzing,
    );

    // Create the application
    let mut app = App::new(cli.tx_type, cli.key, cli.url, cli.fuzzing, prelude).await?;

    // Run the application
    let mut random = SmallRng::seed_from_u64(cli.seed);
    let result = app.run(&mut random).await;
    if let Err(e) = result {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
