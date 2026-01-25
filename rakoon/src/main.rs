mod app;
mod constants;

use std::{thread, time::Duration};

use anyhow::Result;
use app::App;
use clap::Parser;
use constants::{GREEN, HEADER, RED, RESET};
use rand::{rngs::SmallRng, SeedableRng};

#[derive(Parser)]
#[command(
    name = "rakoon",
    about = "Transaction Fuzzer for the Ethereum Protocol",
    author = "nethoxa"
)]
struct Cli {
    #[arg(
        long,
        help = "Transaction type to fuzz (legacy, eip2930, eip1559, eip7702)",
        default_value = "eip7702"
    )]
    tx_type: String,

    #[arg(long, help = "Private key for signing transactions")]
    key: String,

    #[arg(long, help = "Seed for the random generator", default_value_t = 0)]
    seed: u64,

    #[arg(long, help = "URL to send transactions to", default_value = "http://127.0.0.1:8545")]
    url: String,

    #[arg(long, help = "Wether to mutate txs or not before sending them", default_value_t = true)]
    fuzzing: bool,

    #[arg(long, help = "Poll interval to query gas prices (in seconds)", default_value_t = 2)]
    poll_interval: u64,

    #[arg(long, help = "Number of concurrent RPC batches", default_value_t = 50)]
    workers: usize,

    #[arg(long, help = "Number of txs per batch", default_value_t = 100)]
    batch_size: usize,

    #[arg(long, help = "Start delay (nvm, used in private env)", default_value_t = 0)]
    start_delay: u64
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut random = SmallRng::seed_from_u64(cli.seed);

    let header = format!(
        "{HEADER}\n\
        {GREEN}INFO{RESET}      Tx type:         {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Key:             {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Seed:            {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Url:             {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Fuzzing enabled: {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Poll interval:   {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Workers:         {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Batch size:      {RED}{}{RESET}\n\n",
        cli.tx_type,
        cli.key,
        cli.seed,
        cli.url,
        cli.fuzzing,
        cli.poll_interval,
        cli.workers,
        cli.batch_size
    );

    thread::sleep(Duration::from_secs(cli.start_delay));

    // Run the application
    let mut app = App::new(
        cli.tx_type,
        header,
        cli.key,
        cli.url,
        cli.fuzzing,
        cli.workers,
        cli.poll_interval,
        cli.batch_size,
    )
    .await?;

    let result = app.run(&mut random).await;
    if let Err(e) = result {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
