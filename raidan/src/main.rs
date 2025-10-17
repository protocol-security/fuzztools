mod app;
mod constants;

use crate::{
    app::App,
    constants::{Method, GREEN, HEADER, RED, RESET},
};
use anyhow::Result;
use clap::Parser;
use rand::{rngs::SmallRng, SeedableRng};

#[derive(Parser)]
#[command(
    name = "raidan",
    about = "State transition fuzzer for CL clients",
    author = "nethoxa, 0xMushow"
)]
struct Cli {
    #[arg(long, help = "Method to fuzz", value_enum)]
    method: Method,

    #[arg(long, help = "Seed for the random generator")]
    seed: u64,

    #[arg(long, help = "Whether to spin up prysm")]
    prysm: bool,

    #[arg(long, help = "Whether to spin up lighthouse")]
    lighthouse: bool,

    #[arg(long, help = "Whether to spin up teku")]
    teku: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    let prelude = format!(
        "{HEADER}\n{GREEN}INFO{RESET}      Method:                    \
         {RED}{:?}{RESET}\n{GREEN}INFO{RESET}      Seed:                      \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Prysm:                     \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Lighthouse:                \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Teku:                      {RED}{}{RESET}\n\n",
        cli.method, cli.seed, cli.prysm, cli.lighthouse, cli.teku
    );

    let mut app =
        App::new(cli.method, cli.seed, cli.prysm, cli.lighthouse, cli.teku, prelude).await?;

    let mut random = SmallRng::seed_from_u64(cli.seed);
    let result = app.run(&mut random).await;
    if let Err(e) = result {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
