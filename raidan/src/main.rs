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
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    let prelude = format!(
        "{HEADER}\n{GREEN}INFO{RESET}      Method:                    \
         {RED}{:?}{RESET}\n{GREEN}INFO{RESET}      Seed:                      \
         {RED}{}{RESET}\n\n",
        cli.method, cli.seed
    );

    let mut app = App::new(cli.method, cli.seed, prelude).await?;

    let mut random = SmallRng::seed_from_u64(cli.seed);
    let result = app.run(&mut random).await;
    if let Err(e) = result {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
