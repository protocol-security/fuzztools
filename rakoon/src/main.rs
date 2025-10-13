mod constants;
mod app;

use clap::Parser;
use rand::{SeedableRng, rngs::SmallRng};
use anyhow::Result;
use constants::{TransactionType, RESET, GREEN, RED, HEADER};

use crate::app::App;

#[derive(Parser)]
#[command(name = "rakoon", about ="Transaction Fuzzer for the Ethereum Protocol", author = "nethoxa")]
struct Cli {
    #[arg(long, help = "Transaction type to fuzz", value_enum, default_value = "legacy")]
    tx_type: TransactionType,

    #[arg(long, help = "Private key for signing transactions", default_value = "0x8c04e41e317a7cf0cf4c2f7431d0a890a950f352df41ff6d053698df61a73bba")]
    key: String,

    #[arg(long, help = "Seed for the random generator", default_value = "0")]
    seed: u64,

    #[arg(long, help = "IPC url to send transactions to")]
    ipc: Option<String>,

    #[arg(long, help = "WS url in case of IPC is not available")]
    ws: Option<String>,

    #[arg(long, help = "If false, it spams VALID transactions")]
    fuzzing: bool,

    #[arg(long, help = "Number of transactions to send per core", default_value = "100")]
    tx_per_core: u64,

    #[arg(long, help = "Deploy AccessListTarget if true", default_value = "false")]
    deploy_test_contract: bool,

    #[arg(long, help = "Number of concurrent requests", default_value = "100")]
    n: usize,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    
    let prelude = format!(
        "{HEADER}\n\
        {GREEN}INFO{RESET}      IPC:                    {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      WS:                     {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Key:                    {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Seed:                   {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Type:                   {RED}{:?}{RESET}\n\
        {GREEN}INFO{RESET}      Fuzzing enabled:        {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Tx per core:            {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Deploy target:          {RED}{}{RESET}\n\
        {GREEN}INFO{RESET}      Concurrent requests:    {RED}{}{RESET}\n\n",
        cli.ipc.as_deref().unwrap_or("None"),
        cli.ws.as_deref().unwrap_or("None"),
        cli.key,
        cli.seed,
        cli.tx_type,
        cli.fuzzing,
        cli.tx_per_core,
        cli.deploy_test_contract,
        cli.n
    );

    // Create the application
    let mut app = App::new(
        cli.tx_type,
        cli.key,
        cli.seed,
        cli.ipc,
        cli.ws,
        cli.fuzzing,
        cli.tx_per_core,
        cli.deploy_test_contract,
        cli.n,
        prelude
    ).await?;
    
    // Run the application
    let mut random = SmallRng::seed_from_u64(cli.seed);
    let result = app.run(&mut random).await;
    if let Err(e) = result {
        eprintln!("\n\n\x1b[1;31m[!] Error: {e}\x1b[0m");
    }

    Ok(())
}
