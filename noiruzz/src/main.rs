use anyhow::Result;
use clap::Parser;
use fuzztools::compilers::config::Config;
use rand::{rngs::SmallRng, SeedableRng};
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::time::{Duration, Instant};

const HEADER: &str = "
╭─────────────────────────────────────────────────────────────────╮
│                                                                 │
│                           NOIRUZZ                               │
│                 Fuzzer for the Noir Compiler                    │
│                                                                 │
│                        Author: nethoxa                          │
│                       Twitter: @nethoxa                         │
│                                                                 │
╰─────────────────────────────────────────────────────────────────╯
";

const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const RESET: &str = "\x1b[0m";

use nargo::parse_all;
use noirc_driver::{
    CompilationResult, CompileOptions, CompiledProgram, CrateId, compile_main,
    file_manager_with_stdlib, prepare_crate,
};
use noirc_frontend::hir::Context;

/// Prepare a code snippet.
fn prepare_snippet(source: String) -> (Context<'static, 'static>, CrateId) {
    let root = Path::new("");
    let file_name = Path::new("main.nr");
    let mut file_manager = file_manager_with_stdlib(root);
    file_manager.add_file_with_source(file_name, source).expect(
        "Adding source buffer to file manager should never fail when file manager is empty",
    );
    let parsed_files = parse_all(&file_manager);

    let mut context = Context::new(file_manager, parsed_files);
    let root_crate_id = prepare_crate(&mut context, file_name);

    (context, root_crate_id)
}

/// Compile the main function in a code snippet.
///
/// Use `force_brillig` to test it as an unconstrained function without having to change the code.
/// This is useful for methods that use the `runtime::is_unconstrained()` method to change their behavior.
pub(crate) fn prepare_and_compile_snippet(
    source: String,
    force_brillig: bool,
) -> CompilationResult<CompiledProgram> {
    let (mut context, root_crate_id) = prepare_snippet(source);
    let options = CompileOptions { force_brillig, ..Default::default() };
    // @todo Run nargo::ops::transform_program?
    compile_main(&mut context, root_crate_id, &options, None)
}


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

    let crash_report_dir = cli.crash_report_dir.unwrap_or_else(|| "./tests/noiruzz/out".to_string());    
    if !Path::new(&crash_report_dir).exists() {
        fs::create_dir_all(&crash_report_dir).unwrap();
    }

    let debug_dir = "./tests/noiruzz/debug";
    if !Path::new(&debug_dir).exists() {
        fs::create_dir_all(&debug_dir).unwrap();
    }

    let config_path = cli.config.unwrap_or_else(|| "./configs/noiruzz.json".to_string());
    if !Path::new(&config_path).exists() {
        return Err(anyhow::anyhow!("Config file not found: {}", config_path));
    }

    // @audit beware of type explosion!!!
    let config: Config = {
        let config_content = fs::read_to_string(&config_path).unwrap();
        serde_json::from_str(&config_content).unwrap()
    };

    if config.max_type_depth > 4 {
        // @todo
        return Err(anyhow::anyhow!("Max type depth is too high, you risk a type explosion: {}", config.max_type_depth));
    }

    // Print prelude
    let prelude = format!(
        "{HEADER}\n{GREEN}INFO{RESET}      Seed:                   \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Config:                 \
         {RED}{}{RESET}\n{GREEN}INFO{RESET}      Crash report dir:       {RED}{}{RESET}\n\n",
        cli.seed,
        config_path,
        crash_report_dir,
    );

    // Stats
    let start_time = Instant::now();
    let mut last_update = Instant::now();
    let mut total_programs: u64 = 0;
    let mut total_crashes: u64 = 0;
    let mut programs_since_last_update: u64 = 0;

    loop {
        // Generate `n` programs
        let gen_start = Instant::now();
        let program = fuzztools::compilers::Program::random(&mut random, &config);
        let program_string = program.to_string();
        let gen_time = gen_start.elapsed();

        // Compile program
        let compile_start = Instant::now();
        let compiled_program = prepare_and_compile_snippet(program_string.clone(), false);
        let compile_time = compile_start.elapsed();

        total_programs += 1;
        programs_since_last_update += 1;

        if compiled_program.is_err() {
            total_crashes += 1;
            // Write crash report
            let program_hash = format!("{:x}", md5::compute(&program_string));
            let crash_file_path = format!("{}/{}.txt", crash_report_dir, program_hash);
            if let Err(e) = fs::write(&crash_file_path, &program_string) {
                eprintln!("Failed to write crash report to {}: {}", crash_file_path, e);
            }
        } else {
            let program_hash = format!("{:x}", md5::compute(&program_string));
            let debug_file_path = format!("{}/{}.txt", debug_dir, program_hash);
            if let Err(e) = fs::write(&debug_file_path, &program_string) {
                eprintln!("Failed to write debug file to {}: {}", debug_file_path, e);
            }
        }

        // Update terminal logging every second
        if last_update.elapsed().as_secs_f64() >= 1.0 {
            screen(
                &prelude,
                total_programs,
                total_crashes,
                programs_since_last_update,
                &start_time,
                &gen_time,
                &compile_time,
            )?;
            programs_since_last_update = 0;
            last_update = Instant::now();
        }
    }
}

fn screen(
    prelude: &str,
    total_programs: u64,
    total_crashes: u64,
    programs_since_last_update: u64,
    start_time: &Instant,
    gen_time: &Duration,
    compile_time: &Duration,
) -> Result<()> {
    // Clear the screen
    std::process::Command::new("clear").status().unwrap();

    // Print header + configuration
    print!("{}", prelude);

    // Print stats
    print!(
        "[{GREEN}+{RESET}] Total: {RED}{}{RESET} | Crashes: {RED}{}{RESET} | Tick: {RED}{}{RESET} progs | Time: {RED}{:02}h{RESET} {RED}{:02}m{RESET} \
         {RED}{:02}s{RESET} | Gen: {RED}{:>6.2}ms{RESET} | Compile: {RED}{:>6.2}ms{RESET}",
        total_programs,
        total_crashes,
        programs_since_last_update,
        start_time.elapsed().as_secs() / 3600,
        (start_time.elapsed().as_secs() % 3600) / 60,
        start_time.elapsed().as_secs() % 60,
        gen_time.as_secs_f64() * 1000.0,
        compile_time.as_secs_f64() * 1000.0,
    );

    io::stdout().flush()?;

    Ok(())
}
