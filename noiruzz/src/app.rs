use crate::constants::{GREEN, RED, RESET};
use anyhow::Result;
use fuzztools::circuits::{context::Context, Circuit};
use nargo::parse_all;
use noirc_driver::{
    compile_main, file_manager_with_stdlib, prepare_crate, CompilationResult, CompileOptions,
    CompiledProgram, CrateId,
};
use noirc_frontend::hir::Context as NoirContext;
use rand::Rng;
use std::{
    fs,
    io::{self, Write},
    path::Path,
    process::Command,
    time::{Duration, Instant},
};

pub struct App {
    /// Noiruzz prelude to display
    prelude: String,

    /// Circuit generation context
    ctx: Context,

    /// Crash report directory
    crash_report_dir: String,

    // Stats
    total_crashes: u64,
    total_programs: u64,
    programs_since_last_update: u64,
    start_time: Instant,
    last_update: Instant,
    gen_time: Duration,
    mutate_time: Duration,
    compile_time: Duration,
}

impl App {
    pub async fn new(ctx: Context, prelude: String, crash_report_dir: String) -> Result<Self> {
        Ok(Self {
            prelude,
            ctx,
            crash_report_dir,
            total_crashes: 0,
            total_programs: 0,
            programs_since_last_update: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
            gen_time: Duration::from_secs(0),
            mutate_time: Duration::from_secs(0),
            compile_time: Duration::from_secs(0),
        })
    }

    pub async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        let num_cores = std::thread::available_parallelism().unwrap().get();

        loop {
            let gen_start = Instant::now();
            let code = Circuit::random(random, &self.ctx);
            self.gen_time = gen_start.elapsed();

            // @todo mutate the code

            let compile_start = Instant::now();
            let result = self.compile_snippet(&code, false);
            self.compile_time = compile_start.elapsed();

            let hash = format!("{:x}", md5::compute(&code));
            if result.is_err() {
                self.total_crashes += 1;
                let project_dir = format!("{}/{}", self.crash_report_dir, hash);
                let _ = Command::new("nargo").args(&["new", &project_dir]).output();
                let _ = fs::write(format!("{}/src/main.nr", project_dir), &code);
            } else { // @todo debug only
                let project_dir = format!("./tests/noiruzz/debug/{}", hash);
                let _ = Command::new("nargo").args(&["new", &project_dir]).output();
                let _ = fs::write(format!("{}/src/main.nr", project_dir), &code);
            }

            self.total_programs += 1;
            self.programs_since_last_update += 1;

            if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                self.screen()?;
                self.programs_since_last_update = 0;
                self.last_update = Instant::now();
            }
        }
    }

    fn screen(&mut self) -> Result<()> {
        // Clear the screen
        std::process::Command::new("clear").status().unwrap();

        // Print `HEADER` + configuration
        print!("{}", self.prelude);

        // Print stats
        print!(
            "[{GREEN}+{RESET}] Total: {RED}{}{RESET} | Crashes: {RED}{}{RESET} | Tick: \
             {RED}{}{RESET} progs | Time: {RED}{:02}h {:02}m {:02}s{RESET} | Gen: \
             {RED}{:>6.2}ms{RESET} | Compile: {RED}{:>6.2}ms{RESET}",
            self.total_programs,
            self.total_crashes,
            self.programs_since_last_update,
            self.start_time.elapsed().as_secs() / 3600,
            (self.start_time.elapsed().as_secs() % 3600) / 60,
            self.start_time.elapsed().as_secs() % 60,
            self.gen_time.as_secs_f64() * 1000.0,
            self.compile_time.as_secs_f64() * 1000.0,
        );

        io::stdout().flush()?;

        Ok(())
    }

    // -------------------- TAKEN FROM NOIR TOOLING --------------------
    pub fn prepare_snippet(&self, code: String) -> (NoirContext<'static, 'static>, CrateId) {
        let root = Path::new("");
        let file_name = Path::new("main.nr");
        let mut file_manager = file_manager_with_stdlib(root);
        file_manager
            .add_file_with_source(file_name, code)
            .expect("Failed to add source to file manager");

        let parsed_files = parse_all(&file_manager);
        let mut context = NoirContext::new(file_manager, parsed_files);
        let root_crate_id = prepare_crate(&mut context, file_name);
        (context, root_crate_id)
    }

    pub fn compile_snippet(
        &self,
        code: &str,
        force_brillig: bool,
    ) -> CompilationResult<CompiledProgram> {
        let (mut context, root_crate_id) = self.prepare_snippet(code.to_string());
        let options = CompileOptions { force_brillig, ..Default::default() };
        compile_main(&mut context, root_crate_id, &options, None)
    }
}
