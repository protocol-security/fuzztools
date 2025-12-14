use crate::constants::{GREEN, RED, RESET};
use anyhow::Result;
use crossbeam::channel::{bounded, Receiver, Sender};
use fuzztools::{
    builders::CircuitBuilder,
    circuits::{context::Context, Circuit},
};
use nargo::parse_all;
use noirc_driver::{compile_main, file_manager_with_stdlib, prepare_crate, CompileOptions};
use noirc_frontend::hir::Context as NoirContext;
use rand::Rng;
use std::{
    fs,
    io::{self, Write},
    panic::{self, AssertUnwindSafe},
    path::Path,
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
    time::Instant,
};

/// Job sent to worker threads
struct CompileJob {
    circuit: Circuit,
    indexed_name: String,
}

/// Result of compilation
enum CompileResult {
    /// Compiled successfully
    Success { indexed_name: String },
    /// Compiler returned an error (syntax/type error, not a crash)
    CompileError { indexed_name: String },
    /// Compiler panicked (ICE - Internal Compiler Error)
    Panic { circuit: Circuit, indexed_name: String, message: String },
}

pub struct App {
    /// Noiruzz prelude to display
    prelude: String,

    /// Circuit generation context
    ctx: Context,

    /// Crash report directory
    crash_report_dir: String,

    /// Channel to send jobs to workers
    job_sender: Sender<CompileJob>,

    /// Channel to receive results from workers
    result_receiver: Receiver<CompileResult>,

    // Stats (atomic for thread safety)
    total_panics: AtomicU64,
    total_errors: AtomicU64,
    total_programs: u64,
    programs_since_last_update: u64,
    start_time: Instant,
    last_update: Instant,
}

impl App {
    pub fn new(ctx: Context, prelude: String, crash_report_dir: String) -> Result<Self> {
        let num_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);

        // Bounded channels for backpressure
        let (job_sender, job_receiver) = bounded::<CompileJob>(num_workers * 2);
        let (result_sender, result_receiver) = bounded::<CompileResult>(num_workers * 2);

        // Configure rayon with larger stack for Noir's deep recursion
        rayon::ThreadPoolBuilder::new()
            .stack_size(64 * 1024 * 1024) // 64MB stack
            .num_threads(num_workers)
            .build_global()
            .ok(); // Ignore if already initialized

        // Spawn worker threads
        for _ in 0..num_workers {
            let receiver = job_receiver.clone();
            let sender = result_sender.clone();

            rayon::spawn(move || {
                worker_loop(receiver, sender);
            });
        }

        // Drop extra sender so workers can detect shutdown
        drop(result_sender);

        // Create output directories
        fs::create_dir_all(format!("{}/debug", crash_report_dir))?;
        fs::create_dir_all(format!("{}/out", crash_report_dir))?;

        Ok(Self {
            prelude,
            ctx,
            crash_report_dir,
            job_sender,
            result_receiver,
            total_panics: AtomicU64::new(0),
            total_errors: AtomicU64::new(0),
            total_programs: 0,
            programs_since_last_update: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
        })
    }

    pub fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        let builder = CircuitBuilder {};
        loop {
            self.drain_results();
            let circuit = builder.circuit(random, &self.ctx);

            // @todo mutate the code

            let indexed_name = format!("circuit_{}", self.total_programs);
            let debug_dir = format!("{}/debug/{}", self.crash_report_dir, indexed_name);
            let _ = Command::new("nargo").args(["new", &debug_dir]).output();
            let _ = fs::write(format!("{}/src/main.nr", debug_dir), &circuit);

            let job = CompileJob { circuit, indexed_name };
            if self.job_sender.send(job).is_err() {
                break; // All workers died
            }

            self.total_programs += 1;
            self.programs_since_last_update += 1;

            // Update display periodically
            if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                self.screen()?;
                self.programs_since_last_update = 0;
                self.last_update = Instant::now();
            }
        }

        Ok(())
    }

    /// Drain all pending results from workers
    fn drain_results(&mut self) {
        while let Ok(result) = self.result_receiver.try_recv() {
            match result {
                CompileResult::Success { .. } => {
                    // Successfully compiled - nothing to do
                }
                CompileResult::CompileError { .. } => {
                    self.total_errors.fetch_add(1, Ordering::Relaxed);
                }
                CompileResult::Panic { circuit, indexed_name, message } => {
                    self.total_panics.fetch_add(1, Ordering::Relaxed);

                    // Write panic crash to out folder
                    let project_dir = format!("{}/out/{}", self.crash_report_dir, indexed_name);
                    let _ = Command::new("nargo").args(["new", &project_dir]).output();
                    let _ = fs::write(format!("{}/src/main.nr", project_dir), &circuit);

                    // Also write panic message
                    let _ = fs::write(format!("{}/panic.txt", project_dir), &message);
                }
            }
        }
    }

    fn screen(&mut self) -> Result<()> {
        std::process::Command::new("clear").status().unwrap();

        print!("{}", self.prelude);

        let panics = self.total_panics.load(Ordering::Relaxed);
        let errors = self.total_errors.load(Ordering::Relaxed);

        print!(
            "[{GREEN}+{RESET}] Total: {RED}{}{RESET} | Panics: {RED}{}{RESET} | Errors: {RED}{}{RESET} | Tick: \
             {RED}{}{RESET} progs | Time: {RED}{:02}h {:02}m {:02}s{RESET}",
            self.total_programs,
            panics,
            errors,
            self.programs_since_last_update,
            self.start_time.elapsed().as_secs() / 3600,
            (self.start_time.elapsed().as_secs() % 3600) / 60,
            self.start_time.elapsed().as_secs() % 60,
        );

        io::stdout().flush()?;
        Ok(())
    }
}

/// Worker function that runs in each rayon thread
fn worker_loop(receiver: Receiver<CompileJob>, sender: Sender<CompileResult>) {
    while let Ok(job) = receiver.recv() {
        let circuit = job.circuit.clone();
        let indexed_name = job.indexed_name.clone();

        // Use catch_unwind to catch panics (ICEs)
        let result = panic::catch_unwind(AssertUnwindSafe(|| compile_snippet(&circuit)));

        let compile_result = match result {
            Ok(Ok(_)) => {
                // Compiled successfully
                CompileResult::Success { indexed_name }
            }
            Ok(Err(_)) => {
                // Compiler returned an error (not a panic)
                CompileResult::CompileError { indexed_name }
            }
            Err(panic_info) => {
                // Compiler panicked - this is a real crash!
                let message = if let Some(s) = panic_info.downcast_ref::<&str>() {
                    s.to_string()
                } else if let Some(s) = panic_info.downcast_ref::<String>() {
                    s.clone()
                } else {
                    "Unknown panic".to_string()
                };

                CompileResult::Panic { circuit, indexed_name, message }
            }
        };

        let _ = sender.send(compile_result);
    }
}

/// Compile a Noir code snippet
fn compile_snippet(code: &str) -> Result<(), ()> {
    let root = Path::new("");
    let file_name = Path::new("main.nr");

    let mut file_manager = file_manager_with_stdlib(root);
    file_manager
        .add_file_with_source(file_name, code.to_string())
        .expect("Failed to add source to file manager");

    let parsed_files = parse_all(&file_manager);
    let mut context = NoirContext::new(file_manager, parsed_files);
    let root_crate_id = prepare_crate(&mut context, file_name);
    let options = CompileOptions::default();

    compile_main(&mut context, root_crate_id, &options, None).map(|_| ()).map_err(|_| ())
}
