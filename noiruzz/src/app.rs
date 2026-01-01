use crate::constants::{GREEN, RED, RESET};
use anyhow::Result;
use crossbeam::channel::{bounded, Receiver, Sender};
use fuzztools::{
    builders::CircuitBuilder,
    circuits::{context::Context, rewriter::Rewriter, scope::Scope},
};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::{
    fs,
    io::{self, Write},
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
    thread,
    time::Instant,
};
use tempfile::TempDir;

/// Job containing both original and rewritten circuits to test
struct TestJob {
    original_code: String,
    rewritten_code: String,
    scope: Scope,
    executions: usize,
    job_id: u64,
    seed: u64,
}

/// Result of testing a circuit pair
enum TestResult {
    /// Both compiled and all executions matched
    Success,
    /// One compiled, the other didn't
    CompileMismatch {
        original_code: String,
        rewritten_code: String,
        original_compiled: bool,
        error: String,
        job_id: u64,
    },
    /// Both compiled but outputs diverged
    OutputMismatch {
        original_code: String,
        rewritten_code: String,
        prover_toml: String,
        original_output: String,
        rewritten_output: String,
        job_id: u64,
    },
    /// Both failed to compile (not interesting)
    BothFailedCompile,
    /// Known error (overflow, underflow, etc.) - not a bug
    KnownError,
    /// Internal error (temp dir, etc.)
    InternalError { message: String },
}

pub struct App {
    prelude: String,
    ctx: Context,
    crash_dir: String,
    executions: usize,
    job_sender: Sender<TestJob>,
    result_receiver: Receiver<TestResult>,

    // Stats
    total_circuits: AtomicU64,
    compile_mismatches: AtomicU64,
    soundness_bugs: AtomicU64,
    known_errors: AtomicU64,
    circuits_per_tick: u64,
    start_time: Instant,
    last_update: Instant,
}

impl App {
    pub fn new(
        ctx: Context,
        executions: usize,
        prelude: String,
        crash_dir: String,
    ) -> Result<Self> {
        let num_workers = thread::available_parallelism().map(|n| n.get()).unwrap_or(4);

        let (job_sender, job_receiver) = bounded::<TestJob>(num_workers * 2);
        let (result_sender, result_receiver) = bounded::<TestResult>(num_workers * 2);

        // Spawn worker threads
        for i in 0..num_workers {
            let receiver = job_receiver.clone();
            let sender = result_sender.clone();
            let ctx = ctx;

            thread::Builder::new()
                .name(format!("worker-{}", i))
                .spawn(move || worker_loop(receiver, sender, ctx))
                .expect("Failed to spawn worker thread");
        }

        drop(job_receiver);
        drop(result_sender);

        fs::create_dir_all(&crash_dir)?;

        Ok(Self {
            prelude,
            ctx,
            crash_dir,
            executions,
            job_sender,
            result_receiver,
            total_circuits: AtomicU64::new(0),
            compile_mismatches: AtomicU64::new(0),
            soundness_bugs: AtomicU64::new(0),
            known_errors: AtomicU64::new(0),
            circuits_per_tick: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
        })
    }

    pub fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        let builder = CircuitBuilder::default();
        let rewriter = Rewriter::new();
        let mut job_id = 0u64;

        loop {
            self.drain_results();

            // Generate circuit
            let circuit = builder.generate(random, &self.ctx);

            // Apply N random rewrites
            let mut rewritten_forest = circuit.forest.clone();
            let rewrite_count =
                random.random_range(self.ctx.min_rewrites_count..=self.ctx.max_rewrites_count);
            for _ in 0..rewrite_count {
                rewriter.apply_random(random, &mut rewritten_forest, &self.ctx, &circuit.scope);
            }

            let rewritten_code = builder.format_circuit(&circuit.scope, &rewritten_forest);

            // Send job
            let job = TestJob {
                original_code: circuit.code,
                rewritten_code,
                scope: circuit.scope,
                executions: self.executions,
                job_id,
                seed: random.random(),
            };

            if self.job_sender.send(job).is_err() {
                break;
            }

            job_id += 1;

            if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                self.screen()?;
                self.circuits_per_tick = 0;
                self.last_update = Instant::now();
            }
        }

        Ok(())
    }

    fn drain_results(&mut self) {
        while let Ok(result) = self.result_receiver.try_recv() {
            self.total_circuits.fetch_add(1, Ordering::Relaxed);
            self.circuits_per_tick += 1;

            match result {
                TestResult::Success | TestResult::BothFailedCompile => {}
                TestResult::KnownError => {
                    self.known_errors.fetch_add(1, Ordering::Relaxed);
                }
                TestResult::CompileMismatch {
                    original_code,
                    rewritten_code,
                    original_compiled,
                    error,
                    job_id,
                } => {
                    self.compile_mismatches.fetch_add(1, Ordering::Relaxed);
                    self.save_compile_mismatch(
                        job_id,
                        &original_code,
                        &rewritten_code,
                        original_compiled,
                        &error,
                    );
                }
                TestResult::OutputMismatch {
                    original_code,
                    rewritten_code,
                    prover_toml,
                    original_output,
                    rewritten_output,
                    job_id,
                } => {
                    self.soundness_bugs.fetch_add(1, Ordering::Relaxed);
                    self.save_soundness_bug(
                        job_id,
                        &original_code,
                        &rewritten_code,
                        &prover_toml,
                        &original_output,
                        &rewritten_output,
                    );
                }
                TestResult::InternalError { message } => {
                    eprintln!("Internal error: {}", message);
                }
            }
        }
    }

    fn save_compile_mismatch(
        &self,
        job_id: u64,
        original: &str,
        rewritten: &str,
        original_compiled: bool,
        error: &str,
    ) {
        let dir = format!("{}/compile_mismatch_{}", self.crash_dir, job_id);
        let _ = fs::create_dir_all(&dir);

        let (passed, failed) =
            if original_compiled { ("original", "rewritten") } else { ("rewritten", "original") };

        let original_path = format!("{}/original.nr", dir);
        let rewritten_path = format!("{}/rewritten.nr", dir);

        let _ = fs::write(&original_path, original);
        let _ = fs::write(&rewritten_path, rewritten);

        // Format the files
        let _ = Command::new("nargo").args(["fmt", &original_path]).output();
        let _ = Command::new("nargo").args(["fmt", &rewritten_path]).output();

        let _ = fs::write(
            format!("{}/info.txt", dir),
            format!("Passed: {}\nFailed: {}\n\nError:\n{}", passed, failed, error),
        );
    }

    fn save_soundness_bug(
        &self,
        job_id: u64,
        original: &str,
        rewritten: &str,
        prover_toml: &str,
        original_output: &str,
        rewritten_output: &str,
    ) {
        let dir = format!("{}/possible_soundness_bug_{}", self.crash_dir, job_id);
        let _ = fs::create_dir_all(&dir);

        let original_path = format!("{}/original.nr", dir);
        let rewritten_path = format!("{}/rewritten.nr", dir);

        let _ = fs::write(&original_path, original);
        let _ = fs::write(&rewritten_path, rewritten);

        // Format the files
        let _ = Command::new("nargo").args(["fmt", &original_path]).output();
        let _ = Command::new("nargo").args(["fmt", &rewritten_path]).output();

        let _ = fs::write(format!("{}/Prover.toml", dir), prover_toml);
        let _ = fs::write(
            format!("{}/divergence.txt", dir),
            format!(
                "Original output:\n{}\n\nRewritten output:\n{}",
                original_output, rewritten_output
            ),
        );
    }

    fn screen(&mut self) -> Result<()> {
        Command::new("clear").status()?;
        print!("{}", self.prelude);

        let total = self.total_circuits.load(Ordering::Relaxed);
        let mismatches = self.compile_mismatches.load(Ordering::Relaxed);
        let bugs = self.soundness_bugs.load(Ordering::Relaxed);
        let known = self.known_errors.load(Ordering::Relaxed);
        let elapsed = self.start_time.elapsed().as_secs();

        print!(
            "[{GREEN}+{RESET}] Circuits: {RED}{}{RESET} | Mismatches: {RED}{}{RESET} | \
             Bugs: {RED}{}{RESET} | Overflow/UB: {RED}{}{RESET} | Rate: {RED}{}{RESET}/s | Time: {RED}{:02}h {:02}m {:02}s{RESET}",
            total,
            mismatches,
            bugs,
            known,
            self.circuits_per_tick,
            elapsed / 3600,
            (elapsed % 3600) / 60,
            elapsed % 60,
        );

        io::stdout().flush()?;
        Ok(())
    }
}

fn worker_loop(receiver: Receiver<TestJob>, sender: Sender<TestResult>, ctx: Context) {
    let builder = CircuitBuilder::default();

    while let Ok(job) = receiver.recv() {
        let result = process_job(&job, &ctx, &builder);
        let _ = sender.send(result);
    }
}

fn process_job(job: &TestJob, ctx: &Context, builder: &CircuitBuilder) -> TestResult {
    // Create temp directory
    let temp_dir = match TempDir::new() {
        Ok(d) => d,
        Err(e) => {
            return TestResult::InternalError { message: format!("TempDir: {}", e) };
        }
    };

    let temp_path = temp_dir.path();
    let orig_dir = temp_path.join("original");
    let rewr_dir = temp_path.join("rewritten");

    // Setup both projects
    if let Err(e) = setup_project(&orig_dir, &job.original_code) {
        return TestResult::InternalError { message: format!("Setup original: {}", e) };
    }
    if let Err(e) = setup_project(&rewr_dir, &job.rewritten_code) {
        return TestResult::InternalError { message: format!("Setup rewritten: {}", e) };
    }

    // Compile both
    let orig_compile = compile_project(&orig_dir);
    let rewr_compile = compile_project(&rewr_dir);

    match (&orig_compile, &rewr_compile) {
        (Ok(()), Ok(())) => {
            // Both compiled - test with random inputs
            let mut random = SmallRng::seed_from_u64(job.seed);

            for _ in 0..job.executions {
                let prover_toml = builder.generate_prover_toml(&mut random, ctx, &job.scope);

                // Write Prover.toml to both
                if fs::write(orig_dir.join("Prover.toml"), &prover_toml).is_err() ||
                    fs::write(rewr_dir.join("Prover.toml"), &prover_toml).is_err()
                {
                    continue;
                }

                // Execute both
                let orig_exec = execute_project(&orig_dir);
                let rewr_exec = execute_project(&rewr_dir);

                match (orig_exec, rewr_exec) {
                    (Ok(orig_out), Ok(rewr_out)) => {
                        // Compare outputs
                        if normalize_output(&orig_out) != normalize_output(&rewr_out) {
                            return TestResult::OutputMismatch {
                                original_code: job.original_code.clone(),
                                rewritten_code: job.rewritten_code.clone(),
                                prover_toml,
                                original_output: orig_out,
                                rewritten_output: rewr_out,
                                job_id: job.job_id,
                            };
                        }
                    }
                    // One succeeded, one failed during execution - also a mismatch
                    (Ok(orig_out), Err(rewr_err)) => {
                        return TestResult::OutputMismatch {
                            original_code: job.original_code.clone(),
                            rewritten_code: job.rewritten_code.clone(),
                            prover_toml,
                            original_output: orig_out,
                            rewritten_output: format!("EXECUTION FAILED: {}", rewr_err),
                            job_id: job.job_id,
                        };
                    }
                    (Err(orig_err), Ok(rewr_out)) => {
                        return TestResult::OutputMismatch {
                            original_code: job.original_code.clone(),
                            rewritten_code: job.rewritten_code.clone(),
                            prover_toml,
                            original_output: format!("EXECUTION FAILED: {}", orig_err),
                            rewritten_output: rewr_out,
                            job_id: job.job_id,
                        };
                    }
                    // Both failed - could be assertion, skip this input
                    (Err(_), Err(_)) => continue,
                }
            }

            TestResult::Success
        }
        (Ok(()), Err(e)) => {
            // Skip known non-bug errors (overflow, underflow, etc.)
            if is_known_error(&e) {
                return TestResult::KnownError;
            }
            TestResult::CompileMismatch {
                original_code: job.original_code.clone(),
                rewritten_code: job.rewritten_code.clone(),
                original_compiled: true,
                error: e.clone(),
                job_id: job.job_id,
            }
        }
        (Err(e), Ok(())) => {
            // Skip known non-bug errors (overflow, underflow, etc.)
            if is_known_error(&e) {
                return TestResult::KnownError;
            }
            TestResult::CompileMismatch {
                original_code: job.original_code.clone(),
                rewritten_code: job.rewritten_code.clone(),
                original_compiled: false,
                error: e.clone(),
                job_id: job.job_id,
            }
        }
        (Err(_), Err(_)) => TestResult::BothFailedCompile,
    }
}

fn setup_project(dir: &std::path::Path, code: &str) -> Result<(), String> {
    fs::create_dir_all(dir).map_err(|e| e.to_string())?;

    // Create Nargo.toml
    let nargo_toml = r#"[package]
name = "circuit"
type = "bin"
authors = [""]
compiler_version = ">=0.30.0"

[dependencies]
"#;
    fs::write(dir.join("Nargo.toml"), nargo_toml).map_err(|e| e.to_string())?;

    // Create src directory and main.nr
    let src_dir = dir.join("src");
    fs::create_dir_all(&src_dir).map_err(|e| e.to_string())?;
    fs::write(src_dir.join("main.nr"), code).map_err(|e| e.to_string())?;

    Ok(())
}

fn compile_project(dir: &std::path::Path) -> Result<(), String> {
    let output = Command::new("nargo")
        .args(["check", "--silence-warnings"])
        .current_dir(dir)
        .output()
        .map_err(|e| format!("Failed to spawn nargo: {}", e))?;

    if output.status.success() {
        Ok(())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

/// Check if error message indicates a known non-bug (overflow, underflow, etc.)
fn is_known_error(error: &str) -> bool {
    let error_lower = error.to_lowercase();
    error_lower.contains("overflow") ||
        error_lower.contains("underflow") ||
        error_lower.contains("attempt to") ||
        error_lower.contains("division by zero") ||
        error_lower.contains("modulo by zero")
}

fn execute_project(dir: &std::path::Path) -> Result<String, String> {
    let output = Command::new("nargo")
        .args(["execute", "--silence-warnings"])
        .current_dir(dir)
        .output()
        .map_err(|e| format!("Failed to spawn nargo: {}", e))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

/// Normalize output for comparison (ignore paths and timestamps)
fn normalize_output(output: &str) -> String {
    output
        .lines()
        .filter_map(|line| {
            if line.contains("Witness saved to") {
                Some("[circuit] Witness saved")
            } else if line.starts_with("[circuit]") {
                Some(line)
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
