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
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
    thread,
    time::{Duration, Instant},
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
    /// Whether to run the slower prover/verifier stages
    run_later_stages: bool,
}

/// Shared state for power schedule timing across workers
struct PowerSchedule {
    /// Total time spent on initial stages (compilation + witness generation)
    t1_nanos: AtomicU64,
    /// Total time spent on later stages (proof generation + verification)
    t2_nanos: AtomicU64,
    /// Target ratio: only run later stages if T2/(T1+T2) < target_ratio
    target_ratio: f64,
}

impl PowerSchedule {
    fn new(target_ratio: f64) -> Self {
        Self { t1_nanos: AtomicU64::new(0), t2_nanos: AtomicU64::new(0), target_ratio }
    }

    /// Add time spent on initial stages
    fn add_t1(&self, duration: Duration) {
        self.t1_nanos.fetch_add(duration.as_nanos() as u64, Ordering::Relaxed);
    }

    /// Add time spent on later stages
    fn add_t2(&self, duration: Duration) {
        self.t2_nanos.fetch_add(duration.as_nanos() as u64, Ordering::Relaxed);
    }

    /// Check if we should run the later (slower) stages based on current ratio
    fn should_run_later_stages(&self) -> bool {
        let t1 = self.t1_nanos.load(Ordering::Relaxed) as f64;
        let t2 = self.t2_nanos.load(Ordering::Relaxed) as f64;

        // Avoid division by zero - always run later stages initially
        if t1 + t2 == 0.0 {
            return true;
        }

        let current_ratio = t2 / (t1 + t2);
        current_ratio < self.target_ratio
    }

    /// Get current ratio for display
    fn current_ratio(&self) -> f64 {
        let t1 = self.t1_nanos.load(Ordering::Relaxed) as f64;
        let t2 = self.t2_nanos.load(Ordering::Relaxed) as f64;

        if t1 + t2 == 0.0 {
            0.0
        } else {
            t2 / (t1 + t2)
        }
    }
}

/// Result of testing a circuit pair
enum TestResult {
    /// Both compiled and all executions matched
    Success {
        /// Time spent on initial stages (compilation + witness generation)
        t1: Duration,
        /// Time spent on later stages (proof generation + verification)
        t2: Duration,
    },
    /// One compiled, the other didn't
    CompileMismatch {
        original_code: String,
        rewritten_code: String,
        original_compiled: bool,
        error: String,
        job_id: u64,
        t1: Duration,
    },
    /// Both compiled but outputs diverged
    OutputMismatch {
        original_code: String,
        rewritten_code: String,
        prover_toml: String,
        original_output: String,
        rewritten_output: String,
        job_id: u64,
        t1: Duration,
    },
    /// Proof verification mismatch (one succeeded, one failed)
    ProofMismatch {
        original_code: String,
        rewritten_code: String,
        prover_toml: String,
        original_result: String,
        rewritten_result: String,
        job_id: u64,
        t1: Duration,
        t2: Duration,
    },
    /// Both failed to compile (not interesting)
    BothFailedCompile { t1: Duration },
    /// Known error (overflow, underflow, etc.) - not a bug
    KnownError { t1: Duration },
    /// Internal error (temp dir, etc.)
    InternalError { message: String, t1: Duration },
}

pub struct App {
    prelude: String,
    ctx: Context,
    crash_dir: String,
    executions: usize,
    job_sender: Sender<TestJob>,
    result_receiver: Receiver<TestResult>,

    // Power schedule
    power_schedule: Arc<PowerSchedule>,

    // Stats
    total_circuits: AtomicU64,
    compile_mismatches: AtomicU64,
    soundness_bugs: AtomicU64,
    proof_mismatches: AtomicU64,
    known_errors: AtomicU64,
    later_stages_run: AtomicU64,
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
        target_ratio: f64,
    ) -> Result<Self> {
        let num_workers = thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
        let power_schedule = Arc::new(PowerSchedule::new(target_ratio));

        let (job_sender, job_receiver) = bounded::<TestJob>(num_workers * 2);
        let (result_sender, result_receiver) = bounded::<TestResult>(num_workers * 2);

        // Spawn worker threads
        for i in 0..num_workers {
            let receiver = job_receiver.clone();
            let sender = result_sender.clone();

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
            power_schedule,
            total_circuits: AtomicU64::new(0),
            compile_mismatches: AtomicU64::new(0),
            soundness_bugs: AtomicU64::new(0),
            proof_mismatches: AtomicU64::new(0),
            known_errors: AtomicU64::new(0),
            later_stages_run: AtomicU64::new(0),
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

            // Generate circuit (forest + scope)
            let (forest, scope) = builder.generate(random, &self.ctx);
            let original_code = builder.format_circuit(&forest, &scope);

            // Apply N random rewrites
            let mut rewritten_forest = forest.clone();
            let rewrite_count =
                random.random_range(self.ctx.min_rewrites_count..=self.ctx.max_rewrites_count);
            for _ in 0..rewrite_count {
                rewriter.apply_random(random, &mut rewritten_forest, &self.ctx, &scope);
            }
            let rewritten_code = builder.format_circuit(&rewritten_forest, &scope);

            // Check power schedule to decide if we should run later stages
            let run_later_stages = self.power_schedule.should_run_later_stages();

            // Send job
            let job = TestJob {
                original_code,
                rewritten_code,
                scope,
                executions: self.executions,
                job_id,
                seed: random.random(),
                run_later_stages,
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
                TestResult::Success { t1, t2 } => {
                    self.power_schedule.add_t1(t1);
                    self.power_schedule.add_t2(t2);
                    if t2 > Duration::ZERO {
                        self.later_stages_run.fetch_add(1, Ordering::Relaxed);
                    }
                }
                TestResult::BothFailedCompile { t1 } => {
                    self.power_schedule.add_t1(t1);
                }
                TestResult::KnownError { t1 } => {
                    self.power_schedule.add_t1(t1);
                    self.known_errors.fetch_add(1, Ordering::Relaxed);
                }
                TestResult::CompileMismatch {
                    original_code,
                    rewritten_code,
                    original_compiled,
                    error,
                    job_id,
                    t1,
                } => {
                    self.power_schedule.add_t1(t1);
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
                    t1,
                } => {
                    self.power_schedule.add_t1(t1);
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
                TestResult::ProofMismatch {
                    original_code,
                    rewritten_code,
                    prover_toml,
                    original_result,
                    rewritten_result,
                    job_id,
                    t1,
                    t2,
                } => {
                    self.power_schedule.add_t1(t1);
                    self.power_schedule.add_t2(t2);
                    self.later_stages_run.fetch_add(1, Ordering::Relaxed);
                    self.proof_mismatches.fetch_add(1, Ordering::Relaxed);
                    self.save_proof_mismatch(
                        job_id,
                        &original_code,
                        &rewritten_code,
                        &prover_toml,
                        &original_result,
                        &rewritten_result,
                    );
                }
                TestResult::InternalError { message, t1 } => {
                    self.power_schedule.add_t1(t1);
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

    fn save_proof_mismatch(
        &self,
        job_id: u64,
        original: &str,
        rewritten: &str,
        prover_toml: &str,
        original_result: &str,
        rewritten_result: &str,
    ) {
        let dir = format!("{}/proof_mismatch_{}", self.crash_dir, job_id);
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
            format!("{}/proof_divergence.txt", dir),
            format!(
                "Original prove/verify result:\n{}\n\nRewritten prove/verify result:\n{}",
                original_result, rewritten_result
            ),
        );
    }

    fn screen(&mut self) -> Result<()> {
        Command::new("clear").status()?;
        print!("{}", self.prelude);

        let total = self.total_circuits.load(Ordering::Relaxed);
        let mismatches = self.compile_mismatches.load(Ordering::Relaxed);
        let bugs = self.soundness_bugs.load(Ordering::Relaxed);
        let proof_mismatches = self.proof_mismatches.load(Ordering::Relaxed);
        let known = self.known_errors.load(Ordering::Relaxed);
        let later_run = self.later_stages_run.load(Ordering::Relaxed);
        let current_ratio = self.power_schedule.current_ratio();
        let elapsed = self.start_time.elapsed().as_secs();

        print!(
            "[{GREEN}+{RESET}] Circuits: {RED}{}{RESET} | Mismatches: {RED}{}{RESET} | \
             Bugs: {RED}{}{RESET} | Proof: {RED}{}{RESET} | Overflow/UB: {RED}{}{RESET} | \
             Rate: {RED}{}{RESET}/s | Time: {RED}{:02}h {:02}m {:02}s{RESET}\n\
             [{GREEN}+{RESET}] Power Schedule: ρ={RED}{:.3}{RESET} | Later stages run: {RED}{}{RESET}",
            total,
            mismatches,
            bugs,
            proof_mismatches,
            known,
            self.circuits_per_tick,
            elapsed / 3600,
            (elapsed % 3600) / 60,
            elapsed % 60,
            current_ratio,
            later_run,
        );

        io::stdout().flush()?;
        Ok(())
    }
}

fn worker_loop(receiver: Receiver<TestJob>, sender: Sender<TestResult>, ctx: Context) {
    while let Ok(job) = receiver.recv() {
        let result = process_job(&job, &ctx);
        let _ = sender.send(result);
    }
}

fn process_job(job: &TestJob, ctx: &Context) -> TestResult {
    let builder = CircuitBuilder::default();
    let mut t1 = Duration::ZERO;
    let mut t2 = Duration::ZERO;

    // Create temp directory
    let temp_dir = match TempDir::new() {
        Ok(d) => d,
        Err(e) => {
            return TestResult::InternalError { message: format!("TempDir: {}", e), t1 };
        }
    };

    let temp_path = temp_dir.path();
    let orig_dir = temp_path.join("original");
    let rewr_dir = temp_path.join("rewritten");

    // Setup both projects
    if let Err(e) = setup_project(&orig_dir, &job.original_code) {
        return TestResult::InternalError { message: format!("Setup original: {}", e), t1 };
    }
    if let Err(e) = setup_project(&rewr_dir, &job.rewritten_code) {
        return TestResult::InternalError { message: format!("Setup rewritten: {}", e), t1 };
    }

    // Initial stages (T1): Compile both
    let compile_start = Instant::now();
    let orig_compile = compile_project(&orig_dir);
    let rewr_compile = compile_project(&rewr_dir);
    t1 += compile_start.elapsed();

    match (&orig_compile, &rewr_compile) {
        (Ok(()), Ok(())) => {
            // Both compiled - test with random inputs
            let mut random = SmallRng::seed_from_u64(job.seed);
            let mut last_successful_prover_toml = String::new();

            for _ in 0..job.executions {
                let prover_toml = builder.generate_prover_toml(&mut random, ctx, &job.scope);

                // Write Prover.toml to both
                if fs::write(orig_dir.join("Prover.toml"), &prover_toml).is_err() ||
                    fs::write(rewr_dir.join("Prover.toml"), &prover_toml).is_err()
                {
                    continue;
                }

                // Initial stages (T1): Execute (witness generation)
                let exec_start = Instant::now();
                let orig_exec = execute_project(&orig_dir);
                let rewr_exec = execute_project(&rewr_dir);
                t1 += exec_start.elapsed();

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
                                t1,
                            };
                        }
                        // Save successful prover_toml for later stages
                        last_successful_prover_toml = prover_toml;
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
                            t1,
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
                            t1,
                        };
                    }
                    // Both failed - could be assertion, skip this input
                    (Err(_), Err(_)) => continue,
                }
            }

            // Later stages (T2): Proof generation and verification
            // Only run if power schedule allows and we have a successful input
            if job.run_later_stages && !last_successful_prover_toml.is_empty() {
                let prove_start = Instant::now();

                // Generate proofs for both
                let orig_prove = prove_project(&orig_dir);
                let rewr_prove = prove_project(&rewr_dir);

                match (&orig_prove, &rewr_prove) {
                    (Ok(_), Ok(_)) => {
                        // Both generated proofs - now verify
                        let orig_verify = verify_project(&orig_dir);
                        let rewr_verify = verify_project(&rewr_dir);
                        t2 += prove_start.elapsed();

                        match (&orig_verify, &rewr_verify) {
                            (Ok(_), Ok(_)) => {
                                // Both verified successfully
                            }
                            (Ok(_), Err(e)) => {
                                return TestResult::ProofMismatch {
                                    original_code: job.original_code.clone(),
                                    rewritten_code: job.rewritten_code.clone(),
                                    prover_toml: last_successful_prover_toml,
                                    original_result: "Verification succeeded".to_string(),
                                    rewritten_result: format!("Verification failed: {}", e),
                                    job_id: job.job_id,
                                    t1,
                                    t2,
                                };
                            }
                            (Err(e), Ok(_)) => {
                                return TestResult::ProofMismatch {
                                    original_code: job.original_code.clone(),
                                    rewritten_code: job.rewritten_code.clone(),
                                    prover_toml: last_successful_prover_toml,
                                    original_result: format!("Verification failed: {}", e),
                                    rewritten_result: "Verification succeeded".to_string(),
                                    job_id: job.job_id,
                                    t1,
                                    t2,
                                };
                            }
                            (Err(_), Err(_)) => {
                                // Both failed verification - not interesting
                            }
                        }
                    }
                    (Ok(_), Err(e)) => {
                        t2 += prove_start.elapsed();
                        return TestResult::ProofMismatch {
                            original_code: job.original_code.clone(),
                            rewritten_code: job.rewritten_code.clone(),
                            prover_toml: last_successful_prover_toml,
                            original_result: "Proof generation succeeded".to_string(),
                            rewritten_result: format!("Proof generation failed: {}", e),
                            job_id: job.job_id,
                            t1,
                            t2,
                        };
                    }
                    (Err(e), Ok(_)) => {
                        t2 += prove_start.elapsed();
                        return TestResult::ProofMismatch {
                            original_code: job.original_code.clone(),
                            rewritten_code: job.rewritten_code.clone(),
                            prover_toml: last_successful_prover_toml,
                            original_result: format!("Proof generation failed: {}", e),
                            rewritten_result: "Proof generation succeeded".to_string(),
                            job_id: job.job_id,
                            t1,
                            t2,
                        };
                    }
                    (Err(_), Err(_)) => {
                        t2 += prove_start.elapsed();
                        // Both failed proof generation - not interesting
                    }
                }
            }

            TestResult::Success { t1, t2 }
        }
        (Ok(()), Err(e)) => {
            // Skip known non-bug errors (overflow, underflow, etc.)
            if is_known_error(&e) {
                return TestResult::KnownError { t1 };
            }
            TestResult::CompileMismatch {
                original_code: job.original_code.clone(),
                rewritten_code: job.rewritten_code.clone(),
                original_compiled: true,
                error: e.clone(),
                job_id: job.job_id,
                t1,
            }
        }
        (Err(e), Ok(())) => {
            // Skip known non-bug errors (overflow, underflow, etc.)
            if is_known_error(&e) {
                return TestResult::KnownError { t1 };
            }
            TestResult::CompileMismatch {
                original_code: job.original_code.clone(),
                rewritten_code: job.rewritten_code.clone(),
                original_compiled: false,
                error: e.clone(),
                job_id: job.job_id,
                t1,
            }
        }
        (Err(_), Err(_)) => TestResult::BothFailedCompile { t1 },
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

fn prove_project(dir: &std::path::Path) -> Result<String, String> {
    let output = Command::new("nargo")
        .args(["prove", "--silence-warnings"])
        .current_dir(dir)
        .output()
        .map_err(|e| format!("Failed to spawn nargo: {}", e))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

fn verify_project(dir: &std::path::Path) -> Result<String, String> {
    let output = Command::new("nargo")
        .args(["verify", "--silence-warnings"])
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
