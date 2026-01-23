use crate::{
    commands::{
        bb_prove, bb_verify, compile_project, execute_project, is_expected_execution_error,
        setup_project,
    },
    constants::{GREEN, RED, RESET},
    scheduler::PowerScheduler,
};
use anyhow::Result;
use fuzztools::{
    builders::CircuitBuilder,
    circuits::{context::Context, rewriter::Rewriter, scope::Scope},
};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::{
    collections::HashMap,
    fs,
    io::{self, Write},
    process::Command,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::sync::{
    mpsc::{channel, Receiver, Sender},
    Semaphore,
};

pub(crate) struct TestJob {
    original_code: String,
    rewritten_code: String,
    scope: Scope,
    executions: usize,
    job_id: u64,
    seed: u64,
    run_later_stages: bool,
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
    CompilationMismatch {
        job_id: u64,
        original_code: String,
        rewritten_code: String,
        original_compiled: bool,
        error: String,
        t1: Duration,
    },
    /// Both compiled but outputs diverged
    ExecutionMismatch {
        original_code: String,
        rewritten_code: String,
        prover_toml: String,
        original_output: String,
        rewritten_output: String,
        job_id: u64,
        t1: Duration,
    },
    /// Proof verification mismatch
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
    /// Verification mismatch
    VerificationMismatch {
        original_code: String,
        rewritten_code: String,
        prover_toml: String,
        original_result: String,
        rewritten_result: String,
        job_id: u64,
        t1: Duration,
        t2: Duration,
    },

    NotInteresting { t1: Duration, t2: Option<Duration>, is_proof: bool, is_verification: bool},

    /// Known error (overflow, underflow, etc.) - not a bug
    KnownError { t1: Duration },
}

pub(crate) struct App {
    prelude: String,
    ctx: Context,
    crash_dir: String,
    executions: usize,
    job_sender: Sender<TestJob>,
    job_receiver: Option<Receiver<TestJob>>,
    result_sender: Sender<TestResult>,
    result_receiver: Receiver<TestResult>,
    error_signal: Arc<AtomicBool>,
    error_sender: Sender<String>,
    error_receiver: Option<Receiver<String>>,
    worker_semaphore: Arc<Semaphore>,

    // Power schedule
    power_schedule: Arc<PowerScheduler>,

    // Stats
    total_circuits: AtomicU64,
    compile_mismatches: AtomicU64,
    soundness_bugs: AtomicU64,
    proof_mismatches: AtomicU64,
    verification_mismatches: AtomicU64,
    known_errors: AtomicU64,
    total_proofs: AtomicU64,
    total_verifications: AtomicU64,
    circuits_per_tick: u64,
    start_time: Instant,
    last_update: Instant,

    // Rule stats: rule_name -> application_count
    rule_stats: HashMap<String, u64>,
}

impl App {
    pub(crate) fn new(
        ctx: Context,
        executions: usize,
        prelude: String,
        crash_dir: String,
        target_ratio: f64,
        workers: usize,
    ) -> Result<Self> {
        let power_schedule = Arc::new(PowerScheduler::new(target_ratio));
        let worker_semaphore = Arc::new(Semaphore::new(workers));

        let (job_sender, job_receiver) = channel::<TestJob>(workers * 2);
        let (result_sender, result_receiver) = channel::<TestResult>(workers * 2);
        let (error_sender, error_receiver) = channel::<String>(workers * 2);

        fs::create_dir_all(&crash_dir)?;

        // Create worker directories upfront
        for i in 0..workers {
            let worker_dir = format!("./tmp/worker_{}", i);
            let _ = fs::remove_dir_all(&worker_dir);
            fs::create_dir_all(format!("{}/original/src", worker_dir))?;
            fs::create_dir_all(format!("{}/rewritten/src", worker_dir))?;
        }

        Ok(Self {
            prelude,
            ctx,
            crash_dir,
            executions,
            job_sender,
            job_receiver: Some(job_receiver),
            result_sender,
            result_receiver,
            error_signal: Arc::new(AtomicBool::new(false)),
            error_sender,
            error_receiver: Some(error_receiver),
            worker_semaphore,
            power_schedule,
            total_circuits: AtomicU64::new(0),
            compile_mismatches: AtomicU64::new(0),
            soundness_bugs: AtomicU64::new(0),
            proof_mismatches: AtomicU64::new(0),
            verification_mismatches: AtomicU64::new(0),
            known_errors: AtomicU64::new(0),
            total_proofs: AtomicU64::new(0),
            total_verifications: AtomicU64::new(0),
            circuits_per_tick: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
            rule_stats: HashMap::new(),
        })
    }

    pub(crate) async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        // This will spawn a thread that will be listening for incoming circuits and doing all the
        // heavy work of compiling, executing, proving, and verifying them
        self.spawn_dispatcher();

        let builder = CircuitBuilder::default();
        let rewriter = Rewriter::default();
        let mut job_id = 0u64;
        let mut error_receiver = self.error_receiver.take().unwrap();

        loop {
            // If there was an error in one of the workers, we stop the main loop and print the
            // error
            if self.error_signal.load(Ordering::Relaxed) {
                let error = error_receiver.recv().await.unwrap();
                eprintln!("\n\n\x1b[1;31m[!] Error: {error}\x1b[0m");
                break;
            }

            self.drain_results();

            // Generate circuit (forest + scope)
            let (mut forest, scope) = builder.generate(random, &self.ctx);
            let original_code = builder.format_circuit(&scope, &forest);

            let rewrite_count =
                random.random_range(self.ctx.min_rewrites_count..=self.ctx.max_rewrites_count);
            for _ in 0..rewrite_count {
                if let Some(rule_name) =
                    rewriter.apply_random(random, &mut forest, &self.ctx, &scope)
                {
                    *self.rule_stats.entry(rule_name).or_insert(0) += 1;
                }
            }

            let rewritten_code = builder.format_circuit(&scope, &forest);
            // Check power schedule to decide if we should run later stages, or if we always run
            // them
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

            // Send job while draining results periodically
            let sender = self.job_sender.clone();
            let send_fut = sender.send(job);
            tokio::pin!(send_fut);

            loop {
                tokio::select! {
                    result = &mut send_fut => {
                        if result.is_err() {
                            return Ok(());
                        }
                        break;
                    }
                    _ = tokio::time::sleep(Duration::from_millis(50)) => {
                        self.drain_results();
                        if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                            self.screen()?;
                            self.circuits_per_tick = 0;
                            self.last_update = Instant::now();
                        }
                    }
                }
            }

            job_id += 1;
        }

        Ok(())
    }

    fn spawn_dispatcher(&mut self) {
        let mut rx = self.job_receiver.take().unwrap();
        let tx = self.result_sender.clone();
        let signal = self.error_signal.clone();
        let error_sender = self.error_sender.clone();
        let ctx = self.ctx;
        let semaphore = self.worker_semaphore.clone();

        tokio::spawn(async move {
            while let Some(job) = rx.recv().await {
                if signal.load(Ordering::Relaxed) {
                    break;
                }

                let worker_error_sender = error_sender.clone();
                let worker_tx = tx.clone();
                let worker_signal = signal.clone();
                let worker_ctx = ctx;

                let permit = semaphore.clone().acquire_owned().await.unwrap();

                tokio::spawn(async move {
                    let _permit = permit;
                    let builder = CircuitBuilder::default();
                    let mut t1 = Duration::ZERO;
                    let mut t2 = Duration::ZERO;

                    let temp_path = std::path::PathBuf::from(format!("./tmp/job_{}", job.job_id));
                    let orig_dir = temp_path.join("original");
                    let rewr_dir = temp_path.join("rewritten");

                    if let Err(e) = setup_project(&orig_dir, &job.original_code).await {
                        worker_signal.store(true, Ordering::Relaxed);
                        let _ =
                            worker_error_sender.send(format!("Setup original error: {}", e)).await;
                        return;
                    }

                    if let Err(e) = setup_project(&rewr_dir, &job.rewritten_code).await {
                        worker_signal.store(true, Ordering::Relaxed);
                        let _ =
                            worker_error_sender.send(format!("Setup rewritten error: {}", e)).await;
                        return;
                    }

                    // Initial stages (T1): Compile both
                    let compile_start = Instant::now();
                    let orig_compile = compile_project(&orig_dir).await;
                    let rewr_compile = compile_project(&rewr_dir).await;
                    t1 += compile_start.elapsed();

                    match (orig_compile, rewr_compile) {
                        // Both compiled - test with random inputs
                        (Ok(()), Ok(())) => {
                            let mut seeded_random = SmallRng::seed_from_u64(job.seed);
                            let mut found_bug = false;

                            'executions: for _ in 0..job.executions {
                                let prover_toml = builder.generate_prover_toml(
                                    &mut seeded_random,
                                    &worker_ctx,
                                    &job.scope,
                                );

                                if let Err(e) =
                                    fs::write(orig_dir.join("Prover.toml"), &prover_toml)
                                {
                                    worker_signal.store(true, Ordering::Relaxed);
                                    let _ = worker_error_sender
                                        .send(format!("Write Prover.toml error: {}", e))
                                        .await;
                                    break 'executions;
                                }
                                if let Err(e) =
                                    fs::write(rewr_dir.join("Prover.toml"), &prover_toml)
                                {
                                    worker_signal.store(true, Ordering::Relaxed);
                                    let _ = worker_error_sender
                                        .send(format!("Write Prover.toml error: {}", e))
                                        .await;
                                    break 'executions;
                                }

                                // Initial stages (T1)): Execute (witness generation)
                                let exec_start = Instant::now();
                                let orig_exec = execute_project(&orig_dir).await;
                                let rewr_exec = execute_project(&rewr_dir).await;
                                t1 += exec_start.elapsed();

                                match (&orig_exec, &rewr_exec) {
                                    (Ok(orig_out), Ok(rewr_out)) => {
                                        // Both succeeded - compare outputs
                                        let (orig_success, orig_result) =
                                            Self::parse_output(orig_out);
                                        let (rewr_success, rewr_result) =
                                            Self::parse_output(rewr_out);

                                        if orig_success != rewr_success ||
                                            orig_result != rewr_result
                                        {
                                            let _ = worker_tx
                                                .send(TestResult::ExecutionMismatch {
                                                    original_code: job.original_code.clone(),
                                                    rewritten_code: job.rewritten_code.clone(),
                                                    prover_toml: prover_toml.clone(),
                                                    original_output: orig_out.clone(),
                                                    rewritten_output: rewr_out.clone(),
                                                    job_id: job.job_id,
                                                    t1,
                                                })
                                                .await;
                                            found_bug = true;
                                            break 'executions;
                                        }

                                        // If we should run later stages, prove and verify this
                                        // execution
                                        if job.run_later_stages {
                                            // Later stages (T2): Proof generation
                                            let proof_start = Instant::now();
                                            let orig_proof = bb_prove(&orig_dir).await;
                                            let rewr_proof = bb_prove(&rewr_dir).await;
                                            t2 += proof_start.elapsed();

                                            match (orig_proof, rewr_proof) {
                                                (Ok(()), Ok(())) => {
                                                    // Both proofs succeeded - verify
                                                    let verify_start = Instant::now();
                                                    let orig_verify = bb_verify(&orig_dir).await;
                                                    let rewr_verify = bb_verify(&rewr_dir).await;
                                                    t2 += verify_start.elapsed();

                                                    match (orig_verify, rewr_verify) {
                                                        (Ok(()), Ok(())) => {
                                                            // Both verified successfully - continue
                                                        }
                                                        (Ok(()), Err(e)) => {
                                                            let _ = worker_tx.send(TestResult::VerificationMismatch {
                                                                original_code: job.original_code.clone(),
                                                                rewritten_code: job.rewritten_code.clone(),
                                                                prover_toml: prover_toml.clone(),
                                                                original_result: "Verification succeeded".to_string(),
                                                                rewritten_result: format!("Verification failed: {}", e),
                                                                job_id: job.job_id,
                                                                t1,
                                                                t2,
                                                            }).await;
                                                            found_bug = true;
                                                            break 'executions;
                                                        }
                                                        (Err(e), Ok(())) => {
                                                            let _ = worker_tx.send(TestResult::VerificationMismatch {
                                                                original_code: job.original_code.clone(),
                                                                rewritten_code: job.rewritten_code.clone(),
                                                                prover_toml: prover_toml.clone(),
                                                                original_result: format!("Verification failed: {}", e),
                                                                rewritten_result: "Verification succeeded".to_string(),
                                                                job_id: job.job_id,
                                                                t1,
                                                                t2,
                                                            }).await;
                                                            found_bug = true;
                                                            break 'executions;
                                                        }
                                                        _ => {
                                                            let _ = worker_tx
                                                                .send(TestResult::NotInteresting {
                                                                    t1,
                                                                    t2: Some(t2),
                                                                    is_proof: false,
                                                                    is_verification: true
                                                                })
                                                                .await; // @todo shuld be a bug?
                                                        }
                                                    }
                                                }
                                                (Ok(()), Err(e)) => {
                                                    let _ = worker_tx
                                                        .send(TestResult::ProofMismatch {
                                                            original_code: job
                                                                .original_code
                                                                .clone(),
                                                            rewritten_code: job
                                                                .rewritten_code
                                                                .clone(),
                                                            prover_toml: prover_toml.clone(),
                                                            original_result:
                                                                "Proof generation succeeded"
                                                                    .to_string(),
                                                            rewritten_result: format!(
                                                                "Proof generation failed: {}",
                                                                e
                                                            ),
                                                            job_id: job.job_id,
                                                            t1,
                                                            t2,
                                                        })
                                                        .await;
                                                    found_bug = true;
                                                    break 'executions;
                                                }
                                                (Err(e), Ok(())) => {
                                                    let _ = worker_tx
                                                        .send(TestResult::ProofMismatch {
                                                            original_code: job
                                                                .original_code
                                                                .clone(),
                                                            rewritten_code: job
                                                                .rewritten_code
                                                                .clone(),
                                                            prover_toml: prover_toml.clone(),
                                                            original_result: format!(
                                                                "Proof generation failed: {}",
                                                                e
                                                            ),
                                                            rewritten_result:
                                                                "Proof generation succeeded"
                                                                    .to_string(),
                                                            job_id: job.job_id,
                                                            t1,
                                                            t2,
                                                        })
                                                        .await;
                                                    found_bug = true;
                                                    break 'executions;
                                                }
                                                _ => {
                                                    let _ = worker_tx
                                                        .send(TestResult::NotInteresting {
                                                            t1,
                                                            t2: Some(t2),
                                                            is_proof: true,
                                                            is_verification: false
                                                        })
                                                        .await;
                                                }
                                            }
                                        }
                                    }
                                    (Ok(orig_out), Err(rewr_err)) => {
                                        if is_expected_execution_error(rewr_err) {
                                            let _ =
                                                worker_tx.send(TestResult::KnownError { t1 }).await;
                                        } else {
                                            let _ = worker_tx
                                                .send(TestResult::ExecutionMismatch {
                                                    original_code: job.original_code.clone(),
                                                    rewritten_code: job.rewritten_code.clone(),
                                                    prover_toml,
                                                    original_output: orig_out.clone(),
                                                    rewritten_output: format!(
                                                        "UNKNOWN EXECUTION ERROR: {}",
                                                        rewr_err
                                                    ),
                                                    job_id: job.job_id,
                                                    t1,
                                                })
                                                .await;
                                            found_bug = true;
                                            break 'executions;
                                        }
                                    }
                                    _ => {
                                        let _ = worker_tx.send(TestResult::NotInteresting { t1, t2: None, is_proof: false, is_verification: false });
                                    }
                                }
                            }

                            // Only send Success if no bug was found
                            if !found_bug {
                                let _ = worker_tx.send(TestResult::Success { t1, t2 }).await;
                            }
                        }
                        // One compiled, one didn't
                        (Ok(()), Err(e)) => {
                            if is_expected_execution_error(&e) {
                                let _ = worker_tx.send(TestResult::KnownError { t1 }).await;
                            } else {
                                let _ = worker_tx
                                    .send(TestResult::CompilationMismatch {
                                        original_code: job.original_code.clone(),
                                        rewritten_code: job.rewritten_code.clone(),
                                        original_compiled: true,
                                        error: e,
                                        job_id: job.job_id,
                                        t1,
                                    })
                                    .await;
                            }
                        }
                        _ => {
                            let _ = worker_tx.send(TestResult::NotInteresting { t1, t2: None, is_proof: false, is_verification: false });
                        }
                    }

                    // Clean up
                    let _ = fs::remove_dir_all(&temp_path);
                });
            }
        });
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
                        self.total_proofs.fetch_add(1, Ordering::Relaxed);
                        self.total_verifications.fetch_add(1, Ordering::Relaxed);
                    }
                }
                TestResult::CompilationMismatch {
                    job_id,
                    original_code,
                    rewritten_code,
                    original_compiled,
                    error,
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
                TestResult::ExecutionMismatch {
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
                    self.total_proofs.fetch_add(1, Ordering::Relaxed);
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
                TestResult::VerificationMismatch {
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
                    self.total_proofs.fetch_add(1, Ordering::Relaxed);
                    self.total_verifications.fetch_add(1, Ordering::Relaxed);
                    self.verification_mismatches.fetch_add(1, Ordering::Relaxed);
                    self.save_verification_mismatch(
                        job_id,
                        &original_code,
                        &rewritten_code,
                        &prover_toml,
                        &original_result,
                        &rewritten_result,
                    );
                }
                TestResult::NotInteresting { t1, t2, is_proof, is_verification } => {
                    self.power_schedule.add_t1(t1);
                    if let Some(t2) = t2 {
                        self.power_schedule.add_t2(t2);
                    }

                    if is_proof {
                        self.total_proofs.fetch_add(1, Ordering::Relaxed);
                    }

                    if is_verification {
                        self.total_proofs.fetch_add(1, Ordering::Relaxed);
                        self.total_verifications.fetch_add(1, Ordering::Relaxed);
                    }
                }
                TestResult::KnownError { t1 } => {
                    self.power_schedule.add_t1(t1);
                    self.known_errors.fetch_add(1, Ordering::Relaxed);
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

        let _ = fs::write(format!("{}/original.nr", dir), original);
        let _ = fs::write(format!("{}/rewritten.nr", dir), rewritten);
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

        let _ = fs::write(format!("{}/original.nr", dir), original);
        let _ = fs::write(format!("{}/rewritten.nr", dir), rewritten);
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

        let _ = fs::write(format!("{}/original.nr", dir), original);
        let _ = fs::write(format!("{}/rewritten.nr", dir), rewritten);
        let _ = fs::write(format!("{}/Prover.toml", dir), prover_toml);
        let _ = fs::write(
            format!("{}/proof_divergence.txt", dir),
            format!(
                "Original proof result:\n{}\n\nRewritten proof result:\n{}",
                original_result, rewritten_result
            ),
        );
    }

    fn save_verification_mismatch(
        &self,
        job_id: u64,
        original: &str,
        rewritten: &str,
        prover_toml: &str,
        original_result: &str,
        rewritten_result: &str,
    ) {
        let dir = format!("{}/verification_mismatch_{}", self.crash_dir, job_id);
        let _ = fs::create_dir_all(&dir);

        let _ = fs::write(format!("{}/original.nr", dir), original);
        let _ = fs::write(format!("{}/rewritten.nr", dir), rewritten);
        let _ = fs::write(format!("{}/Prover.toml", dir), prover_toml);
        let _ = fs::write(
            format!("{}/verification_divergence.txt", dir),
            format!(
                "Original verification result:\n{}\n\nRewritten verification result:\n{}",
                original_result, rewritten_result
            ),
        );
    }

    fn parse_output(output: &str) -> (bool, String) {
        let mut success = false;
        let mut result = String::new();
        for line in output.lines() {
            if line.contains("witness successfully solved") {
                success = true;
            } else if line.starts_with("Circuit output") {
                if let Some(idx) = line.find(':') {
                    let (_, val) = line.split_at(idx + 1);
                    result.push_str(val.trim_start());
                }
            }
        }
        (success, result)
    }

    fn screen(&self) -> Result<()> {
        Command::new("clear").status()?;
        print!("{}", self.prelude);

        let total = self.total_circuits.load(Ordering::Relaxed);
        let mismatches = self.compile_mismatches.load(Ordering::Relaxed);
        let soundness = self.soundness_bugs.load(Ordering::Relaxed);
        let proof = self.proof_mismatches.load(Ordering::Relaxed);
        let known = self.known_errors.load(Ordering::Relaxed);
        let proofs = self.total_proofs.load(Ordering::Relaxed);
        let verifications = self.total_verifications.load(Ordering::Relaxed);
        let current_ratio = self.power_schedule.current_ratio();
        let elapsed = self.start_time.elapsed().as_secs();

        println!(
            "[{GREEN}+{RESET}] Circuits: {RED}{}{RESET} | Mismatches: {RED}{}{RESET} | \
             Soundness: {RED}{}{RESET} | Proof: {RED}{}{RESET} | Known: {RED}{}{RESET} | \
             Rate: {RED}{}{RESET}/s | Time: {RED}{:02}h {:02}m {:02}s{RESET}",
            total,
            mismatches,
            soundness,
            proof,
            known,
            self.circuits_per_tick,
            elapsed / 3600,
            (elapsed % 3600) / 60,
            elapsed % 60,
        );
        println!(
            "[{GREEN}+{RESET}] Power Schedule: p={RED}{:.6}{RESET} | Proofs: {RED}{}{RESET} | Verifications: {RED}{}{RESET}",
            current_ratio, proofs, verifications,
        );

        // Display rule stats in order defined in RULES
        println!("[{GREEN}+{RESET}] Rule applications:");
        for name in Rewriter::rule_names_ordered() {
            if let Some(&count) = self.rule_stats.get(&name) {
                println!("    {name}: {RED}{count}{RESET}");
            }
        }

        io::stdout().flush()?;
        Ok(())
    }
}
