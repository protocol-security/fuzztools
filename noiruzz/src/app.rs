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
    circuits::{
        context::Context,
        ir::{Type, AST},
        rewritter::apply_random_rule,
    },
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
    mpsc::{channel, error::TrySendError, Receiver, Sender},
    Semaphore,
};

// ────────────────────────────────────────────────────────────────────────────────
// Types
// ────────────────────────────────────────────────────────────────────────────────

/// A job containing the source code of the riginal and rewritten circuits, as well as the list of
/// rules applied.
struct TestJob {
    original_code: String,
    rewritten_code: String,
    inputs: Vec<(String, Type)>,
    executions: usize,
    job_id: u64,
    seed: u64,
    run_later_stages: bool, // proof + verification
    rules_applied: Vec<String>,
}

#[derive(Clone, Copy)]
enum MismatchKind {
    Compilation,       // one compiles, other fails
    Execution,         // different outputs or witness solving
    Proof,             // one proof succeeds, other fails
    Verification,      // one verification succeeds, other fails
    UnexpectedFailure, // both fail at proof or verification (unexpected)
}

enum TestResult {
    Success { t1: Duration, t2: Duration },
    Mismatch(Mismatch),
    NotInteresting { t1: Duration, t2: Option<Duration>, is_proof: bool, is_verification: bool },
    KnownError { t1: Duration },
}

/// Details about a detected mismatch for crash reporting.
struct Mismatch {
    kind: MismatchKind,
    job_id: u64,
    original_code: String,
    rewritten_code: String,
    prover_toml: String,
    original_output: String,
    rewritten_output: String,
    rules_applied: Vec<String>,
    t1: Duration, // compile + execute time
    t2: Duration, // prove + verify time
}

// ────────────────────────────────────────────────────────────────────────────────
// App
// ────────────────────────────────────────────────────────────────────────────────

pub(crate) struct App {
    // Config
    prelude: String,
    ctx: Context,
    crash_dir: String,
    executions: usize,

    // Channels
    job_sender: Sender<TestJob>,
    job_receiver: Option<Receiver<TestJob>>,
    result_sender: Sender<TestResult>,
    result_receiver: Receiver<TestResult>,
    error_signal: Arc<AtomicBool>,
    error_sender: Sender<String>,
    error_receiver: Option<Receiver<String>>,

    // Concurrency
    worker_semaphore: Arc<Semaphore>,
    power_schedule: Arc<PowerScheduler>,

    // Stats
    total_circuits: AtomicU64,
    compile_mismatches: AtomicU64,
    soundness_bugs: AtomicU64,
    proof_mismatches: AtomicU64,
    verification_mismatches: AtomicU64,
    unexpected_failures: AtomicU64,
    known_errors: AtomicU64,
    total_proofs: AtomicU64,
    total_verifications: AtomicU64,
    rule_stats: HashMap<String, u64>,
    circuits_per_tick: u64,
    start_time: Instant,
    last_update: Instant,
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
        let (job_sender, job_receiver) = channel::<TestJob>(workers * 2);
        let (result_sender, result_receiver) = channel::<TestResult>(workers * 2);
        let (error_sender, error_receiver) = channel::<String>(workers * 2);

        fs::create_dir_all(&crash_dir)?;

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
            worker_semaphore: Arc::new(Semaphore::new(workers)),
            power_schedule: Arc::new(PowerScheduler::new(target_ratio)),
            total_circuits: AtomicU64::new(0),
            compile_mismatches: AtomicU64::new(0),
            soundness_bugs: AtomicU64::new(0),
            proof_mismatches: AtomicU64::new(0),
            verification_mismatches: AtomicU64::new(0),
            unexpected_failures: AtomicU64::new(0),
            known_errors: AtomicU64::new(0),
            total_proofs: AtomicU64::new(0),
            total_verifications: AtomicU64::new(0),
            rule_stats: HashMap::new(),
            circuits_per_tick: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
        })
    }

    /// Main fuzzing loop: generate jobs, dispatch to workers, collect results.
    pub(crate) async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        self.spawn_dispatcher();

        let builder = CircuitBuilder::default();
        let mut job_id = 0u64;
        let mut error_receiver = self.error_receiver.take().unwrap();

        loop {
            // Check for fatal errors from workers
            if self.error_signal.load(Ordering::Relaxed) {
                eprintln!(
                    "\n\n\x1b[1;31m[!] Error: {}\x1b[0m",
                    error_receiver.recv().await.unwrap()
                );
                break;
            }

            // Update screen every second
            if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                self.drain_results();
                self.screen()?;
                self.circuits_per_tick = 0;
                self.last_update = Instant::now();
            }

            let job = self.generate_job(random, &builder, job_id);

            // Try non-blocking send first, block only when channel is full
            match self.job_sender.try_send(job) {
                Ok(()) => job_id += 1,
                Err(TrySendError::Full(job)) => {
                    let sender = self.job_sender.clone();
                    let send_fut = sender.send(job);
                    tokio::pin!(send_fut);

                    // Wait for channel space while keeping UI responsive
                    loop {
                        tokio::select! {
                            result = &mut send_fut => {
                                if result.is_err() { return Ok(()); }
                                job_id += 1;
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
                }
                Err(_) => return Ok(()),
            }
        }

        Ok(())
    }

    /// Spawn the dispatcher task that distributes jobs to worker pool.
    fn spawn_dispatcher(&mut self) {
        let ctx = self.ctx;
        let tx = self.result_sender.clone();
        let signal = self.error_signal.clone();
        let error_sender = self.error_sender.clone();
        let semaphore = self.worker_semaphore.clone();
        let mut rx = self.job_receiver.take().unwrap();

        tokio::spawn(async move {
            while let Some(job) = rx.recv().await {
                if signal.load(Ordering::Relaxed) {
                    break;
                }

                let worker_ctx = ctx.clone();
                let worker_tx = tx.clone();
                let worker_signal = signal.clone();
                let worker_error_sender = error_sender.clone();
                let permit = semaphore.clone().acquire_owned().await.unwrap();

                tokio::spawn(async move {
                    let _permit = permit; // hold permit until worker completes
                    let result = process_job(worker_ctx, &job, &worker_signal, &worker_error_sender).await;
                    let _ = worker_tx.send(result).await;
                    let _ = fs::remove_dir_all(format!("./tmp/job_{}", job.job_id));
                });
            }
        });
    }

    // Forest -> AST -> rewrite -> TestJob
    fn generate_job(
        &mut self,
        random: &mut impl Rng,
        builder: &CircuitBuilder,
        job_id: u64,
    ) -> TestJob {
        let circuit = builder.generate(random, &self.ctx);
        let body = circuit.forest.format("    ");
        let original_code = builder.format_main(body, circuit.inputs.clone(), circuit.ret.clone());
        let mut ast = AST::from(&circuit.forest);

        // Apply random rewrites to the AST
        let mut rules_applied = vec![];
        let num_rewrites =
            random.random_range(self.ctx.min_rewrites_count..self.ctx.max_rewrites_count);

        for _ in 0..num_rewrites {
            if let Some(rule) = apply_random_rule(random, &mut ast) {
                let rule_name = format!("{:?}", rule);
                *self.rule_stats.entry(rule_name.clone()).or_insert(0) += 1;
                rules_applied.push(rule_name);
            } else {
                break;
            }
        }

        let rewritten_body = ast.format("    ");
        let inputs = circuit.inputs.clone();
        let rewritten_code = builder.format_main(rewritten_body, circuit.inputs, circuit.ret);

        TestJob {
            original_code,
            rewritten_code,
            inputs,
            executions: self.executions,
            job_id,
            seed: random.random(),
            run_later_stages: self.power_schedule.should_run_later_stages(),
            rules_applied,
        }
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
                TestResult::Mismatch(m) => {
                    self.power_schedule.add_t1(m.t1);
                    if m.t2 > Duration::ZERO {
                        self.power_schedule.add_t2(m.t2);
                    }

                    match m.kind {
                        MismatchKind::Compilation => {
                            self.compile_mismatches.fetch_add(1, Ordering::Relaxed)
                        }
                        MismatchKind::Execution => {
                            self.soundness_bugs.fetch_add(1, Ordering::Relaxed)
                        }
                        MismatchKind::Proof => {
                            self.total_proofs.fetch_add(1, Ordering::Relaxed);
                            self.proof_mismatches.fetch_add(1, Ordering::Relaxed)
                        }
                        MismatchKind::Verification => {
                            self.total_proofs.fetch_add(1, Ordering::Relaxed);
                            self.total_verifications.fetch_add(1, Ordering::Relaxed);
                            self.verification_mismatches.fetch_add(1, Ordering::Relaxed)
                        }
                        MismatchKind::UnexpectedFailure => {
                            self.total_proofs.fetch_add(1, Ordering::Relaxed);
                            self.unexpected_failures.fetch_add(1, Ordering::Relaxed)
                        }
                    };

                    self.save_mismatch(&m);
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

    fn save_mismatch(&self, m: &Mismatch) {
        let (prefix, info_file) = match m.kind {
            MismatchKind::Compilation => ("compile_mismatch", "info.txt"),
            MismatchKind::Execution => ("possible_soundness_bug", "divergence.txt"),
            MismatchKind::Proof => ("proof_mismatch", "proof_divergence.txt"),
            MismatchKind::Verification => ("verification_mismatch", "verification_divergence.txt"),
            MismatchKind::UnexpectedFailure => ("unexpected_failure", "failure.txt"),
        };

        let dir = format!("{}/{}_{}", self.crash_dir, prefix, m.job_id);
        if fs::create_dir_all(&dir).is_err() {
            return;
        }

        let _ = fs::write(format!("{dir}/original.nr"), &m.original_code);
        let _ = fs::write(format!("{dir}/rewritten.nr"), &m.rewritten_code);

        if !m.prover_toml.is_empty() {
            let _ = fs::write(format!("{dir}/Prover.toml"), &m.prover_toml);
        }

        let info = format!(
            "Rules applied: {}\n\nOriginal:\n{}\n\nRewritten:\n{}",
            m.rules_applied.join(", "),
            m.original_output,
            m.rewritten_output
        );
        let _ = fs::write(format!("{dir}/{info_file}"), info);
    }

    fn screen(&self) -> Result<()> {
        Command::new("clear").status()?;
        print!("{}", self.prelude);

        let s = self.start_time.elapsed().as_secs();
        println!(
            "[{GREEN}+{RESET}] Circuits: {RED}{}{RESET} | Mismatches: {RED}{}{RESET} | \
             Soundness: {RED}{}{RESET} | Proof: {RED}{}{RESET} | Unexpected: {RED}{}{RESET} | Known: {RED}{}{RESET} | \
             Rate: {RED}{}{RESET}/s | Time: {RED}{:02}h {:02}m {:02}s{RESET}",
            self.total_circuits.load(Ordering::Relaxed),
            self.compile_mismatches.load(Ordering::Relaxed),
            self.soundness_bugs.load(Ordering::Relaxed),
            self.proof_mismatches.load(Ordering::Relaxed),
            self.unexpected_failures.load(Ordering::Relaxed),
            self.known_errors.load(Ordering::Relaxed),
            self.circuits_per_tick,
            s / 3600, (s % 3600) / 60, s % 60,
        );
        println!(
            "[{GREEN}+{RESET}] Power Schedule: p={RED}{:.6}{RESET} | Proofs: {RED}{}{RESET} | Verifications: {RED}{}{RESET}",
            self.power_schedule.current_ratio(),
            self.total_proofs.load(Ordering::Relaxed),
            self.total_verifications.load(Ordering::Relaxed),
        );

        if !self.rule_stats.is_empty() {
            let total: u64 = self.rule_stats.values().sum();
            let mut sorted: Vec<_> = self.rule_stats.iter().collect();
            sorted.sort_by(|a, b| b.1.cmp(a.1));

            println!("[{GREEN}+{RESET}] Rules:");
            for (rule, count) in sorted {
                println!(
                    "    {:30} {RED}{:6}{RESET} ({RED}{:5.2}%{RESET})",
                    rule,
                    count,
                    (*count as f64 / total as f64) * 100.0
                );
            }
        }

        io::stdout().flush()?;
        Ok(())
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Worker
// ────────────────────────────────────────────────────────────────────────────────

/// Process a test job through all stages: compile, execute, prove, verify.
///
/// Stages are split into T1 (compile + execute) and T2 (prove + verify) for power scheduling.
async fn process_job(
    ctx: Context,
    job: &TestJob,
    signal: &Arc<AtomicBool>,
    error_sender: &Sender<String>,
) -> TestResult {
    let temp_path = std::path::PathBuf::from(format!("./tmp/job_{}", job.job_id));
    let orig_dir = temp_path.join("original");
    let rewr_dir = temp_path.join("rewritten");

    macro_rules! fatal {
        ($msg:expr) => {{
            signal.store(true, Ordering::Relaxed);
            let _ = error_sender.send($msg).await;
            return TestResult::KnownError { t1: Duration::ZERO };
        }};
    }

    macro_rules! mismatch {
        ($kind:expr, $prover:expr, $orig:expr, $rewr:expr, $t1:expr, $t2:expr) => {
            TestResult::Mismatch(Mismatch {
                kind: $kind,
                job_id: job.job_id,
                original_code: job.original_code.clone(),
                rewritten_code: job.rewritten_code.clone(),
                prover_toml: $prover.to_string(),
                original_output: $orig.to_string(),
                rewritten_output: $rewr.to_string(),
                rules_applied: job.rules_applied.clone(),
                t1: $t1,
                t2: $t2,
            })
        };
    }

    // Setup: Create project directories
    if setup_project(&orig_dir, &job.original_code).await.is_err() {
        fatal!("Setup original error".into());
    }
    if setup_project(&rewr_dir, &job.rewritten_code).await.is_err() {
        fatal!("Setup rewritten error".into());
    }

    let mut t1 = Duration::ZERO;
    let mut t2 = Duration::ZERO;

    // ─────────────────────────────────────────────────────────────────────────────
    // Initial stages (T1): Compile both
    // ─────────────────────────────────────────────────────────────────────────────

    let compile_start = Instant::now();
    let orig_compile = compile_project(&orig_dir).await;
    let rewr_compile = compile_project(&rewr_dir).await;
    t1 += compile_start.elapsed();

    match (orig_compile, rewr_compile) {
        (Ok(()), Ok(())) => {} // Both compiled - test with random inputs
        (Ok(()), Err(e)) if !is_expected_execution_error(&e) => {
            return mismatch!(MismatchKind::Compilation, "", "compiled", &e, t1, t2);
        }
        (Err(e), Ok(())) if !is_expected_execution_error(&e) => {
            return mismatch!(MismatchKind::Compilation, "", &e, "compiled", t1, t2);
        }
        _ => {
            return TestResult::NotInteresting {
                t1,
                t2: None,
                is_proof: false,
                is_verification: false,
            }
        }
    }

    let mut random = SmallRng::seed_from_u64(job.seed);

    // ─────────────────────────────────────────────────────────────────────────────
    // Initial stages (T1): Execute both with random inputs
    // ─────────────────────────────────────────────────────────────────────────────

    for _ in 0..job.executions {
        let prover_toml = job
            .inputs
            .iter()
            .map(|(name, ty)| format!("{name} = {}", ty.random_value(&mut random, &ctx)))
            .collect::<Vec<_>>()
            .join("\n");

        if fs::write(orig_dir.join("Prover.toml"), &prover_toml).is_err() ||
            fs::write(rewr_dir.join("Prover.toml"), &prover_toml).is_err()
        {
            fatal!("Write Prover.toml error".into());
        }

        let exec_start = Instant::now();
        let orig_exec = execute_project(&orig_dir).await;
        let rewr_exec = execute_project(&rewr_dir).await;
        t1 += exec_start.elapsed();

        match (&orig_exec, &rewr_exec) {
            (Ok(orig_out), Ok(rewr_out)) => {
                let (orig_ok, orig_res) = parse_output(orig_out);
                let (rewr_ok, rewr_res) = parse_output(rewr_out);

                // Check for execution divergence
                if orig_ok != rewr_ok || orig_res != rewr_res {
                    return mismatch!(
                        MismatchKind::Execution,
                        &prover_toml,
                        orig_out,
                        rewr_out,
                        t1,
                        t2
                    );
                }

                if !job.run_later_stages {
                    continue;
                }

                // ─────────────────────────────────────────────────────────────────
                // Later stages (T2): Proof generation
                // ─────────────────────────────────────────────────────────────────

                let proof_start = Instant::now();
                let orig_proof = bb_prove(&orig_dir).await;
                let rewr_proof = bb_prove(&rewr_dir).await;
                t2 += proof_start.elapsed();

                match (&orig_proof, &rewr_proof) {
                    (Ok(()), Ok(())) => {} // Both proved - verify
                    (Ok(()), Err(e)) => {
                        return mismatch!(
                            MismatchKind::Proof,
                            &prover_toml,
                            "succeeded",
                            &format!("failed: {e}"),
                            t1,
                            t2
                        )
                    }
                    (Err(e), Ok(())) => {
                        return mismatch!(
                            MismatchKind::Proof,
                            &prover_toml,
                            &format!("failed: {e}"),
                            "succeeded",
                            t1,
                            t2
                        )
                    }
                    (Err(e1), Err(e2)) => {
                        return mismatch!(
                            MismatchKind::UnexpectedFailure,
                            &prover_toml,
                            &format!("failed: {e1}"),
                            &format!("failed: {e2}"),
                            t1,
                            t2
                        )
                    }
                }

                // ─────────────────────────────────────────────────────────────────
                // Later stages (T2): Verification
                // ─────────────────────────────────────────────────────────────────

                let verify_start = Instant::now();
                let orig_verify = bb_verify(&orig_dir).await;
                let rewr_verify = bb_verify(&rewr_dir).await;
                t2 += verify_start.elapsed();

                match (&orig_verify, &rewr_verify) {
                    (Ok(()), Ok(())) => {} // Both verified - success
                    (Ok(()), Err(e)) => {
                        return mismatch!(
                            MismatchKind::Verification,
                            &prover_toml,
                            "succeeded",
                            &format!("failed: {e}"),
                            t1,
                            t2
                        )
                    }
                    (Err(e), Ok(())) => {
                        return mismatch!(
                            MismatchKind::Verification,
                            &prover_toml,
                            &format!("failed: {e}"),
                            "succeeded",
                            t1,
                            t2
                        )
                    }
                    (Err(e1), Err(e2)) => {
                        return mismatch!(
                            MismatchKind::UnexpectedFailure,
                            &prover_toml,
                            &format!("failed: {e1}"),
                            &format!("failed: {e2}"),
                            t1,
                            t2
                        )
                    }
                }
            }
            (Ok(orig_out), Err(e)) => {
                if is_expected_execution_error(e) {
                    return TestResult::KnownError { t1 };
                }
                return mismatch!(
                    MismatchKind::Execution,
                    &prover_toml,
                    orig_out,
                    &format!("UNKNOWN ERROR: {e}"),
                    t1,
                    t2
                );
            }
            _ => {
                return TestResult::NotInteresting {
                    t1,
                    t2: None,
                    is_proof: false,
                    is_verification: false,
                }
            }
        }
    }

    TestResult::Success { t1, t2 }
}

// (success, circuit_output)
fn parse_output(output: &str) -> (bool, String) {
    let mut success = false;
    let mut result = String::new();

    for line in output.lines() {
        if line.contains("witness successfully solved") {
            success = true;
        } else if let Some(rest) = line.strip_prefix("Circuit output") {
            if let Some(idx) = rest.find(':') {
                result.push_str(rest[idx + 1..].trim_start());
            }
        }
    }

    (success, result)
}
