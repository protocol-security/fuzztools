use crate::constants::{GREEN, RED, RESET};
use anyhow::Result;
use crossbeam::channel::{bounded, Receiver, Sender};
use fuzztools::{
    builders::circuits::CircuitBuilder,
    circuits::{context::Context, Circuit},
};
use rand::Rng;
use std::{
    fs,
    io::{self, Write},
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
    thread,
    time::Instant,
};
use tempfile::TempDir;

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
    CompileError { circuit: Circuit, indexed_name: String, stderr: String },
    /// Compiler crashed (ICE, stack overflow, or signal)
    Crash { circuit: Circuit, indexed_name: String, message: String },
}

pub struct App {
    /// Noiruzz prelude to display
    prelude: String,

    /// Circuit generation context
    ctx: Context,

    /// Crash report directory
    crash_report_dir: String,

    /// Debug directory for compile errors
    debug_dir: String,

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
    pub fn new(
        ctx: Context,
        prelude: String,
        crash_report_dir: String,
        debug_dir: String,
    ) -> Result<Self> {
        let num_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);

        // Bounded channels for backpressure
        let (job_sender, job_receiver) = bounded::<CompileJob>(num_workers * 2);
        let (result_sender, result_receiver) = bounded::<CompileResult>(num_workers * 2);

        // Spawn worker threads using std::thread (subprocess-based, so no stack overflow risk)
        for i in 0..num_workers {
            let receiver = job_receiver.clone();
            let sender = result_sender.clone();

            thread::Builder::new()
                .name(format!("worker-{}", i))
                .spawn(move || {
                    worker_loop(receiver, sender);
                })
                .expect("Failed to spawn worker thread");
        }

        // Drop extra senders so workers can detect shutdown
        drop(job_receiver);
        drop(result_sender);

        // Create output directories
        fs::create_dir_all(&crash_report_dir)?;
        fs::create_dir_all(&debug_dir)?;

        Ok(Self {
            prelude,
            ctx,
            crash_report_dir,
            debug_dir,
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
        let mut builder = CircuitBuilder::new();
        loop {
            self.drain_results();
            let circuit = builder.circuit(random, &self.ctx);

            // @todo mutate the code

            let indexed_name = format!("circuit_{}", self.total_programs);
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
                CompileResult::CompileError { circuit, indexed_name, stderr } => {
                    self.total_errors.fetch_add(1, Ordering::Relaxed);

                    // Write compile error to debug directory
                    let project_dir = format!("{}/{}", self.debug_dir, indexed_name);
                    let _ = Command::new("nargo").args(["new", &project_dir]).output();
                    let _ = fs::write(format!("{}/src/main.nr", project_dir), &circuit);
                    let _ = fs::write(format!("{}/error.txt", project_dir), &stderr);
                }
                CompileResult::Crash { circuit, indexed_name, message } => {
                    self.total_panics.fetch_add(1, Ordering::Relaxed);

                    // Write crash directly to crash_report_dir
                    let project_dir = format!("{}/{}", self.crash_report_dir, indexed_name);
                    let _ = Command::new("nargo").args(["new", &project_dir]).output();
                    let _ = fs::write(format!("{}/src/main.nr", project_dir), &circuit);

                    // Also write crash message
                    let _ = fs::write(format!("{}/crash.txt", project_dir), &message);
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

/// Worker function that runs in each thread
fn worker_loop(receiver: Receiver<CompileJob>, sender: Sender<CompileResult>) {
    while let Ok(job) = receiver.recv() {
        let circuit = job.circuit.clone();
        let indexed_name = job.indexed_name.clone();

        // Create temp directory for this compilation
        let temp_dir = match TempDir::new() {
            Ok(dir) => dir,
            Err(e) => {
                let message = format!("Failed to create temp dir: {}", e);
                let _ = sender.send(CompileResult::Crash { circuit, indexed_name, message });
                continue;
            }
        };

        // Create nargo project in temp dir
        let temp_path = temp_dir.path();
        if Command::new("nargo").args(["new", "circuit"]).current_dir(temp_path).output().is_err() {
            let _ = sender.send(CompileResult::Crash {
                circuit,
                indexed_name,
                message: "Failed to create nargo project".to_string(),
            });
            continue;
        }

        // Write circuit to the project
        let circuit_dir = temp_path.join("circuit");
        if fs::write(circuit_dir.join("src/main.nr"), &circuit).is_err() {
            let _ = sender.send(CompileResult::Crash {
                circuit,
                indexed_name,
                message: "Failed to write circuit".to_string(),
            });
            continue;
        }

        // Use subprocess to compile - this isolates stack overflows, signals, and panics
        let output = Command::new("nargo").args(["check"]).current_dir(&circuit_dir).output();

        let compile_result = match output {
            Ok(output) => {
                if output.status.success() {
                    // Compiled successfully
                    CompileResult::Success { indexed_name }
                } else if let Some(code) = output.status.code() {
                    // Normal exit with error code - compiler error (syntax/type error)
                    if code == 1 {
                        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                        CompileResult::CompileError { circuit, indexed_name, stderr }
                    } else {
                        // Abnormal exit code - treat as crash
                        let stderr = String::from_utf8_lossy(&output.stderr);
                        let message = format!("Exit code {}: {}", code, stderr);
                        CompileResult::Crash { circuit, indexed_name, message }
                    }
                } else {
                    // Process was killed by signal (stack overflow, segfault, etc.)
                    #[cfg(unix)]
                    let message = {
                        use std::os::unix::process::ExitStatusExt;
                        if let Some(signal) = output.status.signal() {
                            format!("Killed by signal {}", signal_name(signal))
                        } else {
                            "Killed by unknown signal".to_string()
                        }
                    };
                    #[cfg(not(unix))]
                    let message = "Process terminated abnormally".to_string();

                    CompileResult::Crash { circuit, indexed_name, message }
                }
            }
            Err(e) => {
                // Failed to spawn process
                let message = format!("Failed to spawn nargo: {}", e);
                CompileResult::Crash { circuit, indexed_name, message }
            }
        };

        let _ = sender.send(compile_result);
        // temp_dir is automatically cleaned up when dropped
    }
}

/// Convert signal number to name
#[cfg(unix)]
fn signal_name(signal: i32) -> &'static str {
    match signal {
        6 => "SIGABRT (abort/stack overflow)",
        9 => "SIGKILL",
        11 => "SIGSEGV (segmentation fault)",
        15 => "SIGTERM",
        _ => "unknown",
    }
}
