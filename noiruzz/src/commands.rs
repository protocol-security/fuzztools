use std::fs;
use tokio::process::Command;

/// This are normal errors due to creating random values without taking into account bounds
const EXPECTED_EXECUTION_ERRORS: &[&str] = &[
    "bug: Assertion is always false",
    "bug: Assertion is always false: attempt to divide by zero",
    "bug: Assertion is always false: attempt to calculate the remainder with a divisor of zero",
    "bug: Assertion is always false: attempt to subtract with overflow",
    "bug: Assertion is always false: attempt to add with overflow",
    "bug: Assertion is always false: attempt to multiply with overflow",
    "bug: Assertion is always false: attempt to bit-shift with overflow",
    "bug: Assertion is always false: attempt to shr with overflow",
    "bug: Assertion is always false: attempt to bit-shift with overflow",
    "error: Assertion failed: attempt to subtract with overflow",
    "error: Assertion failed: attempt to add with overflow",
    "error: Assertion failed: attempt to multiply with overflow",
    "error: Assertion failed: attempt to bit-shift with overflow",
    "error: Assertion failed: assertion failed", // @todo is this correct?
    "error: Failed constraint",                  // @todo is this correct?
];

#[inline(always)]
pub(crate) fn is_expected_execution_error(error: &str) -> bool {
    EXPECTED_EXECUTION_ERRORS.iter().any(|e| error.contains(e))
}

// @todo use       --inliner-aggressiveness <INLINER_AGGRESSIVENESS> must ensure same output too
// Setting to decide on an inlining strategy for Brillig functions. A more aggressive inliner should
// generate larger programs but more optimized A less aggressive inliner should generate smaller
// programs [default: 9223372036854775807]
pub(crate) async fn setup_project(dir: &std::path::Path, code: &str) -> Result<(), String> {
    // Clean up stale files from previous jobs
    let _ = fs::remove_file(dir.join("Prover.toml"));
    let _ = fs::remove_dir_all(dir.join("target"));

    // Create project structure directly (faster than nargo new)
    fs::create_dir_all(dir).map_err(|e| e.to_string())?;

    fs::write(
        dir.join("Nargo.toml"),
        "[package]\nname = \"circuit\"\ntype = \"bin\"\nauthors = [\"\"]\n\n[dependencies]\n",
    )
    .map_err(|e| e.to_string())?;

    let src_dir = dir.join("src");
    fs::create_dir_all(&src_dir).map_err(|e| e.to_string())?;
    fs::write(src_dir.join("main.nr"), code).map_err(|e| e.to_string())?;

    Ok(())
}

pub(crate) async fn compile_project(dir: &std::path::Path) -> Result<(), String> {
    let output = Command::new("nargo")
        .args(["compile", "--silence-warnings", "--force"])
        .current_dir(dir)
        .output()
        .await
        .map_err(|e| format!("Failed to spawn nargo: {}", e))?;

    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string());
    }

    Ok(())
}

pub(crate) async fn execute_project(dir: &std::path::Path) -> Result<String, String> {
    let output = Command::new("nargo")
        .args(["execute", "--silence-warnings", "--force"])
        .current_dir(dir)
        .output()
        .await
        .map_err(|e| format!("Failed to spawn nargo: {}", e))?;

    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string());
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

pub(crate) async fn bb_prove(dir: &std::path::Path) -> Result<(), String> {
    let noir_json = dir.join("target").join("circuit.json");
    let witness_gz = dir.join("target").join("circuit.gz");

    let output = Command::new("bb")
        .args([
            "prove",
            "-b",
            noir_json.to_str().unwrap(),
            "-w",
            witness_gz.to_str().unwrap(),
            "--write_vk",
            "-o",
            "target",
        ])
        .current_dir(dir)
        .output()
        .await
        .map_err(|e| format!("Failed to spawn bb: {}", e))?;

    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string());
    }

    Ok(())
}

pub(crate) async fn bb_verify(dir: &std::path::Path) -> Result<(), String> {
    let vk = dir.join("target").join("vk");
    let proof = dir.join("target").join("proof");

    let output = Command::new("bb")
        .args(["verify", "-k", vk.to_str().unwrap(), "-p", proof.to_str().unwrap()])
        .current_dir(dir)
        .output()
        .await
        .map_err(|e| format!("Failed to spawn bb: {}", e))?;

    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string());
    }

    Ok(())
}
