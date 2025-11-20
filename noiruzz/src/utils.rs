use fuzztools::utils::RandomChoice;
use rand::Rng;
use std::{
    path::Path,
    process::{Command, Output},
};

const CHARS: [&str; 36] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
];
#[inline(always)]
pub fn random_id<R: Rng>(rng: &mut R, size: usize) -> String {
    (0..size).map(|_| *rng.choice(&CHARS)).collect()
}

// @audit instead of running processes, call the lib directly
pub fn nargo_execute(name: &str, expression_width: u64) -> Result<Output, std::io::Error> {
    let mut command = Command::new("nargo");
    command.arg("execute");
    command.arg("--silence-warnings");
    command.arg("--force");
    command.arg(name);
    command.arg("--expression-width");
    command.arg(expression_width.to_string());

    command.output()
}

pub fn nargo_version() -> Result<Output, std::io::Error> {
    let mut command = Command::new("nargo");
    command.arg("--version");
    command.output()
}

pub fn bb_prove(
    noir_json: &Path,
    witness_gz: &Path,
    vk_file_path: &Path,
    proof: &Path,
) -> Result<Output, std::io::Error> {
    let mut command = Command::new("bb");
    command.arg("prove");
    command.arg("-b");
    command.arg(noir_json.as_os_str());
    command.arg("-w");
    command.arg(witness_gz.as_os_str());
    command.arg("-k");
    command.arg(vk_file_path.as_os_str());
    command.arg("-o");
    command.arg(proof.as_os_str());
    command.output()
}

pub fn bb_write_vk(noir_json: &Path, vk: &Path) -> Result<Output, std::io::Error> {
    let mut command = Command::new("bb");
    command.arg("write_vk");
    command.arg("-b");
    command.arg(noir_json.as_os_str());
    command.arg("-o");
    command.arg(vk.as_os_str());
    command.output()
}

pub fn bb_verify(vk: &Path, proof: &Path, inputs: &Path) -> Result<Output, std::io::Error> {
    let mut command = Command::new("bb");
    command.arg("verify");
    command.arg("-k");
    command.arg(vk.as_os_str());
    command.arg("-p");
    command.arg(proof.as_os_str());
    command.arg("-i");
    command.arg(inputs.as_os_str());
    command.output()
}
