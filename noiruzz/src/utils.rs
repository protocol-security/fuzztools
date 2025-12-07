use fuzztools::utils::RandomChoice;
use rand::Rng;
use std::{
    path::Path,
    process::{Command, Output},
};

#[allow(dead_code)]
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

#[allow(dead_code)]
pub fn bb_write_vk(noir_json: &Path, vk: &Path) -> Result<Output, std::io::Error> {
    let mut command = Command::new("bb");
    command.arg("write_vk");
    command.arg("-b");
    command.arg(noir_json.as_os_str());
    command.arg("-o");
    command.arg(vk.as_os_str());
    command.output()
}

#[allow(dead_code)]
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
