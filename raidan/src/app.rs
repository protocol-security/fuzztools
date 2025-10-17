use crate::constants::{Method, GREEN, RED, RESET};
use anyhow::Result;
use rand::Rng;
use std::io::{self, Write};

pub struct App {
    method: Method,
    seed: u64,
    prysm: bool,
    lighthouse: bool,
    teku: bool,
    prelude: String,
}

impl App {
    pub async fn new(
        method: Method,
        seed: u64,
        prysm: bool,
        lighthouse: bool,
        teku: bool,
        prelude: String,
    ) -> Result<Self> {
        Ok(Self { method, seed, prysm, lighthouse, teku, prelude })
    }

    pub async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        self.screen()?;
        Ok(())
    }

    pub fn screen(&mut self) -> Result<()> {
        // Clear the screen
        std::process::Command::new("clear").status().unwrap();

        // Print `HEADER` + configuration
        print!("{}", self.prelude);

        // Print stats
        print!("WIP");

        io::stdout().flush()?;

        Ok(())
    }
}
