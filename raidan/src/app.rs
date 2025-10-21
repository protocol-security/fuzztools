use crate::constants::{Method, GREEN, RED, RESET};
use anyhow::Result;
use rand::Rng;
use std::io::{self, Write};
use dff::Server;

pub struct App {
    method: Method,
    seed: u64,
    server: Server,
    prelude: String,
}

impl App {
    pub async fn new(
        method: Method,
        seed: u64,
        prelude: String,
    ) -> Result<Self> {
        let server =  Server::new(method.to_string())?;
        Ok(Self { method, seed, server, prelude })
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
