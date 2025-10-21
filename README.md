# Fuzztools

A fuzzing toolkit providing reusable components and utilities for building custom fuzzers.

## Overview

This repository contains a collection of fuzzing tools and utilities designed to simplify the development of custom fuzzers. The `fuzztools` library provides mutation primitives, random number generation, and other common fuzzing infrastructure.

## Getting Started

### 1. Clone the Repository

```sh
git clone git@github.com:protocol-security/fuzztools.git
cd fuzztools
```

### 2. Create a New Fuzzer

Create a new binary crate within the workspace and add `fuzztools` as a dependency:

```sh
cargo new --bin <NAME>
cargo add fuzztools --package <NAME>
cd <NAME>
```

### 3. Implement Your Fuzzer

Add your fuzzing logic to `main.rs`. The typical pattern involves creating a base payload structure and mutating it within a loop:

```rs
#[derive(Mutable)]
struct Payload {
    a: u64,
    b: u64
}

fn main() {
    let mut base = Payload {
        a: 3,
        b: 5
    };
    let mut random = SmallRng::seed_from_u64(0);

    loop {
        base.mutate(random);

        // Check your target condition or send the payload
        if base.a + base.b == 5 {
            panic!("POC");
        }
    }
}
```

and that's it. For example, check the implementation of [rakoon](./rakoon/), where it makes use of it extensively. If you wanna load the Cargo documentation, run

```sh
cargo doc --no-deps --workspace --open
```
