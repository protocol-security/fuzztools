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

```rust
struct Payload {
    a: u64,
    b: u64
}

fn main() {
    let mut base = Payload {
        a: 3,
        b: 5
    };

    loop {
        // Randomly mutate fields
        if StdRng::random_bool(0.5) {
            base.a.mutate();
        } else {
            base.b.mutate();
        }

        // Check your target condition or send the payload
        if base.a + base.b == 5 {
            panic!("POC");
        }
    }
}
```

The core pattern is to call `mutate()` on fields you want to fuzz, then check conditions or send payloads to your target system.

## Examples

See the [rakoon](./rakoon/) implementation for a comprehensive example of using `fuzztools` in a real fuzzer.

## Documentation

Generate and view the API documentation locally:

```sh
cargo doc --no-deps --workspace --open
```
