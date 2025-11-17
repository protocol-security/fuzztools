# Noiruzz

A Rust implementation of metamorphic testing framework for Noir zero-knowledge circuits, based on the [Circuzz](https://github.com/trailofbits/circuzz) project.

## Overview

Noiruzz is a specialized fuzzing tool designed to test Noir ZKP (Zero-Knowledge Proof) circuits through metamorphic testing. It generates random circuits, applies semantic-preserving transformations, and verifies that the circuits behave consistently.

## Features

- **IR-based Circuit Generation**: Generate random boolean and arithmetic circuits
- **Noir Backend Support**: Full support for Noir language compilation and execution
- **Metamorphic Testing**: Test circuits with equivalence and weakening relations
- **Proof Generation & Verification**: Integration with `nargo` and `bb` (Barretenberg)
- **Configurable Testing**: Customizable test iterations, boundary probabilities, and more

## Architecture

The project is structured into several key modules:

### Core Modules

- **`ir/`**: Intermediate Representation for circuits
  - `circuit.rs`: Circuit data structures (operators, expressions, statements)
  - `traits.rs`: Core traits for expressions and statements

- **`generators/`**: Circuit generation logic
  - `boolean.rs`: Boolean circuit generator
  - `arithmetic.rs`: Arithmetic circuit generator (TODO)

- **`noir/`**: Noir backend implementation
  - `nodes.rs`: Noir AST node definitions
  - `types.rs`: Noir type system
  - `operators.rs`: Noir operators
  - `emitter.rs`: Code generation (AST → Noir source)
  - `ir2noir.rs`: IR to Noir transformation
  - `command.rs`: Noir toolchain command execution
  - `utils.rs`: Utility functions for parsing and version management
  - `helper.rs`: Testing orchestration and project setup
  - `config.rs`: Configuration structures
  - `core.rs`: Main testing entry point

- **`cli.rs`**: Command-line interface

## Prerequisites

To use Noiruzz, you need the following tools installed:

- **Rust** (1.70 or higher)
- **Nargo** (Noir compiler) - [Installation Guide](https://noir-lang.org/docs/getting_started/installation)
- **bb** (Barretenberg prover) - Usually installed with Noir

## Installation

```bash
# Clone the repository
git clone <repository-url>
cd fuzztools/noiruzz

# Build the project
cargo build --release

# Run the binary
./target/release/noiruzz --help
```

## Usage

### Basic Usage

```bash
# Run with default settings
noiruzz

# Run with custom seed
noiruzz --seed 42

# Run with custom test iterations
noiruzz --test-iterations 5

# Run with custom working directory
noiruzz --working-dir /tmp/noir-test
```

### Configuration Options

- `--seed <SEED>`: Set the random seed for reproducible tests (default: 12345)
- `--working-dir <DIR>`: Set the working directory for generated files (default: ./obj/test)
- `--test-iterations <N>`: Number of test iterations per circuit pair (default: 2)
- `--boundary-prob <PROB>`: Probability of selecting boundary values (default: 0.1)
- `--help`: Display help information

## How It Works

1. **Circuit Generation**: Generate a random circuit using the IR generator
2. **Transformation**: Apply metamorphic transformations to create a related circuit
3. **Noir Compilation**: Transform both circuits to Noir source code
4. **Execution**: Execute both circuits with random inputs using `nargo execute`
5. **Proving**: Generate proofs using `bb prove`
6. **Verification**: Verify proofs using `bb verify`
7. **Comparison**: Check for metamorphic violations

### Metamorphic Relations

- **Equivalence**: Both circuits should produce identical outputs
- **Weakening**: The transformed circuit should fail no earlier than the original

## Project Structure Comparison

This implementation mirrors the Python Circuzz project structure:

| Python (Circuzz) | Rust (Noiruzz) | Description |
|------------------|----------------|-------------|
| `src/circuzz/ir/nodes.py` | `src/ir/circuit.rs` | IR node definitions |
| `src/backends/noir/nodes.py` | `src/noir/nodes.rs` | Noir AST nodes |
| `src/backends/noir/emitter.py` | `src/noir/emitter.rs` | Code generation |
| `src/backends/noir/ir2noir.py` | `src/noir/ir2noir.rs` | IR transformation |
| `src/backends/noir/command.py` | `src/noir/command.rs` | Command execution |
| `src/backends/noir/helper.py` | `src/noir/helper.rs` | Testing logic |
| `src/backends/noir/config.py` | `src/noir/config.rs` | Configuration |
| `src/backends/noir/core.py` | `src/noir/core.rs` | Main entry point |

## Implementation Status

### Completed ✓

- [x] Noir AST nodes (types, operators, expressions, statements)
- [x] Noir code emitter (AST to Noir source code)
- [x] IR to Noir transformer
- [x] Noir command execution (nargo execute, bb prove/verify)
- [x] Noir helper functions (project setup, input generation, parsing)
- [x] Noir configuration and core testing logic
- [x] Basic CLI interface

### TODO

- [ ] Full CLI argument parsing with clap
- [ ] Arithmetic circuit generator
- [ ] Circuit rewrite rules and transformations
- [ ] Metamorphic relation testing integration
- [ ] Comprehensive test suite
- [ ] Documentation and examples
- [ ] Performance optimizations
- [ ] Error handling improvements

## Differences from Circuzz

While Noiruzz aims to replicate Circuzz's Noir-specific functionality, there are some differences:

1. **Language**: Rust vs Python (better performance, type safety)
2. **Scope**: Currently focuses only on Noir backend (no Circom, Gnark, Corset)
3. **Dependencies**: Uses Rust ecosystem crates instead of Python packages
4. **Testing**: Integrated Rust testing framework

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

This project follows the same license as the parent fuzztools project.

## Acknowledgments

This project is based on [Circuzz](https://github.com/trailofbits/circuzz) by Trail of Bits, which is described in the paper "Fuzzing Processing Pipelines for Zero-Knowledge Circuits".

## References

- [Circuzz Paper](https://arxiv.org/abs/2311.14680)
- [Noir Language](https://noir-lang.org/)
- [Barretenberg](https://github.com/AztecProtocol/barretenberg)

