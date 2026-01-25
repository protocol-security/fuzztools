# Build stage
FROM rust:1.88-bookworm AS builder

WORKDIR /build

# Copy all source files
COPY . .

# Build release binaries for both tools
RUN cargo build --release --bin rakoon --bin noiruzz

# Runtime stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN useradd -m -u 1000 fuzzer

WORKDIR /app

# Copy binaries from builder stage
COPY --from=builder /build/target/release/rakoon /usr/local/bin/
COPY --from=builder /build/target/release/noiruzz /usr/local/bin/

# Copy config files with proper ownership
COPY --chown=fuzzer:fuzzer configs/ ./configs/

# Switch to non-root user
USER fuzzer

# No ENTRYPOINT - user chooses which binary to run
# Examples:
#   docker run --rm fuzztools rakoon --help
#   docker run --rm fuzztools noiruzz --help
