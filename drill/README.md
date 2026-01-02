# hix-drill

CLI Codebase Analyzer for Hix

## Overview

`hix-drill` is a Rust-based CLI tool that analyzes existing repositories, matches known Pattern Packs, and produces deterministic analysis reports and Hix configuration files.

## Status

This is Story 1 of the hix-drill epic - the foundational Rust project bootstrap.

## Building

```bash
cargo build
```

## Running

```bash
# Show help
cargo run -- --help

# Show version
cargo run -- --version

# Scan a repository (not yet implemented)
cargo run -- scan <path>
```

## Dependencies

- `clap` - CLI argument parsing
- `serde`, `serde_json` - Serialization
- `walkdir` - Directory traversal
- `anyhow`, `thiserror` - Error handling
- `sha2` - File hashing
- `tree-sitter` - AST parsing

## Project Structure

```
drill/
├── Cargo.toml
├── README.md
└── src/
    └── main.rs
```

## Related Epic

Part of Epic: Build hix-drill (Rust) — CLI Codebase Analyzer for Hix

## Milestone

M1: Stories 1–4 (scan → facts)

