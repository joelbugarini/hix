# Story 1: Rust Project Bootstrap for hix-drill

## Description
Create a Rust workspace with a runnable CLI skeleton for the hix-drill tool. This is the foundational setup that enables all subsequent development work.

## Outcome
A Rust workspace with a runnable CLI skeleton.

## Tasks
- [x] Create repo `hix-drill` (created in `drill/` folder)
- [x] Setup Rust edition 2021/2024 (using edition 2021)
- [x] Add crates:
  - [x] `clap` (CLI)
  - [x] `serde`, `serde_json`
  - [x] `walkdir`
  - [x] `anyhow`, `thiserror`
  - [x] `sha2` (file hashing)
  - [x] `tree-sitter` (initial grammar support added)
- [x] Add `--version`, `--help`

## Acceptance Criteria
- [x] `hix-drill --help` works
- [x] `hix-drill scan <path>` prints "not implemented" cleanly
- [x] Rust project structure is properly set up
- [x] All required dependencies are configured in Cargo.toml
- [x] Project builds successfully on Linux/macOS/Windows

## Related Epic
Part of Epic: Build hix-drill (Rust) — CLI Codebase Analyzer for Hix

## Milestone
M1: Stories 1–4 (scan → facts)

