# Story 3: Tree-sitter Parsing Pipeline for hix-drill

## Description
Implement a parsing pipeline using Tree-sitter to convert source files into Abstract Syntax Trees (ASTs). This enables semantic analysis of code across different programming languages.

## Outcome
Parse files into AST for at least one language.

## Tasks
- [x] Add Tree-sitter grammar (start with TypeScript or Python; add more later)
  - [x] Added tree-sitter-typescript (supports TypeScript and TSX)
  - [x] Added tree-sitter-python
- [x] Parse file content → Tree-sitter tree
- [x] Capture parse errors but continue (best-effort)

## Acceptance Criteria
- [x] `scan` produces a parse summary: total files, parsed, failed (implemented with ParseSummary showing total, parsed, failed, no_parser)
- [x] At least one language grammar is integrated (TypeScript or Python) (both TypeScript/TSX and Python grammars integrated)
- [x] Parse errors are captured but don't stop processing (parse errors are captured and marked with ⚠, processing continues)
- [x] AST structure is accessible for extraction (Tree objects are stored in ParseResult and can be accessed)

## Related Epic
Part of Epic: Build hix-drill (Rust) — CLI Codebase Analyzer for Hix

## Milestone
M1: Stories 1–4 (scan → facts)

