# Epic: Build **hix-drill** (Rust) — CLI Codebase Analyzer for Hix

## Goal

Create a Rust-based CLI tool (**hix-drill**) that analyzes an existing repository, matches known **Pattern Packs** (formerly “Bits”), and produces:

* A deterministic **analysis report** (what patterns were recognized, coverage, unknowns)
* A deterministic **Hix init/config** file that maps repo artifacts to Hix templates/models
* Optional starter artifacts (template skeletons, model stubs)

The tool must be:

* **CLI-first** (no IDE plugin, no MCP server)
* **Token-free by default** (LLM only for an explicit “mint new pack” workflow later)
* **Multi-language oriented** via Tree-sitter, designed to scale to many languages

---

## Non-Goals (for this epic)

* Full semantic typechecking for each language (Tree-sitter baseline only)
* LLM-powered pack creation (“mint”) — defer to a later epic
* Rendering/generation (handled by **Hix**)

---

## Definition of Done

* `hix-drill` builds and runs on Linux/macOS/Windows
* Can analyze a sample repo and output:

  * `.hixdrill/facts.json`
  * `.hixdrill/report.json`
  * `hix.project.json` (or chosen config filename)
* Loads Pattern Packs from a folder and matches at least **2 real patterns** (e.g., DTO-like and HTTP endpoint-like)
* Deterministic output (stable ordering, same input → same output)
* Includes at least one fixture test that asserts stable output

---

## Architecture Overview

### Components

1. **Scanner**

* Walk repo files
* Detect language by extension and/or Tree-sitter grammar availability
* Parse files → AST

2. **Facts Extractor**

* Convert AST → **Canonical Facts** (language-agnostic-ish)
* Store provenance (file path + byte/line ranges)

3. **Pattern Matcher**

* Load Pattern Packs
* Match patterns over facts
* Produce pattern **instances** with bindings

4. **Initializer / Config Writer**

* Convert instances → `hix.project.json`
* Optionally emit starter templates/models when requested

5. **Reporter**

* Output `report.json` + human-readable `report.md`
* Emit `unknowns/` when no pack matches

---

## Work Items (Stories)

### Story 1 — Rust project bootstrap

**Outcome:** A Rust workspace with a runnable CLI skeleton.

**Tasks**

* Create repo `hix-drill`
* Setup Rust edition 2021/2024
* Add crates:

  * `clap` (CLI)
  * `serde`, `serde_json`
  * `walkdir`
  * `anyhow`, `thiserror`
  * `sha2` or `blake3` (file hashing)
  * `tree-sitter` + initial grammars (start with 1–2)
* Add `--version`, `--help`

**Acceptance Criteria**

* `hix-drill --help` works
* `hix-drill scan <path>` prints “not implemented” cleanly

---

### Story 2 — File scanning + incremental cache

**Outcome:** Scan a repo deterministically and list candidate source files.

**Tasks**

* Implement recursive scanning with ignore rules:

  * ignore `.git/`, `node_modules/`, `bin/`, `obj/`, `target/`, `.hixdrill/`
* Determine language per file extension
* Compute hash per file (for incremental reruns)
* Write `.hixdrill/cache.json` (or sqlite later)

**Acceptance Criteria**

* Running `scan` twice does not reprocess unchanged files
* Output file list is stable sorted

---

### Story 3 — Tree-sitter parsing pipeline

**Outcome:** Parse files into AST for at least one language.

**Tasks**

* Add Tree-sitter grammar (start with TypeScript or Python; add more later)
* Parse file content → Tree-sitter tree
* Capture parse errors but continue (best-effort)

**Acceptance Criteria**

* `scan` produces a parse summary: total files, parsed, failed

---

### Story 4 — Facts schema v1 + extractor

**Outcome:** Emit canonical facts JSON.

**Facts schema (v1) must include**

* `files[]`: path, language, hash
* `symbols[]`: id, kind (type/function/variable), name, file, range
* `members[]`: symbolId, name, kind, typeRef (string), range
* `relations[]`: fromId, relKind (imports/calls/extends/implements), toRef
* `annotations[]`: symbolId, key, value (string), range

**Tasks**

* Define Rust structs + serde
* Implement extractor for one language (enough to find types + props + functions)
* Write `.hixdrill/facts.json`

**Acceptance Criteria**

* `facts.json` is deterministic (stable ordering)
* Includes provenance ranges

---

### Story 5 — Pattern Pack loader (read-only)

**Outcome:** Load Pattern Packs from disk.

**Tasks**

* Decide pack on-disk format (author YAML, compile to JSON later)
* For v1, support JSON only:

  * `pack.json` with metadata
  * `pattern.json` with match rules
* Implement `--packs <folder>` option
* Validate schema version

**Acceptance Criteria**

* `hix-drill analyze --packs ./packs` lists loaded packs

---

### Story 6 — Pattern matcher v1 (minimum viable)

**Outcome:** Match at least 2 patterns from facts.

**Suggested starter patterns**

* `data-structure` (DTO-like: type with only public fields/properties)
* `http-endpoint` (function/method with route annotation or naming convention)

**Tasks**

* Implement a simple DSL for match rules:

  * symbol kind + member predicates + annotation predicates
* Output `.hixdrill/matches.json` (instances + bindings)

**Acceptance Criteria**

* On a sample repo, at least one instance per starter pattern is detected

---

### Story 7 — Report generation (JSON + Markdown)

**Outcome:** Provide a human-facing report.

**Report includes**

* Total files, parsed files
* Loaded packs
* Matched instances by pattern
* Coverage estimate (matched symbols / total symbols)
* Unknown clusters: symbols not matched but recurring shapes

**Acceptance Criteria**

* `.hixdrill/report.json` and `.hixdrill/report.md` generated

---

### Story 8 — Init/config writer v1

**Outcome:** Generate Hix project config mapping recognized instances to templates/models.

**Tasks**

* Define `hix.project.json` schema v1 (minimal)

  * project root
  * generated outputs
  * inputs (models)
  * packs used (name/version)
* Convert instances → config entries

**Acceptance Criteria**

* `hix-drill init` writes config
* Config is stable and references packs + templates (placeholders allowed)

---

### Story 9 — Fixture-based tests

**Outcome:** Ensure determinism and prevent regressions.

**Tasks**

* Add `tests/fixtures/<repo>` with small sample code
* Add golden files for facts/report/config
* Test: run analyzer and compare outputs

**Acceptance Criteria**

* CI test passes locally with `cargo test`

---

## Milestone Plan

* **M1:** Stories 1–4 (scan → facts)
* **M2:** Stories 5–7 (packs → matches → report)
* **M3:** Stories 8–9 (init config + tests)

---

## Epic 2 — Pack Mining & Template Synthesis ("Mine")

### Purpose

Enable the “wow demo” workflow:

```bash
hix drill init --mine .
```

Where Drill:

1. Reads an existing codebase
2. Matches known Pattern Packs
3. Mines unknown repeating structures
4. Synthesizes **new Pattern Packs** (pattern + rules + templates)
5. Writes `hix.project.json`
6. Proves correctness via **round-trip validation** (generate → diff)

This epic is about making template creation **deterministic and transparent**, with optional LLM assistance only for naming/ergonomics.

---

### Success Criteria (Demo Definition)

On a previously unseen repo:

* `hix drill init --mine` produces:

  * `.hixdrill/facts.json`
  * `.hixdrill/report.md`
  * `.hixdrill/packs/<new-pack>/...`
  * `hix.project.json`
* Running `hix generate` after init yields either:

  * **no diffs** (ideal), or
  * only allowed diffs (configurable: whitespace/comments)
* All mined packs include a reproducible fixture test and pass `hix drill validate`

---

### Design Principles

* **Correctness via verification, not trust**: A mined pack is accepted only if the validator passes.
* **Stable output**: same repo state → same mined pack/template (ordering + placeholder naming stable).
* **Safety**: mining never overwrites original code; it creates packs/templates alongside and produces config.
* **Human-auditable**: every placeholder and inferred model field is traceable to source ranges and examples.

---

## Work Items (Stories)

### Story 10 — Unknown discovery + clustering

**Outcome:** Turn “unmatched code” into candidate clusters worth templating.

**Tasks**

* Define what counts as “unmatched” (symbols/files not claimed by any pack)
* Compute a **structural fingerprint** per candidate unit (type/function/file):

  * normalize identifiers
  * normalize literals (optional)
  * produce hash of AST shape
* Cluster candidates by fingerprint similarity
* Rank clusters by:

  * frequency
  * size/complexity
  * churn potential (optional later)
* Emit `.hixdrill/unknowns.json` and a human summary in `report.md`

**Acceptance Criteria**

* Running on a fixture repo produces at least one cluster with >1 member
* Cluster membership is stable across runs

---

### Story 11 — Template synthesis v1 (anti-unification over AST)

**Outcome:** Given a cluster of similar files, synthesize a draft template + placeholder map.

**Tasks**

* Choose a representative “base” sample (medoid or first stable sort)
* Align other samples to base using AST nodes
* Compute:

  * **fixed regions** (common across all)
  * **variable regions** (differ)
* Convert variable regions into placeholders of known kinds:

  * identifier placeholders (e.g., `{{TypeName}}`)
  * type reference placeholders
  * string literal placeholders
  * list placeholders (properties/members/imports)
* Emit:

  * `templates/<lang>/<name>.hix` (draft)
  * `synthesis.json` (placeholder map + examples)

**Acceptance Criteria**

* Draft template is produced deterministically for a cluster
* Placeholder map includes at least one example value per placeholder

---

### Story 12 — Model inference v1 (bindings schema)

**Outcome:** Derive a model schema from placeholders so Hix can render the template.

**Tasks**

* Define a minimal inferred model shape:

  * `Name` / `Namespace`
  * `Members[]` with `{ Name, Type }`
  * optional: `Annotations[]`, `Imports[]`
* Generate a `model.json` per instance or per pattern (pick one for v1)
* Ensure stable ordering and stable field names

**Acceptance Criteria**

* Generated model(s) can render the synthesized template without errors

---

### Story 13 — Pack emission (create new Pattern Pack from mined artifacts)

**Outcome:** Turn synthesized outputs into a reusable Pattern Pack in `.hixdrill/packs/`.

**Tasks**

* Choose pack naming rules (stable):

  * default: `mined/<language>/<cluster-id>`
  * allow override via CLI option
* Write pack folder with:

  * `pack.json` (metadata)
  * `pattern.json` (recognizer rules derived from cluster fingerprint)
  * `rules/<lang>.json` (bindings + file layout)
  * `templates/<lang>/...` (template)
  * `tests/fixtures` (copied minimal samples)
  * `tests/expected` (golden output or expected match list)

**Acceptance Criteria**

* A mined pack is fully self-contained and loadable by `hix-drill analyze`

---

### Story 14 — Round-trip validator (gatekeeper)

**Outcome:** Prove mined packs are correct via deterministic validation.

**Tasks**

* Implement `hix-drill validate --packs <folder> --repo <path>`
* For each pack instance:

  * Render templates using inferred model(s)
  * Compare to original source
* Support diff policies:

  * strict (byte-for-byte)
  * relaxed (ignore whitespace/comments) — optional
* If validation fails:

  * mark pack as `REJECTED` and keep artifacts
  * write a failure report with minimal diffs

**Acceptance Criteria**

* At least one mined pack passes strict validation on a fixture repo
* Failures produce actionable reports

---

### Story 15 — `init --mine` workflow + UX polish

**Outcome:** The single command that skeptics run.

**Tasks**

* Add flag: `hix-drill init --mine`
* Pipeline:

  1. scan → facts
  2. analyze with existing packs
  3. mine unknown clusters
  4. synthesize packs/templates
  5. validate (gate)
  6. write `hix.project.json`
  7. write `report.md`
* Add `--mine-limit` (top N clusters)
* Add `--mine-language` (only mine chosen language)

**Acceptance Criteria**

* `init --mine` produces packs + config + report in one shot

---

### Story 16 — Optional LLM assist (explicit, non-default)

**Outcome:** Improve naming and ergonomics without affecting determinism.

**Tasks**

* Add `--assist` flag (disabled by default)
* Assist only on:

  * pack name suggestions
  * placeholder names
  * description/docs
  * optional: rule readability
* Never accept an assisted pack unless validator passes

**Acceptance Criteria**

* `--assist` changes only metadata/names, not correctness

---

### Story 17 — Hix template emission primitives (sections, placeholders, escaping)

**Outcome:** Ensure mined templates are valid Hix templates and can express common structures like lists.

**Tasks**

* Define a small internal “template emitter” API that outputs Hix syntax deterministically:

  * scalar placeholder: `[[model.field]]`
  * list section: `[[items]] ... [[/items]]`
  * nested object: `[[item.subField]]`
* Add escaping rules so generated templates don’t break:

  * handle `[[` / `]]` appearing in source
  * handle braces/quotes safely when turned into literal text
* Add stable indentation rules for emitted templates

**Acceptance Criteria**

* For a DTO-like cluster, Drill can emit a valid template such as:

  * `public class [[model.className]] {` with a `[[prop]]` section
* Generated templates parse successfully with the Hix lexer/parser

---

### Story 18 — Validator uses the real Hix renderer

**Outcome:** Round-trip validation must render using the actual Hix engine to avoid “works in Drill only” drift.

**Tasks**

* In `hix-drill validate`, invoke the installed `hix` binary (subprocess) to render templates with inferred models
* Capture stdout/stderr and include in failure report
* Provide `--hix-path` override for non-standard installs
* Ensure deterministic env (newline normalization, encoding)

**Acceptance Criteria**

* Validator passes/fails based on real Hix rendering output
* Failure reports include the exact Hix invocation used

---

## Milestone Plan (Epic 2)

* **M4:** Stories 10–11 (clustering + template synthesis draft)
* **M5:** Stories 12–14 (model inference + pack emission + validator)
* **M6:** Stories 15–16 (`init --mine` end-to-end + optional assist)

---

## Notes / Future Epics

* Add pack authoring tools (`hix pack new/test/pack`)
* Add LLM-assisted “mint pack” command (explicit, opt-in)
* Add more languages and better extractors per language
* Add drift detection: compare repo state vs config-managed outputs
* Add refactoring operations: change a pack/pattern and re-render across repo
