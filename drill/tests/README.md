# hix-drill Integration Tests

This directory contains integration tests using golden files (snapshot tests) to ensure deterministic output and prevent regressions.

## Test Structure

```
tests/
├── integration_test.rs          # Integration tests
└── fixtures/
    └── sample-repo/             # Test fixture repository
        ├── src/                 # Sample source code
        ├── packs/               # Pattern packs for testing
        └── golden/             # Expected output files (golden files)
            ├── project.json     # Expected init output
            ├── matches.json     # Expected matches output
            └── report.json      # Expected report output
```

## Running Tests

```bash
# Run all integration tests
cargo test --test integration_test

# Run specific test
cargo test --test integration_test test_init_golden
```

## Golden Files

Golden files are the expected output files that serve as the "truth" for what the tool should produce. When tests run:

1. **First run**: If a golden file doesn't exist, it's created from the actual output
2. **Subsequent runs**: Actual output is compared against the golden file
3. **Mismatch**: Test fails if output doesn't match golden file

## Updating Golden Files

If you intentionally change the output format, update the golden files:

```bash
# Delete golden files to regenerate them
rm tests/fixtures/sample-repo/golden/*.json

# Run tests again (they will create new golden files)
cargo test --test integration_test
```

## Test Coverage

- **test_init_golden**: Tests `hix-drill init` command
  - Verifies `.hix/drill/project.json` is created
  - Compares against golden file

- **test_analyze_golden**: Tests `hix-drill analyze` command
  - Verifies `.hixdrill/matches.json` is created
  - Verifies `.hixdrill/report.json` is created
  - Compares against golden files

## Adding New Tests

1. Add test fixture code to `tests/fixtures/sample-repo/src/`
2. Add pattern packs to `tests/fixtures/sample-repo/packs/`
3. Run the test once to generate golden files
4. Review and commit the golden files

