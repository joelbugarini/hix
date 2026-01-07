# Integration Tests

## Running Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_ai_config_file

# Run tests with output
cargo test -- --nocapture

# Run ignored tests (requires API key)
cargo test -- --ignored
```

## AI Assistance Tests

### Setting API Key for Testing

There are two ways to set the API key for testing:

1. **Test-Specific Environment Variable (Recommended for CI/CD)**:
   ```bash
   export HIX_DRILL_TEST_AI_API_KEY=your-api-key-here
   cargo test test_ai_assistance_integration -- --ignored
   ```

2. **Regular Environment Variable**:
   ```bash
   export HIX_DRILL_AI_API_KEY=your-api-key-here
   cargo test test_ai_assistance_integration -- --ignored
   ```

The `HIX_DRILL_TEST_AI_API_KEY` takes precedence over `HIX_DRILL_AI_API_KEY` for testing purposes. This allows you to use a different API key for testing than for regular use.

### Test Categories

- **Unit Tests**: Test individual modules (run with `cargo test --lib`)
- **Integration Tests**: Test full workflows (run with `cargo test`)
- **Ignored Tests**: Tests that require API keys (run with `cargo test -- --ignored`)

### AI Tests

- `test_ai_config_file`: Tests AI config file creation and reading (no API key required)
- `test_ai_assistance_integration`: Tests full AI assistance workflow (requires API key, marked with `#[ignore]`)
- `test_init_mine_without_assist`: Tests that mining works without AI assistance (no API key required)

### Example: Running AI Tests

```bash
# Set test API key
export HIX_DRILL_TEST_AI_API_KEY=sk-...

# Run all tests including ignored ones
cargo test -- --ignored

# Or run just the AI integration test
cargo test test_ai_assistance_integration -- --ignored --nocapture
```
