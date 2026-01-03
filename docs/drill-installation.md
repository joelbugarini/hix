# Install hix-drill

This guide shows you how to install and build hix-drill from source.

## Prerequisites

- **Rust**: Version 1.70 or later
- **Cargo**: Rust's package manager (included with Rust)
- **Git**: For cloning the repository

### Verify Rust installation

Check if Rust is installed:

```bash
rustc --version
cargo --version
```

If Rust isn't installed, follow the [official Rust installation guide](https://www.rust-lang.org/tools/install).

## Build from source

### Clone the repository

```bash
git clone https://github.com/joelbugarini/hix.git
cd hix/drill
```

### Build the project

Build the release version:

```bash
cargo build --release
```

The binary will be located at `target/release/hix-drill`.

### Install locally

Install to your Cargo bin directory:

```bash
cargo install --path .
```

This installs `hix-drill` to `~/.cargo/bin/` (or `%USERPROFILE%\.cargo\bin` on Windows).

### Add to PATH

Ensure `~/.cargo/bin` is in your PATH:

**Linux/macOS:**
```bash
export PATH="$HOME/.cargo/bin:$PATH"
```

Add to `~/.bashrc` or `~/.zshrc` for persistence.

**Windows:**
Add `%USERPROFILE%\.cargo\bin` to your system PATH via System Properties.

## Verify installation

Test the installation:

```bash
hix-drill --version
hix-drill --help
```

You should see version information and help text.

## Development build

For development, use debug builds:

```bash
cargo build
cargo run -- --help
```

## Troubleshooting

### Build errors

If you encounter build errors:

1. **Update Rust**: `rustup update`
2. **Clean build**: `cargo clean && cargo build`
3. **Check dependencies**: Ensure all system dependencies are installed

### Tree-sitter grammars

Tree-sitter grammars are included as Rust crates. If you encounter parsing issues:

- Ensure you're using Rust edition 2021 or later
- Check that `tree-sitter` version 0.23 is compatible

## Next steps

- [Learn about hix-drill commands](drill-commands.md)
- [Get started with your first analysis](drill-quickstart.md)

