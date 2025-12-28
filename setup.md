# Setup Guide

This guide walks you through setting up the Hix development environment on your local machine.

## Prerequisites

Before you begin, ensure you have the following installed on your system:

- **Stack**: The Haskell Tool Stack (version 3.0 or higher)
  - Download from: [https://docs.haskellstack.org/en/stable/install_and_upgrade/](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
  - Verify installation: `stack --version`

- **Git**: Version control system
  - Download from: [https://git-scm.com/downloads](https://git-scm.com/downloads)

- **Python 3** (optional): Required for documentation development
  - Download from: [https://www.python.org/downloads/](https://www.python.org/downloads/)
  - Required packages: `mkdocs` and `mkdocs-material`
  - Install with: `pip install mkdocs mkdocs-material`

- **VS Code** (optional): For syntax highlighting development
  - Download from: [https://code.visualstudio.com/](https://code.visualstudio.com/)
  - Required extension: VS Code Extension Manager (`npm install -g vsce`)

- **Inno Setup** (optional): For Windows installer creation (Windows only)
  - Download from: [https://jrsoftware.org/isinfo.php](https://jrsoftware.org/isinfo.php)

## Step 1: Clone the repository

1. Open your terminal or command prompt.

2. Navigate to the directory where you want to clone the project:

   ```bash
   cd /path/to/your/workspace
   ```

3. Clone the repository:

   ```bash
   git clone https://github.com/joelbugarini/hix.git
   ```

4. Navigate into the project directory:

   ```bash
   cd hix
   ```

## Step 2: Build the project

1. Build the project using Stack:

   ```bash
   stack build
   ```

   > [!NOTE]
   > The first build may take several minutes as Stack downloads and compiles all dependencies. Subsequent builds will be faster.

2. Verify the build completed successfully. You should see output ending with:

   ```
   Completed X action(s).
   ```

## Step 3: Run tests

1. Run the test suite to verify everything is working:

   ```bash
   stack test
   ```

2. Review the test output. All tests should pass (some tests may fail if optional dependencies aren't installed, but core functionality should work).

   > [!TIP]
   > If you encounter test failures related to the wizard functionality, ensure `stack install` has been run first.

## Step 4: Install the executable (optional)

To use the `hix` command globally on your system:

1. Install the executable:

   ```bash
   stack install
   ```

   This installs the `hix` executable to Stack's binary directory (typically `~/.local/bin` on Linux/macOS or `%APPDATA%\local\bin` on Windows).

2. Add Stack's binary directory to your PATH if it's not already there:

   **Linux/macOS:**
   ```bash
   export PATH=$PATH:~/.local/bin
   ```

   **Windows:**
   Add `%APPDATA%\local\bin` to your system PATH through System Properties.

3. Verify the installation:

   ```bash
   hix --help
   ```

## Step 5: Verify the setup

1. Test that the executable works by running a simple command:

   ```bash
   stack exec hix -- --help
   ```

   You should see the help message for Hix.

2. (Optional) Try generating a template with the example files:

   ```bash
   stack exec hix template.cs.hix Person.json
   ```

   This should generate output based on the template and model files in the project root.

## Troubleshooting

### Build fails with dependency errors

If you encounter dependency resolution errors:

1. Update Stack to the latest version:
   ```bash
   stack upgrade
   ```

2. Clean the build cache:
   ```bash
   stack clean
   ```

3. Rebuild:
   ```bash
   stack build
   ```

### Tests fail with "does not exist" errors

If wizard tests fail with file not found errors:

1. Ensure you've run `stack install` first:
   ```bash
   stack install
   ```

2. Verify the executable exists in Stack's binary directory:
   ```bash
   stack path --local-bin
   ls $(stack path --local-bin)
   ```

### GHC version conflicts

If you encounter GHC version issues:

1. Check the required GHC version in `stack.yaml`.

2. Let Stack manage GHC installation automatically (recommended). Stack will download the correct version on first build.

3. If you need to use a specific system GHC, configure it in `stack.yaml`:
   ```yaml
   system-ghc: true
   ```

### Documentation build issues

If you have issues building documentation:

1. Verify Python 3 is installed:
   ```bash
   python3 --version
   ```

2. Install required Python packages:
   ```bash
   pip install mkdocs mkdocs-material
   ```

3. Serve documentation locally:
   ```bash
   python -m mkdocs serve
   ```

## Development workflow

After setup, you can use these common commands:

### Build
```bash
stack build
```

### Test
```bash
stack test
```

### Run the executable
```bash
stack exec hix -- <arguments>
```

### Generate syntax grammar (for VS Code extension)
```bash
stack exec hix -- --gen-grammar utils/syntaxes/hix.tmLanguage.json
```

### Serve documentation locally
```bash
python -m mkdocs serve
```

### Deploy documentation
```bash
python -m mkdocs gh-deploy
```

## Next steps

- Read the [README.md](README.md) for project overview
- Check [docs/development.md](docs/development.md) for development guidelines
- Review [docs/getting-started.md](docs/getting-started.md) for usage examples
- Explore the [docs/](docs/) directory for comprehensive documentation

## See also

- [Haskell Stack documentation](https://docs.haskellstack.org/)
- [Project documentation](https://joelbugarini.github.io/hix/)
- [GitHub repository](https://github.com/joelbugarini/hix)

