# Development Guide

This guide is for developers who want to contribute to Hix or work with the source code.

## Building from Source

### Using Stack
```bash
stack build
stack install
```

### Using Cabal
```bash
cabal build
cabal install
```

## Development Commands

### Building
```bash
stack build
```

### Running Tests
```bash
stack test
```

### Generating Template
```bash
stack exec hix template.cs.hix Person.json
```

### Syntax Highlighting
To generate and install syntax highlighting for VS Code:
```bash
stack exec hix -- --gen-grammar util/syntaxes/hix.tmLanguage.json
vsce package
code --install-extension hix-syntax-0.0.7.vsix
```

### Documentation
To serve documentation locally:
```bash
python -m mkdocs serve
```

To deploy documentation:
```bash
python -m mkdocs gh-deploy
``` 