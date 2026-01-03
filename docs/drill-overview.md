# hix-drill overview

**hix-drill** is a Rust-based command-line tool that analyzes existing codebases, identifies recognized code patterns, and generates configuration files for the Hix code generation system. It bridges the gap between analyzing existing code and generating new code using Hix templates.

## What is hix-drill?

hix-drill is a static analysis tool that:

- **Scans** repositories to discover source files
- **Parses** source code into language-agnostic facts using Tree-sitter
- **Matches** code patterns using Pattern Packs
- **Reports** analysis results with coverage metrics
- **Initializes** Hix project configuration files

## Key concepts

### Facts

Facts are language-agnostic representations of code structure extracted from source files. They include:

- **Files**: Source file metadata (path, language, hash)
- **Symbols**: Types, functions, variables
- **Members**: Properties, methods, fields
- **Relations**: Imports, calls, extends, implements
- **Annotations**: Decorators, attributes, metadata

### Pattern Packs

Pattern Packs are reusable collections of code patterns that can be matched against facts. Each pack contains:

- **Metadata**: Name, version, description
- **Patterns**: Match rules that identify specific code structures
- **Examples**: Data structures, HTTP endpoints, UI components

### Pattern Matching

The pattern matcher uses a domain-specific language (DSL) to match patterns against facts:

- Symbol kind matching (type, function, variable)
- Member predicates (only fields, method count, etc.)
- Annotation predicates (has decorator, attribute value, etc.)
- Language filtering (TypeScript, Python, C#, etc.)

## Architecture

hix-drill follows a pipeline architecture:

```
Repository → Scanner → Parser → Extractor → Matcher → Reporter/Init Writer
```

1. **Scanner**: Walks the repository, detects languages, computes file hashes
2. **Parser**: Uses Tree-sitter to parse source files into ASTs
3. **Extractor**: Converts ASTs into canonical facts
4. **Matcher**: Matches patterns from Pattern Packs against facts
5. **Reporter**: Generates analysis reports (JSON + Markdown)
6. **Init Writer**: Creates Hix project configuration files

## Supported languages

Currently supported languages (via Tree-sitter):

- TypeScript/TSX
- Python
- C#
- HTML

## Output files

hix-drill generates several output files:

- `.hixdrill/facts.json`: Extracted code facts
- `.hixdrill/matches.json`: Pattern match results
- `.hixdrill/report.json`: Analysis report (machine-readable)
- `.hixdrill/report.md`: Analysis report (human-readable)
- `.hix/drill/project.json`: Hix project configuration

## Next steps

- [Install hix-drill](drill-installation.md)
- [Learn about commands](drill-commands.md)
- [Understand Pattern Packs](drill-pattern-packs.md)
- [View usage examples](drill-examples.md)

