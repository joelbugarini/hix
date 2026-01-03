# hix-drill commands

This reference documents all available hix-drill commands and their options.

## Command overview

hix-drill provides three main commands:

- `scan`: Scan repository and extract facts
- `analyze`: Full analysis with pattern matching and reporting
- `init`: Initialize Hix project configuration

## scan

Scans a repository, parses source files, and extracts facts into `.hixdrill/facts.json`.

### Syntax

```bash
hix-drill scan <PATH>
```

### Parameters

| Parameter | Description | Required |
|-----------|-------------|----------|
| `PATH` | Path to the repository to scan | Yes |

### Example

```bash
hix-drill scan ./my-project
```

### Output

Creates `.hixdrill/facts.json` containing:
- File metadata (path, language, hash)
- Symbols (types, functions, variables)
- Members (properties, methods, fields)
- Relations (imports, calls, extends)
- Annotations (decorators, attributes)

### Supported languages

- TypeScript/TSX
- Python
- C#
- HTML

Files in unsupported languages are skipped with a warning.

## analyze

Performs a full analysis: scans, parses, extracts facts, matches patterns, and generates reports.

### Syntax

```bash
hix-drill analyze <PATH> [OPTIONS]
```

### Parameters

| Parameter | Description | Required |
|-----------|-------------|----------|
| `PATH` | Path to the repository to analyze | Yes |

### Options

| Option | Description | Required |
|--------|-------------|----------|
| `--packs <PATH>` | Path to Pattern Packs directory | No |

### Example

```bash
# Analyze without pattern matching
hix-drill analyze ./my-project

# Analyze with pattern packs
hix-drill analyze ./my-project --packs ./pattern-packs
```

### Output

Creates multiple files in `.hixdrill/`:

- `facts.json`: Extracted code facts
- `matches.json`: Pattern match results (if packs provided)
- `report.json`: Machine-readable analysis report
- `report.md`: Human-readable analysis report

### Report contents

The report includes:

- **Summary**: Total files, symbols, matches, patterns
- **Coverage**: Percentage of symbols matched
- **Pattern matches**: Count and samples per pattern
- **Unknowns**: Unmatched symbols grouped by kind

## init

Initializes a Hix project by generating `.hix/drill/project.json` configuration file.

### Syntax

```bash
hix-drill init <PATH> [OPTIONS]
```

### Parameters

| Parameter | Description | Required |
|-----------|-------------|----------|
| `PATH` | Path to the repository to initialize | Yes |

### Options

| Option | Description | Required |
|--------|-------------|----------|
| `--packs <PATH>` | Path to Pattern Packs directory | No |

### Example

```bash
hix-drill init ./my-project --packs ./pattern-packs
```

### Output

Creates `.hix/drill/project.json` containing:

```json
{
  "packs_used": [
    {
      "name": "data-structures",
      "version": "1.0.0",
      "path": "../packs/data-structures"
    }
  ],
  "pattern_mappings": [
    {
      "pattern": "data-structure",
      "instances": [
        {
          "symbol_id": "sym_0",
          "symbol_name": "Person",
          "source_file": "./Person.cs",
          "model_file": ".hix/models/Person.json",
          "template_file": ".hix/templates/data-structure.hix"
        }
      ]
    }
  ],
  "hix_config": ".hix/config.yaml"
}
```

### What it does

1. Scans the repository
2. Parses source files
3. Extracts facts
4. Matches patterns (if packs provided)
5. Generates configuration mapping patterns to models/templates

## Global options

All commands support these global options:

| Option | Description |
|--------|-------------|
| `--help` | Show help message |
| `--version` | Show version information |

## Exit codes

- `0`: Success
- `1`: Error (invalid arguments, file not found, etc.)

## Next steps

- [Learn about Pattern Packs](drill-pattern-packs.md)
- [View usage examples](drill-examples.md)
- [Understand the output format](drill-output-format.md)

