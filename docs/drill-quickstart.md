# Quickstart: Analyze your first repository

This guide walks you through analyzing a repository with hix-drill.

## Prerequisites

- hix-drill installed (see [Installation guide](drill-installation.md))
- A code repository to analyze
- Optional: Pattern Packs for pattern matching

## Step 1: Prepare a repository

Create a simple test repository or use an existing one:

```bash
mkdir my-test-repo
cd my-test-repo
```

Add some source files. For example, create `Person.cs`:

```csharp
namespace TestNamespace
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }
}
```

## Step 2: Run a scan

Scan the repository to extract facts:

```bash
hix-drill scan .
```

This creates `.hixdrill/facts.json` with extracted code facts.

### View the facts

```bash
cat .hixdrill/facts.json | jq '.symbols[] | {name: .name, kind: .kind}'
```

You should see the `Person` class listed as a symbol.

## Step 3: Run full analysis

Analyze the repository with pattern matching:

```bash
hix-drill analyze . --packs ./pattern-packs
```

> **Note**: If you don't have Pattern Packs yet, you can run `analyze` without the `--packs` option. It will still generate facts and a report, but without pattern matches.

### View the report

```bash
cat .hixdrill/report.md
```

The report shows:
- Total files scanned
- Symbols found
- Pattern matches (if packs provided)
- Coverage metrics
- Unknown symbols

## Step 4: Initialize Hix project

Generate Hix project configuration:

```bash
hix-drill init . --packs ./pattern-packs
```

This creates `.hix/drill/project.json` mapping recognized patterns to Hix templates and models.

### View the configuration

```bash
cat .hix/drill/project.json | jq '.pattern_mappings[]'
```

## Understanding the output

### Facts structure

Facts are organized into sections:

- **files**: Source file metadata
- **symbols**: Types, functions, variables
- **members**: Properties, methods, fields
- **relations**: Dependencies between symbols
- **annotations**: Decorators and attributes

### Report structure

The report includes:

- **Summary**: Overview of analysis results
- **Coverage**: Percentage of code matched by patterns
- **Pattern matches**: Details about matched patterns
- **Unknowns**: Code that didn't match any pattern

### Configuration structure

The init configuration maps:

- **packs_used**: Pattern packs used in analysis
- **pattern_mappings**: Pattern instances → models/templates
- **hix_config**: Reference to main Hix config

## Next steps

- [Learn about Pattern Packs](drill-pattern-packs.md)
- [Create custom patterns](drill-create-patterns.md)
- [Integrate with Hix](drill-hix-integration.md)

