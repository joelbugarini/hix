# hix-drill examples

This guide provides practical examples of using hix-drill in common scenarios.

## Example 1: Analyze a TypeScript project

Analyze a TypeScript/React project:

```bash
# Create a simple TypeScript file
cat > UserComponent.ts << 'EOF'
import React from 'react';

export class UserComponent {
  name: string;
  email: string;
  
  render() {
    return <div>{this.name}</div>;
  }
}
EOF

# Analyze with pattern packs
hix-drill analyze . --packs ./pattern-packs

# View results
cat .hixdrill/report.md
```

## Example 2: Find data structures

Identify DTO-like classes in a C# project:

```bash
# Create Pattern Pack
mkdir -p pattern-packs/data-structures
cat > pattern-packs/data-structures/pack.json << 'EOF'
{
  "schema_version": "1.0.0",
  "name": "data-structures",
  "version": "1.0.0",
  "description": "DTO-like data structures"
}
EOF

cat > pattern-packs/data-structures/pattern.json << 'EOF'
[
  {
    "name": "data-structure",
    "description": "Class with only properties",
    "match_conditions": {
      "symbol_kind": "type",
      "member_predicates": {
        "only_fields": true
      },
      "language": ["csharp"]
    }
  }
]
EOF

# Analyze
hix-drill analyze . --packs ./pattern-packs

# Check matches
cat .hixdrill/matches.json | jq '.instances[] | select(.pattern_name == "data-structure")'
```

## Example 3: Initialize Hix project

Generate Hix configuration from analysis:

```bash
# Run init with pattern packs
hix-drill init . --packs ./pattern-packs

# View generated config
cat .hix/drill/project.json | jq '.pattern_mappings[]'
```

Output shows pattern instances mapped to model and template files:

```json
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
```

## Example 4: Compare before and after

Track changes in your codebase:

```bash
# Initial analysis
hix-drill analyze . --packs ./pattern-packs
cp .hixdrill/report.json baseline-report.json

# Make code changes
# ... edit files ...

# Re-analyze
hix-drill analyze . --packs ./pattern-packs

# Compare
diff baseline-report.json .hixdrill/report.json
```

## Example 5: Find HTTP endpoints

Identify API endpoints in TypeScript:

```bash
# Create HTTP endpoint pattern
cat > pattern-packs/http-endpoints/pattern.json << 'EOF'
[
  {
    "name": "http-endpoint",
    "description": "Function that looks like HTTP endpoint",
    "match_conditions": {
      "symbol_kind": "function",
      "language": ["typescript"]
    }
  }
]
EOF

# Analyze
hix-drill analyze . --packs ./pattern-packs

# List endpoints
cat .hixdrill/matches.json | jq '.instances[] | select(.pattern_name == "http-endpoint") | .symbol_name'
```

## Example 6: Generate coverage report

Get coverage metrics:

```bash
# Analyze
hix-drill analyze . --packs ./pattern-packs

# Extract coverage
cat .hixdrill/report.json | jq '.coverage'
```

Output:

```json
{
  "matched_symbols": 15,
  "unmatched_symbols": 35,
  "symbol_coverage_percent": 30.0
}
```

## Example 7: Work with multiple languages

Analyze a multi-language project:

```bash
# Project structure
my-project/
├── backend/
│   └── api.py          # Python
├── frontend/
│   └── App.tsx         # TypeScript
└── shared/
    └── models.cs       # C#

# Analyze all languages
hix-drill analyze . --packs ./pattern-packs

# View language breakdown
cat .hixdrill/facts.json | jq '.files[] | .language' | sort | uniq -c
```

## Example 8: Custom pattern for specific convention

Match code following a specific naming convention:

```bash
cat > pattern-packs/custom/pattern.json << 'EOF'
[
  {
    "name": "service-class",
    "description": "Classes ending with 'Service'",
    "match_conditions": {
      "symbol_kind": "type",
      "name_pattern": ".*Service$"
    }
  }
]
EOF

# Analyze
hix-drill analyze . --packs ./pattern-packs

# Find service classes
cat .hixdrill/matches.json | jq '.instances[] | select(.pattern_name == "service-class")'
```

## Example 9: Integration with CI/CD

Add hix-drill to your CI pipeline:

```yaml
# .github/workflows/analyze.yml
name: Code Analysis

on: [push, pull_request]

jobs:
  analyze:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Install hix-drill
        run: cargo install --path drill
      - name: Analyze codebase
        run: hix-drill analyze . --packs ./pattern-packs
      - name: Upload report
        uses: actions/upload-artifact@v3
        with:
          name: analysis-report
          path: .hixdrill/report.json
```

## Example 10: Generate Hix models from analysis

Use init output to create Hix models:

```bash
# Initialize project
hix-drill init . --packs ./pattern-packs

# Extract pattern instances
cat .hix/drill/project.json | jq '.pattern_mappings[].instances[]' > instances.json

# Generate Hix models (using hix or custom script)
# ... process instances.json to create model files ...
```

## Next steps

- [Learn about Pattern Packs](drill-pattern-packs.md)
- [Understand output format](drill-output-format.md)
- [Read the command reference](drill-commands.md)

