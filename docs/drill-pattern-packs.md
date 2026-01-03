# Pattern Packs

Pattern Packs are reusable collections of code patterns that hix-drill can match against your codebase. This guide explains how Pattern Packs work and how to use them.

## What are Pattern Packs?

Pattern Packs are JSON files that define:

- **Metadata**: Name, version, description
- **Patterns**: Match rules that identify specific code structures
- **Examples**: Common patterns like data structures, HTTP endpoints, UI components

## Pack structure

A Pattern Pack consists of two files:

1. **pack.json**: Pack metadata
2. **pattern.json**: Pattern definitions

### pack.json

```json
{
  "schema_version": "1.0.0",
  "name": "data-structures",
  "version": "1.0.0",
  "description": "Patterns for DTO-like data structures",
  "author": "Your Name"
}
```

### pattern.json

```json
[
  {
    "name": "data-structure",
    "description": "DTO-like: type with only public fields/properties",
    "match_conditions": {
      "symbol_kind": "type",
      "member_predicates": {
        "only_fields": true
      },
      "language": ["csharp", "python"]
    }
  }
]
```

## Pattern match conditions

Patterns use a domain-specific language (DSL) for matching:

### Symbol kind

Match by symbol type:

```json
{
  "symbol_kind": "type"  // or "function", "variable"
}
```

### Member predicates

Match based on member characteristics:

```json
{
  "member_predicates": {
    "only_fields": true,        // Only fields, no methods
    "min_fields": 2,            // At least 2 fields
    "max_methods": 0            // No methods
  }
}
```

### Annotation predicates

Match based on annotations/decorators:

```json
{
  "annotation_predicates": {
    "has_annotation": "Route",  // Has specific annotation
    "annotation_value": {        // Annotation with value
      "key": "method",
      "value": "GET"
    }
  }
}
```

### Language filtering

Restrict to specific languages:

```json
{
  "language": ["typescript", "python", "csharp"]
}
```

### Name patterns

Match by naming convention:

```json
{
  "name_pattern": "^[A-Z].*Component$"  // Regex pattern
}
```

## Built-in patterns

Common patterns you can use:

### Data structure

Matches DTO-like classes with only fields:

```json
{
  "name": "data-structure",
  "match_conditions": {
    "symbol_kind": "type",
    "member_predicates": {
      "only_fields": true
    }
  }
}
```

### HTTP endpoint

Matches functions that look like HTTP endpoints:

```json
{
  "name": "http-endpoint",
  "match_conditions": {
    "symbol_kind": "function"
  }
}
```

### TypeScript component

Matches React/Angular components:

```json
{
  "name": "typescript-component",
  "match_conditions": {
    "symbol_kind": "type",
    "language": ["typescript", "tsx"],
    "annotation_predicates": {
      "has_annotation": "Component"
    }
  }
}
```

## Using Pattern Packs

### Load packs from directory

```bash
hix-drill analyze . --packs ./pattern-packs
```

hix-drill loads all packs from subdirectories:

```
pattern-packs/
├── data-structures/
│   ├── pack.json
│   └── pattern.json
└── http-endpoints/
    ├── pack.json
    └── pattern.json
```

### Pack discovery

hix-drill automatically:
- Discovers all subdirectories in the packs directory
- Loads `pack.json` and `pattern.json` from each
- Validates pack schema
- Reports errors for invalid packs

## Creating custom patterns

### Step 1: Create pack directory

```bash
mkdir -p my-patterns/my-pack
cd my-patterns/my-pack
```

### Step 2: Create pack.json

```json
{
  "schema_version": "1.0.0",
  "name": "my-pack",
  "version": "1.0.0",
  "description": "My custom patterns",
  "author": "Your Name"
}
```

### Step 3: Create pattern.json

```json
[
  {
    "name": "my-pattern",
    "description": "Matches my specific code pattern",
    "match_conditions": {
      "symbol_kind": "type",
      "member_predicates": {
        "min_fields": 3
      }
    }
  }
]
```

### Step 4: Test the pattern

```bash
hix-drill analyze . --packs ./my-patterns
```

Check the report to see if your pattern matched:

```bash
cat .hixdrill/report.json | jq '.matches_by_pattern."my-pattern"'
```

## Pattern matching process

1. **Extract facts**: hix-drill extracts code facts from your repository
2. **Load patterns**: Patterns are loaded from Pattern Packs
3. **Match**: Each pattern is matched against all facts
4. **Bind**: Matched symbols are bound to pattern instances
5. **Report**: Results are included in the analysis report

## Best practices

- **Start simple**: Begin with basic patterns and refine
- **Test incrementally**: Test patterns on small codebases first
- **Document patterns**: Add clear descriptions to help others understand
- **Version packs**: Use semantic versioning for pack updates
- **Group related patterns**: Keep related patterns in the same pack

## Next steps

- [View pattern examples](drill-examples.md)
- [Learn about the match DSL](drill-pattern-dsl.md)
- [Understand match results](drill-output-format.md)

