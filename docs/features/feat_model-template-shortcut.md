# Model and Template Shortcuts in CLI

## Description
When running Hix commands from the project root, users can now refer to models and templates using short names or identifiers, rather than full paths. This makes the CLI more convenient and user-friendly.

- Models can be referenced as `--model Person` or `--model Person.json` instead of `--model .hix/models/Person.json`.
- Templates can be referenced as `--template Domain/Entity`, `--template Domain/Entity.cs`, or `--template Domain/Entity.cs.hix` instead of the full path.
- Templates can also be referenced by a name identifier defined in the config YAML (e.g., `--template customName`).

## Motivation
- Improve CLI ergonomics and reduce typing
- Make it easier to use Hix in typical project layouts
- Support template lookup by logical name, not just path

## Key Changes
- CLI resolves model and template arguments as short names if run from the project root
- Template config supports a `name` identifier for each template
- Users can reference templates by name (e.g., `--template customName`)
- Documentation updated with examples

## Acceptance Criteria
- [ ] Models and templates can be referenced by short name from project root
- [ ] Templates can be referenced by name identifier from config
- [ ] Full paths are still supported
- [ ] Documentation updated 