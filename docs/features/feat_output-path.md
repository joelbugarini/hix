# Output Path Configuration

## Description
Add support for specifying an `output` field in the configuration file. This field determines where generated code files should be written. The output path can include template variables (e.g., `{Model}`) that are replaced with actual model names or other context-specific values during code generation.

- The `output` field is added to the config file.
- The output path can include placeholders such as `{Model}` to dynamically generate paths like `Views/Employee/typescript/component.ts`.
- All generated files will be written to the resolved output path.

## Motivation
- Allow users to control where generated code is placed.
- Support flexible and organized output directory structures based on model names or other variables.
- Enable better integration with existing project layouts and workflows.

## Key Changes
- Config file schema updated to support an `output` field with path templates.
- Code generation logic updated to resolve and use the output path, replacing placeholders as needed.
- Documentation and examples updated to demonstrate usage.

## Acceptance Criteria
- [ ] Config file supports an `output` field with path templates (e.g., `Views/{Model}/typescript/component.ts`).
- [ ] Code generation writes files to the correct resolved output path, replacing placeholders.
- [ ] Documentation and sample configs are updated.
- [ ] Tests cover config parsing and output path resolution. 