# Model Property Metadata

## Description
Model properties can now include optional metadata fields such as `required`, `minlength`, `maxlength`, and others. These fields are not mandatory, but when present, they can be accessed within templates to enable more advanced code generation and validation scenarios.

- Supported metadata fields include (but are not limited to):
  - `required` (boolean)
  - `minlength` (integer)
  - `maxlength` (integer)
  - `pattern` (string)
  - `default` (any)
- These fields are optional for each property in the model JSON.
- Templates can reference these fields to generate validation logic, annotations, or documentation.

## Motivation
- Enable richer code generation and validation scenarios
- Allow templates to generate more expressive and robust code
- Provide a foundation for future extensibility (custom metadata)

## Key Changes
- Model JSON schema supports additional metadata fields on properties
- Template rendering logic can access and use these fields
- Documentation and examples updated to show usage

## Acceptance Criteria
- [ ] Model properties accept optional metadata fields
- [ ] Templates can access and use these fields
- [ ] Documentation and examples are updated 