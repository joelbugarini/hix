# Init: Create Default Model Example

## Description
During the `hix init` process, Hix now creates a default model file (e.g., `Person.json`) in the models directory. This example helps users understand the expected model format and provides a starting point for their own models.

## Motivation
- Help new users get started quickly
- Provide a reference for the model format
- Reduce friction in the onboarding process

## Key Changes
- `hix init` creates a default model file (e.g., `Person.json`) in `.hix/models/`
- The example model includes typical fields and types
- Documentation updated to mention the default model

## Acceptance Criteria
- [x] `hix init` creates a default model file in `.hix/models/Person.json`
- [x] Example model is valid and helpful
- [x] Documentation updated
- [x] Feature is covered by an automated test

## Implementation Notes
- The default model file is only created if it does not already exist.
- The file is named `Person.json` and contains a sample model with a variety of property types.
- This helps new users get started quickly and provides a reference for model structure.
- The feature is tested in the test suite to ensure the file is created during `hix init`.

## Acceptance Criteria
- [ ] `hix init` creates a default model file
- [ ] Example model is valid and helpful
- [ ] Documentation updated 