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
- [ ] `hix init` creates a default model file
- [ ] Example model is valid and helpful
- [ ] Documentation updated 