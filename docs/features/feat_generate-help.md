# `hix generate` Usage Help

## Description
When running `hix generate` without any arguments, the CLI now provides helpful usage information, including examples and available options. This output is consistent with the help/man documentation, making it easier for users to discover how to use the command.

## Motivation
- Improve user experience and onboarding
- Reduce confusion for new users

## Key Changes
- `hix generate` with no arguments prints usage help
- Help text is consistent with `hix help` and `hix man`
- Examples and option descriptions are included

## Acceptance Criteria
- [ ] `hix generate` with no arguments prints usage help
- [ ] Help text is consistent with other CLI help/man outputs
- [ ] Documentation updated 