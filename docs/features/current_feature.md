# Bug Report: Corrupt YAML Configuration File Generation in `hix init`

## Description
When running `hix init`, the generated `config.yaml` file contains formatting issues that make it invalid YAML. Specifically:
1. The file contains square brackets `[]` in keys which are not valid YAML syntax
2. There are indentation errors in the generated YAML structure

## Impact
- The generated configuration file cannot be properly parsed by YAML parsers
- Users cannot proceed with code generation until they manually fix the YAML file
- This affects the initial setup experience of new users

## Steps to Reproduce
1. Run `hix init` in a new directory
2. Select an architecture type when prompted
3. Check the generated `.hix/config.yaml` file

## Expected Behavior
The generated `config.yaml` should be a valid YAML file with:
- Proper indentation
- Valid YAML syntax for keys and values
- No square brackets in keys
- Consistent formatting throughout the file

## Current Behavior
The generated YAML file contains:
- Invalid YAML syntax with square brackets in keys
- Inconsistent indentation
- Potentially malformed template configurations

## Suggested Fix
1. Add YAML validation tests to ensure the generated configuration file is valid
2. Fix the YAML generation code in `src/Wizard.hs` to properly format the output
3. Add proper escaping for special characters in keys and values
4. Implement consistent indentation rules for the YAML generation

## Priority
High - This affects the initial setup experience and prevents users from using the tool until manually fixed.

## Additional Context
The issue appears to be in the YAML generation code in `src/Wizard.hs`, specifically in the `formatLayer` and `formatTemplate` functions. These functions need to be updated to ensure proper YAML formatting and escaping of special characters.
