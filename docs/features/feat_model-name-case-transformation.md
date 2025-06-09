# Model Name Case Transformation

## Overview
This feature extends the existing case transformation functions (snake_case, lower, upper) to work with model names in addition to property names. This will provide more flexibility in template generation by allowing case transformations on both model and property names. A new kebab-case function will also be added to support this common naming convention.

## Requirements

### Functional Requirements
1. Support case transformation functions for model.Name:
   - snake_case(model.Name)
   - lower(model.Name)
   - upper(model.Name)
   - kebab_case(model.Name)
2. Maintain existing functionality for property names
3. Handle edge cases:
   - Empty model names
   - Model names with special characters
   - Model names with numbers

### Technical Requirements
1. Update the template engine to recognize model.Name as a valid target for case transformations
2. Add unit tests for each case transformation function with model names
3. Add integration tests for templates using model name transformations
4. Update documentation to reflect new functionality
5. Implement new kebab_case function with the following rules:
   - Convert camelCase to kebab-case (e.g., "userName" -> "user-name")
   - Convert PascalCase to kebab-case (e.g., "UserName" -> "user-name")
   - Convert snake_case to kebab-case (e.g., "user_name" -> "user-name")
   - Handle numbers appropriately (e.g., "user2Name" -> "user-2-name")

### Documentation Requirements
1. Update template function documentation to include model.Name examples
2. Add usage examples in the documentation
3. Document any edge cases or limitations

## Implementation Plan
1. Modify the template engine to parse model.Name expressions
2. Extend case transformation functions to handle model names
3. Add test cases
4. Update documentation
5. Add example templates demonstrating the new functionality

## Testing Strategy
1. Unit tests for each case transformation function
2. Integration tests with various template scenarios
3. Edge case testing
4. Performance testing to ensure no degradation

## Success Criteria
1. All case transformation functions work correctly with model.Name
2. No regression in existing functionality
3. Documentation is complete and clear
4. All tests pass
5. Performance impact is minimal

## Dependencies
- Existing template engine
- Case transformation functions
- Template parser

## Notes
- This feature enhances existing functionality without breaking changes
- Should be backward compatible
- May require updates to example templates 