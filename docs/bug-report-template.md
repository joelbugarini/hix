# Bug Report Template and Guidelines

This document outlines the standardized process for creating and managing bug reports in the Hix project.

## Bug Report Template

```markdown
# Bug Report: [Brief Description]

## Description
[Provide a clear and concise description of the bug]

## Impact
- [List the impact on users/functionality]
- [Describe any workarounds available]

## Steps to Reproduce
1. [Step 1]
2. [Step 2]
3. [Step 3]

## Expected Behavior
[Describe what should happen]

## Current Behavior
[Describe what actually happens]

## Environment
- Hix Version: [version]
- Operating System: [OS]
- Additional Context: [any relevant environment details]

## Suggested Fix
[Optional: If you have a suggestion for fixing the bug]

## Priority
- High: Critical functionality is broken
- Medium: Important but not critical
- Low: Minor issue with workaround available

## Additional Context
[Add any other context about the problem here]
```

## Bug Report Guidelines

### 1. Title
- Be specific and concise
- Include the affected component if possible
- Example: "Bug Report: Corrupt YAML Configuration File Generation in `hix init`"

### 2. Description
- Clearly explain what the bug is
- Include error messages if available
- Describe when and where the bug occurs

### 3. Impact
- Explain how the bug affects users
- List any workarounds if available
- Note if the bug blocks critical functionality

### 4. Steps to Reproduce
- List exact steps to trigger the bug
- Include any necessary setup or configuration
- Be specific about input values and conditions

### 5. Expected vs Current Behavior
- Clearly contrast what should happen vs what actually happens
- Include screenshots or logs if helpful
- Note any error messages or unexpected outputs

### 6. Environment
- Specify the Hix version
- List the operating system
- Include any relevant environment details

### 7. Priority
- Use the following priority levels:
  - High: Critical functionality is broken
  - Medium: Important but not critical
  - Low: Minor issue with workaround available

### 8. Additional Context
- Include any relevant logs or error messages
- Add screenshots if helpful
- Reference related issues or pull requests

## Bug Report Review Process

1. **Initial Review**
   - Verify the bug report is complete
   - Check for reproducibility
   - Assign priority level

2. **Technical Review**
   - Analyze the root cause
   - Determine fix complexity
   - Plan implementation

3. **Fix Implementation**
   - Create a fix branch
   - Implement the solution
   - Add tests
   - Update documentation

4. **Verification**
   - Test the fix
   - Verify the bug is resolved
   - Ensure no regressions

## Best Practices

1. **Be Specific**
   - Use exact error messages
   - Include specific versions
   - Provide precise steps

2. **Be Concise**
   - Focus on essential details
   - Avoid unnecessary information
   - Use clear language

3. **Be Complete**
   - Include all necessary context
   - Provide reproduction steps
   - Note any workarounds

4. **Be Professional**
   - Use clear, professional language
   - Avoid emotional language
   - Focus on facts

## Example Bug Report

```markdown
# Bug Report: Corrupt YAML Configuration File Generation in `hix init`

## Description
When running `hix init`, the generated `config.yaml` file contains formatting issues that make it invalid YAML.

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

## Environment
- Hix Version: 0.3.0.0
- Operating System: Windows 10
- Additional Context: Using default configuration

## Suggested Fix
1. Add YAML validation tests
2. Fix the YAML generation code
3. Add proper escaping for special characters
4. Implement consistent indentation rules

## Priority
High - This affects the initial setup experience and prevents users from using the tool until manually fixed.

## Additional Context
The issue appears to be in the YAML generation code in `src/Wizard.hs`, specifically in the `formatLayer` and `formatTemplate` functions.
``` 