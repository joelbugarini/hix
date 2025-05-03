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

# Bug Report: Template Substitution Loop Leaves Unreplaced Elements in Generated Code

## Description
When generating code from templates, the template engine fails to properly clean up after the property substitution loop, leaving unreplaced template elements in the final output. This results in invalid code being generated.

## Impact
- Generated code contains invalid syntax with unreplaced template tags
- Users need to manually clean up the generated files
- This affects the reliability of the code generation process
- May cause compilation errors in the target language

## Steps to Reproduce
1. Create a model file (e.g., `Person.json`) with properties:
```json
{
  "className": "Person",
  "properties": [
    { "name": "Id", "type": "int" },
    { "name": "TitleName", "type": "datetime" },
    { "name": "IsActive", "type": "bool" }
  ]
}
```

2. Run the code generation command:
```bash
hix generate --model Person.json
```

3. Check the generated file (e.g., `PersonModel.cs`)

## Expected Behavior
The generated code should be clean and valid, with all template elements properly replaced:
```csharp
public class Person {
    public int Id { get; set; }
    public datetime TitleName { get; set; }
    public bool IsActive { get; set; }
}
```

## Current Behavior
The generated code contains unreplaced template elements:
```csharp
public class Person {
      public int Id { get; set; }
  public datetime TitleName { get; set; }
  public bool IsActive { get; set; }

    public [[prop.type]] [[prop.name]] { get; set; }
    
}
```

## Environment
- Hix Version: 0.3.0.0
- Operating System: Windows 10
- Additional Context: Using default templates and configuration

## Suggested Fix
1. Review the template engine's property loop handling
2. Add cleanup step after property substitution
3. Implement validation to ensure no template tags remain in output
4. Add tests to verify clean template substitution

## Priority
Medium - While it doesn't break core functionality, it requires manual intervention to fix generated code.

## Additional Context
The issue appears to be in the template engine's handling of the `[[prop]]` loop. After processing all properties, it's not properly cleaning up the template structure, leaving the loop template elements in the output. This affects the generated code's validity and requires manual cleanup.
