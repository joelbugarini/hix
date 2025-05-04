## Description
When generating code from templates, the template engine incorrectly handles property substitution, resulting in unreplaced template tokens appearing in the final output. This indicates a fundamental issue in the transformation logic where template tokens are not being properly processed during the property loop iteration.

## Impact
- Generated code contains invalid syntax with unreplaced template tokens
- Users need to manually fix the generated files
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
The generated code should be clean and valid, with all template tokens properly transformed:
```csharp
public class Person {
    public int Id { get; set; }
    public datetime TitleName { get; set; }
    public bool IsActive { get; set; }
}
```

## Current Behavior
The generated code contains unreplaced template tokens, indicating the transformation logic is not working correctly:
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
1. Review the template engine's property loop transformation logic
2. Ensure template tokens are properly processed during property iteration
3. Add validation to verify all template tokens are transformed
4. Add tests to verify correct template token transformation

## Priority
Medium - While it doesn't break core functionality, it requires manual intervention to fix generated code.

## Additional Context
The issue appears to be in the template engine's handling of the `[[prop]]` loop. The transformation logic is not correctly processing all template tokens during property iteration, resulting in unreplaced tokens appearing in the output. This indicates a fundamental issue in how the template engine handles property substitution rather than a cleanup problem.