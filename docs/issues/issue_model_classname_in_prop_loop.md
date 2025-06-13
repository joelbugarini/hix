# Issue: Model ClassName Transformations Not Working Inside Prop Loops

## Description
The template engine currently has a bug where transformations (like `lowerFirst`, `upper`, `lower`, etc.) applied to `model.className` do not work when used inside a `[[prop]]` loop block. While these transformations work correctly outside of prop loops, they fail to transform the model class name when used within them.

## Current Behavior
```hix
// This works correctly outside prop loop
[[lowerFirst model.className]]  // Outputs: testModel

// This fails inside prop loop
[[prop]]
  [[lowerFirst model.className]]  // Outputs: TestModel (no transformation)
[[/prop]]
```

## Expected Behavior
The transformations should work consistently both inside and outside of prop loops. For example:
```hix
[[prop]]
  [[lowerFirst model.className]]  // Should output: testModel
  [[upper model.className]]       // Should output: TESTMODEL
  [[snake_case model.className]]  // Should output: test_model
[[/prop]]
```

## Root Cause
The issue appears to be in the `renderPropNode` function in `src/Template/RenderProp.hs`. When handling `FuncCall` nodes inside prop loops, it only processes transformations for `prop.name` and `prop.type`, but not for `model.className`. The function needs to be updated to handle model class name transformations within prop loops.

## Impact
This bug affects template authors who need to transform model class names within property loops, which is a common use case when generating code that needs consistent naming conventions throughout the generated output.

## Proposed Fix
1. Update the `renderPropNode` function in `src/Template/RenderProp.hs` to handle model class name transformations
2. Add test cases to verify the fix works for all transformation functions
3. Update documentation to clarify that model class name transformations work in all contexts

## Test Cases Needed
1. Test `lowerFirst` transformation inside prop loop
2. Test `upper` transformation inside prop loop
3. Test `lower` transformation inside prop loop
4. Test `snake_case` transformation inside prop loop
5. Test `kebab_case` transformation inside prop loop
6. Test multiple transformations in the same prop loop
7. Test transformations with nested prop loops

## Priority
Medium - This is a functionality bug that affects template authors but doesn't break existing templates that don't use this feature.

## Related Files
- `src/Template/RenderProp.hs`
- `src/Template/RenderNode.hs`
- `test/Template/RenderNodeTest.hs`
- `test/data/functions/template.hix` 