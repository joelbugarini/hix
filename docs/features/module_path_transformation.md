# Module Path Transformation

## Overview
This feature ensures that when transforming module paths (e.g., `Test.Module.Name`), the dots are preserved. This is crucial for maintaining the correct file structure, especially in scenarios like `CartModule.component.ts` where the output should be `cart-module.component.ts` (not `cart-module-component-ts`).

## Decision
- **Preserve Dots:** The transformation function (`transformModulePath`) splits the path on dots, applies the transformation to each segment, and then rejoins using dots. This ensures that the file extension and other dot-separated parts remain intact.
- **Consistency:** This approach maintains consistency across the codebase and aligns with common file naming conventions.

## Example
- Input: `Test.Module.Name` with kebab_case transformation
- Output: `test.module.name`

## Impact
- Ensures that file extensions and other dot-separated parts are not altered during transformation.
- Supports a wide range of file naming conventions, including those with multiple segments. 