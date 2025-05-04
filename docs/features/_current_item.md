# Migrate Template Parser to Megaparsec for Robustness and Extensibility

## Description
Refactor the template parsing logic to use the Megaparsec library instead of the current custom parser. This will provide better error messages, easier grammar extension, and more maintainable code.

## Expected Behavior
- The template engine uses Megaparsec for parsing templates.
- All existing template features (e.g., `[[model.name]]`, `[[prop]]`, `[[if ...]]`, function calls) are supported.
- Parsing errors produce clear, actionable messages with line/column info.
- The parser is easier to extend for new template features.
- The lexer and parser are unified into a single parsing step.

## Usage Examples
```bash
# Generate files as before, with improved error reporting on malformed templates
hix generate --model ./models/user.json
```
- Malformed template example:
  ```
  public class [[model.className] {
      [[prop]]
      public [[prop.type]] [[prop.name]] { get; set; }
      [[/prop]]
  }
  ```
  Output:  
  ```
  [hix error] Parse error at line 1, column 20: expected closing ']]'
  ```

## Acceptance Criteria
- [x] All template constructs are parsed using Megaparsec.
- [x] Existing tests for template parsing and rendering pass.
- [x] New tests added for error cases and edge cases.
- [x] Error messages include line and column information.
- [x] Documentation updated to reflect the new parser.
- [x] No regressions in CLI or template generation features.

## Technical Considerations
- [x] Remove or refactor the existing `Template.Lexer` and `Template.AST.parseTokens`.
- [x] Implement a Megaparsec parser for all template constructs.
- [x] Ensure recursive/nested constructs are handled correctly.
- [x] Integrate the new parser into the rendering pipeline.
- [x] Update or add property-based tests for parser correctness.
- [x] Ensure backward compatibility with existing templates.
