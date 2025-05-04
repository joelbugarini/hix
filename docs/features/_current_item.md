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


Recommended Implementation Plan
- [x] Start Simple: Parse Literal Text and Basic Tags
  - [x] Implement parsing for plain text (anything not inside [[...]]).
  - [x] Implement parsing for simple variable tags like [[model.name]] and [[prop.name]].
  - [x] Add tests for these cases.
- [x] Handle Block Constructs
  - [x] Implement parsing for blocks: [[prop]]...[[/prop]], [[if ...]]...[[/if]], and [[if ...]]...[[else]]...[[/if]].
  - [x] Support nesting (e.g., [[if ...]] inside [[prop]]).
  - [x] Add tests for block parsing and nesting.
- [x] Support Function Calls
  - [x] Parse constructs like [[upper model.name]], [[lower prop.name]], etc.
  - [x] Add tests for function call parsing.
- [x] Error Handling
  - [x] Ensure the parser produces clear, actionable error messages (line/column info) for malformed templates.
  - [x] Add tests for error cases (e.g., unclosed tags, unknown constructs).
- [x] Integrate and Migrate
  - [x] Gradually replace the old parser in your codebase with the new one.
  - [x] Run all existing tests to ensure no regressions.
  - [x] Update documentation to reflect the new parser.
- [x] Refactor and Remove Old Code
  - [x] Once the new parser is fully integrated and tested, remove the old custom lexer and parser.