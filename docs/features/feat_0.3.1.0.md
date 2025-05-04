# Migrate Template Parser to Megaparsec for Robustness and Extensibility

## Description
This feature migrates the Hix template parser from a custom hand-rolled lexer/parser to a robust, user-friendly Megaparsec-based parser. The new parser provides better error messages, supports all template constructs (blocks, function calls, nesting), and is fully integrated into the CLI and code generation pipeline.

## Motivation
- Improve error handling and user feedback for malformed templates
- Make the parser easier to extend and maintain
- Remove legacy code and technical debt

## Key Changes
- Replaced custom lexer and parser with a Megaparsec-based parser (`Template.Parser`)
- Full support for:
  - Literal text and variable tags
  - Block constructs: `[[prop]]...[[/prop]]`, `[[if ...]]...[[/if]]`, `[[if ...]]...[[else]]...[[/if]]`
  - Function calls: `[[upper model.name]]`, `[[lower prop.name]]`, etc.
  - Nesting of blocks and conditionals
- User-friendly error messages for malformed templates (e.g., unclosed tags)
- CLI and code generation now use the new parser
- All tests migrated and passing
- Old `Template.Lexer` and `parseTokens` logic removed

## Acceptance Criteria
- [x] All template constructs are parsed using Megaparsec
- [x] Existing and new tests pass
- [x] Error messages are clear and actionable
- [x] CLI and code generation use the new parser
- [x] Old lexer/parser code is removed
- [x] Documentation updated 