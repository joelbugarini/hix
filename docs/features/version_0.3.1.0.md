# Version 0.3.1.0 Release Notes

## Highlights
- Migrated template parser to Megaparsec for improved robustness, error handling, and extensibility
- Full support for all template constructs, including blocks, function calls, and nesting
- User-friendly error messages for malformed templates
- CLI and code generation now use the new parser
- All tests migrated and passing
- Old lexer/parser code removed

## Changelog
- [Feature] Megaparsec-based template parser (`Template.Parser`)
- [Removed] Deprecated `Template.Lexer` and `parseTokens` logic
- [Improved] Error messages for malformed templates
- [Test] All tests updated to use the new parser 