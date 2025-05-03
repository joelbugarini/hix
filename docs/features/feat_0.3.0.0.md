# Add CLI command to generate files from templates with layer filtering

## Description
Add functionality to generate files from templates based on a model, with the ability to filter by layer or specific template.

## Expected Behavior
- Command: `hix generate --model <model_path> [--layer <layer_name>] [--template <template_path>]`
- When no layer or template specified: Generate files for all layers using their respective templates
- When layer specified: Generate files only for the specified layer
- When template specified: Generate file only for the specified template
- Support template variables (e.g., [[model.name]])

## Usage Examples
```bash
# Generate all files for a model
hix generate --model ./models/user.json

# Generate files only for Domain layer
hix generate --model ./models/user.json --layer Domain

# Generate file for specific template
hix generate --model ./models/user.json --template ./templates/domain/Archive.hix
```

## Acceptance Criteria
- [x] CLI accepts model path and optional layer/template parameters
- [x] Generates files for all layers when no layer/template specified
- [x] Generates files only for specified layer when layer name provided
- [x] Generates file only for specified template when template path provided
- [x] Handles template variables correctly
- [x] Provides clear error messages for invalid inputs
- [x] Includes tests for all functionality
- [x] Documentation updated with new commands

## Technical Considerations
- [x] Template variable substitution
- [x] Layer name validation
- [x] Template path validation
- [x] File path resolution
- [x] Error handling
- [x] Support for relative and absolute paths
- [x] Template file existence validation