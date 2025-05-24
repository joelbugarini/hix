Add automatic empty template file generation

## Description
Add functionality to automatically generate empty files for defined templates after config file creation, making it easier for users to start working with their templates.

## Expected Behavior
Command: hix init --config <config_path> [--output <output_dir>]
When no output directory specified: Generates files in default template directory (.hix folder)
When output directory specified: Generates files in the specified directory
Creates empty files for all defined templates
Preserves template directory structure
Usage Examples
# Generate empty files in default template directory
hix init --config ./config.json

# Generate empty files in custom directory
hix init --config ./config.json --output ./my-templates

# Example of generated structure
```shell
./templates/
  ├── domain/
  │   └── User.hix
  ├── infrastructure/
  │   └── UserRepository.hix
  └── application/
      └── UserService.hix
```

## Acceptance Criteria
- [ ] CLI accepts config path and optional output directory parameter
- [ ] Generates empty files for all defined templates
- [ ] Creates appropriate directory structure
- [ ] Handles existing files gracefully
- [ ] Provides clear feedback on created files
- [ ] Works across different operating systems
- [ ] No regression in existing functionality
- [ ] Includes tests for all functionality
- [ ] Documentation updated with new commands

Technical Considerations
- File system operations safety
- Directory structure preservation
- File path resolution
- Error handling
- Support for relative and absolute paths
- Cross-platform compatibility
- File permissions management
- Template validation
- Config file parsing