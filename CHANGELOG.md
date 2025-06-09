# Changelog for `hix`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.3.5.1 - 2024-03-19

### Fixed
- Fixed module name transformation in template filenames
  - Added proper handling of module_transform function in template parser
  - Fixed type mismatch in function call handling
  - Improved error messages for invalid module transform arguments

## 0.3.5.2 - 2024-03-19

### Fixed
- Bugfix: `module_transform` now works correctly in template filenames (e.g., `[[module_transform kebab_case model.className]]`).
- Fixed Text/String mismatch in renderer.

## 0.3.5.0 - 2024-03-19

### Added
- Module name case transformation in FileSystem configuration
  - Support for snake_case, kebab_case, lower, upper, and lowerFirst transformations
  - Added module_transform field to template configuration
  - Added validation for transformed module names
  - Added comprehensive test suite for module transformations

## 0.1.0.0 - YYYY-MM-DD
