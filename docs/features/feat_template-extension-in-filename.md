# Template File Extension in Filename

## Description
Template files must now include the target file extension in their filename, such as `Entity.cs.hix` instead of just `Entity.hix`. This makes it clear to users what type of file will be generated from each template.

- Templates should be named with both the intended output extension and the `.hix` suffix (e.g., `Repository.cs.hix`, `ViewModel.kt.hix`).
- This convention applies to all new and existing templates.

## Motivation
- Improve clarity for users about the output file type
- Reduce confusion when working with multiple languages or file types
- Make template management and discovery easier

## Key Changes
- Template lookup and generation logic will expect the file extension to be included in the template filename
- Documentation and examples will be updated to reflect this convention

## Acceptance Criteria
- [ ] All templates use the new naming convention (e.g., `Entity.cs.hix`)
- [ ] Template lookup and generation support the new convention
- [ ] Documentation and examples are updated 