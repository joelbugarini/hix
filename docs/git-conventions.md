# Git Conventions

This document outlines the git conventions and workflow for the hix project.

## Branch Naming Convention

All feature branches should follow this naming pattern:
```
feature/<version>-<feature-description>
```

Where:
- `<version>` is the version number the feature is targeting (e.g., 0.3.0.0)
- `<feature-description>` is a hyphenated description of the feature

### Examples
- `feature/0.3.0.0-template-generation-layer-filtering`
- `bugfix/0.3.0.1-yaml-configuration-fixes`

## Branch Protection Rules

1. **No Direct Pushes to main**
   - The `main` branch is protected
   - All changes must go through pull requests
   - Direct pushes to `main` are not allowed

2. **Pull Request Requirements**
   - All pull requests must be reviewed and approved
   - All tests must pass before merging
   - Pull request descriptions should follow the template provided in the repository

## Commit Message Guidelines

1. **Format**
   ```
   <type>(<scope>): <description>

   [optional body]

   [optional footer]
   ```

2. **Types**
   - `feat`: New feature
   - `fix`: Bug fix
   - `docs`: Documentation changes
   - `style`: Code style changes
   - `refactor`: Code refactoring
   - `test`: Test-related changes
   - `chore`: Maintenance tasks

3. **Examples**
   ```
   feat(template): add layer filtering support
   fix(config): resolve YAML parsing issues
   docs(git): add git conventions documentation
   ```

## Pull Request Title Guidelines

Pull request titles should follow this format:
```
<Type>/<Version>: <short description>
```
Where:
- `<Type>` is one of: Feature, Bugfix, Refactor, Docs, Chore, etc.
- `<Version>` is the version number the PR targets (e.g., 0.3.0.1)
- `<short description>` is a concise summary of the change

### Example
```
Bugfix/0.3.0.1: resolve YAML parsing issues in configuration generation
```

## Workflow

1. Create a new branch following the naming convention
2. Make your changes
3. Write clear commit messages
4. Push your branch
5. Create a pull request
6. Address any review comments
7. Wait for approval and merge

## Version Tags

- Version tags should follow semantic versioning (e.g., v0.3.0.1)
- Tags should be created for each release
- Tag messages should reference the version increment documentation 