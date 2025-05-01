# Hix Roadmap

This roadmap outlines the planned features and improvements for Hix, organized by priority and implementation status.

## Current Version (0.2.0)

### Core Features
- ✅ Basic template parsing
- ✅ JSON model processing
- ✅ Simple code generation
- ✅ Command-line interface

### High Priority Features
- 🔄 Template inheritance
- 🔄 Custom functions in templates
- 🔄 Model validation
- 🔄 Error handling improvements

### Medium Priority Features
- ⏳ Multiple output formats
- ⏳ Template caching
- ⏳ Performance optimizations
- ⏳ Enhanced documentation

### Low Priority Features
- 📋 IDE integration
- 📋 Web interface
- 📋 Template marketplace
- 📋 Advanced model transformations

## Future Versions

### Version 0.3.0 (Next Release)
- Template inheritance
- Custom functions
- Improved error messages
- Performance benchmarks

### Version 0.4.0
- Multiple output formats
- Template caching
- Enhanced documentation
- Community templates

### Version 1.0.0
- Major architecture improvements
- Plugin system
- Advanced model processing
- Full IDE integration

## Feature Status Legend
- ✅ Implemented
- 🔄 In Progress
- ⏳ Planned
- 📋 Under Consideration

## Contributing

New features are selected based on the [Specification](specification.md) document. If you'd like to propose a new feature:

1. Check if it aligns with Hix's goals
2. Create a GitHub Issue with detailed proposal
3. Follow the feature implementation process
4. Submit a pull request when ready

## Community Feedback

We welcome community input on the roadmap. Please:
- Comment on existing issues
- Create new feature requests
- Participate in discussions
- Share your use cases

---

## ✅ Completed in v0.1

- `[[prop]]` and filters
- `[[if]]` / `[[else]]` conditionals
- `[[upper]]`, `[[lower]]`, `[[snake_case]]` functions
- AST-based rendering
- Golden tests
- Grammar generation for syntax highlighting
- Windows installer with wizard UI and PATH integration

---

## 🧩 Planned for v0.2

- `[[include filename]]` for reusable template parts
- Configurable output file paths via CLI
- Command-line options: `--out`, `--format`, `--verbose`
- More filters (e.g., `prop.required=true`)
- Real comment support in templates

---

## 🧪 Testing & CI

- GitHub Actions for auto-testing PRs
- Validate golden outputs
- Run on Windows/Linux builds

---

## 🧠 Stretch Goals

- Language Server Protocol (LSP) integration
- VS Code extension
- Live preview while editing templates
- Web-based playground UI

---

Have an idea? [Open an issue](https://github.com/yourusername/hix/issues) or submit a PR!

