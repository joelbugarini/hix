# Hix 🧩 — Flexible Code Generator

**Hix** is a flexible, template-driven code generator written in Haskell.  
It uses model definitions (in JSON) and dynamic templates to generate custom code with loops, conditionals, and formatting functions.

> ✅ Version: `v0.1`  
> 📦 Includes: CLI tool, Windows installer, syntax highlighter

---

## ✨ Features

- 🔁 `[[prop]]` loops over model properties
- ❓ `[[if ...]]`, `[[else]]` conditionals
- 🔤 Text transformation functions: `[[upper ...]]`, `[[lower ...]]`, `[[snake_case ...]]`
- 🧠 Full AST parser and golden test suite
- 🎨 VS Code / TextMate grammar for syntax highlighting
- 🪟 Windows installer with PATH integration

---

## 🚀 Getting Started

### 1. Using the CLI (Windows)

Download and run the latest installer:

📦 [Download hix-setup.exe](https://github.com/yourusername/hix/releases)

Then open a terminal and run:

```bash
hix template.hix model.json
```

### 2. Using the CLI (Dev / Haskell)

```bash
stack install --local-bin-path=dist
dist/hix template.hix model.json
```

---

## 📂 Template Syntax

```hix
[[prop]]
[[if prop.type=bool]]
  <input type="checkbox" name="[[prop.name]]">
[[else]]
  <input type="text" name="[[snake_case prop.name]]">
[[/if]]
[[/prop]]
```

---

## 🧪 Tests

```bash
stack test
```

Includes golden tests for templates and rendering.

---

## 🎨 Syntax Highlighting

You can generate a `.tmLanguage.json` file for use in VS Code or any TextMate-compatible editor:

```bash
hix --gen-grammar hix.tmLanguage.json
```

---

## 📦 Packaging (Windows)

Build and package using Inno Setup:

```bash
stack install --local-bin-path=dist
cd utils/installer
Open hix_installer.iss in Inno Setup and click Compile
```

---

## 📄 License

Licensed under [GNU GPL v2](LICENSE), like Git.

---

## 🤝 Contributing

Pull requests are welcome! Open an issue to discuss new features, syntax extensions, or improvements.

---

## 🧠 Roadmap

- [ ] `[[include file]]` support for template partials
- [ ] Custom filters and functions
- [ ] Model validation
- [ ] LSP integration / live preview

---

Made with ❤️ by Joel Bugarini

