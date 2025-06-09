# Hix 🧩 — Flexible Code Generator

 **Stop rewriting boilerplate. Start generating structure.**

 **Hix** is a template-driven code generator that turns simple JSON models into full-featured source code — from backend models and database schema to HTML forms and API handlers.

 Designed for developers who value consistency, automation, and clean architecture, Hix helps you build smarter, faster, and with fewer bugs.

 - 🔁 One model → many files
 - 🧩 Template everything: Python, TypeScript, HTML, SQL, C#, you name it
 - ⚙️ Drop-in CLI with Windows installer and editor integration
 - 🧠 Built with Haskell for safe, predictable rendering

 _"Build once. Template forever. Let Hix do the repetition."_

# Hix 🧩 – Flexible Code Generator

**Hix** is a flexible, template-driven code generator for developers who love clean, structured automation.**

Whether you're generating classes, config files, or scaffolding code, Hix combines the power of JSON models and expressive templates to deliver clean, readable output.

## Documentation

Full documentation is available at:

📄 [https://joelbugarini.github.io/hix/](https://joelbugarini.github.io/hix/)

---

## ✨ Features

- 🔁 `[[prop]]` loops over model properties
- ❓ `[[if ...]]`, `[[else]]` conditionals
- 🔤 Text transformation functions: `[[upper ...]]`, `[[lower ...]]`, `[[snake_case ...]]`, `[[kebab_case ...]]`, `[[lowerFirst ...]]`
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

