# Syntax Highlighting Grammar

Hix provides a grammar generator for editors like VS Code or Sublime Text using the TextMate format.

---

## Generating the Grammar

Run the following command:

```bash
hix --gen-grammar hix.tmLanguage.json
```

This generates a `hix.tmLanguage.json` file that can be used in editor extensions or custom themes.

---

## Supported Tokens

Hix grammar supports the following highlights:

### Block Keywords
- `[[prop]]`, `[[/prop]]`: property iteration blocks
- `[[if ...]]`, `[[else]]`, `[[/if]]`: conditional blocks

### Variables
- `[[prop.name]]`: property name
- `[[prop.type]]`: property type
- `[[model.className]]`: model class name
- `[[model.name]]`: model name

### Functions
#### Case Transformations
- `[[upper ...]]`: Convert to uppercase
- `[[lower ...]]`: Convert to lowercase
- `[[snake_case ...]]`: Convert to snake_case
- `[[kebab_case ...]]`: Convert to kebab-case
- `[[lowerFirst ...]]`: Convert first letter to lowercase

#### Module Transformations
- `[[module_transform ...]]`: Transform module names in paths
  - Supports all case transformation functions
  - Works with nested module paths
  - Example: `[[module_transform snake_case]]`

---

## Using in VS Code

1. Copy `hix.tmLanguage.json` into your VS Code extension or custom grammar folder
2. Add it to the `contributes.grammars` section of your `package.json`
3. Reload your editor

---

## Future Plans

- Language Server Protocol (LSP) support
- Live preview pane for rendered templates
- Validation & auto-completion
- Enhanced module path transformations
- Custom function definitions