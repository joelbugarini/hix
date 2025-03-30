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

- `[[prop]]`, `[[/prop]]`: property block keywords
- `[[if ...]]`, `[[else]]`, `[[/if]]`: conditional blocks
- `[[prop.name]]`, `[[prop.type]]`, `[[model.className]]`: variables
- `[[upper ...]]`, `[[lower ...]]`, `[[snake_case ...]]`: functions

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