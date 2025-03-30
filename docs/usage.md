# Usage

## Running Hix

Before running the command, make sure you have created two files:

- `template.hix`: your Hix template
- `model.json`: your input data model


Once installed, you can use Hix via the command line to render templates with your model data:

```bash
hix template.hix model.json
```

This will parse `template.hix`, apply values from `model.json`, and output the result to your terminal.

---

## Example Input Files

### model.json
```json
{
  "className": "User",
  "properties": [
    { "name": "Name", "type": "string" },
    { "name": "IsAdmin", "type": "bool" }
  ]
}
```

### template.hix
```hix
public class [[model.className]] {
[[prop]]
  [[if prop.type=bool]]
    public bool [[prop.name]];
  [[else]]
    public [[prop.type]] [[snake_case prop.name]];
  [[/if]]
[[/prop]]
}
```

### Output
```csharp
public class User {
  public string name;
  public bool IsAdmin;
}
```

---

## Command-Line Arguments

When you run `hix template.hix model.json`, the tool uses:

- The **template file's extension** (e.g., `.hix`, `.cs.hix`, `.html.hix`) to determine the **output file extension**.
- The **model's `className`** as the **base name** for the output file.

For example:
```bash
hix template.cs.hix Person.json
```
Will produce:
```
Person.cs
```

> The CLI accepts two positional arguments:

> The CLI accepts two positional arguments:

1. **template file** (e.g. `template.hix`)
2. **model file** (e.g. `model.json`)

---

## Windows Installer Support

If you installed Hix using the Windows setup wizard, the `hix` command will be available in any terminal.

You may need to restart your terminal session after installation to ensure the `PATH` update is active.

