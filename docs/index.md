# Welcome to Hix

**Hix** is a flexible, template-driven code generator for developers who love clean, structured automation.**

Whether you're generating classes, config files, or scaffolding code, Hix combines the power of JSON models and expressive templates to deliver clean, readable output.

---

## What Can Hix Do?

- 🧠 Generate code from structured models
- 🔁 Loop over model properties with `[[prop]]`
- ❓ Use `[[if]]` and `[[else]]` for conditional rendering
- 🔤 Transform values using `[[upper]]`, `[[lower]]`, and `[[snake_case]]`
- 🧪 Validate behavior using golden tests
- 🎨 Highlight templates with editor grammar
- 🪟 Install cleanly on Windows with a wizard installer

---

## Download

Ready to use Hix on Windows?

📦 [Download Hix v0.1 for Windows](https://github.com/joelbugarini/hix/releases/download/v0.1/hix-setup.exe)

🔗 [View on GitHub](https://github.com/joelbugarini/hix)


---

## Quick Example

Given this model:

```json
{
  "className": "User",
  "properties": [
    { "name": "Name", "type": "string" },
    { "name": "IsAdmin", "type": "bool" }
  ]
}
```

And this template:

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

Hix will generate:

```csharp
public class User {
  public string name;
  public bool IsAdmin;
}
```

---

## Try It

```bash
hix template.hix model.json
```

Explore more in:
- `usage.md` for CLI details
- `templates.md` for syntax reference
- `installer.md` if you're on Windows

Happy generating! 🚀

