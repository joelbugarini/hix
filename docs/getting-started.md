# Getting Started with Hix

This guide will help you get started with Hix in just a few minutes. We'll create a simple example that generates a C# class from a JSON model.

## 1. Install Hix

Choose your preferred installation method:

```bash
# Using Homebrew (macOS/Linux)
brew install hix

# Using Chocolatey (Windows)
choco install hix

# Using Nix
nix-env -i hix
```

## 2. Create a Simple Model

Create a file named `Person.json` with this content:

```json
{
  "className": "Person",
  "properties": [
    { "name": "Name", "type": "string" },
    { "name": "Age", "type": "int" }
  ]
}
```

## 3. Create a Template

Create a file named `Person.cs.hix` with this content:

```hix
public class [[model.className]] {
[[prop]]
    public [[prop.type]] [[prop.name]] { get; set; }
[[/prop]]
}
```

## 4. Generate the Code

Run Hix with your template and model:

```bash
hix Person.cs.hix Person.json
```

This will generate a `Person.cs` file with this content:

```csharp
public class Person {
    public string Name { get; set; }
    public int Age { get; set; }
}
```

## Next Steps

- Learn more about [template syntax](templates.md)
- See [real-world examples](real-world-usecase.md)
- Explore [advanced features](advanced.md) 