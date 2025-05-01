# Usage Guide

This guide explains how to use Hix for code generation in your projects.

## Installation

Hix can be installed through various package managers:

### Using Homebrew (macOS/Linux)
```bash
brew install hix
```

### Using Chocolatey (Windows)
```bash
choco install hix
```

### Using Nix
```bash
nix-env -i hix
```

### Using Docker
```bash
docker pull hix/hix
```

### Manual Installation
Download the latest release from the [releases page](https://github.com/your-org/hix/releases) and add it to your PATH.

## Project Setup

### Initialize a New Project
```bash
mkdir my-project
cd my-project
hix init
```

This creates the following structure:
```
.hix/
├── config.yaml           # Configuration file
├── .gitignore           # Git ignore rules for generated files
├── models/              # Directory for model files
├── output/              # Directory for generated code
│   ├── Domain/         # Domain layer output
│   ├── Application/    # Application layer output
│   ├── Infrastructure/ # Infrastructure layer output
│   └── Presentation/   # Presentation layer output
└── templates/          # Directory for template files
```

## Basic Usage

### 1. Create a Model
Create a JSON file describing your model:

```json
{
  "className": "User",
  "properties": [
    { "name": "Id", "type": "int" },
    { "name": "Name", "type": "string" },
    { "name": "Email", "type": "string" }
  ]
}
```

### 2. Generate Code
Run Hix with your model file:

```bash
hix User.json
```

This will:
1. Load the model from `User.json`
2. Read the configuration from `.hix/config.yaml`
3. Generate code for each layer according to the templates
4. Output the generated files in their respective layer directories

## Advanced Usage

### Interactive Mode
If you run Hix without arguments, it will prompt you for the model name:

```bash
hix
# Then enter the model name when prompted
```

### Help and Documentation
- Show basic help: `hix help` or `hix --help`
- Show detailed manual: `hix man`
- Show version: `hix version` or `hix --version`

### Customizing Configuration
Edit `.hix/config.yaml` to customize:
- Architecture type (clean, onion, hexagonal)
- Output paths
- Layer configurations
- Template mappings

## Examples

### Generate a Domain Entity
1. Create a model file `User.json`:
```json
{
  "className": "User",
  "properties": [
    { "name": "Id", "type": "int" },
    { "name": "Name", "type": "string" }
  ]
}
```

2. Run Hix:
```bash
hix User.json
```

3. Check the generated files in `.hix/output/Domain/`

### Using Templates
1. Create a template in `.hix/templates/domain/Entity.hix`:
```hix
public class [[model.className]] {
[[prop]]
  private [[prop.type]] [[prop.name]];
[[/prop]]
}
```

2. Add the template to your config:
```yaml
layers:
  - name: Domain
    path: ./src/Domain
    templates:
      - name: Entity
        filename: [[model.name]].java
        template: templates/domain/Entity.hix
```

3. Generate code:
```bash
hix User.json
```

## Troubleshooting

### Common Issues

1. **Model Not Found**
   - Ensure the model file exists and is in the correct location
   - Check file permissions

2. **Configuration Errors**
   - Verify `config.yaml` syntax
   - Check template paths are correct

3. **Template Errors**
   - Check template syntax
   - Verify all required model fields are present

### Getting Help

- Use `hix help` for basic command information
- Use `hix man` for detailed documentation
- Check the [documentation](docs/) for more information
- Report issues on our [GitHub repository](https://github.com/yourusername/hix)

---

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

