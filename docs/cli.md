# CLI Commands

Hix provides a command-line interface with several commands to help you manage your code generation workflow.

## Basic Commands

### `hix init`
Initializes a new hix project by creating the necessary directory structure and configuration files.

```bash
hix init
```

This command:
- Creates a `.hix` directory in your current working directory
- Sets up the default directory structure:
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
- Creates a default `config.yaml` with clean architecture layers
- Prompts you to configure templates for each layer

### `hix help`
Shows the basic help message with available commands and options.

```bash
hix help
# or
hix --help
```

### `hix man`
Shows the detailed manual with comprehensive documentation.

```bash
hix man
```

### `hix version`
Displays the current version of hix.

```bash
hix version
# or
hix --version
```

## Code Generation

### Basic Usage
To generate code from a model file, use the `generate` command:

```bash
hix generate --model model.json
```

This will:
1. Load the model from `model.json`
2. Read the configuration from `.hix/config.yaml`
3. Generate code for each layer according to the templates
4. Output the generated files in their respective layer directories

### Generate Command Options

The `generate` command supports several options:

#### Required Parameters
- `--model <path>`: Specifies the path to the model file (required)

#### Optional Parameters
- `--layer <name>`: Generate code only for the specified layer
- `--template <path>`: Generate code only for the specified template

Examples:
```bash
# Generate code for all layers
hix generate --model model.json

# Generate code only for the Entities layer
hix generate --model model.json --layer Entities

# Generate code only for a specific template
hix generate --model model.json --template .hix/templates/template.cs.hix
```

Note: You cannot specify both `--layer` and `--template` options at the same time.

### Interactive Mode
If you run hix without arguments, it will prompt you for the model name:

```bash
hix
# Then enter the model name when prompted
```

## Configuration

The `config.yaml` file created by `hix init` contains the following structure:

```yaml
architecture: clean
output_root: ./src
layers:
  - name: Domain
    path: ./src/Domain
    description: Core business logic and entities
    templates: []
  - name: Application
    path: ./src/Application
    description: Application services and use cases
    templates: []
  - name: Infrastructure
    path: ./src/Infrastructure
    description: External interfaces and implementations
    templates: []
  - name: Presentation
    path: ./src/Presentation
    description: User interface and API endpoints
    templates: []
```

You can customize this configuration by:
1. Editing the `config.yaml` file directly
2. Adding templates to each layer
3. Changing the output paths
4. Modifying the architecture type

## Examples

### Initialize a New Project
```bash
mkdir my-project
cd my-project
hix init
```

### Generate Code from a Model
```bash
# Create a model file
echo '{
  "className": "User",
  "properties": [
    { "name": "Id", "type": "int" },
    { "name": "Name", "type": "string" }
  ]
}' > User.json

# Generate code
hix User.json
```

### View Help
```bash
hix help
```

### View Manual
```bash
hix man
``` 