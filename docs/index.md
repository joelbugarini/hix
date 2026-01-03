# Hix - A Code Generation Tool for Clean Architecture

Hix is a powerful code generation tool designed to help you implement clean architecture patterns in your projects. It uses templates and models to generate consistent, well-structured code across different layers of your application.

## Features

- **Template-based Code Generation**: Write templates once, generate code for multiple models
- **Clean Architecture Support**: Built-in support for Domain, Application, Infrastructure, and Presentation layers
- **Flexible Template Language**: Simple yet powerful template syntax with conditionals and loops
- **Model-driven Development**: Define your models in JSON and generate code automatically
- **CLI Interface**: Easy-to-use command-line interface with initialization and help features
- **Customizable Architecture**: Support for different architecture patterns (Clean, Onion, Hexagonal)
- **Extensible**: Add your own templates and customize the generation process

## Quick Start

1. **Install Hix**:
   ```bash
   stack install hix
   ```

2. **Initialize a Project**:
   ```bash
   mkdir my-project
   cd my-project
   hix init
   ```

3. **Create a Model**:
   ```json
   {
     "className": "User",
     "properties": [
       { "name": "Id", "type": "int" },
       { "name": "Name", "type": "string" }
     ]
   }
   ```

4. **Generate Code**:
   ```bash
   hix User.json
   ```

## Documentation

### Hix (Code Generation)

- [CLI Commands](cli.md) - Learn about available commands and options
- [Usage Guide](usage.md) - Get started with basic usage
- [Template Syntax](grammar.md) - Understand the template language
- [Advanced Features](advanced.md) - Explore advanced capabilities
- [Templates](templates.md) - Learn about creating and using templates
- [Real-world Use Cases](real-world-usecase.md) - See how Hix is used in practice
- [Roadmap](roadmap.md) - Future plans and upcoming features

### hix-drill (Code Analysis)

hix-drill is a Rust-based CLI tool that analyzes codebases and generates Hix project configurations.

- **[hix-drill Documentation](drill-index.md)** - Complete hix-drill documentation index
- [Overview](drill-overview.md) - What is hix-drill and how it works
- [Installation](drill-installation.md) - Install and build hix-drill
- [Quickstart](drill-quickstart.md) - Analyze your first repository
- [Commands](drill-commands.md) - Command reference
- [Pattern Packs](drill-pattern-packs.md) - Understanding and creating patterns
- [Output Format](drill-output-format.md) - Output file format reference
- [Examples](drill-examples.md) - Practical usage examples

## Getting Help

- Use `hix help` for basic command information
- Use `hix man` for detailed documentation
- Check the [documentation](docs/) for more information
- Report issues on our [GitHub repository](https://github.com/yourusername/hix)

## License

Hix is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

