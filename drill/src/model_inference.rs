use crate::facts::Facts;
use crate::template_synthesis::{Placeholder, PlaceholderKind, SynthesisMetadata};
use crate::unknown_discovery::SymbolCluster;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// Model inference module
/// Infers model schemas from synthesized templates and placeholders

/// Property in the inferred model
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModelProperty {
    /// Property name
    pub name: String,
    /// Property type (inferred from examples or context)
    pub r#type: String,
}

/// Inferred model schema (compatible with Hix model format)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct InferredModel {
    /// Class/type name
    pub className: String,
    /// Namespace (optional, inferred from file path)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub namespace: Option<String>,
    /// Properties/members
    pub properties: Vec<ModelProperty>,
    /// Annotations (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub annotations: Option<Vec<ModelAnnotation>>,
    /// Imports (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub imports: Option<Vec<String>>,
}

/// Model annotation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModelAnnotation {
    /// Annotation key
    pub key: String,
    /// Annotation value
    pub value: String,
}

pub struct ModelInferrer;

impl ModelInferrer {
    pub fn new() -> Self {
        ModelInferrer
    }

    /// Infer a model from synthesis metadata and cluster
    pub fn infer_model(
        &self,
        synthesis: &SynthesisMetadata,
        cluster: &SymbolCluster,
        facts: &Facts,
    ) -> Result<InferredModel, String> {
        // Infer className from cluster samples or placeholders
        let className = self.infer_class_name(synthesis, cluster, facts)?;

        // Infer namespace from base sample file path
        let namespace = self.infer_namespace(&synthesis.base_sample_file);

        // Infer properties from placeholders
        let properties = self.infer_properties(synthesis, facts)?;

        // For v1, we'll skip annotations and imports (can be added later)
        let annotations = None;
        let imports = None;

        Ok(InferredModel {
            className,
            namespace,
            properties,
            annotations,
            imports,
        })
    }

    /// Infer class name from cluster or placeholders
    fn infer_class_name(
        &self,
        synthesis: &SynthesisMetadata,
        cluster: &SymbolCluster,
        facts: &Facts,
    ) -> Result<String, String> {
        // Try to get the symbol name from the first sample in the cluster
        if let Some(first_symbol_id) = cluster.symbol_ids.first() {
            if let Some(symbol) = facts.symbols.iter().find(|s| s.id == *first_symbol_id) {
                // If it's a type symbol, use its name
                if matches!(symbol.kind, crate::facts::SymbolKind::Type) {
                    return Ok(symbol.name.clone());
                }
                
                // If it's a variable/member, find the containing type
                if matches!(symbol.kind, crate::facts::SymbolKind::Variable) {
                    // Find the member that corresponds to this symbol
                    if let Some(member) = facts.members.iter().find(|m| {
                        // Check if member name matches symbol name and they're in the same file
                        m.name == symbol.name && m.symbol_id != symbol.id
                    }) {
                        // Find the type symbol that owns this member
                        if let Some(type_symbol) = facts.symbols.iter().find(|s| s.id == member.symbol_id) {
                            if matches!(type_symbol.kind, crate::facts::SymbolKind::Type) {
                                return Ok(type_symbol.name.clone());
                            }
                        }
                    }
                    
                    // Alternative: look for a type symbol in the same file
                    if let Some(type_symbol) = facts.symbols.iter().find(|s| {
                        s.file == symbol.file && matches!(s.kind, crate::facts::SymbolKind::Type)
                    }) {
                        return Ok(type_symbol.name.clone());
                    }
                }
            }
        }

        // Fallback: try to infer from placeholder examples
        // Look for a placeholder that might represent the class name
        for placeholder in &synthesis.placeholders {
            if placeholder.kind == PlaceholderKind::Identifier && !placeholder.examples.is_empty() {
                // Use the first example as a potential class name
                // Prefer examples that look like class names (PascalCase)
                for example in &placeholder.examples {
                    if self.looks_like_class_name(example) {
                        return Ok(example.clone());
                    }
                }
            }
        }

        // Last resort: use cluster_id or template_name
        Ok(format!("Inferred{}", cluster.cluster_id.replace("cluster_", "")))
    }

    /// Check if a string looks like a class name (PascalCase)
    fn looks_like_class_name(&self, name: &str) -> bool {
        if name.is_empty() {
            return false;
        }
        // Check if first character is uppercase
        name.chars().next().map_or(false, |c| c.is_uppercase())
    }

    /// Infer namespace from file path
    fn infer_namespace(&self, file_path: &str) -> Option<String> {
        let path = Path::new(file_path);
        
        // Try to extract namespace from directory structure
        // For example: "src/Models/Person.cs" -> "Models"
        if let Some(parent) = path.parent() {
            if let Some(parent_name) = parent.file_name() {
                let parent_str = parent_name.to_string_lossy();
                // Only use if it looks like a namespace (PascalCase or contains dots)
                if self.looks_like_class_name(&parent_str) || parent_str.contains('.') {
                    return Some(parent_str.to_string());
                }
            }
        }
        
        None
    }

    /// Infer properties from placeholders
    fn infer_properties(
        &self,
        synthesis: &SynthesisMetadata,
        facts: &Facts,
    ) -> Result<Vec<ModelProperty>, String> {
        let mut properties: Vec<ModelProperty> = Vec::new();

        // For each placeholder, try to infer if it's a property
        for placeholder in &synthesis.placeholders {
            if placeholder.kind == PlaceholderKind::Identifier {
                // Check if examples look like property names
                for example in &placeholder.examples {
                    if self.looks_like_property_name(example) {
                        // Infer type from context or use a default
                        let prop_type = self.infer_property_type(placeholder, facts)?;
                        
                        // Create property (avoid duplicates)
                        let prop = ModelProperty {
                            name: example.clone(),
                            r#type: prop_type,
                        };
                        
                        if !properties.iter().any(|p| p.name == prop.name) {
                            properties.push(prop);
                        }
                    }
                }
            }
        }

        // If no properties found from placeholders, try to infer from cluster symbols
        if properties.is_empty() {
            // This is a fallback - in a real scenario, we'd analyze the actual code
            // For v1, we'll create properties from placeholder examples
            for placeholder in &synthesis.placeholders {
                if placeholder.kind == PlaceholderKind::Identifier && !placeholder.examples.is_empty() {
                    let prop_type = self.infer_property_type(placeholder, facts)?;
                    let prop = ModelProperty {
                        name: placeholder.examples[0].clone(),
                        r#type: prop_type,
                    };
                    properties.push(prop);
                }
            }
        }

        // Sort properties for stable ordering
        properties.sort();

        Ok(properties)
    }

    /// Check if a string looks like a property name
    fn looks_like_property_name(&self, name: &str) -> bool {
        if name.is_empty() {
            return false;
        }
        // Property names are typically PascalCase (C#) or camelCase (TypeScript)
        // For v1, we'll accept any non-keyword identifier
        !self.is_keyword(name)
    }

    /// Check if a string is a keyword
    fn is_keyword(&self, word: &str) -> bool {
        matches!(
            word,
            "class" | "function" | "const" | "let" | "var" | "if" | "else" | "for" | "while"
                | "return" | "import" | "export" | "from" | "def" | "self" | "public"
                | "private" | "protected" | "static" | "void" | "int" | "string" | "bool"
                | "true" | "false" | "null" | "undefined" | "this" | "super" | "get" | "set"
        )
    }

    /// Infer property type from placeholder context or examples
    fn infer_property_type(
        &self,
        placeholder: &Placeholder,
        _facts: &Facts,
    ) -> Result<String, String> {
        // Try to infer type from facts if we have symbol context
        // For v1, we'll use a simple heuristic based on placeholder name and examples
        
        // Check if any example looks like a type name
        for example in &placeholder.examples {
            let lower = example.to_lowercase();
            if matches!(
                lower.as_str(),
                "int" | "integer" | "number" | "float" | "double" | "decimal"
                    | "string" | "str" | "text" | "bool" | "boolean" | "date" | "datetime"
                    | "time" | "uuid" | "guid" | "binary"
            ) {
                // Normalize to Hix type names
                return Ok(self.normalize_type_name(&lower));
            }
        }

        // Default: infer from placeholder name patterns
        let name_lower = placeholder.name.to_lowercase();
        if name_lower.contains("id") || name_lower.contains("key") {
            return Ok("int".to_string());
        }
        if name_lower.contains("name") || name_lower.contains("title") || name_lower.contains("description") {
            return Ok("string".to_string());
        }
        if name_lower.contains("date") || name_lower.contains("time") {
            return Ok("datetime".to_string());
        }
        if name_lower.contains("is") || name_lower.contains("has") || name_lower.contains("can") {
            return Ok("bool".to_string());
        }

        // Default to string
        Ok("string".to_string())
    }

    /// Normalize type name to Hix format
    fn normalize_type_name(&self, type_name: &str) -> String {
        match type_name {
            "int" | "integer" | "number" => "int".to_string(),
            "float" => "float".to_string(),
            "double" => "double".to_string(),
            "decimal" => "decimal".to_string(),
            "string" | "str" | "text" => "string".to_string(),
            "bool" | "boolean" => "bool".to_string(),
            "date" | "datetime" => "datetime".to_string(),
            "time" => "time".to_string(),
            "uuid" | "guid" => "uuid".to_string(),
            "binary" => "binary".to_string(),
            _ => "string".to_string(), // Default
        }
    }

    /// Write inferred model to file
    pub fn write_model(
        &self,
        model: &InferredModel,
        output_path: &Path,
    ) -> Result<(), String> {
        // Create parent directory if needed
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create model directory: {}", e))?;
        }

        // Serialize to JSON
        let json = serde_json::to_string_pretty(model)
            .map_err(|e| format!("Failed to serialize model: {}", e))?;

        // Write to file
        fs::write(output_path, json)
            .map_err(|e| format!("Failed to write model file: {}", e))?;

        Ok(())
    }
}

impl Default for ModelInferrer {
    fn default() -> Self {
        Self::new()
    }
}

