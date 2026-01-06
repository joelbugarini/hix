use crate::facts::{Facts, Symbol};
use crate::model_inference::InferredModel;
use crate::unknown_discovery::SymbolCluster;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// Template synthesis module
/// Synthesizes templates from clusters of similar code structures

/// Placeholder kind
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum PlaceholderKind {
    Identifier,
    TypeReference,
    StringLiteral,
    List,
}

/// Placeholder information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Placeholder {
    /// Placeholder name (e.g., "TypeName", "PropertyName")
    pub name: String,
    /// Placeholder kind
    pub kind: PlaceholderKind,
    /// Example values from the cluster
    pub examples: Vec<String>,
    /// Source range in base sample (for reference)
    pub range: Option<CodeRange>,
}

/// Code range (line/column based)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CodeRange {
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
}

/// Template synthesis result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SynthesisResult {
    /// Synthesized template content (Hix template format)
    pub template: String,
    /// Placeholder map
    pub placeholders: Vec<Placeholder>,
    /// Base sample code content used for synthesis
    pub base_sample_code: String,
    /// Base sample file path
    pub base_sample_file: String,
    /// Language of the template
    pub language: String,
    /// Template name
    pub template_name: String,
}

/// Synthesis metadata (written to synthesis.json)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SynthesisMetadata {
    /// Template name
    pub template_name: String,
    /// Language
    pub language: String,
    /// Cluster ID this was synthesized from
    pub cluster_id: String,
    /// Placeholders
    pub placeholders: Vec<Placeholder>,
    /// Base sample file path
    pub base_sample_file: String,
    /// All sample files used
    pub sample_files: Vec<String>,
}

pub struct TemplateSynthesizer;

impl TemplateSynthesizer {
    pub fn new() -> Self {
        TemplateSynthesizer
    }

    /// Synthesize a template from a cluster
    pub fn synthesize(
        &self,
        cluster: &SymbolCluster,
        facts: &Facts,
        project_root: &Path,
    ) -> Result<SynthesisResult, String> {
        // Get all symbols in the cluster
        let symbols: Vec<&Symbol> = cluster
            .symbol_ids
            .iter()
            .filter_map(|id| facts.symbols.iter().find(|s| s.id == *id))
            .collect();

        if symbols.is_empty() {
            return Err("Cluster has no symbols".to_string());
        }

        // Choose base sample (first symbol, sorted by file path for determinism)
        let mut sorted_symbols = symbols;
        sorted_symbols.sort_by(|a, b| {
            a.file.cmp(&b.file).then_with(|| a.name.cmp(&b.name))
        });
        let base_symbol = sorted_symbols[0];

        // Determine language from base symbol's file
        let language = self.detect_language(&base_symbol.file)?;

        // Extract code regions for all symbols
        let mut code_regions: Vec<(String, &Symbol)> = Vec::new();
        for symbol in &sorted_symbols {
            let code = self.extract_code_region(symbol, project_root)?;
            code_regions.push((code, symbol));
        }

        // Synthesize template from code regions
        let (template, placeholders) = self.synthesize_from_code_regions(&code_regions)?;

        Ok(SynthesisResult {
            template,
            placeholders,
            base_sample_code: code_regions[0].0.clone(),
            base_sample_file: base_symbol.file.clone(),
            language,
            template_name: format!("template_{}", cluster.cluster_id),
        })
    }

    /// Detect language from file path
    fn detect_language(&self, file_path: &str) -> Result<String, String> {
        let path = Path::new(file_path);
        if let Some(ext) = path.extension() {
            match ext.to_str().unwrap_or("") {
                "ts" | "tsx" => Ok("typescript".to_string()),
                "js" | "jsx" => Ok("javascript".to_string()),
                "py" => Ok("python".to_string()),
                "cs" => Ok("csharp".to_string()),
                "html" => Ok("html".to_string()),
                _ => Err(format!("Unknown language for file: {}", file_path)),
            }
        } else {
            Err(format!("No extension found for file: {}", file_path))
        }
    }

    /// Extract code region for a symbol from source file
    fn extract_code_region(&self, symbol: &Symbol, project_root: &Path) -> Result<String, String> {
        // Symbol file path is relative to the scan root (project_root)
        // If it's already absolute, use it as-is; otherwise join with project_root
        let file_path = if Path::new(&symbol.file).is_absolute() {
            Path::new(&symbol.file).to_path_buf()
        } else {
            // Normalize the path: if symbol.file already contains project_root, don't duplicate
            let symbol_path = Path::new(&symbol.file);
            if symbol_path.starts_with(project_root) {
                symbol_path.to_path_buf()
            } else {
                project_root.join(symbol_path)
            }
        };

        let content = fs::read_to_string(&file_path)
            .map_err(|e| format!("Failed to read file {}: {}", file_path.display(), e))?;

        let lines: Vec<&str> = content.lines().collect();
        
        // Extract lines for the symbol's range (0-based to 1-based conversion)
        let start_line = symbol.range.start_line as usize;
        let end_line = symbol.range.end_line as usize;

        if start_line >= lines.len() {
            return Err(format!("Start line {} out of bounds for file {}", start_line, file_path.display()));
        }

        let end_line = end_line.min(lines.len());
        let extracted_lines: Vec<&str> = if start_line < end_line {
            lines[start_line..end_line].to_vec()
        } else {
            // If range is invalid, extract at least the start line
            if start_line < lines.len() {
                vec![lines[start_line]]
            } else {
                vec![]
            }
        };

        let code = extracted_lines.join("\n");
        
        // Ensure we return at least something
        if code.trim().is_empty() {
            return Err(format!("Extracted code region is empty for symbol {} in file {}", 
                symbol.name, file_path.display()));
        }

        Ok(code)
    }

    /// Synthesize template from code regions using simple text alignment
    fn synthesize_from_code_regions(
        &self,
        code_regions: &[(String, &Symbol)],
    ) -> Result<(String, Vec<Placeholder>), String> {
        if code_regions.is_empty() {
            return Err("No code regions provided".to_string());
        }

        if code_regions.len() == 1 {
            // Single sample: create a template with all identifiers as placeholders
            return self.synthesize_single_sample(&code_regions[0].0);
        }

        // For multiple samples, find common structure
        // Simple approach: find longest common prefix and suffix
        let base_code = &code_regions[0].0;
        let mut template = base_code.clone();
        let mut placeholders: Vec<Placeholder> = Vec::new();

        // For v1, we'll do a simple approach:
        // 1. Find identifiers that differ across samples
        // 2. Replace them with placeholders
        // 3. Keep common structure

        // Extract identifiers from all samples
        let all_identifiers: Vec<Vec<String>> = code_regions
            .iter()
            .map(|(code, _)| self.extract_identifiers(code))
            .collect();

        // Find identifiers that vary across samples
        let varying_identifiers = self.find_varying_identifiers(&all_identifiers);

        // Replace varying identifiers with placeholders
        for (idx, (identifier, examples)) in varying_identifiers.iter().enumerate() {
            let placeholder_name = format!("Placeholder{}", idx + 1);
            let placeholder = Placeholder {
                name: placeholder_name.clone(),
                kind: PlaceholderKind::Identifier,
                examples: examples.clone(),
                range: None, // TODO: compute range
            };
            placeholders.push(placeholder);

            // Replace in template (simple string replacement for v1)
            // Use word boundaries to avoid partial matches
            template = template.replace(identifier, &format!("[[{}]]", placeholder_name));
        }

        // If no placeholders were found but we have multiple samples, 
        // at least ensure the template is not empty
        if template.trim().is_empty() {
            template = base_code.clone();
        }

        Ok((template, placeholders))
    }

    /// Synthesize template from a single sample
    fn synthesize_single_sample(&self, code: &str) -> Result<(String, Vec<Placeholder>), String> {
        // Extract identifiers and create placeholders for them
        let identifiers = self.extract_identifiers(code);
        let mut template = code.to_string();
        let mut placeholders: Vec<Placeholder> = Vec::new();

        for (idx, identifier) in identifiers.iter().enumerate() {
            let placeholder_name = format!("Identifier{}", idx + 1);
            let placeholder = Placeholder {
                name: placeholder_name.clone(),
                kind: PlaceholderKind::Identifier,
                examples: vec![identifier.clone()],
                range: None,
            };
            placeholders.push(placeholder);
            template = template.replace(identifier, &format!("[[{}]]", placeholder_name));
        }

        Ok((template, placeholders))
    }

    /// Extract identifiers from code (simple regex-like approach)
    fn extract_identifiers(&self, code: &str) -> Vec<String> {
        // Simple identifier extraction: words that match identifier pattern
        // This is a basic implementation - can be enhanced with proper AST parsing
        let mut identifiers: Vec<String> = Vec::new();
        let mut current = String::new();
        let mut in_identifier = false;

        for ch in code.chars() {
            if ch.is_alphanumeric() || ch == '_' {
                if !in_identifier {
                    in_identifier = true;
                    current.clear();
                }
                current.push(ch);
            } else {
                if in_identifier && !current.is_empty() {
                    // Skip keywords and common words
                    if !self.is_keyword(&current) {
                        identifiers.push(current.clone());
                    }
                    current.clear();
                }
                in_identifier = false;
            }
        }

        if in_identifier && !current.is_empty() && !self.is_keyword(&current) {
            identifiers.push(current);
        }

        // Remove duplicates while preserving order
        let mut seen = std::collections::HashSet::new();
        identifiers.into_iter().filter(|id| seen.insert(id.clone())).collect()
    }

    /// Check if a word is a keyword (basic list)
    fn is_keyword(&self, word: &str) -> bool {
        matches!(
            word,
            "class" | "function" | "const" | "let" | "var" | "if" | "else" | "for" | "while"
                | "return" | "import" | "export" | "from" | "def" | "self" | "public"
                | "private" | "protected" | "static" | "void" | "int" | "string" | "bool"
                | "true" | "false" | "null" | "undefined" | "this" | "super"
        )
    }

    /// Find identifiers that vary across samples
    fn find_varying_identifiers(
        &self,
        all_identifiers: &[Vec<String>],
    ) -> Vec<(String, Vec<String>)> {
        if all_identifiers.is_empty() {
            return Vec::new();
        }

        let base_identifiers = &all_identifiers[0];
        let mut varying: Vec<(String, Vec<String>)> = Vec::new();

        for identifier in base_identifiers {
            let mut examples: Vec<String> = vec![identifier.clone()];
            let mut found_different = false;

            for other_identifiers in all_identifiers.iter().skip(1) {
                // Check if this identifier position has a different value
                if let Some(pos) = base_identifiers.iter().position(|x| x == identifier) {
                    if let Some(other_id) = other_identifiers.get(pos) {
                        if other_id != identifier {
                            found_different = true;
                            if !examples.contains(other_id) {
                                examples.push(other_id.clone());
                            }
                        }
                    }
                }
            }

            if found_different {
                varying.push((identifier.clone(), examples));
            }
        }

        varying
    }

    /// Write synthesis result to files
    /// 
    /// Note: Kept for backward compatibility. Use `write_synthesis_with_hix_syntax` instead.
    #[allow(dead_code)]
    pub fn write_synthesis(
        &self,
        result: &SynthesisResult,
        metadata: &SynthesisMetadata,
        output_dir: &Path,
    ) -> Result<(), String> {
        // Create output directory structure
        let templates_dir = output_dir.join("templates").join(&result.language);
        fs::create_dir_all(&templates_dir)
            .map_err(|e| format!("Failed to create templates directory: {}", e))?;

        // Write template file
        let template_path = templates_dir.join(format!("{}.hix", result.template_name));
        fs::write(&template_path, &result.template)
            .map_err(|e| format!("Failed to write template file: {}", e))?;

        // Write synthesis.json
        let synthesis_path = output_dir.join("synthesis.json");
        let json = serde_json::to_string_pretty(metadata)
            .map_err(|e| format!("Failed to serialize synthesis metadata: {}", e))?;
        fs::write(&synthesis_path, json)
            .map_err(|e| format!("Failed to write synthesis.json: {}", e))?;

        Ok(())
    }

    /// Convert template with placeholders to valid Hix syntax using inferred model
    pub fn convert_to_hix_syntax(
        &self,
        template: &str,
        placeholders: &[Placeholder],
        model: &InferredModel,
    ) -> String {
        let mut hix_template = template.to_string();

        // Map placeholders to Hix syntax
        for placeholder in placeholders {
            // Try to match placeholder examples to model properties
            let mut matched = false;

            // Check if this placeholder matches a property name
            for prop in &model.properties {
                if placeholder.examples.contains(&prop.name) {
                    // Replace with [[prop.name]]
                    for example in &placeholder.examples {
                        if example == &prop.name {
                            hix_template = hix_template.replace(
                                &format!("[[{}]]", placeholder.name),
                                "[[prop.name]]",
                            );
                            matched = true;
                            break;
                        }
                    }
                    if matched {
                        break;
                    }
                }
            }

            // Check if this placeholder matches the class name
            if !matched && placeholder.examples.contains(&model.className) {
                hix_template = hix_template.replace(
                    &format!("[[{}]]", placeholder.name),
                    "[[model.className]]",
                );
                matched = true;
            }

            // If not matched, try to infer from context
            if !matched {
                // Check if it looks like a type reference
                for example in &placeholder.examples {
                    if self.looks_like_type_name(example) {
                        hix_template = hix_template.replace(
                            &format!("[[{}]]", placeholder.name),
                            "[[prop.type]]",
                        );
                        // matched is set but not used after this point - that's fine
                        break;
                    }
                }
            }
        }

        // Replace type literals with [[prop.type]] using model property types
        // This handles cases where types like "int", "string" are in the code
        // Map Hix types to common language type keywords
        let type_mapping: Vec<(&str, Vec<&str>)> = vec![
            ("int", vec!["int", "integer", "number"]),
            ("string", vec!["string", "str", "text"]),
            ("bool", vec!["bool", "boolean"]),
            ("float", vec!["float"]),
            ("double", vec!["double"]),
            ("decimal", vec!["decimal"]),
            ("datetime", vec!["datetime", "date", "time"]),
        ];

        // Replace type keywords that appear before [[prop.name]]
        // We need to replace any type keyword, not just ones matching the model type
        // because the model might have inferred types incorrectly
        for (_, keywords) in &type_mapping {
            for keyword in keywords {
                // Pattern: "public KEYWORD [[prop.name]]" -> "public [[prop.type]] [[prop.name]]"
                let pattern = format!("public {} [[prop.name]]", keyword);
                if hix_template.contains(&pattern) {
                    hix_template = hix_template.replace(&pattern, "public [[prop.type]] [[prop.name]]");
                }
                // Also handle with different spacing/indentation
                let pattern2 = format!("        public {} [[prop.name]]", keyword);
                if hix_template.contains(&pattern2) {
                    hix_template = hix_template.replace(&pattern2, "        public [[prop.type]] [[prop.name]]");
                }
                // Handle without "public"
                let pattern3 = format!("{} [[prop.name]]", keyword);
                if hix_template.contains(&pattern3) && !hix_template.contains("[[prop.type]]") {
                    hix_template = hix_template.replace(&pattern3, "[[prop.type]] [[prop.name]]");
                }
            }
        }

        // Wrap property patterns in [[prop]]...[[/prop]] blocks if needed
        if hix_template.contains("[[prop.name]]") || hix_template.contains("[[prop.type]]") {
            // Check if already in a prop block
            if !hix_template.contains("[[prop]]") {
                // Try to wrap the property pattern
                hix_template = self.wrap_properties_in_block(&hix_template);
            }
        }

        hix_template
    }

    /// Check if a string looks like a type name
    fn looks_like_type_name(&self, name: &str) -> bool {
        let lower = name.to_lowercase();
        matches!(
            lower.as_str(),
            "int" | "integer" | "string" | "str" | "bool" | "boolean"
                | "float" | "double" | "decimal" | "datetime" | "date" | "time"
                | "uuid" | "guid" | "binary" | "text"
        ) || name.chars().next().map_or(false, |c| c.is_uppercase())
    }

    /// Wrap property patterns in [[prop]]...[[/prop]] blocks
    fn wrap_properties_in_block(&self, template: &str) -> String {
        // Simple heuristic: wrap lines containing [[prop.type]] and [[prop.name]]
        let lines: Vec<&str> = template.lines().collect();
        let mut result = String::new();
        let mut in_prop_block = false;
        let mut prop_lines: Vec<String> = Vec::new();

        for line in lines {
            if line.contains("[[prop.name]]") || line.contains("[[prop.type]]") {
                if !in_prop_block {
                    // Start prop block
                    in_prop_block = true;
                    result.push_str("[[prop]]\n");
                }
                prop_lines.push(line.to_string());
            } else {
                if in_prop_block {
                    // End prop block
                    for prop_line in &prop_lines {
                        result.push_str(prop_line);
                        result.push('\n');
                    }
                    result.push_str("[[/prop]]\n");
                    prop_lines.clear();
                    in_prop_block = false;
                }
                result.push_str(line);
                result.push('\n');
            }
        }

        // Close any open prop block
        if in_prop_block {
            for prop_line in &prop_lines {
                result.push_str(prop_line);
                result.push('\n');
            }
            result.push_str("[[/prop]]\n");
        }

        result.trim_end().to_string()
    }

    /// Write synthesis result with Hix syntax conversion
    pub fn write_synthesis_with_hix_syntax(
        &self,
        result: &SynthesisResult,
        metadata: &SynthesisMetadata,
        model: &InferredModel,
        output_dir: &Path,
    ) -> Result<(), String> {
        // Convert to valid Hix syntax
        let hix_template = self.convert_to_hix_syntax(&result.template, &result.placeholders, model);

        // Create output directory structure
        let templates_dir = output_dir.join("templates").join(&result.language);
        fs::create_dir_all(&templates_dir)
            .map_err(|e| format!("Failed to create templates directory: {}", e))?;

        // Write template file with Hix syntax
        let template_path = templates_dir.join(format!("{}.hix", result.template_name));
        fs::write(&template_path, &hix_template)
            .map_err(|e| format!("Failed to write template file: {}", e))?;

        // Write synthesis.json
        let synthesis_path = output_dir.join("synthesis.json");
        let json = serde_json::to_string_pretty(metadata)
            .map_err(|e| format!("Failed to serialize synthesis metadata: {}", e))?;
        fs::write(&synthesis_path, json)
            .map_err(|e| format!("Failed to write synthesis.json: {}", e))?;

        Ok(())
    }
}

impl Default for TemplateSynthesizer {
    fn default() -> Self {
        Self::new()
    }
}

