use crate::facts::{Facts, Symbol};
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
}

impl Default for TemplateSynthesizer {
    fn default() -> Self {
        Self::new()
    }
}

