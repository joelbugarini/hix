use crate::facts::{Facts, Symbol};
use crate::model_inference::InferredModel;
use crate::unknown_discovery::SymbolCluster;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;
use regex::Regex;

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
        // If symbols are members (variables), extract the full containing class structure
        let mut code_regions: Vec<(String, &Symbol)> = Vec::new();
        for symbol in &sorted_symbols {
            let code = if matches!(symbol.kind, crate::facts::SymbolKind::Variable) {
                // Try to extract full class structure
                self.extract_full_class_structure(symbol, facts, project_root)
                    .unwrap_or_else(|_| {
                        // Fallback to just the symbol region
                        self.extract_code_region(symbol, project_root).unwrap_or_default()
                    })
            } else {
                self.extract_code_region(symbol, project_root)?
            };
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

    /// Extract full class structure (namespace, class declaration, members) for a member symbol
    fn extract_full_class_structure(
        &self,
        member_symbol: &Symbol,
        facts: &Facts,
        project_root: &Path,
    ) -> Result<String, String> {
        // Find the member that corresponds to this symbol (by name and file)
        let member = facts.members.iter()
            .find(|m| m.name == member_symbol.name)
            .ok_or_else(|| "Member not found".to_string())?;

        // Find the containing type symbol (the member's symbol_id points to the containing type)
        let type_symbol = facts.symbols.iter()
            .find(|s| s.id == member.symbol_id && matches!(s.kind, crate::facts::SymbolKind::Type))
            .ok_or_else(|| "Containing type not found".to_string())?;

        // Extract full class code from the file
        let file_path = if Path::new(&type_symbol.file).is_absolute() {
            Path::new(&type_symbol.file).to_path_buf()
        } else {
            let symbol_path = Path::new(&type_symbol.file);
            if symbol_path.starts_with(project_root) {
                symbol_path.to_path_buf()
            } else {
                project_root.join(symbol_path)
            }
        };

        let content = fs::read_to_string(&file_path)
            .map_err(|e| format!("Failed to read file {}: {}", file_path.display(), e))?;

        let lines: Vec<&str> = content.lines().collect();
        
        // Find namespace (for C#)
        let mut namespace_start: Option<usize> = None;
        let _namespace_end: Option<usize> = None;
        let class_start = type_symbol.range.start_line as usize;
        let mut class_end = type_symbol.range.end_line as usize;

        // Look for namespace declaration before the class
        for (i, line) in lines.iter().enumerate() {
            if i < class_start && line.trim().starts_with("namespace") {
                namespace_start = Some(i);
                    // Find the opening brace (we don't need to track the end, just the start)
                    // namespace_end is not used, but kept for potential future use
                break;
            }
        }

        // Find the class closing brace
        // Start from class declaration and find matching brace
        let mut brace_count = 0;
        let mut found_class_brace = false;
        for i in class_start..lines.len() {
            let line = lines[i];
            for ch in line.chars() {
                if ch == '{' {
                    brace_count += 1;
                    found_class_brace = true;
                } else if ch == '}' {
                    brace_count -= 1;
                    if found_class_brace && brace_count == 0 {
                        class_end = i + 1;
                        break;
                    }
                }
            }
            if found_class_brace && brace_count == 0 {
                break;
            }
        }

        // Extract the full structure
        let start_line = namespace_start.unwrap_or(class_start);
        let end_line = class_end.max(class_start + 1);

        if start_line >= lines.len() {
            return Err(format!("Start line {} out of bounds", start_line));
        }

        let end_line = end_line.min(lines.len());
        let extracted_lines: Vec<&str> = if start_line < end_line {
            lines[start_line..end_line].to_vec()
        } else {
            vec![lines[start_line]]
        };

        let code = extracted_lines.join("\n");

        if code.trim().is_empty() {
            return Err("Extracted code is empty".to_string());
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

        // If no varying identifiers found (all samples identical), 
        // still identify class name and property names for Hix conversion
        if varying_identifiers.is_empty() && !all_identifiers.is_empty() {
            // Extract class name and property names from the code
            let class_name = self.extract_class_name_from_code(base_code);
            let property_names = self.extract_property_names_from_code(base_code);
            
            // Add class name as placeholder if found
            if let Some(class_name) = class_name {
                let placeholder = Placeholder {
                    name: "ClassName".to_string(),
                    kind: PlaceholderKind::Identifier,
                    examples: vec![class_name.clone()],
                    range: None,
                };
                placeholders.push(placeholder);
                template = template.replace(&class_name, "[[ClassName]]");
            }
            
            // Add property names as placeholders
            // When all samples are identical, we only need ONE property declaration template
            // The [[prop]] block will iterate over all properties in the model
            let mut new_lines = Vec::new();
            let mut found_first_property = false;
            
            for line in template.lines() {
                let trimmed = line.trim();
                let mut is_property_line = false;
                
                // Check if this is a property declaration line
                for prop_name in &property_names {
                    if trimmed.contains("public ") && trimmed.contains(prop_name) && trimmed.contains("{ get; set; }") {
                        is_property_line = true;
                        break;
                    }
                }
                
                if is_property_line {
                    if !found_first_property {
                        // Keep the first property declaration and replace property name with placeholder
                        // We'll use the first property name as the template
                        if let Some(first_prop) = property_names.first() {
                            let placeholder = Placeholder {
                                name: "Property1".to_string(),
                                kind: PlaceholderKind::Identifier,
                                examples: vec![first_prop.clone()],
                                range: None,
                            };
                            placeholders.push(placeholder);
                            
                            // Replace the property name with [[Property1]] (will be converted to [[prop.name]] later)
                            let new_line = line.replace(first_prop, "[[Property1]]");
                            new_lines.push(new_line);
                            found_first_property = true;
                        }
                    }
                    // Skip other property lines - we only need one template
                } else {
                    new_lines.push(line.to_string());
                }
            }
            template = new_lines.join("\n");
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

    /// Extract class name from code (for C# and similar languages)
    fn extract_class_name_from_code(&self, code: &str) -> Option<String> {
        // Look for "class ClassName" pattern
        for line in code.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("class ") {
                let parts: Vec<&str> = trimmed.split_whitespace().collect();
                if parts.len() >= 2 {
                    return Some(parts[1].to_string());
                }
            }
        }
        None
    }

    /// Extract property names from code (for C# properties)
    fn extract_property_names_from_code(&self, code: &str) -> Vec<String> {
        let mut property_names = Vec::new();
        
        // Look for "public TYPE PropertyName { get; set; }" pattern
        for line in code.lines() {
            let trimmed = line.trim();
            if trimmed.contains("public ") && trimmed.contains("{ get; set; }") {
                // Extract the property name (word before "{ get; set; }")
                if let Some(brace_pos) = trimmed.find("{ get; set; }") {
                    let before_brace = &trimmed[..brace_pos];
                    let parts: Vec<&str> = before_brace.split_whitespace().collect();
                    // Last part before the brace should be the property name
                    if let Some(prop_name) = parts.last() {
                        if !self.is_keyword(prop_name) {
                            property_names.push(prop_name.to_string());
                        }
                    }
                }
            }
        }
        
        property_names
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
        // Start with the template (which may contain [[PlaceholderX]] placeholders)
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

        // Detect and convert conditional patterns to Hix conditionals
        hix_template = self.detect_and_convert_conditionals(&hix_template, model);

        // Wrap property patterns in [[prop]]...[[/prop]] blocks if needed
        if hix_template.contains("[[prop.name]]") || hix_template.contains("[[prop.type]]") {
            // Check if already in a prop block
            if !hix_template.contains("[[prop]]") {
                // Try to wrap the property pattern
                hix_template = self.wrap_properties_in_block(&hix_template);
            }
        }

        // Now escape any literal [[ or ]] that appear in source code but aren't Hix tags
        // This must happen AFTER converting placeholders to Hix syntax
        hix_template = self.escape_hix_delimiters(&hix_template);
        
        // Normalize indentation
        hix_template = self.normalize_indentation(&hix_template);
        
        hix_template
    }

    /// Escape literal [[ and ]] in source code that aren't Hix tags
    /// Hix uses [[ and ]] as delimiters, so we need to escape any that appear in source
    /// We escape them as \[\[ and \]\] which can be unescaped during rendering if needed
    fn escape_hix_delimiters(&self, code: &str) -> String {
        // For now, use a simple approach: escape all [[ and ]] that don't match Hix tag patterns
        // This is a conservative approach - we'll escape everything and let the Hix parser handle it
        // In practice, if source code contains [[ or ]], they should be rare and escaping is safe
        
        // Use regex to find potential Hix tags and only escape non-tag occurrences
        let hix_tag_pattern = Regex::new(r"\[\[([^\]]+)\]\]").unwrap();
        
        // First, mark all valid Hix tags
        let mut protected_ranges: Vec<(usize, usize)> = Vec::new();
        for cap in hix_tag_pattern.captures_iter(code) {
            let full_match = cap.get(0).unwrap();
            let tag_content = cap.get(1).unwrap().as_str().trim();
            
            if self.looks_like_hix_tag(tag_content) {
                protected_ranges.push((full_match.start(), full_match.end()));
            }
        }
        
        // Now escape all [[ and ]] that are not in protected ranges
        let mut escaped = String::new();
        let mut pos = 0;
        let chars: Vec<char> = code.chars().collect();
        
        while pos < chars.len() {
            if pos < chars.len() - 1 && chars[pos] == '[' && chars[pos + 1] == '[' {
                // Check if this is in a protected range
                let in_protected = protected_ranges.iter().any(|(start, end)| {
                    pos >= *start && pos < *end
                });
                
                if in_protected {
                    // Keep as-is (it's a Hix tag)
                    escaped.push(chars[pos]);
                    escaped.push(chars[pos + 1]);
                    pos += 2;
                } else {
                    // Escape it
                    escaped.push_str("\\[\\[");
                    pos += 2;
                }
            } else if pos < chars.len() - 1 && chars[pos] == ']' && chars[pos + 1] == ']' {
                // Check if this is in a protected range
                let in_protected = protected_ranges.iter().any(|(start, end)| {
                    pos >= *start && pos < *end
                });
                
                if in_protected {
                    // Keep as-is (it's a Hix tag)
                    escaped.push(chars[pos]);
                    escaped.push(chars[pos + 1]);
                    pos += 2;
                } else {
                    // Escape it
                    escaped.push_str("\\]\\]");
                    pos += 2;
                }
            } else {
                escaped.push(chars[pos]);
                pos += 1;
            }
        }
        
        escaped
    }

    /// Check if a string looks like a valid Hix tag content
    fn looks_like_hix_tag(&self, content: &str) -> bool {
        let trimmed = content.trim();
        
        // Valid Hix tag patterns:
        // - model.*
        // - prop.*
        // - prop, /prop
        // - if *, else, /if
        // - function calls (upper, lower, etc.)
        
        if trimmed.starts_with("model.") || trimmed.starts_with("prop.") {
            return true;
        }
        
        if trimmed == "prop" || trimmed == "/prop" {
            return true;
        }
        
        if trimmed.starts_with("if ") || trimmed == "else" || trimmed == "/if" {
            return true;
        }
        
        // Function calls
        let parts: Vec<&str> = trimmed.split_whitespace().collect();
        if !parts.is_empty() {
            let func_name = parts[0];
            if ["upper", "lower", "snake_case", "kebab_case", "lowerFirst", "module_transform"]
                .contains(&func_name) && parts.len() >= 2 {
                return true;
            }
        }
        
        false
    }

    /// Detect conditional patterns in code and convert them to Hix conditionals
    /// This looks for patterns like:
    /// - Different code for different property types (bool vs string, etc.)
    /// - if/else statements that check types
    /// - Type-specific property declarations
    fn detect_and_convert_conditionals(&self, template: &str, model: &InferredModel) -> String {
        let mut result = template.to_string();
        
        // Pattern 1: Detect type-specific property declarations
        // Look for patterns where different types have different code structures
        // Example: "public bool PropertyName;" vs "public string PropertyName;"
        
        // Check if we have properties with different types that might need conditionals
        // This information could be used for future enhancements to detect type-based conditionals
        // For now, we'll just detect explicit if/else patterns in the code
        let _has_bool_props = model.properties.iter().any(|p| {
            p.r#type.to_lowercase() == "bool" || p.r#type.to_lowercase() == "boolean"
        });
        let _has_non_bool_props = model.properties.iter().any(|p| {
            p.r#type.to_lowercase() != "bool" && p.r#type.to_lowercase() != "boolean"
        });
        
        // Future enhancement: If we have both bool and non-bool properties, and the template has type-specific code,
        // we might want to generate conditionals automatically
        // For now, we'll do a simple heuristic: if we see "bool" and other types in the template,
        // and they're used in similar contexts, suggest a conditional
        
        // Pattern 2: Detect if/else patterns in source code
        // Look for simple if/else patterns that check types
        result = self.convert_if_else_patterns(&result);
        
        result
    }

    /// Convert if/else patterns in source code to Hix conditionals
    fn convert_if_else_patterns(&self, code: &str) -> String {
        use regex::Regex;
        let mut result = code.to_string();
        
        // Pattern: if (type == "bool") or if (type == "string") etc.
        // This is a simple pattern - we'll look for if statements that check property types
        // and convert them to Hix conditionals
        
        // Pattern 1: if (prop.type == "bool") { ... } else { ... }
        // Using regular string with proper escaping
        let if_type_pattern = Regex::new("(?s)if\\s*\\([^)]*type[^)]*==\\s*[\"']bool[\"'][^)]*\\)\\s*\\{([^}]+)\\}\\s*else\\s*\\{([^}]+)\\}").unwrap();
        result = if_type_pattern.replace_all(&result, |caps: &regex::Captures| {
            let true_branch = caps.get(1).map(|m| m.as_str()).unwrap_or("");
            let false_branch = caps.get(2).map(|m| m.as_str()).unwrap_or("");
            format!("[[if prop.type=bool]]{}\n[[else]]\n{}\n[[/if]]", true_branch.trim(), false_branch.trim())
        }).to_string();
        
        // Pattern 2: if (prop.type == "string") { ... }
        let if_string_pattern = Regex::new("(?s)if\\s*\\([^)]*type[^)]*==\\s*[\"']string[\"'][^)]*\\)\\s*\\{([^}]+)\\}").unwrap();
        result = if_string_pattern.replace_all(&result, |caps: &regex::Captures| {
            let true_branch = caps.get(1).map(|m| m.as_str()).unwrap_or("");
            format!("[[if prop.type=string]]{}\n[[/if]]", true_branch.trim())
        }).to_string();
        
        // Pattern 3: Simple type-based conditionals in property declarations
        // Look for patterns like: "public bool" vs "public string" in similar contexts
        // This is more heuristic-based - for now, we'll skip this as it's complex
        
        result
    }

    /// Normalize indentation in template to ensure consistent formatting
    fn normalize_indentation(&self, template: &str) -> String {
        let lines: Vec<&str> = template.lines().collect();
        if lines.is_empty() {
            return template.to_string();
        }
        
        // Detect base indentation (use first non-empty line)
        let base_indent = lines.iter()
            .find(|line| !line.trim().is_empty())
            .map(|line| {
                line.chars()
                    .take_while(|c| c.is_whitespace())
                    .count()
            })
            .unwrap_or(0);
        
        let mut result = String::new();
        let mut in_prop_block = false;
        let mut prop_indent_level = 0;
        
        for (idx, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            
            // Track prop block state
            if trimmed == "[[prop]]" {
                in_prop_block = true;
                prop_indent_level = line.chars().take_while(|c| c.is_whitespace()).count();
            } else if trimmed == "[[/prop]]" {
                in_prop_block = false;
                prop_indent_level = 0;
            }
            
            // Preserve empty lines
            if trimmed.is_empty() {
                result.push('\n');
                continue;
            }
            
            // For lines inside prop blocks, maintain relative indentation
            if in_prop_block && (trimmed.contains("[[prop.") || trimmed.contains("[[prop.type]]")) {
                // Calculate relative indent from prop block start
                let current_indent = line.chars().take_while(|c| c.is_whitespace()).count();
                let relative_indent = if current_indent > prop_indent_level {
                    current_indent - prop_indent_level
                } else {
                    0
                };
                
                // Use consistent indentation (4 spaces per level)
                let normalized_indent = " ".repeat(prop_indent_level + relative_indent);
                result.push_str(&normalized_indent);
                result.push_str(trimmed);
            } else {
                // For other lines, preserve original indentation relative to base
                let current_indent = line.chars().take_while(|c| c.is_whitespace()).count();
                let relative_indent = if current_indent >= base_indent {
                    current_indent - base_indent
                } else {
                    0
                };
                
                let normalized_indent = " ".repeat(base_indent + relative_indent);
                result.push_str(&normalized_indent);
                result.push_str(trimmed);
            }
            
            if idx < lines.len() - 1 {
                result.push('\n');
            }
        }
        
        result
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model_inference::InferredModel;

    #[test]
    fn test_escape_hix_delimiters() {
        let synthesizer = TemplateSynthesizer::new();
        
        // Test escaping literal [[ and ]] in source code
        let code = r#"public class MyClass {
    public string GetValue() {
        return "[[value]]";
    }
}"#;
        let escaped = synthesizer.escape_hix_delimiters(code);
        assert!(escaped.contains("\\[\\[value\\]\\]"), "Should escape literal [[ and ]]");
        
        // Test that Hix tags are not escaped
        let code_with_hix = r#"public class [[model.className]] {
    [[prop]]public [[prop.type]] [[prop.name]];[[/prop]]
}"#;
        let escaped_hix = synthesizer.escape_hix_delimiters(code_with_hix);
        assert!(escaped_hix.contains("[[model.className]]"), "Should not escape Hix tags");
        assert!(escaped_hix.contains("[[prop]]"), "Should not escape Hix tags");
    }

    #[test]
    fn test_looks_like_hix_tag() {
        let synthesizer = TemplateSynthesizer::new();
        
        assert!(synthesizer.looks_like_hix_tag("model.className"));
        assert!(synthesizer.looks_like_hix_tag("prop.name"));
        assert!(synthesizer.looks_like_hix_tag("prop"));
        assert!(synthesizer.looks_like_hix_tag("/prop"));
        assert!(synthesizer.looks_like_hix_tag("if prop.type=bool"));
        assert!(synthesizer.looks_like_hix_tag("else"));
        assert!(synthesizer.looks_like_hix_tag("/if"));
        assert!(synthesizer.looks_like_hix_tag("upper prop.name"));
        
        assert!(!synthesizer.looks_like_hix_tag("Placeholder1"));
        assert!(!synthesizer.looks_like_hix_tag("random text"));
    }

    #[test]
    fn test_normalize_indentation() {
        let synthesizer = TemplateSynthesizer::new();
        
        let template = r#"namespace Test {
    public class Person {
[[prop]]
        public string Name { get; set; }
[[/prop]]
    }
}"#;
        let normalized = synthesizer.normalize_indentation(template);
        // Should maintain consistent indentation
        assert!(normalized.contains("[[prop]]"), "Should preserve prop blocks");
        assert!(normalized.contains("[[/prop]]"), "Should preserve closing prop blocks");
    }

    #[test]
    fn test_convert_to_hix_syntax_with_escaping() {
        let synthesizer = TemplateSynthesizer::new();
        
        let model = InferredModel {
            className: "Person".to_string(),
            properties: vec![
                crate::model_inference::ModelProperty {
                    name: "Name".to_string(),
                    r#type: "string".to_string(),
                },
            ],
            annotations: None,
            imports: None,
            namespace: None,
        };
        
        let placeholders = vec![
            Placeholder {
                name: "Property1".to_string(),
                kind: PlaceholderKind::Identifier,
                examples: vec!["Name".to_string()],
                range: None,
            },
        ];
        
        // Template with literal [[ in source code
        let template = r#"public class Person {
    public string [[Property1]] { get; set; }
    // Comment with [[brackets]]
}"#;
        
        let hix_template = synthesizer.convert_to_hix_syntax(template, &placeholders, &model);
        
        // Should convert Property1 to prop.name
        assert!(hix_template.contains("[[prop.name]]"), "Should convert placeholder to Hix syntax");
        // Should escape literal [[ in comment
        assert!(hix_template.contains("\\[\\[brackets\\]\\]"), "Should escape literal [[ in comments");
    }

    #[test]
    fn test_convert_if_else_patterns() {
        let synthesizer = TemplateSynthesizer::new();
        
        // Test if/else pattern conversion
        let code_with_if = r#"if (property.type == "bool") {
    public bool PropertyName;
} else {
    public string PropertyName;
}"#;
        
        let converted = synthesizer.convert_if_else_patterns(code_with_if);
        assert!(converted.contains("[[if prop.type=bool]]"), "Should convert if pattern to Hix conditional");
        assert!(converted.contains("[[else]]"), "Should include else branch");
        assert!(converted.contains("[[/if]]"), "Should close conditional");
        
        // Test if-only pattern
        let code_with_if_only = r#"if (property.type == "string") {
    public string PropertyName;
}"#;
        
        let converted_if_only = synthesizer.convert_if_else_patterns(code_with_if_only);
        assert!(converted_if_only.contains("[[if prop.type=string]]"), "Should convert if-only pattern");
        assert!(converted_if_only.contains("[[/if]]"), "Should close conditional");
    }
}

