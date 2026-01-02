use crate::facts::{Facts, Member, MemberKind, Symbol, SymbolKind};
use crate::pack::PatternRule;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Match rule DSL structure
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MatchRule {
    /// Symbol kind to match (type, function, variable)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbol_kind: Option<String>,
    /// Member predicates (conditions on members)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub member_predicates: Option<MemberPredicates>,
    /// Annotation predicates (conditions on annotations)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub annotation_predicates: Option<AnnotationPredicates>,
    /// Language filter (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<Vec<String>>,
    /// Name pattern (optional regex or prefix)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name_pattern: Option<String>,
}

/// Member predicates
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MemberPredicates {
    /// Minimum number of members
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_count: Option<usize>,
    /// Maximum number of members
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_count: Option<usize>,
    /// Only fields/properties (no methods)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub only_fields: Option<bool>,
    /// Only methods (no fields)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub only_methods: Option<bool>,
}

/// Annotation predicates
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AnnotationPredicates {
    /// Required annotation keys
    #[serde(skip_serializing_if = "Option::is_none")]
    pub required_keys: Option<Vec<String>>,
    /// Annotation key-value pairs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub key_value_pairs: Option<HashMap<String, String>>,
}

/// Match instance with bindings
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MatchInstance {
    /// Pattern name that matched
    pub pattern_name: String,
    /// Matched symbol ID
    pub symbol_id: String,
    /// Symbol name
    pub symbol_name: String,
    /// File path
    pub file: String,
    /// Bindings (key-value pairs extracted from the match)
    pub bindings: HashMap<String, String>,
}

/// Match results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchResults {
    /// All match instances
    pub instances: Vec<MatchInstance>,
}

impl MatchResults {
    pub fn new() -> Self {
        MatchResults {
            instances: Vec::new(),
        }
    }

    pub fn sort(&mut self) {
        self.instances.sort_by(|a, b| {
            a.pattern_name
                .cmp(&b.pattern_name)
                .then_with(|| a.symbol_id.cmp(&b.symbol_id))
        });
    }
}

impl Default for MatchResults {
    fn default() -> Self {
        Self::new()
    }
}

pub struct PatternMatcher;

impl PatternMatcher {
    pub fn new() -> Self {
        PatternMatcher
    }

    /// Match patterns against facts
    pub fn match_patterns(
        &self,
        facts: &Facts,
        rules: &[PatternRule],
    ) -> Result<MatchResults, String> {
        let mut results = MatchResults::new();

        for rule in rules {
            // Parse match conditions from rule
            let match_rule = self.parse_match_rule(rule)?;

            // Match based on pattern name
            match rule.name.as_str() {
                "data-structure" => {
                    self.match_data_structure(facts, &match_rule, &mut results);
                }
                "http-endpoint" => {
                    self.match_http_endpoint(facts, &match_rule, &mut results);
                }
                "html-form" => {
                    self.match_html_form(facts, &match_rule, &mut results);
                }
                "typescript-component" => {
                    self.match_typescript_component(facts, &match_rule, &mut results);
                }
                _ => {
                    // Unknown pattern, skip
                    continue;
                }
            }
        }

        results.sort();
        Ok(results)
    }

    fn parse_match_rule(&self, rule: &PatternRule) -> Result<MatchRule, String> {
        if let Some(conditions) = &rule.match_conditions {
            serde_json::from_value(conditions.clone())
                .map_err(|e| format!("Failed to parse match rule: {}", e))
        } else {
            // Default empty rule
            Ok(MatchRule {
                symbol_kind: None,
                member_predicates: None,
                annotation_predicates: None,
                language: None,
                name_pattern: None,
            })
        }
    }

    /// Match data-structure pattern (DTO-like: type with only public fields/properties)
    fn match_data_structure(
        &self,
        facts: &Facts,
        rule: &MatchRule,
        results: &mut MatchResults,
    ) {
        for symbol in &facts.symbols {
            // Must be a type
            if symbol.kind != SymbolKind::Type {
                continue;
            }

            // Check language filter
            if let Some(languages) = &rule.language {
                let file_lang = facts
                    .files
                    .iter()
                    .find(|f| f.path == symbol.file)
                    .map(|f| f.language.as_str());
                if let Some(lang) = file_lang {
                    if !languages.iter().any(|l| lang == l || lang.starts_with(l)) {
                        continue;
                    }
                }
            }

            // Get all members for this symbol
            let members: Vec<&Member> = facts
                .members
                .iter()
                .filter(|m| m.symbol_id == symbol.id)
                .collect();

            // Separate methods and fields
            let methods: Vec<&Member> = members.iter().filter(|m| matches!(m.kind, MemberKind::Method)).copied().collect();
            let fields: Vec<&Member> = members.iter().filter(|m| matches!(m.kind, MemberKind::Field | MemberKind::Property)).copied().collect();

            // Check member predicates
            if let Some(predicates) = &rule.member_predicates {
                if let Some(min) = predicates.min_count {
                    if fields.len() < min {
                        continue;
                    }
                }
                if let Some(max) = predicates.max_count {
                    if fields.len() > max {
                        continue;
                    }
                }
                if predicates.only_fields == Some(true) {
                    // Must have only fields/properties, no methods (or only __init__ for Python)
                    let has_non_init_methods = methods.iter().any(|m| m.name != "__init__");
                    if has_non_init_methods {
                        continue;
                    }
                }
            } else {
                // Default: must have at least one field/property and no methods (or only __init__ for Python)
                if fields.is_empty() {
                    continue;
                }
                // Allow __init__ method for Python data structures
                let has_non_init_methods = methods.iter().any(|m| m.name != "__init__");
                if has_non_init_methods {
                    continue;
                }
            }

            // Match found!
            let mut bindings = HashMap::new();
            bindings.insert("type_name".to_string(), symbol.name.clone());
            bindings.insert("member_count".to_string(), fields.len().to_string());

            results.instances.push(MatchInstance {
                pattern_name: "data-structure".to_string(),
                symbol_id: symbol.id.clone(),
                symbol_name: symbol.name.clone(),
                file: symbol.file.clone(),
                bindings,
            });
        }
    }

    /// Match http-endpoint pattern (function/method with route annotation or naming convention)
    fn match_http_endpoint(
        &self,
        facts: &Facts,
        _rule: &MatchRule,
        results: &mut MatchResults,
    ) {
        for symbol in &facts.symbols {
            // Must be a function
            if symbol.kind != SymbolKind::Function {
                continue;
            }

            // Check name pattern (e.g., starts with "get", "post", "put", "delete", "patch")
            let name_lower = symbol.name.to_lowercase();
            let http_methods = ["get", "post", "put", "delete", "patch", "head", "options"];
            let has_http_naming = http_methods.iter().any(|method| name_lower.starts_with(method));

            // Check for route annotations
            let has_route_annotation = facts
                .annotations
                .iter()
                .any(|a| a.symbol_id == symbol.id && (a.key == "route" || a.key == "path" || a.key.contains("route") || a.key.contains("path")));

            if !has_http_naming && !has_route_annotation {
                continue;
            }

            // Match found!
            let mut bindings = HashMap::new();
            bindings.insert("function_name".to_string(), symbol.name.clone());
            if has_route_annotation {
                bindings.insert("has_route_annotation".to_string(), "true".to_string());
            }
            if has_http_naming {
                bindings.insert("http_naming".to_string(), "true".to_string());
            }

            results.instances.push(MatchInstance {
                pattern_name: "http-endpoint".to_string(),
                symbol_id: symbol.id.clone(),
                symbol_name: symbol.name.clone(),
                file: symbol.file.clone(),
                bindings,
            });
        }
    }

    /// Match html-form pattern (html form with angular style)
    fn match_html_form(&self, facts: &Facts, _rule: &MatchRule, results: &mut MatchResults) {
        // Look for HTML files with form elements
        for file in &facts.files {
            if file.language != "html" {
                continue;
            }

            // Check file name for form-related patterns
            let file_lower = file.path.to_lowercase();
            let has_form_in_name = file_lower.contains("form");

            // Check if there are form-related symbols (custom elements or form tags)
            let form_symbols: Vec<&Symbol> = facts
                .symbols
                .iter()
                .filter(|s| {
                    s.file == file.path
                        && (s.name.to_lowercase() == "form"
                            || s.name.to_lowercase().contains("form")
                            || s.name.to_lowercase().contains("component"))
                })
                .collect();

            // If file name contains "form" or we found form-related symbols, create a match
            if has_form_in_name || !form_symbols.is_empty() {
                // Create a synthetic symbol ID for the form if no symbol exists
                let symbol_id = if !form_symbols.is_empty() {
                    form_symbols[0].id.clone()
                } else {
                    format!("form_{}", file.path.replace("/", "_").replace(".", "_"))
                };

                let symbol_name = if !form_symbols.is_empty() {
                    form_symbols[0].name.clone()
                } else {
                    "form".to_string()
                };

                let mut bindings = HashMap::new();
                bindings.insert("form_name".to_string(), symbol_name.clone());
                bindings.insert("file_path".to_string(), file.path.clone());
                if has_form_in_name {
                    bindings.insert("has_form_in_filename".to_string(), "true".to_string());
                }

                results.instances.push(MatchInstance {
                    pattern_name: "html-form".to_string(),
                    symbol_id,
                    symbol_name,
                    file: file.path.clone(),
                    bindings,
                });
            }
        }
    }

    /// Match typescript-component pattern (typescript angular component)
    fn match_typescript_component(
        &self,
        facts: &Facts,
        _rule: &MatchRule,
        results: &mut MatchResults,
    ) {
        for symbol in &facts.symbols {
            // Must be a type (class)
            if symbol.kind != SymbolKind::Type {
                continue;
            }

            // Check if file is TypeScript/TSX
            let file_lang = facts
                .files
                .iter()
                .find(|f| f.path == symbol.file)
                .map(|f| f.language.as_str());
            
            if let Some(lang) = file_lang {
                if lang != "typescript" && lang != "tsx" {
                    continue;
                }
            } else {
                continue;
            }

            // Check for component naming convention (ends with "Component" or similar)
            let name_lower = symbol.name.to_lowercase();
            if !name_lower.ends_with("component") && !name_lower.contains("component") {
                continue;
            }

            // Check for component decorator annotation or Angular-style patterns
            let has_component_annotation = facts
                .annotations
                .iter()
                .any(|a| a.symbol_id == symbol.id && (a.key == "Component" || a.key.contains("component")));

            // Get members to check for Angular component patterns
            let members: Vec<&Member> = facts
                .members
                .iter()
                .filter(|m| m.symbol_id == symbol.id)
                .collect();

            // Match found if it looks like a component
            if has_component_annotation || !members.is_empty() {
                let mut bindings = HashMap::new();
                bindings.insert("component_name".to_string(), symbol.name.clone());
                bindings.insert("member_count".to_string(), members.len().to_string());
                if has_component_annotation {
                    bindings.insert("has_component_decorator".to_string(), "true".to_string());
                }

                results.instances.push(MatchInstance {
                    pattern_name: "typescript-component".to_string(),
                    symbol_id: symbol.id.clone(),
                    symbol_name: symbol.name.clone(),
                    file: symbol.file.clone(),
                    bindings,
                });
            }
        }
    }
}

impl Default for PatternMatcher {
    fn default() -> Self {
        Self::new()
    }
}


