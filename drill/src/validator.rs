/// Round-trip validator module
/// Validates that mined packs can reproduce original source code

use crate::facts::Facts;
use crate::pack_loader::{LoadedPack, PackLoader};
use crate::matcher::PatternMatcher;
use crate::scanner::Scanner;
use crate::parser::ParserRegistry;
use crate::extractor::Extractor;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::collections::HashMap;
use walkdir::WalkDir;
use similar::{ChangeTag, TextDiff};

/// Validation result for a single pack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackValidationResult {
    /// Pack name
    pub pack_name: String,
    /// Pack path
    pub pack_path: String,
    /// Whether validation passed
    pub passed: bool,
    /// Number of instances validated
    pub instances_validated: usize,
    /// Number of instances that passed
    pub instances_passed: usize,
    /// Validation errors
    pub errors: Vec<ValidationError>,
}

/// Validation error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    /// Instance identifier (file path, symbol name, etc.)
    pub instance: String,
    /// Error message
    pub message: String,
    /// Diff between expected and actual (if applicable)
    pub diff: Option<String>,
}

/// Overall validation results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResults {
    /// Total packs validated
    pub total_packs: usize,
    /// Packs that passed
    pub packs_passed: usize,
    /// Packs that failed
    pub packs_failed: usize,
    /// Per-pack results
    pub pack_results: Vec<PackValidationResult>,
}

pub struct Validator {
    hix_path: PathBuf,
}

impl Validator {
    pub fn new(hix_path: Option<PathBuf>) -> Self {
        let hix_path = hix_path.unwrap_or_else(|| {
            // Default to "hix" in PATH
            PathBuf::from("hix")
        });
        
        Validator { hix_path }
    }

    /// Validate packs against a repository
    pub fn validate_packs(
        &self,
        packs_dir: &Path,
        repo_path: &Path,
    ) -> Result<ValidationResults> {
        // Load all packs (handle nested structure like mined/<lang>/<cluster-id>)
        let pack_loader = PackLoader::new();
        let mut loaded_packs = Vec::new();
        
        // Try loading from the directory directly first
        match pack_loader.load_packs(packs_dir) {
            Ok(packs) => {
                if !packs.is_empty() {
                    loaded_packs.extend(packs);
                } else {
                    // If no packs found, try recursive search
                    loaded_packs = self.find_packs_recursively(packs_dir)?;
                }
            }
            Err(_) => {
                // If that fails, try to find pack.json files recursively
                loaded_packs = self.find_packs_recursively(packs_dir)?;
            }
        }

        if loaded_packs.is_empty() {
            anyhow::bail!("No packs found in {:?}", packs_dir);
        }

        // Scan and parse repository
        let scanner = Scanner::new(repo_path);
        let files = scanner.scan(repo_path)
            .with_context(|| "Failed to scan repository")?;

        // Parse files (same approach as analyze command)
        let parser = ParserRegistry::new();
        let mut parse_results: HashMap<String, crate::parser::ParseResult> = HashMap::new();
        let mut file_contents: HashMap<String, String> = HashMap::new();

        for file in &files {
            if let Some(lang) = &file.language {
                let has_parser = matches!(lang.as_str(), "typescript" | "tsx" | "python" | "csharp" | "html");
                if has_parser {
                    let content = match fs::read_to_string(&file.path) {
                        Ok(c) => c,
                        Err(_) => continue,
                    };

                    file_contents.insert(file.path.clone(), content.clone());
                    let parse_result = parser.parse(&content, lang);
                    
                    if parse_result.tree.is_some() {
                        parse_results.insert(file.path.clone(), parse_result);
                    }
                }
            }
        }

        let extractor = Extractor::new();
        let mut facts = extractor.extract_facts(&files, &parse_results, &file_contents);
        facts.sort();

        // Match patterns for each pack
        let matcher = PatternMatcher::new();
        let mut pack_results: Vec<PackValidationResult> = Vec::new();

        for loaded_pack in &loaded_packs {
            let pack_result = self.validate_pack(
                &loaded_pack.path,
                &loaded_pack.pack,
                &facts,
                &matcher,
                repo_path,
            )?;
            pack_results.push(pack_result);
        }

        let packs_passed = pack_results.iter().filter(|r| r.passed).count();
        let packs_failed = pack_results.len() - packs_passed;

        Ok(ValidationResults {
            total_packs: pack_results.len(),
            packs_passed,
            packs_failed,
            pack_results,
        })
    }

    /// Validate a single pack
    fn validate_pack(
        &self,
        pack_path: &Path,
        pack: &crate::pack::PatternPack,
        facts: &Facts,
        matcher: &PatternMatcher,
        repo_path: &Path,
    ) -> Result<PackValidationResult> {
        let pack_name = pack.metadata.name.clone();
        let mut errors: Vec<ValidationError> = Vec::new();
        let mut instances_validated = 0;
        let mut instances_passed = 0;

        // Match patterns from this pack
        match matcher.match_patterns(facts, &pack.patterns) {
            Ok(match_results) => {
                // For each match instance, validate it
                for instance in &match_results.instances {
                    instances_validated += 1;
                    
                    // Find the template and model for this instance
                    // For v1, we'll look for templates in the pack directory
                    match self.validate_instance(
                        pack_path,
                        instance,
                        repo_path,
                    ) {
                        Ok((passed, diff)) => {
                            if passed {
                                instances_passed += 1;
                            } else {
                                errors.push(ValidationError {
                                    instance: format!("{} ({})", instance.symbol_name, instance.file),
                                    message: "Rendered output does not match original source".to_string(),
                                    diff: Some(diff),
                                });
                            }
                        }
                        Err(e) => {
                            errors.push(ValidationError {
                                instance: format!("{:?}", instance),
                                message: format!("Validation error: {}", e),
                                diff: None,
                            });
                        }
                    }
                }
            }
            Err(e) => {
                errors.push(ValidationError {
                    instance: "pack".to_string(),
                    message: format!("Failed to match patterns: {}", e),
                    diff: None,
                });
            }
        }

        let passed = errors.is_empty() && instances_validated > 0 && instances_passed == instances_validated;

        Ok(PackValidationResult {
            pack_name,
            pack_path: pack_path.to_string_lossy().to_string(),
            passed,
            instances_validated,
            instances_passed,
            errors,
        })
    }

    /// Validate a single instance
    /// Returns (passed, diff) where diff is empty if passed, or contains diff string if failed
    fn validate_instance(
        &self,
        pack_path: &Path,
        instance: &crate::matcher::MatchInstance,
        repo_path: &Path,
    ) -> Result<(bool, String)> {
        // Find template file in pack
        let templates_dir = pack_path.join("templates");
        let template_path = self.find_template_file(&templates_dir)
            .ok_or_else(|| anyhow::anyhow!("No template file found in pack"))?;

        // Find model.json (should be in synthesis directory or pack)
        // For v1, we'll look in the synthesis directory based on cluster_id
        // This is a simplified approach - in a real implementation, we'd store model.json in the pack
        let model_path = self.find_model_file(pack_path, instance)
            .ok_or_else(|| anyhow::anyhow!("No model.json found for instance"))?;

        // Render template using hix binary
        let rendered = self.render_template(&template_path, &model_path)?;

        // Get original source file
        let original_path = if Path::new(&instance.file).is_absolute() {
            PathBuf::from(&instance.file)
        } else {
            repo_path.join(&instance.file)
        };

        let original = fs::read_to_string(&original_path)
            .with_context(|| format!("Failed to read original file: {:?}", original_path))?;

        // Compare (strict mode for v1)
        // Normalize line endings for comparison
        let rendered_normalized = rendered.replace("\r\n", "\n").trim().to_string();
        let original_normalized = original.replace("\r\n", "\n").trim().to_string();

        if rendered_normalized == original_normalized {
            Ok((true, String::new()))
        } else {
            // Generate diff
            let diff = self.generate_diff(&original_normalized, &rendered_normalized);
            Ok((false, diff))
        }
    }

    /// Find template file in pack directory
    fn find_template_file(&self, templates_dir: &Path) -> Option<PathBuf> {
        if !templates_dir.exists() {
            return None;
        }

        // Look for .hix files recursively
        for entry in fs::read_dir(templates_dir).ok()? {
            let entry = entry.ok()?;
            let path = entry.path();
            
            if path.is_file() && path.extension() == Some(std::ffi::OsStr::new("hix")) {
                return Some(path);
            }
            
            if path.is_dir() {
                if let Some(found) = self.find_template_file(&path) {
                    return Some(found);
                }
            }
        }
        
        None
    }

    /// Find model.json file
    fn find_model_file(&self, pack_path: &Path, _instance: &crate::matcher::MatchInstance) -> Option<PathBuf> {
        // For v1, look for model.json in the synthesis directory
        // The pack path structure is: .hix/drill/packs/mined/<lang>/<cluster-id>
        // The synthesis path should be: .hix/drill/synthesis/<cluster-id>/model.json
        
        if let Some(cluster_id) = pack_path.file_name() {
            if let Some(repo_root) = pack_path.ancestors().find(|p| p.ends_with(".hix/drill/packs")) {
                if let Some(drill_dir) = repo_root.parent() {
                    let synthesis_dir = drill_dir.join("synthesis");
                    let model_path = synthesis_dir.join(cluster_id).join("model.json");
                    if model_path.exists() {
                        return Some(model_path);
                    }
                }
            }
        }
        
        None
    }

    /// Render template using hix binary
    fn render_template(&self, template_path: &Path, model_path: &Path) -> Result<String> {
        let output = Command::new(&self.hix_path)
            .args(&[
                "generate",
                "--model",
                model_path.to_str().ok_or_else(|| anyhow::anyhow!("Invalid model path"))?,
                "--template",
                template_path.to_str().ok_or_else(|| anyhow::anyhow!("Invalid template path"))?,
            ])
            .output()
            .with_context(|| format!("Failed to execute hix binary: {:?}", self.hix_path))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Hix rendering failed: {}", stderr);
        }

        let stdout = String::from_utf8(output.stdout)
            .with_context(|| "Failed to decode hix output")?;

        Ok(stdout)
    }

    /// Find packs recursively in nested directory structure
    fn find_packs_recursively(&self, packs_dir: &Path) -> Result<Vec<LoadedPack>> {
        let mut loaded_packs = Vec::new();
        let pack_loader = PackLoader::new();
        
        // Walk directory tree looking for pack.json files
        for entry in WalkDir::new(packs_dir) {
            let entry = entry?;
            let path = entry.path();
            
            if path.is_file() && path.file_name() == Some(std::ffi::OsStr::new("pack.json")) {
                // Found a pack.json, try to load the pack from its directory
                if let Some(pack_dir) = path.parent() {
                    match pack_loader.load_pack(pack_dir) {
                        Ok(pack) => {
                            loaded_packs.push(LoadedPack {
                                pack,
                                path: pack_dir.to_path_buf(),
                            });
                        }
                        Err(e) => {
                            eprintln!("Warning: Failed to load pack from {:?}: {}", pack_dir, e);
                        }
                    }
                }
            }
        }
        
        Ok(loaded_packs)
    }

    /// Generate a unified diff between original and rendered text
    fn generate_diff(&self, original: &str, rendered: &str) -> String {
        let diff = TextDiff::from_lines(original, rendered);
        
        let mut diff_output = String::new();
        diff_output.push_str("--- Original (expected)\n");
        diff_output.push_str("+++ Rendered (actual)\n");
        diff_output.push_str("@@\n");
        
        for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
            if idx > 0 {
                diff_output.push_str("...\n");
            }
            
            for op in group {
                for change in diff.iter_changes(op) {
                    let sign = match change.tag() {
                        ChangeTag::Delete => "-",
                        ChangeTag::Insert => "+",
                        ChangeTag::Equal => " ",
                    };
                    
                    // Show line number and change
                    let old_line = change.old_index().map(|i| i + 1).unwrap_or(0);
                    let new_line = change.new_index().map(|i| i + 1).unwrap_or(0);
                    
                    match change.tag() {
                        ChangeTag::Delete => {
                            diff_output.push_str(&format!(
                                "{}{} |{}| {}\n",
                                sign,
                                old_line,
                                new_line,
                                change.value().trim_end()
                            ));
                        }
                        ChangeTag::Insert => {
                            diff_output.push_str(&format!(
                                "{}{} |{}| {}\n",
                                sign,
                                old_line,
                                new_line,
                                change.value().trim_end()
                            ));
                        }
                        ChangeTag::Equal => {
                            // Optionally show context lines (first 3 and last 3)
                            if idx < 3 || idx >= diff.grouped_ops(3).len() - 3 {
                                diff_output.push_str(&format!(
                                    "{}{} |{}| {}\n",
                                    sign,
                                    old_line,
                                    new_line,
                                    change.value().trim_end()
                                ));
                            }
                        }
                    }
                }
            }
        }
        
        // If the diff is too long, truncate it
        let line_count = diff_output.lines().count();
        if line_count > 50 {
            let lines: Vec<&str> = diff_output.lines().take(50).collect();
            diff_output = lines.join("\n");
            diff_output.push_str(&format!("\n... (diff truncated, showing first 50 of {} lines)", line_count));
        }
        
        diff_output
    }
}

impl Default for Validator {
    fn default() -> Self {
        Self::new(None)
    }
}

