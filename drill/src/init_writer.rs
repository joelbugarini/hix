use crate::matcher::{MatchInstance, MatchResults};
use crate::pack_loader::LoadedPack;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Drill project configuration schema v1
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DrillProjectConfig {
    /// Pattern packs used in analysis
    pub packs_used: Vec<PackInfo>,
    /// Pattern mappings (instances → models/templates)
    pub pattern_mappings: Vec<PatternMapping>,
    /// Reference to hix's main config file
    pub hix_config: String,
}

/// Pack information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackInfo {
    /// Pack name
    pub name: String,
    /// Pack version
    pub version: String,
    /// Pack path (relative to project root)
    pub path: String,
}

/// Pattern mapping (pattern → instances)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternMapping {
    /// Pattern name
    pub pattern: String,
    /// Instances of this pattern
    pub instances: Vec<PatternInstance>,
}

/// Pattern instance mapping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternInstance {
    /// Symbol ID from facts
    pub symbol_id: String,
    /// Symbol name
    pub symbol_name: String,
    /// Source file path (relative to project root)
    pub source_file: String,
    /// Model file path (relative to project root, placeholder)
    pub model_file: String,
    /// Template file path (relative to project root, placeholder)
    pub template_file: String,
}

pub struct InitWriter;

impl InitWriter {
    pub fn new() -> Self {
        InitWriter
    }

    /// Generate drill project config from match results and loaded packs
    pub fn generate_config(
        &self,
        match_results: &MatchResults,
        loaded_packs: &[LoadedPack],
        project_root: &str,
        packs_dir: &str,
    ) -> DrillProjectConfig {
        // Build packs_used list
        let project_root_path = std::path::Path::new(project_root);
        let packs_dir_path = std::path::Path::new(packs_dir);
        
        let packs_used: Vec<PackInfo> = loaded_packs
            .iter()
            .map(|lp| {
                // Calculate relative path from project root to pack directory
                // Use pathdiff-like logic: count how many ../ needed
                let pack_path = &lp.path;
                
                // Get the pack directory name (last component of the path)
                let pack_dir_name = pack_path.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("pack");
                
                // Get the packs directory name
                let packs_dir_name = packs_dir_path.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("packs");
                
                // Simple heuristic: if packs_dir is a sibling of project_root, use ../
                // Otherwise, try to find common ancestor
                let relative_path = if packs_dir_path.parent() == project_root_path.parent() {
                    // Sibling directories
                    format!("../{}/{}", packs_dir_name, pack_dir_name)
                } else if packs_dir_path.starts_with(project_root_path) {
                    // Packs are inside project root
                    format!("./{}", packs_dir_path.strip_prefix(project_root_path)
                        .unwrap_or(packs_dir_path)
                        .join(pack_dir_name)
                        .to_string_lossy()
                        .replace('\\', "/"))
                } else {
                    // Fallback: use relative path from project root
                    // Count directory depth difference
                    let project_depth = project_root_path.components().count();
                    let packs_depth = packs_dir_path.components().count();
                    let depth_diff = packs_depth.saturating_sub(project_depth);
                    
                    if depth_diff > 0 {
                        format!("{}/{}/{}", "../".repeat(depth_diff), packs_dir_name, pack_dir_name)
                    } else {
                        format!("../{}/{}", packs_dir_name, pack_dir_name)
                    }
                };

                PackInfo {
                    name: lp.pack.metadata.name.clone(),
                    version: lp.pack.metadata.version.clone(),
                    path: relative_path,
                }
            })
            .collect();

        // Group matches by pattern
        let mut pattern_map: HashMap<String, Vec<&MatchInstance>> = HashMap::new();
        for instance in &match_results.instances {
            pattern_map
                .entry(instance.pattern_name.clone())
                .or_insert_with(Vec::new)
                .push(instance);
        }

        // Convert to pattern mappings
        let mut pattern_mappings: Vec<PatternMapping> = pattern_map
            .into_iter()
            .map(|(pattern_name, instances)| {
                let pattern_instances: Vec<PatternInstance> = instances
                    .iter()
                    .map(|instance| {
                        // Convert file paths to relative paths from project root
                        // The instance.file contains path like "./test_init_log/samples/Person.cs"
                        // We need to extract just "Person.cs" relative to project root
                        let source_file_path = std::path::Path::new(&instance.file);
                        let source_file = {
                            // Get the project root directory name (last component)
                            let project_root_name = project_root_path.file_name()
                                .and_then(|n| n.to_str())
                                .unwrap_or("");
                            
                            // Try to find project root name in the file path
                            let file_str = instance.file.replace('\\', "/");
                            
                            // Strategy 1: Try strip_prefix with canonical paths
                            let project_root_canonical = project_root_path.canonicalize()
                                .unwrap_or_else(|_| project_root_path.to_path_buf());
                            let file_canonical = source_file_path.canonicalize()
                                .unwrap_or_else(|_| source_file_path.to_path_buf());
                            
                            if let Ok(rel) = file_canonical.strip_prefix(&project_root_canonical) {
                                // Successfully stripped - this is the cleanest approach
                                format!("./{}", rel.to_string_lossy().replace('\\', "/"))
                            } else if let Some(idx) = file_str.find(&format!("{}/", project_root_name)) {
                                // Found project root name in path - extract everything after it
                                let after_root = &file_str[idx + project_root_name.len() + 1..];
                                format!("./{}", after_root)
                            } else if let Some(idx) = file_str.rfind('/') {
                                // Just use the filename (last component)
                                format!("./{}", &file_str[idx + 1..])
                            } else {
                                // Fallback: use the whole path
                                if file_str.starts_with("./") {
                                    file_str
                                } else {
                                    format!("./{}", file_str)
                                }
                            }
                        };

                        // Generate placeholder paths for model and template
                        // These are placeholders that can be customized later
                        let model_file = format!(
                            ".hix/models/{}.json",
                            instance.symbol_name
                        );
                        let template_file = format!(
                            ".hix/templates/{}.hix",
                            pattern_name
                        );

                        PatternInstance {
                            symbol_id: instance.symbol_id.clone(),
                            symbol_name: instance.symbol_name.clone(),
                            source_file,
                            model_file,
                            template_file,
                        }
                    })
                    .collect();

                PatternMapping {
                    pattern: pattern_name,
                    instances: pattern_instances,
                }
            })
            .collect();

        // Sort for deterministic output
        pattern_mappings.sort_by(|a, b| a.pattern.cmp(&b.pattern));
        for mapping in &mut pattern_mappings {
            mapping.instances.sort_by(|a, b| a.symbol_name.cmp(&b.symbol_name));
        }

        DrillProjectConfig {
            packs_used,
            pattern_mappings,
            hix_config: ".hix/config.yaml".to_string(),
        }
    }
}

impl Default for InitWriter {
    fn default() -> Self {
        Self::new()
    }
}

