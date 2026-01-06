/// Pack emission module
/// Creates reusable Pattern Packs from synthesized templates and clusters

use crate::facts::Facts;
use crate::model_inference::InferredModel;
use crate::pack::{PackMetadata, PatternRule, PACK_SCHEMA_VERSION};
use crate::template_synthesis::SynthesisMetadata;
use crate::unknown_discovery::SymbolCluster;
use anyhow::{Context, Result};
use serde_json::json;
use std::fs;
use std::path::{Path, PathBuf};

pub struct PackEmitter;

impl PackEmitter {
    pub fn new() -> Self {
        PackEmitter
    }

    /// Emit a Pattern Pack from synthesized artifacts
    pub fn emit_pack(
        &self,
        synthesis: &SynthesisMetadata,
        cluster: &SymbolCluster,
        model: &InferredModel,
        facts: &Facts,
        project_root: &Path,
        output_dir: &Path,
    ) -> Result<PathBuf> {
        // Determine pack name: mined/<language>/<cluster-id>
        let pack_name = format!("mined/{}/{}", synthesis.language, cluster.cluster_id);
        let pack_dir = output_dir.join(&pack_name);

        // Create pack directory structure
        fs::create_dir_all(&pack_dir)
            .with_context(|| format!("Failed to create pack directory: {:?}", pack_dir))?;

        // 1. Generate pack.json
        let pack_metadata = self.generate_pack_metadata(synthesis, cluster, model);
        let pack_json_path = pack_dir.join("pack.json");
        let pack_json = serde_json::to_string_pretty(&pack_metadata)
            .with_context(|| "Failed to serialize pack.json")?;
        fs::write(&pack_json_path, pack_json)
            .with_context(|| format!("Failed to write pack.json to {:?}", pack_json_path))?;

        // 2. Generate pattern.json
        let pattern_rule = self.generate_pattern_rule(cluster, facts, &synthesis.language);
        let pattern_json_path = pack_dir.join("pattern.json");
        let pattern_json = serde_json::to_string_pretty(&vec![pattern_rule])
            .with_context(|| "Failed to serialize pattern.json")?;
        fs::write(&pattern_json_path, pattern_json)
            .with_context(|| format!("Failed to write pattern.json to {:?}", pattern_json_path))?;

        // 3. Copy templates from synthesis
        let synthesis_template_dir = project_root
            .join(".hix")
            .join("drill")
            .join("synthesis")
            .join(&cluster.cluster_id)
            .join("templates")
            .join(&synthesis.language);
        
        if synthesis_template_dir.exists() {
            let pack_templates_dir = pack_dir.join("templates").join(&synthesis.language);
            fs::create_dir_all(&pack_templates_dir)
                .with_context(|| "Failed to create templates directory")?;
            
            // Copy all .hix files
            for entry in fs::read_dir(&synthesis_template_dir)
                .with_context(|| "Failed to read synthesis templates directory")?
            {
                let entry = entry?;
                let src_path = entry.path();
                if src_path.is_file() && src_path.extension() == Some(std::ffi::OsStr::new("hix")) {
                    let dst_path = pack_templates_dir.join(src_path.file_name().unwrap());
                    fs::copy(&src_path, &dst_path)
                        .with_context(|| format!("Failed to copy template from {:?} to {:?}", src_path, dst_path))?;
                }
            }
        }

        // 4. Copy sample files to tests/fixtures
        let fixtures_dir = pack_dir.join("tests").join("fixtures");
        fs::create_dir_all(&fixtures_dir)
            .with_context(|| "Failed to create fixtures directory")?;

        for sample_file in &synthesis.sample_files {
            // Try multiple path resolution strategies
            let src_path = if Path::new(sample_file).is_absolute() {
                PathBuf::from(sample_file)
            } else {
                // First try: path as-is (might already be relative to project_root)
                let direct_path = project_root.join(sample_file);
                if direct_path.exists() {
                    direct_path
                } else {
                    // Second try: path relative to current directory
                    PathBuf::from(sample_file)
                }
            };

            // Also try stripping project_root prefix if sample_file contains it
            let src_path = if !src_path.exists() {
                let sample_file_str = sample_file.replace('\\', "/");
                if let Some(stripped) = sample_file_str.strip_prefix(&project_root.to_string_lossy().replace('\\', "/")) {
                    let stripped_path = stripped.trim_start_matches('/');
                    project_root.join(stripped_path)
                } else {
                    src_path
                }
            } else {
                src_path
            };

            if src_path.exists() && src_path.is_file() {
                // Create relative path structure in fixtures
                let file_name = src_path.file_name()
                    .and_then(|n| n.to_str())
                    .ok_or_else(|| anyhow::anyhow!("Invalid file name: {:?}", src_path))?;
                
                let dst_path = fixtures_dir.join(file_name);
                fs::copy(&src_path, &dst_path)
                    .with_context(|| format!("Failed to copy sample file from {:?} to {:?}", src_path, dst_path))?;
            }
        }

        // 5. Create tests/expected directory (empty for now, can be populated later)
        let expected_dir = pack_dir.join("tests").join("expected");
        fs::create_dir_all(&expected_dir)
            .with_context(|| "Failed to create expected directory")?;

        Ok(pack_dir)
    }

    /// Generate pack metadata
    fn generate_pack_metadata(
        &self,
        synthesis: &SynthesisMetadata,
        cluster: &SymbolCluster,
        _model: &InferredModel,
    ) -> PackMetadata {
        let description = format!(
            "Mined pattern from {} cluster ({} samples). Generated from {}.",
            cluster.cluster_id,
            cluster.size,
            synthesis.base_sample_file
        );

        PackMetadata {
            schema_version: PACK_SCHEMA_VERSION.to_string(),
            name: format!("mined-{}-{}", synthesis.language, cluster.cluster_id),
            version: "1.0.0".to_string(),
            description: Some(description),
            author: Some("hix-drill".to_string()),
        }
    }

    /// Generate pattern rule from cluster fingerprint
    fn generate_pattern_rule(
        &self,
        cluster: &SymbolCluster,
        facts: &Facts,
        language: &str,
    ) -> PatternRule {
        // Use the fingerprint's kind, which is more accurate
        let mut symbol_kind = cluster.fingerprint.kind.clone();
        
        // If cluster symbols are variables (properties), check if they're members of a type
        // If so, we want to match the type, not the variables
        if symbol_kind == "variable" {
            // Check if any of the cluster symbols are members of a type
            for symbol_id in &cluster.symbol_ids {
                if let Some(symbol) = facts.symbols.iter().find(|s| s.id == *symbol_id) {
                    // Check if this symbol is a member of another symbol (a type)
                    if let Some(member) = facts.members.iter().find(|m| m.name == symbol.name) {
                        // Find the containing symbol
                        if let Some(containing_symbol) = facts.symbols.iter().find(|s| s.id == member.symbol_id) {
                            if matches!(containing_symbol.kind, crate::facts::SymbolKind::Type) {
                                symbol_kind = "type".to_string();
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Analyze member structure from cluster samples
        let mut member_predicates = serde_json::Map::new();
        
        // Count members for samples in cluster
        let mut member_counts: Vec<usize> = Vec::new();
        for symbol_id in &cluster.symbol_ids {
            let member_count = facts.members.iter()
                .filter(|m| m.symbol_id == *symbol_id)
                .count();
            member_counts.push(member_count);
        }

        if !member_counts.is_empty() {
            let min_members = *member_counts.iter().min().unwrap();
            let max_members = *member_counts.iter().max().unwrap();
            
            if min_members > 0 {
                member_predicates.insert("min_fields".to_string(), json!(min_members));
            }
            
            // If all samples have same member count, set max
            if min_members == max_members {
                member_predicates.insert("max_fields".to_string(), json!(max_members));
            }
        }

        // Build match conditions
        let mut match_conditions = serde_json::Map::new();
        match_conditions.insert("symbol_kind".to_string(), json!(symbol_kind));
        
        if !member_predicates.is_empty() {
            match_conditions.insert("member_predicates".to_string(), json!(member_predicates));
        }
        
        match_conditions.insert("language".to_string(), json!(vec![language]));

        let description = format!(
            "Mined pattern from cluster {} ({} symbols, fingerprint: {})",
            cluster.cluster_id,
            cluster.size,
            cluster.fingerprint.shape_hash
        );

        PatternRule {
            name: format!("mined-{}", cluster.cluster_id),
            description: Some(description),
            match_conditions: Some(json!(match_conditions)),
        }
    }
}

impl Default for PackEmitter {
    fn default() -> Self {
        Self::new()
    }
}

