use crate::pack::{PatternPack, PACK_SCHEMA_VERSION};
use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};

pub struct PackLoader;

#[derive(Debug, Clone)]
pub struct LoadedPack {
    pub pack: PatternPack,
    pub path: PathBuf,
}

impl PackLoader {
    pub fn new() -> Self {
        PackLoader
    }

    /// Load all packs from a directory
    pub fn load_packs(&self, packs_dir: &Path) -> Result<Vec<LoadedPack>> {
        if !packs_dir.exists() {
            anyhow::bail!("Packs directory does not exist: {:?}", packs_dir);
        }

        if !packs_dir.is_dir() {
            anyhow::bail!("Packs path is not a directory: {:?}", packs_dir);
        }

        let mut loaded_packs = Vec::new();

        // Iterate through subdirectories (each subdirectory is a pack)
        for entry in fs::read_dir(packs_dir)
            .with_context(|| format!("Failed to read packs directory: {:?}", packs_dir))?
        {
            let entry = entry.with_context(|| "Failed to read directory entry")?;
            let pack_path = entry.path();

            if pack_path.is_dir() {
                match self.load_pack(&pack_path) {
                    Ok(pack) => {
                        loaded_packs.push(LoadedPack {
                            pack,
                            path: pack_path,
                        });
                    }
                    Err(e) => {
                        eprintln!("Warning: Failed to load pack from {:?}: {}", pack_path, e);
                        // Continue loading other packs
                    }
                }
            }
        }

        // Sort packs by name for deterministic output
        loaded_packs.sort_by(|a, b| a.pack.metadata.name.cmp(&b.pack.metadata.name));

        Ok(loaded_packs)
    }

    /// Load a single pack from a directory
    fn load_pack(&self, pack_dir: &Path) -> Result<PatternPack> {
        let pack_json_path = pack_dir.join("pack.json");
        let pattern_json_path = pack_dir.join("pattern.json");

        // Load pack metadata
        let pack_content = fs::read_to_string(&pack_json_path)
            .with_context(|| format!("Failed to read pack.json from {:?}", pack_dir))?;

        let metadata: crate::pack::PackMetadata = serde_json::from_str(&pack_content)
            .with_context(|| format!("Failed to parse pack.json from {:?}", pack_dir))?;

        // Validate schema version
        if metadata.schema_version != PACK_SCHEMA_VERSION {
            anyhow::bail!(
                "Unsupported schema version: {}. Expected: {}",
                metadata.schema_version,
                PACK_SCHEMA_VERSION
            );
        }

        // Load pattern rules
        let pattern_content = fs::read_to_string(&pattern_json_path)
            .with_context(|| format!("Failed to read pattern.json from {:?}", pack_dir))?;

        let patterns: Vec<crate::pack::PatternRule> = serde_json::from_str(&pattern_content)
            .with_context(|| format!("Failed to parse pattern.json from {:?}", pack_dir))?;

        let pack = PatternPack { metadata, patterns };

        // Validate the pack
        pack.validate()
            .map_err(|e| anyhow::anyhow!("Pack validation failed: {}", e))?;

        Ok(pack)
    }
}

impl Default for PackLoader {
    fn default() -> Self {
        Self::new()
    }
}

