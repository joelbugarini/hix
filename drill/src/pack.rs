use serde::{Deserialize, Serialize};

/// Pack metadata schema version
pub const PACK_SCHEMA_VERSION: &str = "1.0.0";

/// Pattern Pack metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackMetadata {
    /// Schema version (e.g., "1.0.0")
    pub schema_version: String,
    /// Pack name/identifier
    pub name: String,
    /// Pack version
    pub version: String,
    /// Pack description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Pack author
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<String>,
}

/// Pattern match rule
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PatternRule {
    /// Rule name/identifier
    pub name: String,
    /// Rule description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Match conditions (to be implemented in Story 6)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub match_conditions: Option<serde_json::Value>,
}

/// Pattern Pack definition
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PatternPack {
    /// Pack metadata
    pub metadata: PackMetadata,
    /// Pattern rules
    pub patterns: Vec<PatternRule>,
}

impl PatternPack {
    /// Validate the pack schema version
    pub fn validate_schema_version(&self) -> Result<(), String> {
        if self.metadata.schema_version != PACK_SCHEMA_VERSION {
            return Err(format!(
                "Unsupported schema version: {}. Expected: {}",
                self.metadata.schema_version, PACK_SCHEMA_VERSION
            ));
        }
        Ok(())
    }

    /// Validate the pack structure
    pub fn validate(&self) -> Result<(), String> {
        // Validate schema version
        self.validate_schema_version()?;

        // Validate required fields
        if self.metadata.name.is_empty() {
            return Err("Pack name cannot be empty".to_string());
        }

        if self.metadata.version.is_empty() {
            return Err("Pack version cannot be empty".to_string());
        }

        // Validate pattern rules
        for (idx, pattern) in self.patterns.iter().enumerate() {
            if pattern.name.is_empty() {
                return Err(format!("Pattern rule {} has empty name", idx));
            }
        }

        Ok(())
    }
}

