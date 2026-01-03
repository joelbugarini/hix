use crate::facts::{Facts, Symbol, Member};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use sha2::{Digest, Sha256};

/// Unknown discovery and clustering module
/// Identifies unmatched symbols and groups them by structural similarity

/// Structural fingerprint for a symbol
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct StructuralFingerprint {
    /// Normalized symbol kind
    pub kind: String,
    /// Normalized member structure (count and types)
    pub member_signature: String,
    /// Hash of structural shape
    pub shape_hash: String,
}

/// Cluster of similar symbols
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolCluster {
    /// Cluster ID
    pub cluster_id: String,
    /// Structural fingerprint for this cluster
    pub fingerprint: StructuralFingerprint,
    /// Number of symbols in this cluster
    pub size: usize,
    /// Symbol IDs in this cluster
    pub symbol_ids: Vec<String>,
    /// Sample symbols (first 5)
    pub samples: Vec<ClusterSample>,
    /// Rank score (higher = more important)
    pub rank_score: f64,
}

/// Sample symbol in a cluster
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClusterSample {
    /// Symbol ID
    pub symbol_id: String,
    /// Symbol name
    pub symbol_name: String,
    /// File path
    pub file: String,
    /// Symbol kind
    pub symbol_kind: String,
}

/// Unknown discovery results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnknownDiscovery {
    /// Total unmatched symbols
    pub total_unmatched: usize,
    /// Clusters of similar symbols
    pub clusters: Vec<SymbolCluster>,
    /// Unmatched symbols by kind (for backward compatibility)
    pub by_kind: HashMap<String, usize>,
    /// Sample unmatched symbols (not in clusters)
    pub samples: Vec<UnknownSample>,
}

/// Sample unknown symbol (for backward compatibility)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnknownSample {
    /// Symbol name
    pub symbol_name: String,
    /// Symbol kind
    pub symbol_kind: String,
    /// File path
    pub file: String,
    /// Symbol ID
    pub symbol_id: String,
}

pub struct UnknownDiscoverer;

impl UnknownDiscoverer {
    pub fn new() -> Self {
        UnknownDiscoverer
    }

    /// Discover and cluster unknown symbols
    pub fn discover_unknowns(
        &self,
        facts: &Facts,
        matched_symbol_ids: &HashSet<String>,
    ) -> UnknownDiscovery {
        // Find unmatched symbols
        let unmatched_symbols: Vec<&Symbol> = facts
            .symbols
            .iter()
            .filter(|s| !matched_symbol_ids.contains(&s.id))
            .collect();

        let total_unmatched = unmatched_symbols.len();

        // Group by kind for backward compatibility
        let mut by_kind: HashMap<String, usize> = HashMap::new();
        for symbol in &unmatched_symbols {
            let kind_str = format!("{:?}", symbol.kind).to_lowercase();
            *by_kind.entry(kind_str).or_insert(0) += 1;
        }

        // Compute fingerprints for all unmatched symbols
        let mut fingerprints: Vec<(StructuralFingerprint, &Symbol)> = Vec::new();
        for symbol in &unmatched_symbols {
            let fingerprint = self.compute_fingerprint(symbol, facts);
            fingerprints.push((fingerprint, symbol));
        }

        // Cluster by fingerprint
        let clusters = self.cluster_by_fingerprint(&fingerprints, facts);

        // Get samples for symbols not in clusters
        let mut samples: Vec<UnknownSample> = Vec::new();
        let clustered_symbol_ids: HashSet<String> = clusters
            .iter()
            .flat_map(|c| c.symbol_ids.iter().cloned())
            .collect();
        
        for symbol in &unmatched_symbols {
            if !clustered_symbol_ids.contains(&symbol.id) && samples.len() < 10 {
                samples.push(UnknownSample {
                    symbol_name: symbol.name.clone(),
                    symbol_kind: format!("{:?}", symbol.kind).to_lowercase(),
                    file: symbol.file.clone(),
                    symbol_id: symbol.id.clone(),
                });
            }
        }

        UnknownDiscovery {
            total_unmatched,
            clusters,
            by_kind,
            samples,
        }
    }

    /// Compute structural fingerprint for a symbol
    fn compute_fingerprint(&self, symbol: &Symbol, facts: &Facts) -> StructuralFingerprint {
        // Get members for this symbol
        let members: Vec<&Member> = facts
            .members
            .iter()
            .filter(|m| m.symbol_id == symbol.id)
            .collect();

        // Normalize member structure
        let member_signature = self.normalize_member_signature(&members);

        // Compute shape hash
        let shape_hash = self.compute_shape_hash(symbol, &members);

        StructuralFingerprint {
            kind: format!("{:?}", symbol.kind).to_lowercase(),
            member_signature,
            shape_hash,
        }
    }

    /// Normalize member signature (structure without names)
    fn normalize_member_signature(&self, members: &[&Member]) -> String {
        // Group by kind and count
        let mut by_kind: HashMap<String, usize> = HashMap::new();
        for member in members {
            let kind_str = format!("{:?}", member.kind).to_lowercase();
            *by_kind.entry(kind_str).or_insert(0) += 1;
        }

        // Create signature: "field:3,method:2" format
        let mut parts: Vec<String> = by_kind
            .iter()
            .map(|(k, v)| format!("{}:{}", k, v))
            .collect();
        parts.sort(); // Ensure deterministic ordering

        parts.join(",")
    }

    /// Compute hash of structural shape
    fn compute_shape_hash(&self, symbol: &Symbol, members: &[&Member]) -> String {
        let mut hasher = Sha256::new();
        
        // Include symbol kind
        hasher.update(format!("{:?}", symbol.kind).as_bytes());
        
        // Include member structure (kind and type, but not names)
        let mut member_info: Vec<String> = members
            .iter()
            .map(|m| {
                let type_info = m.type_ref.as_ref()
                    .map(|t| self.normalize_type(t))
                    .unwrap_or_else(|| "?".to_string());
                format!("{:?}:{}", m.kind, type_info)
            })
            .collect();
        member_info.sort(); // Deterministic ordering
        hasher.update(member_info.join("|").as_bytes());

        // Get hex digest
        format!("{:x}", hasher.finalize())
    }

    /// Normalize type reference (remove generic parameters, normalize casing)
    fn normalize_type(&self, type_ref: &str) -> String {
        // Remove generic parameters for now (e.g., "List<String>" -> "List")
        // This is a simple normalization - can be enhanced later
        if let Some(open_bracket) = type_ref.find('<') {
            type_ref[..open_bracket].to_string()
        } else {
            type_ref.to_string()
        }
    }

    /// Cluster symbols by fingerprint similarity
    fn cluster_by_fingerprint(
        &self,
        fingerprints: &[(StructuralFingerprint, &Symbol)],
        facts: &Facts,
    ) -> Vec<SymbolCluster> {
        // Group by exact fingerprint match
        let mut fingerprint_groups: HashMap<StructuralFingerprint, Vec<&Symbol>> = HashMap::new();
        
        for (fingerprint, symbol) in fingerprints {
            fingerprint_groups
                .entry(fingerprint.clone())
                .or_insert_with(Vec::new)
                .push(symbol);
        }

        // Create clusters from groups with >1 member
        let mut clusters: Vec<SymbolCluster> = Vec::new();
        let mut cluster_id_counter = 0;

        for (fingerprint, symbols) in fingerprint_groups {
            if symbols.len() > 1 {
                // Only cluster groups with multiple members
                let symbol_ids: Vec<String> = symbols.iter().map(|s| s.id.clone()).collect();
                
                // Get samples (first 5)
                let samples: Vec<ClusterSample> = symbols
                    .iter()
                    .take(5)
                    .map(|s| ClusterSample {
                        symbol_id: s.id.clone(),
                        symbol_name: s.name.clone(),
                        file: s.file.clone(),
                        symbol_kind: format!("{:?}", s.kind).to_lowercase(),
                    })
                    .collect();

                // Calculate rank score (frequency * complexity)
                let frequency = symbols.len() as f64;
                let complexity = self.calculate_complexity(&symbols[0], facts);
                let rank_score = frequency * complexity;

                clusters.push(SymbolCluster {
                    cluster_id: format!("cluster_{}", cluster_id_counter),
                    fingerprint: fingerprint.clone(),
                    size: symbols.len(),
                    symbol_ids,
                    samples,
                    rank_score,
                });
                cluster_id_counter += 1;
            }
        }

        // Sort clusters by rank score (descending)
        clusters.sort_by(|a, b| b.rank_score.partial_cmp(&a.rank_score).unwrap_or(std::cmp::Ordering::Equal));

        clusters
    }

    /// Calculate complexity score for a symbol (based on member count)
    fn calculate_complexity(&self, symbol: &Symbol, facts: &Facts) -> f64 {
        let member_count = facts
            .members
            .iter()
            .filter(|m| m.symbol_id == symbol.id)
            .count();
        
        // Simple complexity: log of member count + 1
        // This gives higher scores to symbols with more members
        (member_count as f64 + 1.0).ln()
    }
}

impl Default for UnknownDiscoverer {
    fn default() -> Self {
        Self::new()
    }
}

