use crate::facts::Facts;
use crate::matcher::MatchResults;
use crate::pack_loader::LoadedPack;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Report schema for JSON output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Report {
    /// Summary statistics
    pub summary: ReportSummary,
    /// Loaded pattern packs
    pub packs: Vec<PackInfo>,
    /// Matched instances grouped by pattern
    pub matches_by_pattern: HashMap<String, PatternMatches>,
    /// Coverage metrics
    pub coverage: CoverageMetrics,
    /// Unknown clusters (symbols not matched)
    pub unknowns: UnknownClusters,
}

/// Summary statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReportSummary {
    /// Total number of files scanned
    pub total_files: usize,
    /// Number of files successfully parsed
    pub parsed_files: usize,
    /// Number of files that failed to parse
    pub failed_files: usize,
    /// Number of files with no parser available
    pub no_parser_files: usize,
    /// Total number of symbols extracted
    pub total_symbols: usize,
    /// Total number of matches found
    pub total_matches: usize,
    /// Number of unique patterns matched
    pub unique_patterns: usize,
}

/// Pattern pack information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackInfo {
    /// Pack name
    pub name: String,
    /// Pack version
    pub version: String,
    /// Pack description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Number of patterns in the pack
    pub pattern_count: usize,
}

/// Matches for a specific pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternMatches {
    /// Pattern name
    pub pattern_name: String,
    /// Number of matches
    pub count: usize,
    /// List of matched symbol IDs
    pub matched_symbol_ids: Vec<String>,
    /// Sample matches (first 5)
    pub samples: Vec<MatchSample>,
}

/// Sample match for display
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchSample {
    /// Symbol name
    pub symbol_name: String,
    /// File path
    pub file: String,
    /// Symbol ID
    pub symbol_id: String,
}

/// Coverage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageMetrics {
    /// Percentage of symbols matched (0.0 to 100.0)
    pub symbol_coverage_percent: f64,
    /// Number of matched symbols
    pub matched_symbols: usize,
    /// Number of unmatched symbols
    pub unmatched_symbols: usize,
}

/// Unknown clusters (symbols not matched by any pattern)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnknownClusters {
    /// Total number of unmatched symbols
    pub total_unmatched: usize,
    /// Unmatched symbols grouped by kind
    pub by_kind: HashMap<String, usize>,
    /// Sample unmatched symbols (first 10)
    pub samples: Vec<UnknownSample>,
}

/// Sample unknown symbol
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

pub struct ReportGenerator;

impl ReportGenerator {
    pub fn new() -> Self {
        ReportGenerator
    }

    /// Generate a report from facts, matches, and loaded packs
    pub fn generate_report(
        &self,
        facts: &Facts,
        match_results: &MatchResults,
        loaded_packs: &[LoadedPack],
        parse_summary: &crate::parser::ParseSummary,
    ) -> Report {
        // Calculate summary
        let total_symbols = facts.symbols.len();
        let total_matches = match_results.instances.len();
        
        // Get matched symbol IDs
        let matched_symbol_ids: std::collections::HashSet<String> = match_results
            .instances
            .iter()
            .map(|m| m.symbol_id.clone())
            .collect();
        
        let matched_symbols_count = matched_symbol_ids.len();
        let unmatched_symbols_count = total_symbols.saturating_sub(matched_symbols_count);
        
        // Calculate coverage
        let symbol_coverage_percent = if total_symbols > 0 {
            (matched_symbols_count as f64 / total_symbols as f64) * 100.0
        } else {
            0.0
        };

        // Group matches by pattern
        let mut matches_by_pattern: HashMap<String, PatternMatches> = HashMap::new();
        for instance in &match_results.instances {
            let pattern_matches = matches_by_pattern
                .entry(instance.pattern_name.clone())
                .or_insert_with(|| PatternMatches {
                    pattern_name: instance.pattern_name.clone(),
                    count: 0,
                    matched_symbol_ids: Vec::new(),
                    samples: Vec::new(),
                });
            
            pattern_matches.count += 1;
            if !pattern_matches.matched_symbol_ids.contains(&instance.symbol_id) {
                pattern_matches.matched_symbol_ids.push(instance.symbol_id.clone());
            }
            
            // Add sample (first 5)
            if pattern_matches.samples.len() < 5 {
                pattern_matches.samples.push(MatchSample {
                    symbol_name: instance.symbol_name.clone(),
                    file: instance.file.clone(),
                    symbol_id: instance.symbol_id.clone(),
                });
            }
        }

        // Sort matched symbol IDs for each pattern
        for pattern_matches in matches_by_pattern.values_mut() {
            pattern_matches.matched_symbol_ids.sort();
        }

        // Identify unknown clusters
        let mut unknown_by_kind: HashMap<String, usize> = HashMap::new();
        let mut unknown_samples: Vec<UnknownSample> = Vec::new();
        
        for symbol in &facts.symbols {
            if !matched_symbol_ids.contains(&symbol.id) {
                let kind_str = format!("{:?}", symbol.kind).to_lowercase();
                *unknown_by_kind.entry(kind_str.clone()).or_insert(0) += 1;
                
                // Add sample (first 10)
                if unknown_samples.len() < 10 {
                    unknown_samples.push(UnknownSample {
                        symbol_name: symbol.name.clone(),
                        symbol_kind: kind_str,
                        file: symbol.file.clone(),
                        symbol_id: symbol.id.clone(),
                    });
                }
            }
        }

        // Build pack info
        let packs: Vec<PackInfo> = loaded_packs
            .iter()
            .map(|lp| PackInfo {
                name: lp.pack.metadata.name.clone(),
                version: lp.pack.metadata.version.clone(),
                description: lp.pack.metadata.description.clone(),
                pattern_count: lp.pack.patterns.len(),
            })
            .collect();

        Report {
            summary: ReportSummary {
                total_files: parse_summary.total_files,
                parsed_files: parse_summary.parsed,
                failed_files: parse_summary.failed,
                no_parser_files: parse_summary.no_parser,
                total_symbols,
                total_matches,
                unique_patterns: matches_by_pattern.len(),
            },
            packs,
            matches_by_pattern,
            coverage: CoverageMetrics {
                symbol_coverage_percent,
                matched_symbols: matched_symbols_count,
                unmatched_symbols: unmatched_symbols_count,
            },
            unknowns: UnknownClusters {
                total_unmatched: unmatched_symbols_count,
                by_kind: unknown_by_kind,
                samples: unknown_samples,
            },
        }
    }

    /// Generate markdown report from JSON report
    pub fn generate_markdown(&self, report: &Report) -> String {
        let mut md = String::new();
        
        md.push_str("# hix-drill Analysis Report\n\n");
        
        // Summary
        md.push_str("## Summary\n\n");
        md.push_str(&format!("- **Total Files**: {}\n", report.summary.total_files));
        md.push_str(&format!("- **Parsed Files**: {}\n", report.summary.parsed_files));
        md.push_str(&format!("- **Failed Files**: {}\n", report.summary.failed_files));
        md.push_str(&format!("- **No Parser**: {}\n", report.summary.no_parser_files));
        md.push_str(&format!("- **Total Symbols**: {}\n", report.summary.total_symbols));
        md.push_str(&format!("- **Total Matches**: {}\n", report.summary.total_matches));
        md.push_str(&format!("- **Unique Patterns**: {}\n\n", report.summary.unique_patterns));
        
        // Coverage
        md.push_str("## Coverage\n\n");
        md.push_str(&format!(
            "- **Symbol Coverage**: {:.2}%\n",
            report.coverage.symbol_coverage_percent
        ));
        md.push_str(&format!("- **Matched Symbols**: {}\n", report.coverage.matched_symbols));
        md.push_str(&format!("- **Unmatched Symbols**: {}\n\n", report.coverage.unmatched_symbols));
        
        // Packs
        md.push_str("## Loaded Pattern Packs\n\n");
        if report.packs.is_empty() {
            md.push_str("No pattern packs loaded.\n\n");
        } else {
            for pack in &report.packs {
                md.push_str(&format!("### {} v{}\n", pack.name, pack.version));
                if let Some(desc) = &pack.description {
                    md.push_str(&format!("{}\n", desc));
                }
                md.push_str(&format!("- Patterns: {}\n\n", pack.pattern_count));
            }
        }
        
        // Matches by pattern
        md.push_str("## Pattern Matches\n\n");
        if report.matches_by_pattern.is_empty() {
            md.push_str("No pattern matches found.\n\n");
        } else {
            let mut patterns: Vec<(&String, &PatternMatches)> = report.matches_by_pattern.iter().collect();
            patterns.sort_by(|a, b| a.0.cmp(b.0));
            
            for (_, pattern_matches) in patterns {
                md.push_str(&format!("### {}\n", pattern_matches.pattern_name));
                md.push_str(&format!("- **Matches**: {}\n", pattern_matches.count));
                md.push_str(&format!("- **Unique Symbols**: {}\n", pattern_matches.matched_symbol_ids.len()));
                md.push_str("\n**Sample Matches:**\n\n");
                for sample in &pattern_matches.samples {
                    md.push_str(&format!("- `{}` in `{}`\n", sample.symbol_name, sample.file));
                }
                if pattern_matches.count > pattern_matches.samples.len() {
                    md.push_str(&format!(
                        "\n*... and {} more matches*\n",
                        pattern_matches.count - pattern_matches.samples.len()
                    ));
                }
                md.push_str("\n");
            }
        }
        
        // Unknowns
        md.push_str("## Unknown Clusters\n\n");
        md.push_str(&format!("- **Total Unmatched**: {}\n", report.unknowns.total_unmatched));
        
        if !report.unknowns.by_kind.is_empty() {
            md.push_str("\n**By Symbol Kind:**\n\n");
            let mut kinds: Vec<(&String, &usize)> = report.unknowns.by_kind.iter().collect();
            kinds.sort_by(|a, b| b.1.cmp(a.1)); // Sort by count descending
            
            for (kind, count) in kinds {
                md.push_str(&format!("- **{}**: {}\n", kind, count));
            }
        }
        
        if !report.unknowns.samples.is_empty() {
            md.push_str("\n**Sample Unmatched Symbols:**\n\n");
            for sample in &report.unknowns.samples {
                md.push_str(&format!(
                    "- `{}` ({}) in `{}`\n",
                    sample.symbol_name, sample.symbol_kind, sample.file
                ));
            }
            if report.unknowns.total_unmatched > report.unknowns.samples.len() {
                md.push_str(&format!(
                    "\n*... and {} more unmatched symbols*\n",
                    report.unknowns.total_unmatched - report.unknowns.samples.len()
                ));
            }
        }
        
        md.push_str("\n---\n\n");
        md.push_str("*Report generated by hix-drill*\n");
        
        md
    }
}

impl Default for ReportGenerator {
    fn default() -> Self {
        Self::new()
    }
}

