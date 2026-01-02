mod extractor;
mod facts;
mod matcher;
mod pack;
mod pack_loader;
mod parser;
mod report;
mod scanner;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use extractor::Extractor;
use matcher::PatternMatcher;
use pack_loader::PackLoader;
use parser::{ParseResult, ParserRegistry, ParseSummary};
use report::ReportGenerator;
use scanner::Scanner;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Parser)]
#[command(name = "hix-drill")]
#[command(version = "0.1.0")]
#[command(about = "CLI Codebase Analyzer for Hix", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Scan a repository for analysis
    Scan {
        /// Path to the repository to scan
        path: String,
    },
    /// Analyze a repository using pattern packs
    Analyze {
        /// Path to the repository to analyze
        path: String,
        /// Path to the pattern packs directory
        #[arg(long)]
        packs: Option<String>,
    },
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Scan { path }) => {
            let repo_path = Path::new(path);
            
            if !repo_path.exists() {
                anyhow::bail!("Path does not exist: {}", path);
            }

            if !repo_path.is_dir() {
                anyhow::bail!("Path is not a directory: {}", path);
            }

            let scanner = Scanner::new(repo_path);
            let files = scanner.scan(repo_path)
                .with_context(|| format!("Failed to scan repository: {}", path))?;

            // Parse files
            let parser_registry = ParserRegistry::new();
            let mut parse_summary = ParseSummary::new();
            let mut parse_results: HashMap<String, ParseResult> = HashMap::new();
            let mut file_contents: HashMap<String, String> = HashMap::new();

            println!("Scanned {} files", files.len());
            for file in &files {
                if let Some(lang) = &file.language {
                    // Check if we have a parser for this language (0.23 API)
                    // Supported: typescript, tsx, python, csharp, html
                    let has_parser = matches!(lang.as_str(), "typescript" | "tsx" | "python" | "csharp" | "html");
                    if has_parser {
                        // Try to parse if we have a parser for this language
                        let content = match fs::read_to_string(&file.path) {
                            Ok(c) => c,
                            Err(e) => {
                                eprintln!("Warning: Failed to read {}: {}", file.path, e);
                                parse_summary.total_files += 1;
                                parse_summary.failed += 1;
                                continue;
                            }
                        };

                        // Store content for facts extraction
                        file_contents.insert(file.path.clone(), content.clone());

                        let parse_result = parser_registry.parse(&content, lang);
                        parse_summary.add_result(&parse_result);

                        let status = match &parse_result {
                            ParseResult { tree: Some(_), error: None } => "✓",
                            ParseResult { tree: Some(_), error: Some(_) } => "⚠",
                            ParseResult { tree: None, error: Some(_) } => "✗",
                            _ => "○",
                        };
                        println!("  {} {} [{}]", status, file.path, lang);
                        
                        // Store parse result for facts extraction (only if we have a tree)
                        if parse_result.tree.is_some() {
                            parse_results.insert(file.path.clone(), parse_result);
                        }
                    } else {
                        // No parser available for this language
                        println!("  ○ {} [{}]", file.path, lang);
                        parse_summary.total_files += 1;
                        parse_summary.no_parser += 1;
                    }
                } else {
                    println!("  ○ {}", file.path);
                    parse_summary.total_files += 1;
                    parse_summary.no_parser += 1;
                }
            }

            // Print parse summary
            println!("\nParse Summary:");
            println!("  Total files: {}", parse_summary.total_files);
            println!("  Parsed successfully: {}", parse_summary.parsed);
            println!("  Parse errors: {}", parse_summary.failed);
            println!("  No parser available: {}", parse_summary.no_parser);

            // Extract facts and write to .hixdrill/facts.json
            let extractor = Extractor::new();
            let mut facts = extractor.extract_facts(&files, &parse_results, &file_contents);
            
            // Ensure deterministic ordering
            facts.sort();
            
            let facts_path = repo_path.join(".hixdrill").join("facts.json");
            fs::create_dir_all(facts_path.parent().unwrap())
                .with_context(|| "Failed to create .hixdrill directory")?;
            
            let facts_json = serde_json::to_string_pretty(&facts)
                .with_context(|| "Failed to serialize facts to JSON")?;
            
            fs::write(&facts_path, facts_json)
                .with_context(|| format!("Failed to write facts.json to {:?}", facts_path))?;
            
            println!("\nFacts extracted:");
            println!("  Files: {}", facts.files.len());
            println!("  Symbols: {}", facts.symbols.len());
            println!("  Members: {}", facts.members.len());
            println!("  Relations: {}", facts.relations.len());
            println!("  Annotations: {}", facts.annotations.len());
            println!("  Written to: {:?}", facts_path);
        }
        Some(Commands::Analyze { path, packs }) => {
            let repo_path = Path::new(path);
            
            if !repo_path.exists() {
                anyhow::bail!("Path does not exist: {}", path);
            }

            if !repo_path.is_dir() {
                anyhow::bail!("Path is not a directory: {}", path);
            }

            // First, scan and extract facts (reuse scan logic)
            let scanner = Scanner::new(repo_path);
            let files = scanner.scan(repo_path)
                .with_context(|| format!("Failed to scan repository: {}", path))?;

            // Parse files
            let parser_registry = ParserRegistry::new();
            let mut parse_summary = ParseSummary::new();
            let mut parse_results: HashMap<String, ParseResult> = HashMap::new();
            let mut file_contents: HashMap<String, String> = HashMap::new();

            for file in &files {
                if let Some(lang) = &file.language {
                    let has_parser = matches!(lang.as_str(), "typescript" | "tsx" | "python" | "csharp" | "html");
                    if has_parser {
                        let content = match fs::read_to_string(&file.path) {
                            Ok(c) => c,
                            Err(e) => {
                                eprintln!("Warning: Failed to read {}: {}", file.path, e);
                                parse_summary.total_files += 1;
                                parse_summary.failed += 1;
                                continue;
                            }
                        };

                        file_contents.insert(file.path.clone(), content.clone());
                        let parse_result = parser_registry.parse(&content, lang);
                        parse_summary.add_result(&parse_result);
                        
                        if parse_result.tree.is_some() {
                            parse_results.insert(file.path.clone(), parse_result);
                        }
                    } else {
                        parse_summary.total_files += 1;
                        parse_summary.no_parser += 1;
                    }
                } else {
                    parse_summary.total_files += 1;
                    parse_summary.no_parser += 1;
                }
            }

            // Extract facts
            let extractor = Extractor::new();
            let mut facts = extractor.extract_facts(&files, &parse_results, &file_contents);
            facts.sort();
            
            // Load and match patterns if packs are provided
            if let Some(packs_path) = packs {
                let packs_dir = Path::new(&packs_path);
                let loader = PackLoader::new();
                
                match loader.load_packs(packs_dir) {
                    Ok(loaded_packs) => {
                        println!("Loaded {} pattern pack(s):", loaded_packs.len());
                        for loaded_pack in &loaded_packs {
                            println!("  - {} v{}", 
                                loaded_pack.pack.metadata.name,
                                loaded_pack.pack.metadata.version
                            );
                            if let Some(desc) = &loaded_pack.pack.metadata.description {
                                println!("    {}", desc);
                            }
                            println!("    Patterns: {}", loaded_pack.pack.patterns.len());
                        }

                        // Collect all pattern rules
                        let mut all_rules = Vec::new();
                        for loaded_pack in &loaded_packs {
                            all_rules.extend(loaded_pack.pack.patterns.clone());
                        }

                        // Match patterns against facts
                        let matcher = PatternMatcher::new();
                        match matcher.match_patterns(&facts, &all_rules) {
                            Ok(match_results) => {
                                println!("\nPattern Matching Results:");
                                println!("  Total matches: {}", match_results.instances.len());
                                
                                // Group by pattern name
                                let mut by_pattern: HashMap<String, Vec<&matcher::MatchInstance>> = HashMap::new();
                                for instance in &match_results.instances {
                                    by_pattern
                                        .entry(instance.pattern_name.clone())
                                        .or_insert_with(Vec::new)
                                        .push(instance);
                                }

                                for (pattern_name, instances) in &mut by_pattern {
                                    instances.sort_by(|a, b| a.symbol_name.cmp(&b.symbol_name));
                                    println!("  {}: {} matches", pattern_name, instances.len());
                                    for instance in instances.iter().take(5) {
                                        println!("    - {} ({})", instance.symbol_name, instance.file);
                                    }
                                    if instances.len() > 5 {
                                        println!("    ... and {} more", instances.len() - 5);
                                    }
                                }

                                // Write matches.json
                                let matches_path = repo_path.join(".hixdrill").join("matches.json");
                                fs::create_dir_all(matches_path.parent().unwrap())
                                    .with_context(|| "Failed to create .hixdrill directory")?;
                                
                                let matches_json = serde_json::to_string_pretty(&match_results)
                                    .with_context(|| "Failed to serialize matches to JSON")?;
                                
                                fs::write(&matches_path, matches_json)
                                    .with_context(|| format!("Failed to write matches.json to {:?}", matches_path))?;
                                
                                println!("\nMatches written to: {:?}", matches_path);

                                // Generate report
                                let report_generator = ReportGenerator::new();
                                let report = report_generator.generate_report(
                                    &facts,
                                    &match_results,
                                    &loaded_packs,
                                    &parse_summary,
                                );

                                // Write report.json
                                let report_json_path = repo_path.join(".hixdrill").join("report.json");
                                let report_json = serde_json::to_string_pretty(&report)
                                    .with_context(|| "Failed to serialize report to JSON")?;
                                
                                fs::write(&report_json_path, report_json)
                                    .with_context(|| format!("Failed to write report.json to {:?}", report_json_path))?;

                                // Write report.md
                                let report_md = report_generator.generate_markdown(&report);
                                let report_md_path = repo_path.join(".hixdrill").join("report.md");
                                fs::write(&report_md_path, report_md)
                                    .with_context(|| format!("Failed to write report.md to {:?}", report_md_path))?;

                                println!("Report written to: {:?}", report_json_path);
                                println!("Markdown report written to: {:?}", report_md_path);
                            }
                            Err(e) => {
                                eprintln!("Warning: Pattern matching failed: {}", e);
                            }
                        }
                    }
                    Err(e) => {
                        anyhow::bail!("Failed to load pattern packs: {}", e);
                    }
                }
            } else {
                println!("No pattern packs specified. Use --packs <folder> to load packs.");
            }

            println!("\nAnalysis complete");
        }
        None => {
            // No command provided, show help
            let _ = Cli::parse();
        }
    }

    Ok(())
}
