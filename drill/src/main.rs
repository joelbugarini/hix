mod ai_assistant;
mod ai_config;
mod extractor;
mod facts;
mod init_writer;
mod matcher;
mod model_inference;
mod pack;
mod pack_emitter;
mod pack_loader;
mod parser;
mod report;
mod scanner;
mod template_synthesis;
mod template_validator;
mod unknown_discovery;
mod validator;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use extractor::Extractor;
use init_writer::InitWriter;
use matcher::PatternMatcher;
use pack_emitter::PackEmitter;
use pack_loader::PackLoader;
use parser::{ParseResult, ParserRegistry, ParseSummary};
use report::ReportGenerator;
use scanner::Scanner;
use template_synthesis::{SynthesisMetadata, TemplateSynthesizer};
use model_inference::ModelInferrer;
use validator::Validator;
use unknown_discovery::UnknownDiscoverer;
use ai_config::AiConfig;
use ai_assistant::AiAssistant;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

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
    /// Initialize hix-drill project configuration
    Init {
        /// Path to the repository to initialize
        path: String,
        /// Path to the pattern packs directory
        #[arg(long)]
        packs: Option<String>,
        /// Enable mining workflow (scan, analyze, mine, synthesize, validate, init)
        #[arg(long)]
        mine: bool,
        /// Limit mining to top N clusters (default: all)
        #[arg(long)]
        mine_limit: Option<usize>,
        /// Only mine specified language (e.g., "csharp", "typescript")
        #[arg(long)]
        mine_language: Option<String>,
        /// Enable AI assistance for pack creation (requires API key)
        #[arg(long)]
        assist: bool,
        /// AI provider (openai, anthropic, ollama, custom)
        #[arg(long)]
        ai_provider: Option<String>,
        /// AI API key (or use env var HIX_DRILL_AI_API_KEY)
        #[arg(long)]
        ai_key: Option<String>,
        /// AI API URL (for custom providers or Ollama)
        #[arg(long)]
        ai_url: Option<String>,
        /// AI model name
        #[arg(long)]
        ai_model: Option<String>,
    },
    /// Validate mined packs against repository
    Validate {
        /// Path to the pattern packs directory
        #[arg(long)]
        packs: String,
        /// Path to the repository to validate against
        #[arg(long)]
        repo: String,
        /// Path to hix binary (optional, defaults to "hix" in PATH)
        #[arg(long)]
        hix_path: Option<String>,
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

            // Extract facts and write to .hix/drill/facts.json
            let extractor = Extractor::new();
            let mut facts = extractor.extract_facts(&files, &parse_results, &file_contents);
            
            // Ensure deterministic ordering
            facts.sort();
            
            let facts_path = repo_path.join(".hix").join("drill").join("facts.json");
            fs::create_dir_all(facts_path.parent().unwrap())
                .with_context(|| "Failed to create .hix/drill directory")?;
            
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
                                let matches_path = repo_path.join(".hix").join("drill").join("matches.json");
                                fs::create_dir_all(matches_path.parent().unwrap())
                                    .with_context(|| "Failed to create .hix/drill directory")?;
                                
                                let matches_json = serde_json::to_string_pretty(&match_results)
                                    .with_context(|| "Failed to serialize matches to JSON")?;
                                
                                fs::write(&matches_path, matches_json)
                                    .with_context(|| format!("Failed to write matches.json to {:?}", matches_path))?;
                                
                                println!("\nMatches written to: {:?}", matches_path);

                                // Generate report
                                let report_generator = ReportGenerator::new();
                                let (report, unknown_discovery) = report_generator.generate_report(
                                    &facts,
                                    &match_results,
                                    &loaded_packs,
                                    &parse_summary,
                                );

                                // Write unknowns.json
                                let unknowns_path = repo_path.join(".hix").join("drill").join("unknowns.json");
                                let unknowns_json = serde_json::to_string_pretty(&unknown_discovery)
                                    .with_context(|| "Failed to serialize unknowns to JSON")?;
                                
                                fs::write(&unknowns_path, unknowns_json)
                                    .with_context(|| format!("Failed to write unknowns.json to {:?}", unknowns_path))?;
                                
                                println!("\nUnknowns written to: {:?}", unknowns_path);
                                println!("  Clusters found: {}", unknown_discovery.clusters.len());

                                // Synthesize templates for top clusters (v1: top 3)
                                if !unknown_discovery.clusters.is_empty() {
                                    let synthesizer = TemplateSynthesizer::new();
                                    let synthesis_dir = repo_path.join(".hix").join("drill").join("synthesis");
                                    fs::create_dir_all(&synthesis_dir)
                                        .with_context(|| "Failed to create synthesis directory")?;

                                    let top_clusters: Vec<_> = unknown_discovery.clusters.iter().take(3).collect();
                                    println!("\nSynthesizing templates for top {} cluster(s)...", top_clusters.len());

                                    for cluster in top_clusters {
                                        match synthesizer.synthesize(cluster, &facts, repo_path) {
                                            Ok(result) => {
                                                // Create cluster-specific synthesis directory
                                                let cluster_dir = synthesis_dir.join(&cluster.cluster_id);
                                                fs::create_dir_all(&cluster_dir)
                                                    .with_context(|| "Failed to create cluster synthesis directory")?;

                                                // Build metadata
                                                let sample_files: Vec<String> = cluster
                                                    .samples
                                                    .iter()
                                                    .map(|s| s.file.clone())
                                                    .collect();
                                                
                                                let metadata = SynthesisMetadata {
                                                    template_name: result.template_name.clone(),
                                                    language: result.language.clone(),
                                                    cluster_id: cluster.cluster_id.clone(),
                                                    placeholders: result.placeholders.clone(),
                                                    base_sample_file: result.base_sample_file.clone(),
                                                    sample_files,
                                                };

                                                // Infer model from synthesis first (needed for Hix syntax conversion)
                                                let inferrer = ModelInferrer::new();
                                                match inferrer.infer_model(&metadata, cluster, &facts) {
                                                    Ok(model) => {
                                                        // Write model.json
                                                        let model_path = cluster_dir.join("model.json");
                                                        match inferrer.write_model(&model, &model_path) {
                                                            Ok(_) => {
                                                                println!("  ✓ Inferred model for {}: {}", 
                                                                    cluster.cluster_id, model.className);
                                                                
                                                                // Write synthesis files with Hix syntax conversion
                                                                match synthesizer.write_synthesis_with_hix_syntax(
                                                                    &result, &metadata, &model, &cluster_dir
                                                                ) {
                                                                    Ok(_) => {
                                                                        println!("  ✓ Synthesized template for {}: {}", 
                                                                            cluster.cluster_id, result.template_name);
                                                                        
                                                                        // Emit Pattern Pack from synthesized artifacts
                                                                        let pack_emitter = PackEmitter::new();
                                                                        let packs_dir = repo_path.join(".hix").join("drill").join("packs");
                                                                        match pack_emitter.emit_pack(
                                                                            &metadata,
                                                                            cluster,
                                                                            &model,
                                                                            &facts,
                                                                            repo_path,
                                                                            &packs_dir,
                                                                            None, // No AI assistance in analyze command
                                                                        ) {
                                                                            Ok(pack_path) => {
                                                                                println!("  ✓ Emitted pack for {}: {:?}", 
                                                                                    cluster.cluster_id, pack_path);
                                                                            }
                                                                            Err(e) => {
                                                                                eprintln!("  ✗ Failed to emit pack for {}: {}", 
                                                                                    cluster.cluster_id, e);
                                                                            }
                                                                        }
                                                                    }
                                                                    Err(e) => {
                                                                        eprintln!("  ✗ Failed to write synthesis for {}: {}", 
                                                                            cluster.cluster_id, e);
                                                                    }
                                                                }
                                                            }
                                                            Err(e) => {
                                                                eprintln!("  ✗ Failed to write model for {}: {}", 
                                                                    cluster.cluster_id, e);
                                                            }
                                                        }
                                                    }
                                                    Err(e) => {
                                                        eprintln!("  ✗ Failed to infer model for {}: {}", 
                                                            cluster.cluster_id, e);
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                eprintln!("  ✗ Failed to synthesize template for {}: {}", 
                                                    cluster.cluster_id, e);
                                            }
                                        }
                                    }
                                }

                                // Write report.json
                                let report_json_path = repo_path.join(".hix").join("drill").join("report.json");
                                let report_json = serde_json::to_string_pretty(&report)
                                    .with_context(|| "Failed to serialize report to JSON")?;
                                
                                fs::write(&report_json_path, report_json)
                                    .with_context(|| format!("Failed to write report.json to {:?}", report_json_path))?;

                                // Write report.md
                                let report_md = report_generator.generate_markdown(&report);
                                let report_md_path = repo_path.join(".hix").join("drill").join("report.md");
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
        Some(Commands::Init { path, packs, mine, mine_limit, mine_language, assist, ai_provider, ai_key, ai_url, ai_model }) => {
            let repo_path = Path::new(path);
            
            if !repo_path.exists() {
                anyhow::bail!("Path does not exist: {}", path);
            }

            if !repo_path.is_dir() {
                anyhow::bail!("Path is not a directory: {}", path);
            }

            if *mine {
                // Mining workflow: full pipeline
                println!("🔍 Starting mining workflow...\n");
                
                // Load AI configuration if --assist is enabled
                let ai_config = if *assist {
                    match AiConfig::load(
                        repo_path,
                        ai_provider.as_deref(),
                        ai_key.as_deref(),
                        ai_url.as_deref(),
                        ai_model.as_deref(),
                    ) {
                        Ok(Some(config)) => {
                            println!("🤖 AI assistance enabled (provider: {}, model: {})", 
                                config.provider, config.model);
                            Some(config)
                        }
                        Ok(None) => {
                            println!("⚠ AI assistance requested but no API key found.");
                            println!("   Run setup wizard or set HIX_DRILL_AI_API_KEY environment variable.");
                            if let Ok(Some(config)) = AiConfig::setup_wizard(repo_path) {
                                Some(config)
                            } else {
                                println!("   Continuing without AI assistance...");
                                None
                            }
                        }
                        Err(e) => {
                            eprintln!("⚠ Failed to load AI config: {}. Continuing without AI assistance...", e);
                            None
                        }
                    }
                } else {
                    None
                };
                
                // Step 1: Scan repository
                println!("[1/7] Scanning repository...");
                let scanner = Scanner::new(repo_path);
                let files = scanner.scan(repo_path)
                    .with_context(|| format!("Failed to scan repository: {}", path))?;
                println!("  ✓ Scanned {} files", files.len());

                // Step 2: Parse files
                println!("\n[2/7] Parsing files...");
                let parser_registry = ParserRegistry::new();
                let mut parse_summary = ParseSummary::new();
                let mut parse_results: HashMap<String, ParseResult> = HashMap::new();
                let mut file_contents: HashMap<String, String> = HashMap::new();

                for file in &files {
                    if let Some(lang) = &file.language {
                        // Apply language filter if specified
                        if let Some(ref filter_lang) = mine_language {
                            if lang != filter_lang && !(lang == "tsx" && filter_lang == "typescript") {
                                continue;
                            }
                        }
                        
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
                println!("  ✓ Parsed {} files", parse_results.len());

                // Step 3: Extract facts
                println!("\n[3/7] Extracting facts...");
                let extractor = Extractor::new();
                let mut facts = extractor.extract_facts(&files, &parse_results, &file_contents);
                facts.sort();
                println!("  ✓ Extracted {} symbols, {} members", facts.symbols.len(), facts.members.len());

                // Step 4: Analyze with existing packs (if provided)
                let mut match_results = None;
                let mut loaded_packs = Vec::new();
                
                if let Some(packs_path) = packs {
                    println!("\n[4/7] Analyzing with existing packs...");
                    let packs_dir = Path::new(&packs_path);
                    let loader = PackLoader::new();
                    
                    match loader.load_packs(packs_dir) {
                        Ok(packs) => {
                            loaded_packs = packs;
                            println!("  ✓ Loaded {} pattern pack(s)", loaded_packs.len());
                            
                            // Collect all pattern rules
                            let mut all_rules = Vec::new();
                            for loaded_pack in &loaded_packs {
                                all_rules.extend(loaded_pack.pack.patterns.clone());
                            }

                            // Match patterns against facts
                            let matcher = PatternMatcher::new();
                            match matcher.match_patterns(&facts, &all_rules) {
                                Ok(results) => {
                                    match_results = Some(results);
                                    println!("  ✓ Found {} matches", match_results.as_ref().unwrap().instances.len());
                                }
                                Err(e) => {
                                    eprintln!("  ⚠ Pattern matching failed: {}", e);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("  ⚠ Failed to load pattern packs: {}", e);
                        }
                    }
                } else {
                    println!("\n[4/7] Skipping pack analysis (no packs provided)");
                }

                // Step 5: Mine unknown clusters
                println!("\n[5/7] Mining unknown clusters...");
                let matched_symbol_ids: std::collections::HashSet<String> = match_results.as_ref()
                    .map(|mr| mr.instances.iter().map(|i| i.symbol_id.clone()).collect())
                    .unwrap_or_default();
                
                let discoverer = UnknownDiscoverer::new();
                let unknown_discovery = discoverer.discover_unknowns(&facts, &matched_symbol_ids);
                
                // Apply language filter to clusters
                let mut clusters_to_mine: Vec<_> = unknown_discovery.clusters.iter().collect();
                if let Some(ref filter_lang) = mine_language {
                    clusters_to_mine.retain(|c| {
                        c.samples.iter().any(|s| {
                            let file_lang = Path::new(&s.file).extension()
                                .and_then(|ext| ext.to_str())
                                .map(|ext| match ext {
                                    "cs" => "csharp",
                                    "ts" | "tsx" => "typescript",
                                    "py" => "python",
                                    "html" => "html",
                                    _ => "",
                                });
                            if let Some(lang) = file_lang {
                                lang == filter_lang.as_str() || (lang == "typescript" && filter_lang == "tsx")
                            } else {
                                false
                            }
                        })
                    });
                }
                
                // Apply limit
                if let Some(limit) = mine_limit {
                    clusters_to_mine.truncate(*limit);
                }
                
                println!("  ✓ Found {} cluster(s) to mine", clusters_to_mine.len());
                
                if clusters_to_mine.is_empty() {
                    println!("\n⚠ No clusters found to mine. Exiting.");
                    return Ok(());
                }

                // Step 6: Synthesize, infer models, and emit packs
                println!("\n[6/7] Synthesizing templates and emitting packs...");
                let synthesizer = TemplateSynthesizer::new();
                let synthesis_dir = repo_path.join(".hix").join("drill").join("synthesis");
                fs::create_dir_all(&synthesis_dir)
                    .with_context(|| "Failed to create synthesis directory")?;
                
                let pack_emitter = PackEmitter::new();
                let packs_output_dir = repo_path.join(".hix").join("drill").join("packs");
                let mut emitted_packs: Vec<PathBuf> = Vec::new();
                let mut successful_clusters = 0;

                // Create AI assistant if configured
                let ai_assistant = ai_config.as_ref().map(|config| {
                    AiAssistant::new(config.clone())
                });

                for cluster in &clusters_to_mine {
                    // Get AI suggestions if available
                    let ai_suggestion: Option<ai_assistant::PackSuggestion> = if let Some(ref assistant) = ai_assistant {
                        println!("  🤖 Getting AI suggestions for cluster {}...", cluster.cluster_id);
                        // Use tokio runtime for async AI calls
                        let rt = tokio::runtime::Runtime::new().unwrap();
                        let result: Result<ai_assistant::PackSuggestion, anyhow::Error> = rt.block_on(assistant.suggest_pack(cluster, &facts));
                        match result {
                            Ok(suggestion) => {
                                println!("    ✓ AI suggested pack name: {}", suggestion.name);
                                Some(suggestion)
                            }
                            Err(e) => {
                                eprintln!("    ⚠ AI suggestion failed: {}. Using defaults.", e);
                                None
                            }
                        }
                    } else {
                        None
                    };
                    match synthesizer.synthesize(cluster, &facts, repo_path) {
                        Ok(result) => {
                            let cluster_dir = synthesis_dir.join(&cluster.cluster_id);
                            fs::create_dir_all(&cluster_dir)
                                .with_context(|| "Failed to create cluster synthesis directory")?;

                            let sample_files: Vec<String> = cluster
                                .samples
                                .iter()
                                .map(|s| s.file.clone())
                                .collect();
                            
                            let metadata = SynthesisMetadata {
                                template_name: result.template_name.clone(),
                                language: result.language.clone(),
                                cluster_id: cluster.cluster_id.clone(),
                                placeholders: result.placeholders.clone(),
                                base_sample_file: result.base_sample_file.clone(),
                                sample_files,
                            };

                            let inferrer = ModelInferrer::new();
                            match inferrer.infer_model(&metadata, cluster, &facts) {
                                Ok(model) => {
                                    let model_path = cluster_dir.join("model.json");
                                    if inferrer.write_model(&model, &model_path).is_ok() {
                                        match synthesizer.write_synthesis_with_hix_syntax(
                                            &result, &metadata, &model, &cluster_dir
                                        ) {
                                            Ok(_) => {
                                                // Emit pack (with AI suggestion if available)
                                                match pack_emitter.emit_pack(
                                                    &metadata,
                                                    cluster,
                                                    &model,
                                                    &facts,
                                                    repo_path,
                                                    &packs_output_dir,
                                                    ai_suggestion.as_ref(),
                                                ) {
                                                    Ok(pack_path) => {
                                                        emitted_packs.push(pack_path);
                                                        successful_clusters += 1;
                                                        println!("  ✓ Mined cluster {}: {} symbols", 
                                                            cluster.cluster_id, cluster.size);
                                                    }
                                                    Err(e) => {
                                                        eprintln!("  ✗ Failed to emit pack for {}: {}", 
                                                            cluster.cluster_id, e);
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                eprintln!("  ✗ Failed to write synthesis for {}: {}", 
                                                    cluster.cluster_id, e);
                                            }
                                        }
                                    }
                                }
                                Err(e) => {
                                    eprintln!("  ✗ Failed to infer model for {}: {}", 
                                        cluster.cluster_id, e);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("  ✗ Failed to synthesize template for {}: {}", 
                                cluster.cluster_id, e);
                        }
                    }
                }

                println!("  ✓ Successfully mined {} cluster(s)", successful_clusters);

                // Step 7: Validate mined packs (optional gate)
                println!("\n[7/7] Validating mined packs...");
                if !emitted_packs.is_empty() {
                    let validator = Validator::new(None);
                    match validator.validate_packs(&packs_output_dir, repo_path) {
                        Ok(results) => {
                            let passed = results.packs_passed;
                            let failed = results.packs_failed;
                            println!("  ✓ Validation complete: {} passed, {} failed", passed, failed);
                            
                            if failed > 0 {
                                println!("  ⚠ Some packs failed validation. Check validation.json for details.");
                            }
                        }
                        Err(e) => {
                            eprintln!("  ⚠ Validation failed: {}", e);
                        }
                    }
                } else {
                    println!("  ⚠ No packs to validate");
                }

                // Generate project.json from mined packs
                println!("\n📝 Generating project configuration...");
                if !emitted_packs.is_empty() {
                    // Load emitted packs
                    let loader = PackLoader::new();
                    match loader.load_packs(&packs_output_dir) {
                        Ok(mined_packs) => {
                            let init_writer = InitWriter::new();
                            let project_root = repo_path.canonicalize()
                                .unwrap_or_else(|_| repo_path.to_path_buf())
                                .to_string_lossy()
                                .to_string();
                            
                            let packs_dir_canonical = packs_output_dir.canonicalize()
                                .unwrap_or_else(|_| packs_output_dir.to_path_buf())
                                .to_string_lossy()
                                .to_string();
                            
                            // Create a minimal match results for config generation
                            let empty_matches = matcher::MatchResults { instances: Vec::new() };
                            
                            let config = init_writer.generate_config(
                                &empty_matches,
                                &mined_packs,
                                &project_root,
                                &packs_dir_canonical,
                            );

                            let hix_drill_dir = repo_path.join(".hix").join("drill");
                            fs::create_dir_all(&hix_drill_dir)
                                .with_context(|| "Failed to create .hix/drill directory")?;

                            let config_path = hix_drill_dir.join("project.json");
                            let config_json = serde_json::to_string_pretty(&config)
                                .with_context(|| "Failed to serialize config to JSON")?;
                            
                            fs::write(&config_path, config_json)
                                .with_context(|| format!("Failed to write project.json to {:?}", config_path))?;

                            println!("  ✓ Project config written to: {:?}", config_path);
                        }
                        Err(e) => {
                            eprintln!("  ⚠ Failed to load mined packs for config generation: {}", e);
                        }
                    }
                }

                // Generate report.md
                if let Some(ref matches) = match_results {
                    let report_generator = ReportGenerator::new();
                    let (report, _) = report_generator.generate_report(
                        &facts,
                        matches,
                        &loaded_packs,
                        &parse_summary,
                    );
                    
                    let report_md = report_generator.generate_markdown(&report);
                    let report_md_path = repo_path.join(".hix").join("drill").join("report.md");
                    fs::write(&report_md_path, report_md)
                        .with_context(|| format!("Failed to write report.md to {:?}", report_md_path))?;
                    
                    println!("  ✓ Report written to: {:?}", report_md_path);
                }

                // Summary
                println!("\n✅ Mining workflow complete!");
                println!("  • Files scanned: {}", files.len());
                println!("  • Clusters mined: {}", successful_clusters);
                println!("  • Packs emitted: {}", emitted_packs.len());
                println!("  • Project config: .hix/drill/project.json");
                println!("  • Report: .hix/drill/report.md");
                
            } else {
                // Regular init workflow (existing behavior)
                // First, run analysis (same as Analyze command)
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
                    let packs_dir_canonical = packs_dir.canonicalize()
                        .unwrap_or_else(|_| packs_dir.to_path_buf())
                        .to_string_lossy()
                        .to_string();
                    let loader = PackLoader::new();
                    
                    match loader.load_packs(packs_dir) {
                        Ok(loaded_packs) => {
                            println!("Loaded {} pattern pack(s):", loaded_packs.len());
                            for loaded_pack in &loaded_packs {
                                println!("  - {} v{}", 
                                    loaded_pack.pack.metadata.name,
                                    loaded_pack.pack.metadata.version
                                );
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

                                    // Generate drill project config
                                    let init_writer = InitWriter::new();
                                    let project_root = repo_path.canonicalize()
                                        .unwrap_or_else(|_| repo_path.to_path_buf())
                                        .to_string_lossy()
                                        .to_string();
                                    
                                    let config = init_writer.generate_config(
                                        &match_results,
                                        &loaded_packs,
                                        &project_root,
                                        &packs_dir_canonical,
                                    );

                                    // Create .hix/drill/ directory
                                    let hix_drill_dir = repo_path.join(".hix").join("drill");
                                    fs::create_dir_all(&hix_drill_dir)
                                        .with_context(|| format!("Failed to create .hix/drill directory: {:?}", hix_drill_dir))?;

                                    // Write .hix/drill/project.json
                                    let config_path = hix_drill_dir.join("project.json");
                                    let config_json = serde_json::to_string_pretty(&config)
                                        .with_context(|| "Failed to serialize config to JSON")?;
                                    
                                    fs::write(&config_path, config_json)
                                        .with_context(|| format!("Failed to write project.json to {:?}", config_path))?;

                                    println!("\nDrill project config written to: {:?}", config_path);
                                    println!("  Packs used: {}", config.packs_used.len());
                                    println!("  Pattern mappings: {}", config.pattern_mappings.len());
                                }
                                Err(e) => {
                                    anyhow::bail!("Pattern matching failed: {}", e);
                                }
                            }
                        }
                        Err(e) => {
                            anyhow::bail!("Failed to load pattern packs: {}", e);
                        }
                    }
                } else {
                    anyhow::bail!("Pattern packs required for init. Use --packs <folder> to specify packs, or --mine to mine new packs.");
                }

                println!("\nInitialization complete");
            }
        }
        Some(Commands::Validate { packs, repo, hix_path }) => {
            let packs_dir = Path::new(packs);
            let repo_path = Path::new(repo);
            
            if !packs_dir.exists() {
                anyhow::bail!("Packs directory does not exist: {}", packs);
            }
            
            if !packs_dir.is_dir() {
                anyhow::bail!("Packs path is not a directory: {}", packs);
            }
            
            if !repo_path.exists() {
                anyhow::bail!("Repository path does not exist: {}", repo);
            }
            
            if !repo_path.is_dir() {
                anyhow::bail!("Repository path is not a directory: {}", repo);
            }

            println!("Validating packs in {:?} against repository {:?}", packs_dir, repo_path);
            
            let hix_path_buf = hix_path.as_ref().map(|s| PathBuf::from(s));
            let validator = Validator::new(hix_path_buf);
            
            match validator.validate_packs(packs_dir, repo_path) {
                Ok(results) => {
                    println!("\nValidation Results:");
                    println!("  Total packs: {}", results.total_packs);
                    println!("  Passed: {}", results.packs_passed);
                    println!("  Failed: {}", results.packs_failed);
                    
                    for pack_result in &results.pack_results {
                        let status = if pack_result.passed { "✓" } else { "✗" };
                        println!("\n  {} {} ({} instances: {}/{} passed)", 
                            status,
                            pack_result.pack_name,
                            pack_result.instances_validated,
                            pack_result.instances_passed,
                            pack_result.instances_validated
                        );
                        
                        if !pack_result.errors.is_empty() {
                            println!("    Errors:");
                            for error in &pack_result.errors {
                                println!("      - {}: {}", error.instance, error.message);
                                if let Some(ref diff) = error.diff {
                                    println!("        Diff:");
                                    for line in diff.lines().take(20) {
                                        println!("        {}", line);
                                    }
                                    if diff.lines().count() > 20 {
                                        println!("        ... (diff truncated)");
                                    }
                                }
                            }
                        }
                    }
                    
                    // Write validation report
                    let report_path = repo_path.join(".hix").join("drill").join("validation.json");
                    fs::create_dir_all(report_path.parent().unwrap())
                        .with_context(|| "Failed to create validation report directory")?;
                    
                    let report_json = serde_json::to_string_pretty(&results)
                        .with_context(|| "Failed to serialize validation results")?;
                    
                    fs::write(&report_path, report_json)
                        .with_context(|| format!("Failed to write validation report to {:?}", report_path))?;
                    
                    println!("\nValidation report written to: {:?}", report_path);
                    
                    if results.packs_failed > 0 {
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    anyhow::bail!("Validation failed: {}", e);
                }
            }
        }
        None => {
            // No command provided, show help
            let _ = Cli::parse();
        }
    }

    Ok(())
}
