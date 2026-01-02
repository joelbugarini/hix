mod scanner;
mod parser;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use parser::{ParserRegistry, ParseSummary};
use scanner::Scanner;
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

                        let parse_result = parser_registry.parse(&content, lang);
                        parse_summary.add_result(&parse_result);

                        let status = match &parse_result {
                            parser::ParseResult { tree: Some(_), error: None } => "✓",
                            parser::ParseResult { tree: Some(_), error: Some(_) } => "⚠",
                            parser::ParseResult { tree: None, error: Some(_) } => "✗",
                            _ => "○",
                        };
                        println!("  {} {} [{}]", status, file.path, lang);
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
        }
        None => {
            // No command provided, show help
            let _ = Cli::parse();
        }
    }

    Ok(())
}
