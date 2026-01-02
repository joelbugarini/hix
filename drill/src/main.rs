mod scanner;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use scanner::Scanner;
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

            println!("Scanned {} files", files.len());
            for file in &files {
                if let Some(lang) = &file.language {
                    println!("  {} [{}]", file.path, lang);
                } else {
                    println!("  {}", file.path);
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
