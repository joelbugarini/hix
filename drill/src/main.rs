use clap::{Parser, Subcommand};

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
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Scan { path: _ }) => {
            println!("not implemented");
        }
        None => {
            // No command provided, show help
            let _ = Cli::parse();
        }
    }
}
