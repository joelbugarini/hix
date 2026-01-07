/// AI configuration module
/// Handles configuration for LLM assistance (API keys, providers, etc.)

use serde::{Deserialize, Serialize};
use std::env;
use std::fs;
use std::path::Path;
use anyhow::{Context, Result};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AiConfig {
    /// AI provider (openai, anthropic, ollama, gemini, custom)
    pub provider: String,
    /// API key (can be from env var)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key: Option<String>,
    /// Environment variable name for API key
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key_env: Option<String>,
    /// API URL (for custom providers or Ollama)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_url: Option<String>,
    /// Model name
    pub model: String,
    /// Request timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout_seconds: u64,
    /// Max retries for failed requests
    #[serde(default = "default_retries")]
    pub max_retries: u32,
}

fn default_timeout() -> u64 {
    30
}

fn default_retries() -> u32 {
    3
}

impl Default for AiConfig {
    fn default() -> Self {
        AiConfig {
            provider: "openai".to_string(),
            api_key: None,
            api_key_env: Some("HIX_DRILL_AI_API_KEY".to_string()),
            api_url: None,
            model: "gpt-4o-mini".to_string(),
            timeout_seconds: 30,
            max_retries: 3,
        }
    }
}

impl AiConfig {
    /// Load AI configuration from various sources
    /// Priority: CLI args > env vars > config file > defaults
    /// 
    /// For testing, you can set HIX_DRILL_TEST_AI_API_KEY environment variable
    pub fn load(
        repo_path: &Path,
        provider: Option<&str>,
        api_key: Option<&str>,
        api_url: Option<&str>,
        model: Option<&str>,
    ) -> Result<Option<AiConfig>> {
        // Start with defaults
        let mut config = AiConfig::default();

        // Try to load from config file
        let config_path = repo_path.join(".hix").join("drill").join("assist.json");
        if config_path.exists() {
            let content = fs::read_to_string(&config_path)
                .with_context(|| format!("Failed to read AI config from {:?}", config_path))?;
            let file_config: AiConfig = serde_json::from_str(&content)
                .with_context(|| format!("Failed to parse AI config from {:?}", config_path))?;
            config = file_config;
        }

        // Override with environment variables
        if let Ok(env_provider) = env::var("HIX_DRILL_AI_PROVIDER") {
            config.provider = env_provider;
        }
        if let Ok(env_model) = env::var("HIX_DRILL_AI_MODEL") {
            config.model = env_model;
        }
        if let Ok(env_url) = env::var("HIX_DRILL_AI_URL") {
            config.api_url = Some(env_url);
        }
        
        // Check for test API key (for testing purposes) - check before regular API key
        let test_api_key = env::var("HIX_DRILL_TEST_AI_API_KEY").ok();

        // Override with CLI arguments (highest priority)
        if let Some(p) = provider {
            config.provider = p.to_string();
        }
        if let Some(key) = api_key {
            config.api_key = Some(key.to_string());
        } else if let Some(test_key) = test_api_key {
            // Use test API key if available (for testing)
            config.api_key = Some(test_key);
        } else if let Some(env_var_name) = &config.api_key_env {
            // Try to read from environment variable
            if let Ok(key) = env::var(env_var_name) {
                config.api_key = Some(key);
            }
        }
        if let Some(url) = api_url {
            config.api_url = Some(url.to_string());
        }
        if let Some(m) = model {
            config.model = m.to_string();
        }

        // If no API key found, return None (assistance disabled)
        if config.api_key.is_none() {
            return Ok(None);
        }

        // Set default API URLs based on provider
        if config.api_url.is_none() {
            config.api_url = Some(match config.provider.as_str() {
                "openai" => "https://api.openai.com/v1/chat/completions".to_string(),
                "anthropic" => "https://api.anthropic.com/v1/messages".to_string(),
                "ollama" => "http://localhost:11434/api/generate".to_string(),
                "gemini" => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent".to_string(),
                _ => return Ok(None), // Custom provider requires URL
            });
        }

        Ok(Some(config))
    }

    /// Save configuration to file
    pub fn save(&self, repo_path: &Path) -> Result<()> {
        let config_dir = repo_path.join(".hix").join("drill");
        fs::create_dir_all(&config_dir)
            .with_context(|| format!("Failed to create config directory: {:?}", config_dir))?;

        let config_path = config_dir.join("assist.json");
        let json = serde_json::to_string_pretty(self)
            .with_context(|| "Failed to serialize AI config")?;
        
        fs::write(&config_path, json)
            .with_context(|| format!("Failed to write AI config to {:?}", config_path))?;

        Ok(())
    }

    /// Interactive wizard for first-time setup
    pub fn setup_wizard(repo_path: &Path) -> Result<Option<AiConfig>> {
        use std::io::{self, Write};

        println!("AI assistance not configured. Run setup wizard? [y/n]");
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        if input.trim().to_lowercase() != "y" {
            return Ok(None);
        }

        println!("\nSelect AI provider:");
        println!("  1) OpenAI");
        println!("  2) Anthropic (Claude)");
        println!("  3) Ollama (local)");
        println!("  4) Google Gemini");
        println!("  5) Custom API");
        print!("> ");
        io::stdout().flush()?;
        
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let provider_choice = input.trim();

        let (provider, default_model) = match provider_choice {
            "1" => ("openai", "gpt-4o-mini"),
            "2" => ("anthropic", "claude-3-haiku-20240307"),
            "3" => ("ollama", "llama2"),
            "4" => ("gemini", "gemini-2.0-flash"),
            "5" => ("custom", ""),
            _ => {
                eprintln!("Invalid choice");
                return Ok(None);
            }
        };

        print!("\nEnter API key (or leave empty to use HIX_DRILL_AI_API_KEY env var):\n> ");
        io::stdout().flush()?;
        let mut api_key = String::new();
        io::stdin().read_line(&mut api_key)?;
        let api_key = api_key.trim();
        let api_key = if api_key.is_empty() { None } else { Some(api_key.to_string()) };

        let mut api_url = None;
        if provider == "custom" || provider == "ollama" {
            print!("\nEnter API URL:\n> ");
            io::stdout().flush()?;
            let mut url = String::new();
            io::stdin().read_line(&mut url)?;
            let url = url.trim();
            if !url.is_empty() {
                api_url = Some(url.to_string());
            }
        }

        print!("\nSelect model [{}]:\n> ", default_model);
        io::stdout().flush()?;
        let mut model = String::new();
        io::stdin().read_line(&mut model)?;
        let model = model.trim();
        let model = if model.is_empty() { default_model } else { model };

        let config = AiConfig {
            provider: provider.to_string(),
            api_key,
            api_key_env: Some("HIX_DRILL_AI_API_KEY".to_string()),
            api_url,
            model: model.to_string(),
            timeout_seconds: 30,
            max_retries: 3,
        };

        config.save(repo_path)?;
        println!("\n✓ Configuration saved to .hix/drill/assist.json");

        Ok(Some(config))
    }
}

