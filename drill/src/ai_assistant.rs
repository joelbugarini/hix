/// AI assistant module
/// Handles LLM API calls for pack creation assistance

use crate::ai_config::AiConfig;
use crate::unknown_discovery::SymbolCluster;
use crate::facts::Facts;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::json;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackSuggestion {
    /// Suggested pack name (e.g., "csharp-data-class" instead of "mined/csharp/cluster_0")
    pub name: String,
    /// Suggested pack description
    pub description: String,
    /// Suggested pattern matching rules (for pattern.json)
    pub pattern_rules: serde_json::Value,
    /// Suggested placeholder names (e.g., "propertyName" instead of "Property1")
    pub placeholder_names: Vec<String>,
}

pub struct AiAssistant {
    config: AiConfig,
}

impl AiAssistant {
    pub fn new(config: AiConfig) -> Self {
        AiAssistant { config }
    }

    /// Suggest pack structure for a cluster
    pub async fn suggest_pack(
        &self,
        cluster: &SymbolCluster,
        _facts: &Facts,
    ) -> Result<PackSuggestion> {
        // Build context from cluster samples
        let samples: Vec<String> = cluster.samples.iter()
            .take(3) // Limit to first 3 samples for context
            .map(|s| {
                // Get file content for sample
                if let Ok(content) = std::fs::read_to_string(&s.file) {
                    // Extract first 30 lines as context
                    let context: String = content.lines()
                        .take(30)
                        .collect::<Vec<&str>>()
                        .join("\n");
                    format!("File: {}\nSymbol: {} ({})\n```\n{}\n```", 
                        s.file, s.symbol_name, s.symbol_kind, context)
                } else {
                    format!("File: {}\nSymbol: {} ({})", s.file, s.symbol_name, s.symbol_kind)
                }
            })
            .collect();

        let samples_text = samples.join("\n\n---\n\n");
        let prompt = format!(
            r#"Analyze this code cluster and suggest a pattern pack structure.

Cluster Info:
- Cluster ID: {}
- Size: {} symbols

Code Samples:
{}

Based on these samples, suggest:
1. A descriptive pack name (e.g., "csharp-data-class", "typescript-api-endpoint")
2. A clear description of what pattern this pack matches
3. Pattern matching rules (JSON structure for pattern.json) that would match similar code
4. Better placeholder names (instead of generic "Property1", "Property2")

Respond in JSON format:
{{
  "name": "suggested-pack-name",
  "description": "Clear description of the pattern",
  "pattern_rules": {{"symbol_kind": "...", "member_predicates": [...]}},
  "placeholder_names": ["propertyName", "className", ...]
}}"#,
            cluster.cluster_id,
            cluster.size,
            samples_text
        );

        let response = self.call_llm(&prompt).await?;
        self.parse_suggestion(&response)
    }

    /// Call LLM API based on provider
    async fn call_llm(&self, prompt: &str) -> Result<String> {
        match self.config.provider.as_str() {
            "openai" => self.call_openai(prompt).await,
            "anthropic" => self.call_anthropic(prompt).await,
            "ollama" => self.call_ollama(prompt).await,
            "gemini" => self.call_gemini(prompt).await,
            "custom" => self.call_custom(prompt).await,
            _ => anyhow::bail!("Unsupported provider: {}", self.config.provider),
        }
    }

    /// Call OpenAI API
    async fn call_openai(&self, prompt: &str) -> Result<String> {
        let client = reqwest::Client::new();
        let api_key = self.config.api_key.as_ref()
            .ok_or_else(|| anyhow::anyhow!("API key not configured"))?;

        let payload = json!({
            "model": self.config.model,
            "messages": [
                {
                    "role": "system",
                    "content": "You are a code analysis assistant. Respond only with valid JSON."
                },
                {
                    "role": "user",
                    "content": prompt
                }
            ],
            "temperature": 0.3,
        });

        let url = self.config.api_url.as_ref()
            .ok_or_else(|| anyhow::anyhow!("API URL not configured"))?;

        let response = client
            .post(url)
            .header("Authorization", format!("Bearer {}", api_key))
            .header("Content-Type", "application/json")
            .json(&payload)
            .timeout(std::time::Duration::from_secs(self.config.timeout_seconds))
            .send()
            .await
            .with_context(|| "Failed to send request to OpenAI")?;

        let status = response.status();
        if !status.is_success() {
            let error_text = response.text().await.unwrap_or_default();
            anyhow::bail!("OpenAI API error ({}): {}", status, error_text);
        }

        let json: serde_json::Value = response.json().await
            .with_context(|| "Failed to parse OpenAI response")?;

        let content = json["choices"][0]["message"]["content"]
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("No content in OpenAI response"))?;

        Ok(content.to_string())
    }

    /// Call Anthropic API
    async fn call_anthropic(&self, prompt: &str) -> Result<String> {
        let client = reqwest::Client::new();
        let api_key = self.config.api_key.as_ref()
            .ok_or_else(|| anyhow::anyhow!("API key not configured"))?;

        let payload = json!({
            "model": self.config.model,
            "max_tokens": 4096,
            "messages": [
                {
                    "role": "user",
                    "content": format!("{}\n\nRespond only with valid JSON.", prompt)
                }
            ],
        });

        let url = self.config.api_url.as_ref()
            .ok_or_else(|| anyhow::anyhow!("API URL not configured"))?;

        let response = client
            .post(url)
            .header("x-api-key", api_key)
            .header("anthropic-version", "2023-06-01")
            .header("Content-Type", "application/json")
            .json(&payload)
            .timeout(std::time::Duration::from_secs(self.config.timeout_seconds))
            .send()
            .await
            .with_context(|| "Failed to send request to Anthropic")?;

        let status = response.status();
        if !status.is_success() {
            let error_text = response.text().await.unwrap_or_default();
            anyhow::bail!("Anthropic API error ({}): {}", status, error_text);
        }

        let json: serde_json::Value = response.json().await
            .with_context(|| "Failed to parse Anthropic response")?;

        let content = json["content"][0]["text"]
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("No content in Anthropic response"))?;

        Ok(content.to_string())
    }

    /// Call Ollama API (local)
    async fn call_ollama(&self, prompt: &str) -> Result<String> {
        let client = reqwest::Client::new();
        let url = self.config.api_url.as_ref()
            .ok_or_else(|| anyhow::anyhow!("API URL not configured"))?;

        let payload = json!({
            "model": self.config.model,
            "prompt": format!("{}\n\nRespond only with valid JSON.", prompt),
            "stream": false,
        });

        let response = client
            .post(url)
            .json(&payload)
            .timeout(std::time::Duration::from_secs(self.config.timeout_seconds))
            .send()
            .await
            .with_context(|| "Failed to send request to Ollama")?;

        let status = response.status();
        if !status.is_success() {
            let error_text = response.text().await.unwrap_or_default();
            anyhow::bail!("Ollama API error ({}): {}", status, error_text);
        }

        let json: serde_json::Value = response.json().await
            .with_context(|| "Failed to parse Ollama response")?;

        let content = json["response"]
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("No response in Ollama output"))?;

        Ok(content.to_string())
    }

    /// Call Google Gemini API
    async fn call_gemini(&self, prompt: &str) -> Result<String> {
        let client = reqwest::Client::new();
        let api_key = self.config.api_key.as_ref()
            .ok_or_else(|| anyhow::anyhow!("API key not configured"))?;

        // Gemini API uses a different format
        let payload = json!({
            "contents": [
                {
                    "parts": [
                        {
                            "text": format!("{}\n\nRespond only with valid JSON.", prompt)
                        }
                    ]
                }
            ],
            "generationConfig": {
                "temperature": 0.3,
                "maxOutputTokens": 4096,
            }
        });

        let url = self.config.api_url.as_ref()
            .ok_or_else(|| anyhow::anyhow!("API URL not configured"))?;

        // Build URL with API key as query parameter or use header
        let url_with_key = if url.contains("?key=") {
            url.clone()
        } else {
            format!("{}?key={}", url, api_key)
        };

        let response = client
            .post(&url_with_key)
            .header("Content-Type", "application/json")
            .json(&payload)
            .timeout(std::time::Duration::from_secs(self.config.timeout_seconds))
            .send()
            .await
            .with_context(|| "Failed to send request to Gemini")?;

        let status = response.status();
        if !status.is_success() {
            let error_text = response.text().await.unwrap_or_default();
            anyhow::bail!("Gemini API error ({}): {}", status, error_text);
        }

        let json: serde_json::Value = response.json().await
            .with_context(|| "Failed to parse Gemini response")?;

        let content = json["candidates"][0]["content"]["parts"][0]["text"]
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("No content in Gemini response"))?;

        Ok(content.to_string())
    }

    /// Call custom API (OpenAI-compatible)
    async fn call_custom(&self, prompt: &str) -> Result<String> {
        // Custom API should be OpenAI-compatible
        self.call_openai(prompt).await
    }

    /// Parse LLM response into PackSuggestion
    fn parse_suggestion(&self, response: &str) -> Result<PackSuggestion> {
        // Try to extract JSON from response (might have markdown code blocks)
        let json_str = if response.trim().starts_with('{') {
            response.trim()
        } else if let Some(start) = response.find("```json") {
            let end = response[start..].find("```").map(|i| start + i + 3);
            if let Some(end) = end {
                &response[start + 7..end - 3]
            } else {
                response.trim()
            }
        } else if let Some(start) = response.find('{') {
            let end = response.rfind('}').map(|i| i + 1);
            if let Some(end) = end {
                &response[start..end]
            } else {
                response.trim()
            }
        } else {
            anyhow::bail!("Could not find JSON in LLM response");
        };

        let parsed: serde_json::Value = serde_json::from_str(json_str)
            .with_context(|| "Failed to parse LLM response as JSON")?;

        Ok(PackSuggestion {
            name: parsed["name"]
                .as_str()
                .ok_or_else(|| anyhow::anyhow!("Missing 'name' in suggestion"))?
                .to_string(),
            description: parsed["description"]
                .as_str()
                .ok_or_else(|| anyhow::anyhow!("Missing 'description' in suggestion"))?
                .to_string(),
            pattern_rules: parsed["pattern_rules"].clone(),
            placeholder_names: parsed["placeholder_names"]
                .as_array()
                .ok_or_else(|| anyhow::anyhow!("Missing 'placeholder_names' in suggestion"))?
                .iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect(),
        })
    }
}

