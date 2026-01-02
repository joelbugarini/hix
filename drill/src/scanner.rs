use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileInfo {
    pub path: String,
    pub language: Option<String>,
    pub hash: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Cache {
    pub files: Vec<FileInfo>,
}

pub struct Scanner {
    cache_dir: PathBuf,
    ignore_dirs: Vec<&'static str>,
}

impl Scanner {
    pub fn new(base_path: &Path) -> Self {
        let cache_dir = base_path.join(".hixdrill");
        Scanner {
            cache_dir,
            ignore_dirs: vec![
                ".git",
                "node_modules",
                "bin",
                "obj",
                "target",
                ".hixdrill",
                ".stack-work",
            ],
        }
    }

    pub fn scan(&self, repo_path: &Path) -> Result<Vec<FileInfo>> {
        let mut files = Vec::new();
        let cache_path = self.cache_dir.join("cache.json");
        let old_cache: HashMap<String, FileInfo> = self.load_cache(&cache_path)?;

        for entry in WalkDir::new(repo_path)
            .into_iter()
            .filter_entry(|e| self.should_process_entry(e))
        {
            let entry = match entry {
                Ok(e) => e,
                Err(e) => {
                    // Skip files we can't access (permissions, etc.)
                    eprintln!("Warning: Skipping entry due to error: {}", e);
                    continue;
                }
            };
            let path = entry.path();

            if path.is_file() {
                if let Some(language) = self.detect_language(path) {
                    let path_str = path.to_string_lossy().to_string();
                    
                    // Try to compute hash, skip if we can't read the file
                    let hash = match self.compute_hash(path) {
                        Ok(h) => h,
                        Err(e) => {
                            eprintln!("Warning: Skipping {} due to error: {}", path_str, e);
                            continue;
                        }
                    };

                    // Check if file changed
                    let file_info = if let Some(cached) = old_cache.get(&path_str) {
                        if cached.hash == hash {
                            // File unchanged, use cached info
                            cached.clone()
                        } else {
                            // File changed, create new info
                            FileInfo {
                                path: path_str,
                                language: Some(language),
                                hash,
                            }
                        }
                    } else {
                        // New file
                        FileInfo {
                            path: path_str,
                            language: Some(language),
                            hash,
                        }
                    };

                    files.push(file_info);
                }
            }
        }

        // Stable sort by path
        files.sort_by(|a, b| a.path.cmp(&b.path));

        // Save cache
        self.save_cache(&cache_path, &files)?;

        Ok(files)
    }

    fn should_process_entry(&self, entry: &walkdir::DirEntry) -> bool {
        let path = entry.path();
        
        // Check if any path component matches ignore list
        for component in path.components() {
            if let std::path::Component::Normal(name) = component {
                if let Some(name_str) = name.to_str() {
                    // Check exact match
                    if self.ignore_dirs.contains(&name_str) {
                        return false;
                    }
                    // Also check if component starts with ignored directory name
                    // (handles cases like .stack-work vs stack-work)
                    for ignore_dir in &self.ignore_dirs {
                        if name_str == *ignore_dir || name_str.starts_with(ignore_dir) {
                            return false;
                        }
                    }
                }
            }
        }
        
        true
    }

    fn detect_language(&self, path: &Path) -> Option<String> {
        path.extension()
            .and_then(|ext| ext.to_str())
            .and_then(|ext| {
                match ext.to_lowercase().as_str() {
                    // Common languages
                    "rs" => Some("rust".to_string()),
                    "py" => Some("python".to_string()),
                    "js" | "jsx" => Some("javascript".to_string()),
                    "ts" | "tsx" => Some("typescript".to_string()),
                    "java" => Some("java".to_string()),
                    "cs" => Some("csharp".to_string()),
                    "cpp" | "cc" | "cxx" | "c++" => Some("cpp".to_string()),
                    "c" => Some("c".to_string()),
                    "go" => Some("go".to_string()),
                    "rb" => Some("ruby".to_string()),
                    "php" => Some("php".to_string()),
                    "swift" => Some("swift".to_string()),
                    "kt" => Some("kotlin".to_string()),
                    "scala" => Some("scala".to_string()),
                    "hs" => Some("haskell".to_string()),
                    "ml" | "mli" => Some("ocaml".to_string()),
                    "clj" | "cljs" => Some("clojure".to_string()),
                    "erl" | "hrl" => Some("erlang".to_string()),
                    "ex" | "exs" => Some("elixir".to_string()),
                    "lua" => Some("lua".to_string()),
                    "r" => Some("r".to_string()),
                    "sh" | "bash" => Some("shell".to_string()),
                    "zsh" => Some("zsh".to_string()),
                    "fish" => Some("fish".to_string()),
                    "ps1" => Some("powershell".to_string()),
                    "sql" => Some("sql".to_string()),
                    "html" | "htm" => Some("html".to_string()),
                    "css" => Some("css".to_string()),
                    "scss" | "sass" => Some("scss".to_string()),
                    "json" => Some("json".to_string()),
                    "xml" => Some("xml".to_string()),
                    "yaml" | "yml" => Some("yaml".to_string()),
                    "toml" => Some("toml".to_string()),
                    "md" => Some("markdown".to_string()),
                    "txt" => Some("text".to_string()),
                    _ => None,
                }
            })
    }

    fn compute_hash(&self, path: &Path) -> Result<String> {
        let contents = fs::read(path)
            .with_context(|| format!("Failed to read file: {}", path.display()))?;
        let mut hasher = Sha256::new();
        hasher.update(&contents);
        let hash = hasher.finalize();
        Ok(format!("{:x}", hash))
    }

    fn load_cache(&self, cache_path: &Path) -> Result<HashMap<String, FileInfo>> {
        if !cache_path.exists() {
            return Ok(HashMap::new());
        }

        let contents = fs::read_to_string(cache_path)
            .with_context(|| format!("Failed to read cache: {}", cache_path.display()))?;
        
        let cache: Cache = serde_json::from_str(&contents)
            .context("Failed to parse cache JSON")?;

        Ok(cache
            .files
            .into_iter()
            .map(|f| (f.path.clone(), f))
            .collect())
    }

    fn save_cache(&self, cache_path: &Path, files: &[FileInfo]) -> Result<()> {
        // Create cache directory if it doesn't exist
        if let Some(parent) = cache_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create cache directory: {}", parent.display()))?;
        }

        let cache = Cache {
            files: files.to_vec(),
        };

        let json = serde_json::to_string_pretty(&cache)
            .context("Failed to serialize cache")?;

        fs::write(cache_path, json)
            .with_context(|| format!("Failed to write cache: {}", cache_path.display()))?;

        Ok(())
    }
}

