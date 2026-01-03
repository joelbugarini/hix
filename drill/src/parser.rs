use tree_sitter::{Parser, Tree};
use tree_sitter_typescript as ts_tsx;
use tree_sitter_python as py;
use tree_sitter_c_sharp as csharp;
use tree_sitter_html as html;

pub struct ParseResult {
    pub tree: Option<Tree>,
    pub error: Option<String>,
}

pub struct ParserRegistry;

impl ParserRegistry {
    pub fn new() -> Self {
        // All languages in 0.23 use LANGUAGE constants (LanguageFn)
        // We handle them directly in parse() method
        // TypeScript/TSX: ts_tsx::LANGUAGE_TYPESCRIPT, ts_tsx::LANGUAGE_TSX
        // Python: py::LANGUAGE
        // C#: csharp::LANGUAGE
        // HTML: html::LANGUAGE
        ParserRegistry
    }

    pub fn parse(&self, content: &str, language: &str) -> ParseResult {
        let mut parser = Parser::new();
        
        // In 0.23, all languages use LanguageFn constants
        // LanguageFn implements Into<Language>, so we use .into()
        // Example from tree-sitter-python source: parser.set_language(&LANGUAGE.into())
        let set_lang_result = match language {
            "typescript" => parser.set_language(&ts_tsx::LANGUAGE_TYPESCRIPT.into()),
            "tsx" => parser.set_language(&ts_tsx::LANGUAGE_TSX.into()),
            "python" => parser.set_language(&py::LANGUAGE.into()),
            "csharp" => parser.set_language(&csharp::LANGUAGE.into()),
            "html" => parser.set_language(&html::LANGUAGE.into()),
            _ => {
                return ParseResult {
                    tree: None,
                    error: Some(format!("No parser available for language: {}", language)),
                };
            }
        };
        
        if let Err(e) = set_lang_result {
            return ParseResult {
                tree: None,
                error: Some(format!("Failed to set language {}: {:?}", language, e)),
            };
        }

        match parser.parse(content, None) {
            Some(tree) => {
                // Check for parse errors
                if tree.root_node().has_error() {
                    ParseResult {
                        tree: Some(tree),
                        error: Some("Parse tree contains errors".to_string()),
                    }
                } else {
                    ParseResult {
                        tree: Some(tree),
                        error: None,
                    }
                }
            }
            None => ParseResult {
                tree: None,
                error: Some("Failed to parse file".to_string()),
            },
        }
    }
}

pub struct ParseSummary {
    pub total_files: usize,
    pub parsed: usize,
    pub failed: usize,
    pub no_parser: usize,
}

impl ParseSummary {
    pub fn new() -> Self {
        ParseSummary {
            total_files: 0,
            parsed: 0,
            failed: 0,
            no_parser: 0,
        }
    }

    pub fn add_result(&mut self, result: &ParseResult) {
        self.total_files += 1;
        match result {
            ParseResult { tree: Some(_), error: None } => {
                self.parsed += 1;
            }
            ParseResult { tree: Some(_), error: Some(_) } => {
                self.failed += 1;
            }
            ParseResult { tree: None, error: Some(_) } => {
                self.failed += 1;
            }
            _ => {
                self.no_parser += 1;
            }
        }
    }
}

impl Default for ParseSummary {
    fn default() -> Self {
        Self::new()
    }
}

