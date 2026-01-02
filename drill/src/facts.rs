use serde::{Deserialize, Serialize};

/// Range in source code (0-based, inclusive start, exclusive end)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range {
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
}

/// File information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct File {
    pub path: String,
    pub language: String,
    pub hash: String,
}

/// Symbol kind
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
pub enum SymbolKind {
    Type,
    Function,
    Variable,
}

/// Symbol information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol {
    pub id: String,
    pub kind: SymbolKind,
    pub name: String,
    pub file: String,
    pub range: Range,
}

/// Member kind
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
pub enum MemberKind {
    Property,
    Method,
    Field,
}

/// Member information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Member {
    pub symbol_id: String,
    pub name: String,
    pub kind: MemberKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub type_ref: Option<String>,
    pub range: Range,
}

/// Relation kind
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
pub enum RelationKind {
    Imports,
    Calls,
    Extends,
    Implements,
}

/// Relation information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Relation {
    pub from_id: String,
    pub rel_kind: RelationKind,
    pub to_ref: String,
}

/// Annotation information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Annotation {
    pub symbol_id: String,
    pub key: String,
    pub value: String,
    pub range: Range,
}

/// Complete facts schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Facts {
    pub files: Vec<File>,
    pub symbols: Vec<Symbol>,
    pub members: Vec<Member>,
    pub relations: Vec<Relation>,
    pub annotations: Vec<Annotation>,
}

impl Facts {
    pub fn new() -> Self {
        Facts {
            files: Vec::new(),
            symbols: Vec::new(),
            members: Vec::new(),
            relations: Vec::new(),
            annotations: Vec::new(),
        }
    }

    /// Sort all collections to ensure deterministic output
    pub fn sort(&mut self) {
        self.files.sort();
        self.symbols.sort();
        self.members.sort();
        self.relations.sort();
        self.annotations.sort();
    }
}

impl Default for Facts {
    fn default() -> Self {
        Self::new()
    }
}
