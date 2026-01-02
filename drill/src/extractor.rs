use crate::facts::{
    Facts, File, Member, MemberKind, Range, Relation, RelationKind, Symbol, SymbolKind,
};
use crate::parser::ParseResult;
use crate::scanner::FileInfo;
use std::collections::HashMap;
use tree_sitter::{Node, Tree};

pub struct Extractor;

impl Extractor {
    pub fn new() -> Self {
        Extractor
    }

    /// Extract facts from parsed files
    pub fn extract_facts(
        &self,
        file_infos: &[FileInfo],
        parse_results: &HashMap<String, ParseResult>,
        file_contents: &HashMap<String, String>,
    ) -> Facts {
        let mut facts = Facts::new();
        let mut symbol_id_counter = 0;

        for file_info in file_infos {
            // Add file to facts
            facts.files.push(File {
                path: file_info.path.clone(),
                language: file_info.language.clone().unwrap_or_else(|| "unknown".to_string()),
                hash: file_info.hash.clone(),
            });

            // Extract symbols from parsed tree if available
            if let Some(parse_result) = parse_results.get(&file_info.path) {
                if let Some(tree) = &parse_result.tree {
                    if let Some(lang) = &file_info.language {
                        if let Some(content) = file_contents.get(&file_info.path) {
                            match lang.as_str() {
                                "typescript" | "tsx" => {
                                    self.extract_typescript_facts(
                                        tree,
                                        &file_info.path,
                                        content,
                                        &mut facts,
                                        &mut symbol_id_counter,
                                    );
                                }
                                "python" => {
                                    self.extract_python_facts(
                                        tree,
                                        &file_info.path,
                                        content,
                                        &mut facts,
                                        &mut symbol_id_counter,
                                    );
                                }
                                "csharp" => {
                                    self.extract_csharp_facts(
                                        tree,
                                        &file_info.path,
                                        content,
                                        &mut facts,
                                        &mut symbol_id_counter,
                                    );
                                }
                                "html" => {
                                    self.extract_html_facts(
                                        tree,
                                        &file_info.path,
                                        content,
                                        &mut facts,
                                        &mut symbol_id_counter,
                                    );
                                }
                                _ => {
                                    // Other languages can be added later
                                }
                            }
                        }
                    }
                }
            }
        }

        // Sort all collections for deterministic output
        facts.sort();
        facts
    }

    fn extract_typescript_facts(
        &self,
        tree: &Tree,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let root_node = tree.root_node();
        self.walk_typescript_node(root_node, file_path, content, facts, symbol_id_counter);
    }

    fn walk_typescript_node(
        &self,
        node: Node,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let node_type = node.kind();

        match node_type {
            // TypeScript/TSX class declarations
            "class_declaration" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Type,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });

                    // Extract class members
                    if let Some(body_node) = node.child_by_field_name("body") {
                        self.extract_class_members(
                            body_node,
                            &symbol_id,
                            file_path,
                            content,
                            facts,
                            symbol_id_counter,
                        );
                    }

                    // Extract extends/implements relations
                    if let Some(heritage_node) = node.child_by_field_name("superclass") {
                        if let Some(super_name) = heritage_node.child_by_field_name("value") {
                            let super_name_text = super_name.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                            facts.relations.push(Relation {
                                from_id: symbol_id.clone(),
                                rel_kind: RelationKind::Extends,
                                to_ref: super_name_text,
                            });
                        }
                    }
                }
            }
            // TypeScript interface declarations
            "interface_declaration" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Type,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });

                    // Extract interface members
                    if let Some(body_node) = node.child_by_field_name("body") {
                        self.extract_class_members(
                            body_node,
                            &symbol_id,
                            file_path,
                            content,
                            facts,
                            symbol_id_counter,
                        );
                    }
                }
            }
            // TypeScript function declarations
            "function_declaration" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Function,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });
                }
            }
            // TypeScript variable declarations (const, let, var)
            "lexical_declaration" | "variable_declaration" => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if child.kind() == "variable_declarator" {
                        if let Some(name_node) = child.child_by_field_name("name") {
                            let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                            let symbol_id = format!("sym_{}", symbol_id_counter);
                            *symbol_id_counter += 1;

                            facts.symbols.push(Symbol {
                                id: symbol_id.clone(),
                                kind: SymbolKind::Variable,
                                name: name.clone(),
                                file: file_path.to_string(),
                                range: self.node_to_range(&name_node),
                            });
                        }
                    }
                }
            }
            _ => {}
        }

        // Recursively walk children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.walk_typescript_node(child, file_path, content, facts, symbol_id_counter);
        }
    }

    fn extract_class_members(
        &self,
        body_node: Node,
        symbol_id: &str,
        _file_path: &str,
        content: &str,
        facts: &mut Facts,
        _symbol_id_counter: &mut u32,
    ) {
        let mut cursor = body_node.walk();
        for child in body_node.children(&mut cursor) {
            match child.kind() {
                "property_signature" | "method_signature" | "method_definition" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                        let kind = if child.kind().contains("method") {
                            MemberKind::Method
                        } else {
                            MemberKind::Property
                        };

                        let type_ref = child
                            .child_by_field_name("type")
                            .and_then(|t| t.utf8_text(content.as_bytes()).ok())
                            .map(|s| s.to_string());

                        facts.members.push(Member {
                            symbol_id: symbol_id.to_string(),
                            name,
                            kind,
                            type_ref,
                            range: self.node_to_range(&name_node),
                        });
                    }
                }
                _ => {}
            }
        }
    }

    fn extract_python_facts(
        &self,
        tree: &Tree,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let root_node = tree.root_node();
        self.walk_python_node(root_node, file_path, content, facts, symbol_id_counter);
    }

    fn walk_python_node(
        &self,
        node: Node,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let node_type = node.kind();

        match node_type {
            // Python class definitions
            "class_definition" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Type,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });

                    // Extract class members (methods, attributes)
                    if let Some(body_node) = node.child_by_field_name("body") {
                        self.extract_python_class_members(
                            body_node,
                            &symbol_id,
                            file_path,
                            content,
                            facts,
                            symbol_id_counter,
                        );
                    }

                    // Extract inheritance (superclasses)
                    if let Some(superclasses_node) = node.child_by_field_name("superclasses") {
                        let mut cursor = superclasses_node.walk();
                        for superclass in superclasses_node.children(&mut cursor) {
                            // In Python, superclasses are typically identifier nodes
                            let super_name = superclass.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                            if !super_name.is_empty() {
                                facts.relations.push(Relation {
                                    from_id: symbol_id.clone(),
                                    rel_kind: RelationKind::Extends,
                                    to_ref: super_name,
                                });
                            }
                        }
                    }
                }
            }
            // Python function definitions
            "function_definition" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Function,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });
                }
            }
            // Python assignments (variables)
            "assignment" => {
                if let Some(target_node) = node.child_by_field_name("left") {
                    if let Some(name_node) = target_node.child_by_field_name("value") {
                        let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                        let symbol_id = format!("sym_{}", symbol_id_counter);
                        *symbol_id_counter += 1;

                        facts.symbols.push(Symbol {
                            id: symbol_id.clone(),
                            kind: SymbolKind::Variable,
                            name: name.clone(),
                            file: file_path.to_string(),
                            range: self.node_to_range(&name_node),
                        });
                    }
                }
            }
            _ => {}
        }

        // Recursively walk children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.walk_python_node(child, file_path, content, facts, symbol_id_counter);
        }
    }

    fn extract_python_class_members(
        &self,
        body_node: Node,
        symbol_id: &str,
        _file_path: &str,
        content: &str,
        facts: &mut Facts,
        _symbol_id_counter: &mut u32,
    ) {
        let mut cursor = body_node.walk();
        for child in body_node.children(&mut cursor) {
            match child.kind() {
                "function_definition" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();

                        facts.members.push(Member {
                            symbol_id: symbol_id.to_string(),
                            name,
                            kind: MemberKind::Method,
                            type_ref: None, // Python doesn't have explicit type annotations in all cases
                            range: self.node_to_range(&name_node),
                        });
                    }
                }
                "assignment" => {
                    if let Some(target_node) = child.child_by_field_name("left") {
                        if let Some(name_node) = target_node.child_by_field_name("value") {
                            let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();

                            facts.members.push(Member {
                                symbol_id: symbol_id.to_string(),
                                name,
                                kind: MemberKind::Field,
                                type_ref: None,
                                range: self.node_to_range(&name_node),
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn extract_csharp_facts(
        &self,
        tree: &Tree,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let root_node = tree.root_node();
        self.walk_csharp_node(root_node, file_path, content, facts, symbol_id_counter);
    }

    fn walk_csharp_node(
        &self,
        node: Node,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let node_type = node.kind();

        match node_type {
            // C# class declarations
            "class_declaration" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Type,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });

                    // Extract class members
                    if let Some(body_node) = node.child_by_field_name("body") {
                        self.extract_csharp_class_members(
                            body_node,
                            &symbol_id,
                            file_path,
                            content,
                            facts,
                            symbol_id_counter,
                        );
                    }

                    // Extract base class (inheritance)
                    if let Some(base_list_node) = node.child_by_field_name("base_list") {
                        let mut cursor = base_list_node.walk();
                        for base in base_list_node.children(&mut cursor) {
                            if base.kind() == "type_identifier" {
                                let base_name = base.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                                if !base_name.is_empty() {
                                    facts.relations.push(Relation {
                                        from_id: symbol_id.clone(),
                                        rel_kind: RelationKind::Extends,
                                        to_ref: base_name,
                                    });
                                }
                            }
                        }
                    }
                }
            }
            // C# interface declarations
            "interface_declaration" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Type,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });

                    // Extract interface members
                    if let Some(body_node) = node.child_by_field_name("body") {
                        self.extract_csharp_class_members(
                            body_node,
                            &symbol_id,
                            file_path,
                            content,
                            facts,
                            symbol_id_counter,
                        );
                    }
                }
            }
            // C# method declarations (standalone methods)
            "method_declaration" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Function,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });
                }
            }
            // C# property declarations (standalone)
            "property_declaration" => {
                if let Some(name_node) = node.child_by_field_name("name") {
                    let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    let symbol_id = format!("sym_{}", symbol_id_counter);
                    *symbol_id_counter += 1;

                    facts.symbols.push(Symbol {
                        id: symbol_id.clone(),
                        kind: SymbolKind::Variable,
                        name: name.clone(),
                        file: file_path.to_string(),
                        range: self.node_to_range(&name_node),
                    });
                }
            }
            // C# field declarations (standalone)
            "field_declaration" => {
                if let Some(declarator_node) = node.child_by_field_name("declarator") {
                    if let Some(name_node) = declarator_node.child_by_field_name("name") {
                        let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                        let symbol_id = format!("sym_{}", symbol_id_counter);
                        *symbol_id_counter += 1;

                        facts.symbols.push(Symbol {
                            id: symbol_id.clone(),
                            kind: SymbolKind::Variable,
                            name: name.clone(),
                            file: file_path.to_string(),
                            range: self.node_to_range(&name_node),
                        });
                    }
                }
            }
            _ => {}
        }

        // Recursively walk children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.walk_csharp_node(child, file_path, content, facts, symbol_id_counter);
        }
    }

    fn extract_csharp_class_members(
        &self,
        body_node: Node,
        symbol_id: &str,
        _file_path: &str,
        content: &str,
        facts: &mut Facts,
        _symbol_id_counter: &mut u32,
    ) {
        let mut cursor = body_node.walk();
        for child in body_node.children(&mut cursor) {
            match child.kind() {
                "method_declaration" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();

                        let type_ref = child
                            .child_by_field_name("type")
                            .and_then(|t| t.utf8_text(content.as_bytes()).ok())
                            .map(|s| s.to_string());

                        facts.members.push(Member {
                            symbol_id: symbol_id.to_string(),
                            name,
                            kind: MemberKind::Method,
                            type_ref,
                            range: self.node_to_range(&name_node),
                        });
                    }
                }
                "property_declaration" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();

                        let type_ref = child
                            .child_by_field_name("type")
                            .and_then(|t| t.utf8_text(content.as_bytes()).ok())
                            .map(|s| s.to_string());

                        facts.members.push(Member {
                            symbol_id: symbol_id.to_string(),
                            name,
                            kind: MemberKind::Property,
                            type_ref,
                            range: self.node_to_range(&name_node),
                        });
                    }
                }
                "field_declaration" => {
                    if let Some(declarator_node) = child.child_by_field_name("declarator") {
                        if let Some(name_node) = declarator_node.child_by_field_name("name") {
                            let name = name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();

                            let type_ref = child
                                .child_by_field_name("type")
                                .and_then(|t| t.utf8_text(content.as_bytes()).ok())
                                .map(|s| s.to_string());

                            facts.members.push(Member {
                                symbol_id: symbol_id.to_string(),
                                name,
                                kind: MemberKind::Field,
                                type_ref,
                                range: self.node_to_range(&name_node),
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn extract_html_facts(
        &self,
        tree: &Tree,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let root_node = tree.root_node();
        self.walk_html_node(root_node, file_path, content, facts, symbol_id_counter);
    }

    fn walk_html_node(
        &self,
        node: Node,
        file_path: &str,
        content: &str,
        facts: &mut Facts,
        symbol_id_counter: &mut u32,
    ) {
        let node_type = node.kind();

        match node_type {
            // HTML start tags - treat custom elements (non-standard tags) as symbols
            "start_tag" => {
                if let Some(tag_name_node) = node.child_by_field_name("tag_name") {
                    let tag_name = tag_name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                    
                    // Only treat custom elements (not standard HTML tags) as symbols
                    // Custom elements typically contain a hyphen or start with uppercase
                    if tag_name.contains('-') || (tag_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)) {
                        let symbol_id = format!("sym_{}", symbol_id_counter);
                        *symbol_id_counter += 1;

                        facts.symbols.push(Symbol {
                            id: symbol_id.clone(),
                            kind: SymbolKind::Type, // Treat custom elements as types
                            name: tag_name.clone(),
                            file: file_path.to_string(),
                            range: self.node_to_range(&tag_name_node),
                        });

                        // Extract attributes as members
                        let mut cursor = node.walk();
                        for child in node.children(&mut cursor) {
                            if child.kind() == "attribute" {
                                if let Some(attr_name_node) = child.child_by_field_name("name") {
                                    let attr_name = attr_name_node.utf8_text(content.as_bytes()).unwrap_or("").to_string();
                                    
                                    facts.members.push(Member {
                                        symbol_id: symbol_id.clone(),
                                        name: attr_name,
                                        kind: MemberKind::Property,
                                        type_ref: None,
                                        range: self.node_to_range(&attr_name_node),
                                    });
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        // Recursively walk children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.walk_html_node(child, file_path, content, facts, symbol_id_counter);
        }
    }

    fn node_to_range(&self, node: &Node) -> Range {
        let start = node.start_position();
        let end = node.end_position();
        Range {
            start_line: start.row as u32,
            start_column: start.column as u32,
            end_line: end.row as u32,
            end_column: end.column as u32,
        }
    }
}

impl Default for Extractor {
    fn default() -> Self {
        Self::new()
    }
}

