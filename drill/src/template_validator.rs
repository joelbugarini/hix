/// Template validator module
/// Validates that generated templates follow Hix syntax rules
/// 
/// Note: Currently used in tests. Will be integrated with Story 18 (Hix renderer validation)

use regex::Regex;

/// Validation result
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub errors: Vec<String>,
}

#[allow(dead_code)]
impl ValidationResult {
    pub fn new() -> Self {
        ValidationResult {
            is_valid: true,
            errors: Vec::new(),
        }
    }

    pub fn add_error(&mut self, error: String) {
        self.is_valid = false;
        self.errors.push(error);
    }
}

impl Default for ValidationResult {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(dead_code)]
pub struct TemplateValidator;

#[allow(dead_code)]
impl TemplateValidator {
    pub fn new() -> Self {
        TemplateValidator
    }

    /// Validate that a template follows Hix syntax rules
    pub fn validate(&self, template: &str) -> ValidationResult {
        let mut result = ValidationResult::new();

        // Check 1: Tag balance ([[ matches ]])
        let open_tags = template.matches("[[").count();
        let close_tags = template.matches("]]").count();
        if open_tags != close_tags {
            result.add_error(format!(
                "Tag imbalance: {} opening tags '[[', {} closing tags ']]'",
                open_tags, close_tags
            ));
        }

        // Check 2: Extract and validate all tags
        let tag_pattern = Regex::new(r"\[\[([^\]]+)\]\]").unwrap();
        for cap in tag_pattern.captures_iter(template) {
            let tag_content = cap.get(1).unwrap().as_str().trim();
            if !self.is_valid_hix_tag(tag_content) {
                result.add_error(format!(
                    "Invalid Hix tag: '[[{}]]'",
                    tag_content
                ));
            }
        }

        // Check 3: Block structure validation
        self.validate_block_structure(template, &mut result);

        result
    }

    /// Check if a tag content is valid Hix syntax
    fn is_valid_hix_tag(&self, tag: &str) -> bool {
        let trimmed = tag.trim();

        // Valid patterns:
        // - model.className, model.name (model fields)
        // - prop.name, prop.type (property fields)
        // - prop (block start)
        // - /prop (block end)
        // - if prop.type=bool (conditional)
        // - else
        // - /if
        // - upper/lower/snake_case/etc. prop.name (functions)
        // - module_transform snake_case model.name (module transform)

        // Model fields
        if trimmed.starts_with("model.") {
            return true;
        }

        // Property fields
        if trimmed.starts_with("prop.") {
            return true;
        }

        // Block tags
        if trimmed == "prop" || trimmed == "/prop" {
            return true;
        }

        // Conditional tags
        if trimmed.starts_with("if ") {
            return true;
        }
        if trimmed == "else" || trimmed == "/if" {
            return true;
        }

        // Function calls
        if self.is_function_call(trimmed) {
            return true;
        }

        false
    }

    /// Check if a tag is a function call
    fn is_function_call(&self, tag: &str) -> bool {
        let parts: Vec<&str> = tag.split_whitespace().collect();
        
        if parts.is_empty() {
            return false;
        }

        let function_name = parts[0];

        // Valid Hix functions
        let valid_functions = [
            "upper", "lower", "snake_case", "kebab_case", "lowerFirst", "module_transform",
        ];

        if valid_functions.contains(&function_name) {
            // Function should have at least one argument
            return parts.len() >= 2;
        }

        false
    }

    /// Validate block structure ([[prop]]...[[/prop]], [[if]]...[[/if]])
    fn validate_block_structure(&self, template: &str, result: &mut ValidationResult) {
        // Extract all block tags
        let tag_pattern = Regex::new(r"\[\[([^\]]+)\]\]").unwrap();
        let mut prop_stack: Vec<usize> = Vec::new();
        let mut if_stack: Vec<usize> = Vec::new();
        let mut tag_positions: Vec<(usize, String)> = Vec::new();

        for cap in tag_pattern.captures_iter(template) {
            let tag_content = cap.get(1).unwrap().as_str().trim();
            let start = cap.get(0).unwrap().start();
            tag_positions.push((start, tag_content.to_string()));
        }

        for (pos, tag) in tag_positions {
            match tag.as_str() {
                "prop" => {
                    prop_stack.push(pos);
                }
                "/prop" => {
                    if prop_stack.is_empty() {
                        result.add_error(format!(
                            "Unmatched closing tag '[[/prop]]' at position {}",
                            pos
                        ));
                    } else {
                        prop_stack.pop();
                    }
                }
                tag if tag.starts_with("if ") => {
                    if_stack.push(pos);
                }
                "/if" => {
                    if if_stack.is_empty() {
                        result.add_error(format!(
                            "Unmatched closing tag '[[/if]]' at position {}",
                            pos
                        ));
                    } else {
                        if_stack.pop();
                    }
                }
                _ => {}
            }
        }

        // Check for unclosed blocks
        for pos in prop_stack {
            result.add_error(format!(
                "Unclosed '[[prop]]' block starting at position {}",
                pos
            ));
        }

        for pos in if_stack {
            result.add_error(format!(
                "Unclosed '[[if ...]]' block starting at position {}",
                pos
            ));
        }
    }

    /// Quick check if template is valid (returns bool only)
    pub fn is_valid(&self, template: &str) -> bool {
        self.validate(template).is_valid
    }
}

impl Default for TemplateValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_model_tag() {
        let validator = TemplateValidator::new();
        assert!(validator.is_valid("public class [[model.className]] { }"));
    }

    #[test]
    fn test_valid_prop_tags() {
        let validator = TemplateValidator::new();
        assert!(validator.is_valid("[[prop]]public [[prop.type]] [[prop.name]];[[/prop]]"));
    }

    #[test]
    fn test_valid_function_calls() {
        let validator = TemplateValidator::new();
        assert!(validator.is_valid("[[upper model.className]]"));
        assert!(validator.is_valid("[[lower prop.name]]"));
        assert!(validator.is_valid("[[snake_case prop.name]]"));
    }

    #[test]
    fn test_invalid_tag_balance() {
        let validator = TemplateValidator::new();
        let result = validator.validate("[[model.className");
        assert!(!result.is_valid);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_invalid_tag_name() {
        let validator = TemplateValidator::new();
        let result = validator.validate("[[Placeholder1]]");
        assert!(!result.is_valid);
    }

    #[test]
    fn test_unclosed_prop_block() {
        let validator = TemplateValidator::new();
        let result = validator.validate("[[prop]]public int x;");
        assert!(!result.is_valid);
    }

    #[test]
    fn test_valid_conditional() {
        let validator = TemplateValidator::new();
        assert!(validator.is_valid("[[if prop.type=bool]]bool[[else]]string[[/if]]"));
    }
}

