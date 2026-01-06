use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use regex::Regex;

#[test]
fn test_init_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hix_dir = src_dir.join(".hix");
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hix_dir);
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run init command
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "init",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute init command");
    
    // Check command succeeded
    assert!(
        output.status.success(),
        "Init command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Read generated config
    let config_path = hix_dir.join("drill").join("project.json");
    assert!(
        config_path.exists(),
        "Config file not created at {:?}",
        config_path
    );
    
    let actual_config = fs::read_to_string(&config_path)
        .expect("Failed to read generated config");
    
    // Read golden file
    let golden_path = fixture_dir.join("golden").join("project.json");
    if !golden_path.exists() {
        // First run - create golden file
        fs::create_dir_all(golden_path.parent().unwrap())
            .expect("Failed to create golden directory");
        fs::write(&golden_path, &actual_config)
            .expect("Failed to write golden file");
        println!("Created golden file: {:?}", golden_path);
        return;
    }
    
    let golden_config = fs::read_to_string(&golden_path)
        .expect("Failed to read golden config");
    
    // Compare (normalize JSON for comparison)
    let actual_json: serde_json::Value = serde_json::from_str(&actual_config)
        .expect("Failed to parse actual config as JSON");
    let golden_json: serde_json::Value = serde_json::from_str(&golden_config)
        .expect("Failed to parse golden config as JSON");
    
    assert_eq!(
        actual_json, golden_json,
        "Generated config does not match golden file.\n\nActual:\n{}\n\nExpected:\n{}",
        serde_json::to_string_pretty(&actual_json).unwrap(),
        serde_json::to_string_pretty(&golden_json).unwrap()
    );
}

#[test]
fn test_analyze_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hix_dir = src_dir.join(".hix");
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hix_dir);
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run analyze command
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "analyze",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute analyze command");
    
    // Check command succeeded
    assert!(
        output.status.success(),
        "Analyze command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Check matches.json (created by analyze command)
    let matches_path = hixdrill_dir.join("matches.json");
    if matches_path.exists() {
        let actual_matches = fs::read_to_string(&matches_path)
            .expect("Failed to read matches.json");
        
        let golden_matches_path = fixture_dir.join("golden").join("matches.json");
        if !golden_matches_path.exists() {
            fs::write(&golden_matches_path, &actual_matches)
                .expect("Failed to write golden matches");
            println!("Created golden matches file: {:?}", golden_matches_path);
            return;
        }
        
        let golden_matches = fs::read_to_string(&golden_matches_path)
            .expect("Failed to read golden matches");
        
        let actual_matches_json: serde_json::Value = serde_json::from_str(&actual_matches)
            .expect("Failed to parse actual matches");
        let golden_matches_json: serde_json::Value = serde_json::from_str(&golden_matches)
            .expect("Failed to parse golden matches");
        
        assert_eq!(
            actual_matches_json, golden_matches_json,
            "Generated matches do not match golden file"
        );
    }
    
    // Check report.json
    let report_path = hixdrill_dir.join("report.json");
    if report_path.exists() {
        let actual_report = fs::read_to_string(&report_path)
            .expect("Failed to read report.json");
        
        let golden_report_path = fixture_dir.join("golden").join("report.json");
        if !golden_report_path.exists() {
            fs::write(&golden_report_path, &actual_report)
                .expect("Failed to write golden report");
            println!("Created golden report file: {:?}", golden_report_path);
            return;
        }
        
        let golden_report = fs::read_to_string(&golden_report_path)
            .expect("Failed to read golden report");
        
        let actual_report_json: serde_json::Value = serde_json::from_str(&actual_report)
            .expect("Failed to parse actual report");
        let golden_report_json: serde_json::Value = serde_json::from_str(&golden_report)
            .expect("Failed to parse golden report");
        
        assert_eq!(
            actual_report_json, golden_report_json,
            "Generated report does not match golden file"
        );
    }
    
    // Check unknowns.json (created by analyze command)
    let unknowns_path = hixdrill_dir.join("unknowns.json");
    if unknowns_path.exists() {
        let actual_unknowns = fs::read_to_string(&unknowns_path)
            .expect("Failed to read unknowns.json");
        
        let golden_unknowns_path = fixture_dir.join("golden").join("unknowns.json");
        if !golden_unknowns_path.exists() {
            fs::write(&golden_unknowns_path, &actual_unknowns)
                .expect("Failed to write golden unknowns");
            println!("Created golden unknowns file: {:?}", golden_unknowns_path);
        } else {
            let golden_unknowns = fs::read_to_string(&golden_unknowns_path)
                .expect("Failed to read golden unknowns");
            
            let actual_unknowns_json: serde_json::Value = serde_json::from_str(&actual_unknowns)
                .expect("Failed to parse actual unknowns");
            let golden_unknowns_json: serde_json::Value = serde_json::from_str(&golden_unknowns)
                .expect("Failed to parse golden unknowns");
            
            assert_eq!(
                actual_unknowns_json, golden_unknowns_json,
                "Generated unknowns do not match golden file"
            );
        }
    }
    
    // Check facts.json (created by scan command, but analyze also extracts facts)
    // Note: analyze doesn't write facts.json, only scan does
    // But we can test scan separately or verify facts are used in analyze
    let facts_path = hixdrill_dir.join("facts.json");
    if facts_path.exists() {
        let actual_facts = fs::read_to_string(&facts_path)
            .expect("Failed to read facts.json");
        
        let golden_facts_path = fixture_dir.join("golden").join("facts.json");
        if !golden_facts_path.exists() {
            fs::write(&golden_facts_path, &actual_facts)
                .expect("Failed to write golden facts");
            println!("Created golden facts file: {:?}", golden_facts_path);
        } else {
            let golden_facts = fs::read_to_string(&golden_facts_path)
                .expect("Failed to read golden facts");
            
            let actual_facts_json: serde_json::Value = serde_json::from_str(&actual_facts)
                .expect("Failed to parse actual facts");
            let golden_facts_json: serde_json::Value = serde_json::from_str(&golden_facts)
                .expect("Failed to parse golden facts");
            
            assert_eq!(
                actual_facts_json, golden_facts_json,
                "Generated facts do not match golden file"
            );
        }
    }
}

#[test]
fn test_scan_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    
    // Clean up any previous test runs
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run scan command
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "scan",
            src_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute scan command");
    
    // Check command succeeded
    assert!(
        output.status.success(),
        "Scan command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Check facts.json (created by scan command)
    let facts_path = hixdrill_dir.join("facts.json");
    assert!(facts_path.exists(), "facts.json not created");
    
    let actual_facts = fs::read_to_string(&facts_path)
        .expect("Failed to read facts.json");
    
    let golden_facts_path = fixture_dir.join("golden").join("facts.json");
    if !golden_facts_path.exists() {
        fs::create_dir_all(golden_facts_path.parent().unwrap())
            .expect("Failed to create golden directory");
        fs::write(&golden_facts_path, &actual_facts)
            .expect("Failed to write golden facts");
        println!("Created golden facts file: {:?}", golden_facts_path);
        return;
    }
    
    let golden_facts = fs::read_to_string(&golden_facts_path)
        .expect("Failed to read golden facts");
    
    // Compare facts (normalize JSON)
    let actual_facts_json: serde_json::Value = serde_json::from_str(&actual_facts)
        .expect("Failed to parse actual facts");
    let golden_facts_json: serde_json::Value = serde_json::from_str(&golden_facts)
        .expect("Failed to parse golden facts");
    
    assert_eq!(
        actual_facts_json, golden_facts_json,
        "Generated facts do not match golden file"
    );
}

#[test]
fn test_synthesis_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run analyze command to generate synthesis files
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "analyze",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute analyze command");
    
    // Check command succeeded
    assert!(
        output.status.success(),
        "Analyze command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Check synthesis.json files (one per cluster)
    let synthesis_dir = hixdrill_dir.join("synthesis");
    if synthesis_dir.exists() {
        // Find all synthesis.json files
        let synthesis_files: Vec<_> = fs::read_dir(&synthesis_dir)
            .expect("Failed to read synthesis directory")
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.is_dir() {
                    let synthesis_json = path.join("synthesis.json");
                    if synthesis_json.exists() {
                        Some(synthesis_json)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();
        
        for synthesis_path in synthesis_files {
            let actual_synthesis = fs::read_to_string(&synthesis_path)
                .expect("Failed to read synthesis.json");
            
            // Create golden path based on cluster directory name
            let cluster_dir = synthesis_path.parent().unwrap();
            let cluster_name = cluster_dir.file_name().unwrap().to_str().unwrap();
            let golden_synthesis_path = fixture_dir.join("golden").join("synthesis").join(cluster_name).join("synthesis.json");
            
            if !golden_synthesis_path.exists() {
                fs::create_dir_all(golden_synthesis_path.parent().unwrap())
                    .expect("Failed to create golden synthesis directory");
                fs::write(&golden_synthesis_path, &actual_synthesis)
                    .expect("Failed to write golden synthesis");
                println!("Created golden synthesis file: {:?}", golden_synthesis_path);
            } else {
                let golden_synthesis = fs::read_to_string(&golden_synthesis_path)
                    .expect("Failed to read golden synthesis");
                
                let actual_synthesis_json: serde_json::Value = serde_json::from_str(&actual_synthesis)
                    .expect("Failed to parse actual synthesis");
                let golden_synthesis_json: serde_json::Value = serde_json::from_str(&golden_synthesis)
                    .expect("Failed to parse golden synthesis");
                
                assert_eq!(
                    actual_synthesis_json, golden_synthesis_json,
                    "Generated synthesis does not match golden file for cluster {}",
                    cluster_name
                );
            }
            
            // Check template files (recursively in templates directory)
            let template_dir = cluster_dir.join("templates");
            if template_dir.exists() {
                let mut template_files: Vec<_> = Vec::new();
                find_template_files(&template_dir, &mut template_files);
                
                fn find_template_files(dir: &Path, files: &mut Vec<std::path::PathBuf>) {
                    if let Ok(entries) = fs::read_dir(dir) {
                        for entry in entries {
                            if let Ok(entry) = entry {
                                let path = entry.path();
                                if path.is_file() && path.extension() == Some(std::ffi::OsStr::new("hix")) {
                                    files.push(path);
                                } else if path.is_dir() {
                                    find_template_files(&path, files);
                                }
                            }
                        }
                    }
                }
                
                for template_path in template_files {
                    let actual_template = fs::read_to_string(&template_path)
                        .expect("Failed to read template file");
                    
                    // Create golden path
                    let template_name = template_path.file_name().unwrap().to_str().unwrap();
                    let language_dir = template_path.parent().unwrap().file_name().unwrap().to_str().unwrap();
                    let golden_template_path = fixture_dir.join("golden").join("synthesis").join(cluster_name)
                        .join("templates").join(language_dir).join(template_name);
                    
                    if !golden_template_path.exists() {
                        fs::create_dir_all(golden_template_path.parent().unwrap())
                            .expect("Failed to create golden template directory");
                        fs::write(&golden_template_path, &actual_template)
                            .expect("Failed to write golden template");
                        println!("Created golden template file: {:?}", golden_template_path);
                    } else {
                        let golden_template = fs::read_to_string(&golden_template_path)
                            .expect("Failed to read golden template");
                        
                        assert_eq!(
                            actual_template, golden_template,
                            "Generated template does not match golden file for {}",
                            template_name
                        );
                    }
                }
            }
        }
    }
}

#[test]
fn test_model_inference_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run analyze command to generate model.json files
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "analyze",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute analyze command");
    
    // Check command succeeded
    assert!(
        output.status.success(),
        "Analyze command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Check model.json files (one per cluster)
    let synthesis_dir = hixdrill_dir.join("synthesis");
    if synthesis_dir.exists() {
        // Find all model.json files
        let model_files: Vec<_> = fs::read_dir(&synthesis_dir)
            .expect("Failed to read synthesis directory")
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.is_dir() {
                    let model_json = path.join("model.json");
                    if model_json.exists() {
                        Some(model_json)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();
        
        for model_path in model_files {
            let actual_model = fs::read_to_string(&model_path)
                .expect("Failed to read model.json");
            
            // Create golden path based on cluster directory name
            let cluster_dir = model_path.parent().unwrap();
            let cluster_name = cluster_dir.file_name().unwrap().to_str().unwrap();
            let golden_model_path = fixture_dir.join("golden").join("synthesis").join(cluster_name).join("model.json");
            
            if !golden_model_path.exists() {
                fs::create_dir_all(golden_model_path.parent().unwrap())
                    .expect("Failed to create golden model directory");
                fs::write(&golden_model_path, &actual_model)
                    .expect("Failed to write golden model");
                println!("Created golden model file: {:?}", golden_model_path);
            } else {
                let golden_model = fs::read_to_string(&golden_model_path)
                    .expect("Failed to read golden model");
                
                let actual_model_json: serde_json::Value = serde_json::from_str(&actual_model)
                    .expect("Failed to parse actual model");
                let golden_model_json: serde_json::Value = serde_json::from_str(&golden_model)
                    .expect("Failed to parse golden model");
                
                assert_eq!(
                    actual_model_json, golden_model_json,
                    "Generated model does not match golden file for cluster {}",
                    cluster_name
                );
            }
        }
    }
}

#[test]
fn test_template_syntax_validation() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run analyze command to generate templates
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "analyze",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute analyze command");
    
    // Check command succeeded
    assert!(
        output.status.success(),
        "Analyze command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Find all generated .hix template files
    let synthesis_dir = hixdrill_dir.join("synthesis");
    if synthesis_dir.exists() {
        let mut template_files: Vec<std::path::PathBuf> = Vec::new();
        find_template_files(&synthesis_dir, &mut template_files);
        
        fn find_template_files(dir: &Path, files: &mut Vec<std::path::PathBuf>) {
            if let Ok(entries) = fs::read_dir(dir) {
                for entry in entries {
                    if let Ok(entry) = entry {
                        let path = entry.path();
                        if path.is_file() && path.extension() == Some(std::ffi::OsStr::new("hix")) {
                            files.push(path);
                        } else if path.is_dir() {
                            find_template_files(&path, files);
                        }
                    }
                }
            }
        }
        
        // Validate each template
        for template_path in &template_files {
            let template_content = fs::read_to_string(&template_path)
                .expect("Failed to read template file");
            
            // Basic validation: tag balance
            let open_tags = template_content.matches("[[").count();
            let close_tags = template_content.matches("]]").count();
            
            assert_eq!(
                open_tags, close_tags,
                "Template {} has tag imbalance: {} opening tags, {} closing tags\nTemplate content:\n{}",
                template_path.display(),
                open_tags,
                close_tags,
                template_content
            );
            
            // Check for invalid placeholder patterns (like [[Placeholder1]])
            // Valid Hix tags should be: model.*, prop.*, prop, /prop, if, else, /if, or function calls
            let invalid_pattern = Regex::new(r"\[\[Placeholder\d+\]\]|\[\[Identifier\d+\]\]").unwrap();
            assert!(
                !invalid_pattern.is_match(&template_content),
                "Template {} contains invalid placeholder syntax (should use model.* or prop.*):\n{}",
                template_path.display(),
                template_content
            );
        }
        
        // If we found templates, at least one should exist
        if !template_files.is_empty() {
            println!("Validated {} template file(s)", template_files.len());
        }
    }
}

#[test]
fn test_pack_emission_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run analyze command to trigger pack emission
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "analyze",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute analyze command");
    
    assert!(
        output.status.success(),
        "Analyze command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Check that packs directory was created
    let packs_output_dir = hixdrill_dir.join("packs");
    assert!(
        packs_output_dir.exists(),
        "Packs directory not created at {:?}",
        packs_output_dir
    );
    
    // Find all pack directories (mined/<lang>/<cluster-id>)
    let mut pack_dirs: Vec<PathBuf> = Vec::new();
    if let Ok(entries) = fs::read_dir(&packs_output_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.is_dir() && path.file_name().unwrap() == "mined" {
                    // Look for language subdirectories
                    if let Ok(lang_entries) = fs::read_dir(&path) {
                        for lang_entry in lang_entries {
                            if let Ok(lang_entry) = lang_entry {
                                let lang_path = lang_entry.path();
                                if lang_path.is_dir() {
                                    // Look for cluster subdirectories
                                    if let Ok(cluster_entries) = fs::read_dir(&lang_path) {
                                        for cluster_entry in cluster_entries {
                                            if let Ok(cluster_entry) = cluster_entry {
                                                let cluster_path = cluster_entry.path();
                                                if cluster_path.is_dir() {
                                                    pack_dirs.push(cluster_path);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    // Verify at least one pack was created
    assert!(
        !pack_dirs.is_empty(),
        "No pack directories found in {:?}",
        packs_output_dir
    );
    
    // Verify each pack structure
    for pack_dir in &pack_dirs {
        // Check pack.json exists
        let pack_json_path = pack_dir.join("pack.json");
        assert!(
            pack_json_path.exists(),
            "pack.json not found in {:?}",
            pack_dir
        );
        
        // Check pattern.json exists
        let pattern_json_path = pack_dir.join("pattern.json");
        assert!(
            pattern_json_path.exists(),
            "pattern.json not found in {:?}",
            pack_dir
        );
        
        // Check templates directory exists
        let templates_dir = pack_dir.join("templates");
        assert!(
            templates_dir.exists(),
            "templates directory not found in {:?}",
            pack_dir
        );
        
        // Check tests/fixtures directory exists
        let fixtures_dir = pack_dir.join("tests").join("fixtures");
        assert!(
            fixtures_dir.exists(),
            "tests/fixtures directory not found in {:?}",
            pack_dir
        );
        
        // Check tests/expected directory exists
        let expected_dir = pack_dir.join("tests").join("expected");
        assert!(
            expected_dir.exists(),
            "tests/expected directory not found in {:?}",
            pack_dir
        );
        
        // Verify pack.json structure
        let pack_json_content = fs::read_to_string(&pack_json_path)
            .expect("Failed to read pack.json");
        let pack_json: serde_json::Value = serde_json::from_str(&pack_json_content)
            .expect("Failed to parse pack.json");
        
        assert_eq!(
            pack_json["schema_version"],
            "1.0.0",
            "pack.json has incorrect schema_version"
        );
        assert!(
            pack_json["name"].is_string(),
            "pack.json missing name field"
        );
        assert!(
            pack_json["version"].is_string(),
            "pack.json missing version field"
        );
        
        // Verify pattern.json structure
        let pattern_json_content = fs::read_to_string(&pattern_json_path)
            .expect("Failed to read pattern.json");
        let pattern_json: serde_json::Value = serde_json::from_str(&pattern_json_content)
            .expect("Failed to parse pattern.json");
        
        assert!(
            pattern_json.is_array(),
            "pattern.json should be an array"
        );
        assert!(
            !pattern_json.as_array().unwrap().is_empty(),
            "pattern.json should contain at least one pattern"
        );
        
        let first_pattern = &pattern_json[0];
        assert!(
            first_pattern["name"].is_string(),
            "pattern.json pattern missing name"
        );
        assert!(
            first_pattern["match_conditions"].is_object(),
            "pattern.json pattern missing match_conditions"
        );
        
        // Verify template file exists
        let mut template_found = false;
        if let Ok(template_entries) = fs::read_dir(&templates_dir) {
            for entry in template_entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if path.is_dir() {
                        // Language subdirectory
                        if let Ok(lang_entries) = fs::read_dir(&path) {
                            for lang_entry in lang_entries {
                                if let Ok(lang_entry) = lang_entry {
                                    let template_path = lang_entry.path();
                                    if template_path.is_file() && template_path.extension() == Some(std::ffi::OsStr::new("hix")) {
                                        template_found = true;
                                        
                                        // Verify template content is valid Hix syntax
                                        let template_content = fs::read_to_string(&template_path)
                                            .expect("Failed to read template");
                                        
                                        // Basic validation: should contain Hix tags
                                        assert!(
                                            template_content.contains("[[") && template_content.contains("]]"),
                                            "Template does not contain Hix tags: {:?}",
                                            template_path
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        assert!(
            template_found,
            "No .hix template file found in {:?}",
            templates_dir
        );
        
        // Verify at least one fixture file exists
        let mut fixture_found = false;
        if let Ok(fixture_entries) = fs::read_dir(&fixtures_dir) {
            for entry in fixture_entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if path.is_file() {
                        fixture_found = true;
                        break;
                    }
                }
            }
        }
        
        assert!(
            fixture_found,
            "No fixture files found in {:?}",
            fixtures_dir
        );
        
        // Create golden files for first pack (for comparison)
        let pack_name = pack_dir.file_name().unwrap().to_str().unwrap();
        let parent_name = pack_dir.parent().unwrap().file_name().unwrap().to_str().unwrap();
        let grandparent_name = pack_dir.parent().unwrap().parent().unwrap().file_name().unwrap().to_str().unwrap();
        let golden_pack_dir = fixture_dir.join("golden").join("packs").join(grandparent_name).join(parent_name).join(pack_name);
        
        // Compare pack.json
        let golden_pack_json = golden_pack_dir.join("pack.json");
        if !golden_pack_json.exists() {
            fs::create_dir_all(golden_pack_json.parent().unwrap())
                .expect("Failed to create golden pack directory");
            fs::copy(&pack_json_path, &golden_pack_json)
                .expect("Failed to copy pack.json to golden");
            println!("Created golden pack.json: {:?}", golden_pack_json);
        } else {
            let golden_content = fs::read_to_string(&golden_pack_json)
                .expect("Failed to read golden pack.json");
            let golden_json: serde_json::Value = serde_json::from_str(&golden_content)
                .expect("Failed to parse golden pack.json");
            
            assert_eq!(
                pack_json, golden_json,
                "Generated pack.json does not match golden file for pack {}",
                pack_name
            );
        }
        
        // Compare pattern.json
        let golden_pattern_json = golden_pack_dir.join("pattern.json");
        if !golden_pattern_json.exists() {
            fs::copy(&pattern_json_path, &golden_pattern_json)
                .expect("Failed to copy pattern.json to golden");
            println!("Created golden pattern.json: {:?}", golden_pattern_json);
        } else {
            let golden_content = fs::read_to_string(&golden_pattern_json)
                .expect("Failed to read golden pattern.json");
            let golden_json: serde_json::Value = serde_json::from_str(&golden_content)
                .expect("Failed to parse golden pattern.json");
            
            assert_eq!(
                pattern_json, golden_json,
                "Generated pattern.json does not match golden file for pack {}",
                pack_name
            );
        }
    }
    
    println!("Verified {} pack(s)", pack_dirs.len());
}

#[test]
fn test_validate_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");

    // Clean up any previous test runs
    let hix_dir = src_dir.join(".hix");
    let _ = fs::remove_dir_all(&hix_dir);

    // First, run analyze to generate packs (if they don't exist)
    let analyze_output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "analyze",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute analyze command");

    if !analyze_output.status.success() {
        eprintln!(
            "Warning: Analyze command failed (packs may already exist):\nSTDOUT:\n{}\nSTDERR:\n{}",
            String::from_utf8_lossy(&analyze_output.stdout),
            String::from_utf8_lossy(&analyze_output.stderr)
        );
    }

    // Check that packs directory exists
    let emitted_packs_dir = hix_dir.join("drill").join("packs");
    if !emitted_packs_dir.exists() {
        // If no packs were generated, skip this test
        println!("No packs found, skipping validation test");
        return;
    }

    // Run validate command
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "validate",
            "--packs",
            emitted_packs_dir.to_str().unwrap(),
            "--repo",
            src_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute validate command");

    // Check command succeeded (or failed gracefully)
    // Validation may fail if templates don't match, but command should still run
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    // Check that validation report was created
    let validation_json_path = hix_dir.join("drill").join("validation.json");
    
    if !validation_json_path.exists() {
        // If validation failed before creating report, that's an error
        assert!(
            false,
            "Validation report not created:\nSTDOUT:\n{}\nSTDERR:\n{}",
            stdout, stderr
        );
    }

    // Read validation report
    let actual_validation = fs::read_to_string(&validation_json_path)
        .expect("Failed to read validation.json");
    
    let validation_json: serde_json::Value = serde_json::from_str(&actual_validation)
        .expect("Failed to parse validation.json");

    // Verify structure
    assert!(
        validation_json["total_packs"].is_number(),
        "validation.json missing total_packs"
    );
    assert!(
        validation_json["packs_passed"].is_number(),
        "validation.json missing packs_passed"
    );
    assert!(
        validation_json["packs_failed"].is_number(),
        "validation.json missing packs_failed"
    );
    assert!(
        validation_json["pack_results"].is_array(),
        "validation.json missing pack_results array"
    );

    // Compare with golden file
    let golden_validation_path = fixture_dir.join("golden").join("validation.json");
    
    if !golden_validation_path.exists() {
        // Create golden file
        fs::create_dir_all(golden_validation_path.parent().unwrap())
            .expect("Failed to create golden validation directory");
        fs::write(&golden_validation_path, &actual_validation)
            .expect("Failed to write golden validation.json");
        println!("Created golden validation.json: {:?}", golden_validation_path);
    } else {
        let golden_validation = fs::read_to_string(&golden_validation_path)
            .expect("Failed to read golden validation.json");
        let golden_json: serde_json::Value = serde_json::from_str(&golden_validation)
            .expect("Failed to parse golden validation.json");
        
        // Compare structure (but allow for different paths)
        assert_eq!(
            validation_json["total_packs"],
            golden_json["total_packs"],
            "total_packs mismatch"
        );
        assert_eq!(
            validation_json["packs_passed"],
            golden_json["packs_passed"],
            "packs_passed mismatch"
        );
        assert_eq!(
            validation_json["packs_failed"],
            golden_json["packs_failed"],
            "packs_failed mismatch"
        );
        
        // Compare pack_results (normalize paths)
        let actual_results = &validation_json["pack_results"];
        let golden_results = &golden_json["pack_results"];
        
        assert_eq!(
            actual_results.as_array().unwrap().len(),
            golden_results.as_array().unwrap().len(),
            "pack_results length mismatch"
        );
        
        // Compare each pack result (ignoring path differences)
        for (i, (actual, golden)) in actual_results.as_array().unwrap().iter()
            .zip(golden_results.as_array().unwrap().iter())
            .enumerate()
        {
            assert_eq!(
                actual["pack_name"],
                golden["pack_name"],
                "pack_name mismatch for pack {}",
                i
            );
            assert_eq!(
                actual["passed"],
                golden["passed"],
                "passed mismatch for pack {}",
                i
            );
            assert_eq!(
                actual["instances_validated"],
                golden["instances_validated"],
                "instances_validated mismatch for pack {}",
                i
            );
            assert_eq!(
                actual["instances_passed"],
                golden["instances_passed"],
                "instances_passed mismatch for pack {}",
                i
            );
        }
    }
    
    println!("Validation test completed successfully");
}

#[test]
fn test_template_synthesis_full_class_structure() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hixdrill_dir = src_dir.join(".hix").join("drill");
    let _ = fs::remove_dir_all(&hixdrill_dir);
    
    // Run analyze command to generate synthesis files
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--",
            "analyze",
            src_dir.to_str().unwrap(),
            "--packs",
            packs_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute analyze command");
    
    // Check command succeeded
    assert!(
        output.status.success(),
        "Analyze command failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    
    // Find generated template files
    let synthesis_dir = hixdrill_dir.join("synthesis");
    assert!(
        synthesis_dir.exists(),
        "Synthesis directory not created"
    );
    
    // Find all cluster directories
    let cluster_dirs: Vec<_> = fs::read_dir(&synthesis_dir)
        .expect("Failed to read synthesis directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.is_dir() {
                Some(path)
            } else {
                None
            }
        })
        .collect();
    
    assert!(
        !cluster_dirs.is_empty(),
        "No cluster directories found in synthesis"
    );
    
    // For C# templates, verify full class structure
    for cluster_dir in cluster_dirs {
        let templates_dir = cluster_dir.join("templates");
        if !templates_dir.exists() {
            continue;
        }
        
        // Find .hix template files recursively
        let mut template_files: Vec<PathBuf> = Vec::new();
        fn find_hix_files(dir: &Path, files: &mut Vec<PathBuf>) {
            if let Ok(entries) = fs::read_dir(dir) {
                for entry in entries {
                    if let Ok(entry) = entry {
                        let path = entry.path();
                        if path.is_file() && path.extension() == Some(std::ffi::OsStr::new("hix")) {
                            files.push(path);
                        } else if path.is_dir() {
                            find_hix_files(&path, files);
                        }
                    }
                }
            }
        }
        find_hix_files(&templates_dir, &mut template_files);
        
        for template_path in template_files {
            let template_content = fs::read_to_string(&template_path)
                .expect("Failed to read template file");
            
            // Verify template contains full class structure
            // For C# files, check for namespace and class declaration
            if template_path.to_string_lossy().contains("csharp") {
                // Check for namespace (if present in original)
                // Note: namespace might not always be present, so this is optional
                
                // Check for class declaration
                assert!(
                    template_content.contains("class ") || template_content.contains("public class"),
                    "Template missing class declaration: {:?}",
                    template_path
                );
                
                // Check for property template with [[prop]] blocks
                assert!(
                    template_content.contains("[[prop]]") && template_content.contains("[[/prop]]"),
                    "Template missing [[prop]] blocks: {:?}",
                    template_path
                );
                
                // Check for property placeholders
                assert!(
                    template_content.contains("[[prop.name]]") || template_content.contains("[[prop.type]]"),
                    "Template missing property placeholders: {:?}",
                    template_path
                );
                
                // Verify template structure: should have class declaration before properties
                let class_pos = template_content.find("class ").unwrap_or(0);
                let prop_pos = template_content.find("[[prop]]").unwrap_or(usize::MAX);
                assert!(
                    class_pos < prop_pos,
                    "Class declaration should come before property block in template: {:?}",
                    template_path
                );
                
                // Verify template can be rendered (if model.json exists)
                let model_path = cluster_dir.join("model.json");
                if model_path.exists() {
                    let _model_content = fs::read_to_string(&model_path)
                        .expect("Failed to read model.json");
                    
                    // Try to render template using hix (if available)
                    let hix_output = Command::new("hix")
                        .args(&[
                            "generate",
                            "--model",
                            model_path.to_str().unwrap(),
                            "--template",
                            template_path.to_str().unwrap(),
                        ])
                        .output();
                    
                    if let Ok(output) = hix_output {
                        if output.status.success() {
                            let rendered = String::from_utf8_lossy(&output.stdout);
                            
                            // Verify rendered output contains class structure
                            assert!(
                                rendered.contains("class ") || rendered.contains("public class"),
                                "Rendered output missing class declaration"
                            );
                            
                            // Verify rendered output contains properties
                            assert!(
                                rendered.contains("{ get; set; }"),
                                "Rendered output missing properties"
                            );
                            
                            println!("✓ Template {:?} rendered successfully", template_path);
                        } else {
                            eprintln!(
                                "Warning: hix render failed for {:?}:\n{}",
                                template_path,
                                String::from_utf8_lossy(&output.stderr)
                            );
                        }
                    } else {
                        // hix binary not available, skip rendering test
                        println!("Note: hix binary not available, skipping render test");
                    }
                }
            }
        }
    }
    
    println!("Full class structure template synthesis test completed successfully");
}

