use std::fs;
use std::path::Path;
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

