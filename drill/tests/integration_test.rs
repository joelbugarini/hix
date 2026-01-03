use std::fs;
use std::path::Path;
use std::process::Command;

#[test]
fn test_init_golden() {
    let fixture_dir = Path::new("tests/fixtures/sample-repo");
    let src_dir = fixture_dir.join("src");
    let packs_dir = fixture_dir.join("packs");
    
    // Clean up any previous test runs
    let hix_dir = src_dir.join(".hix");
    let hixdrill_dir = src_dir.join(".hixdrill");
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
    
    // Clean up any previous test runs (both .hix and .hixdrill)
    let hix_dir = src_dir.join(".hix");
    let hixdrill_dir = src_dir.join(".hixdrill");
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
    let hixdrill_dir = src_dir.join(".hixdrill");
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

