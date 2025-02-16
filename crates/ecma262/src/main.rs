use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};
use std::thread;

use serde::Deserialize;
use yaml_rust::Yaml;
use yaml_rust::YamlLoader;

#[derive(Debug)]
struct TestMetadata {
    negative: Option<NegativeMetadata>,
    features: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct NegativeMetadata {
    phase: String,
    r#type: String,
}

#[derive(Debug)]
struct Stats {
    total_tests: usize,
    compilation_errors: usize,
    runtime_errors: usize,
    panic_locations: HashMap<String, usize>,
}

impl Stats {
    fn new() -> Self {
        Stats {
            total_tests: 0,
            compilation_errors: 0,
            runtime_errors: 0,
            panic_locations: HashMap::new(),
        }
    }

    fn increment_total(&mut self) {
        self.total_tests += 1;
    }

    fn add_result(&mut self, result: i32, panic_location: Option<String>) {
        match result {
            100 => self.compilation_errors += 1,
            101 => self.runtime_errors += 1,
            _ => (),
        }
        if let Some(location) = panic_location {
            *self.panic_locations.entry(location).or_insert(0) += 1;
        }
    }
}

fn main() -> io::Result<()> {
    let test262_dir = "test262";
    let js_interpreter = "target/debug/wasm";

    // Validate directories and files
    let test_dir = Path::new(&test262_dir).join("test");
    let harness_dir = Path::new(&test262_dir).join("harness");
    let sta_js = fs::read_to_string(harness_dir.join("sta.js"))?;
    let assert_js = fs::read_to_string(harness_dir.join("assert.js"))?;
    let harness_content = sta_js + &assert_js;

    // Load excluded features
    let excluded_features = parse_features_file(&test262_dir)?;

    // Get test files
    let test_files = if let Some(arg) = env::args().nth(1) {
        let test_path = if Path::new(&arg).is_absolute() {
            PathBuf::from(arg)
        } else {
            Path::new(&test262_dir).join(arg)
        };
        if test_path.exists() {
            vec![test_path]
        } else {
            eprintln!("Error: Test file not found: {}", test_path.display());
            return Ok(());
        }
    } else {
        find_test_files(&test_dir, &excluded_features)?
    };

    // Initialize stats
    let stats = Arc::new(Mutex::new(Stats::new()));
    let output_mutex = Arc::new(Mutex::new(()));

    // Create thread pool
    let thread_count = 6;
    let mut handles = vec![];
    let test_queue = Arc::new(Mutex::new(test_files));

    for _ in 0..thread_count {
        let test_queue = Arc::clone(&test_queue);
        let stats = Arc::clone(&stats);
        let output_mutex = Arc::clone(&output_mutex);
        let harness_content = harness_content.clone();
        let js_interpreter = js_interpreter;
        let test262_dir = test262_dir;

        let handle = thread::spawn(move || {
            while let Some(test_file) = test_queue.lock().unwrap().pop() {
                stats.lock().unwrap().increment_total();
                let relative_path = test_file.strip_prefix(&test262_dir).unwrap_or(&test_file);

                let (result, panic_location) = run_test(&test_file, &harness_content, &js_interpreter);

                // Thread-safe output
                let _lock = output_mutex.lock().unwrap();
                print!("Running test262/{}... ", relative_path.display());
                io::stdout().flush().unwrap();
                if let Some(location) = &panic_location {
                    println!("🔥 (Panic at {})", location);
                } else {
                    match result {
                        0 => println!("✅ Successful"),
                        100 => println!("❌ (Compilation Error)"),
                        101 => println!("❌ (Runtime Error)"),
                        _ => println!("⏭️  (Skipped)"),
                    }
                }

                stats.lock().unwrap().add_result(result, panic_location);
            }
        });
        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }

    // Get final stats
    let stats = Arc::try_unwrap(stats).unwrap().into_inner().unwrap();
    let total_tests = stats.total_tests;
    let compilation_errors = stats.compilation_errors;
    let runtime_errors = stats.runtime_errors;
    let panic_locations = stats.panic_locations;

    // Calculate statistics
    let total_failures = compilation_errors + runtime_errors;
    let compilation_rate = (compilation_errors as f64 / total_tests as f64 * 100.0).round();
    let runtime_rate = (runtime_errors as f64 / total_tests as f64 * 100.0).round();
    let success_rate = ((total_tests - total_failures) as f64 / total_tests as f64 * 100.0).round();

    // Print summary
    println!("\nTest Summary:");
    println!("Total tests: {}", total_tests);
    println!("Compilation errors: {} ({}%)", compilation_errors, compilation_rate);
    println!("Runtime errors: {} ({}%)", runtime_errors, runtime_rate);
    println!("Total failures: {}", total_failures);
    println!("Success rate: {}%", success_rate);

    if !panic_locations.is_empty() {
        println!("\nPanic Locations (sorted by frequency):");
        let mut sorted_locations: Vec<_> = panic_locations.iter().collect();
        sorted_locations.sort_by(|a, b| b.1.cmp(a.1));
        for (location, count) in sorted_locations {
            println!("  {}: {} occurrences", location, count);
        }
    }

    Ok(())
}

fn parse_features_file(test262_dir: &str) -> io::Result<Vec<String>> {
    let features_path = Path::new(test262_dir).join("features.txt");
    let content = fs::read_to_string(features_path)?;
    Ok(content
        .lines()
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(|line| line.trim().to_string())
        .collect())
}

fn find_test_files(test_dir: &Path, excluded_features: &[String]) -> io::Result<Vec<PathBuf>> {
    let mut test_files = vec![];
    for entry in fs::read_dir(test_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            test_files.extend(find_test_files(&path, excluded_features)?);
        } else if path.extension().and_then(|s| s.to_str()) == Some("js") {
            if !should_skip_test(&path, excluded_features)? {
                test_files.push(path);
            }
        }
    }
    Ok(test_files)
}

fn should_skip_test(test_path: &Path, excluded_features: &[String]) -> io::Result<bool> {
    let content = fs::read_to_string(test_path)?;
    if content.contains("/*---") && content.contains("flags: [optional]") {
        return Ok(true);
    }
    if let Some(features) = extract_features(&content) {
        if features.iter().any(|f| excluded_features.contains(f)) {
            return Ok(true);
        }
    }
    Ok(false)
}

fn extract_features(content: &str) -> Option<Vec<String>> {
    if let Some(caps) = content
        .split("/*---")
        .nth(1)?
        .split("---*/")
        .next()?
        .lines()
        .find(|line| line.contains("features:"))
    {
        let features = caps
            .split('[')
            .nth(1)?
            .split(']')
            .next()?
            .split(',')
            .map(|s| s.trim().to_string())
            .collect();
        Some(features)
    } else {
        None
    }
}

fn run_test(test_path: &Path, harness_content: &str, js_interpreter: &str) -> (i32, Option<String>) {
    let test_content = fs::read_to_string(test_path).unwrap();
    let metadata = parse_test_metadata(&test_content);
    let temp_file = Path::new("/tmp").join(format!("test262_{}.js", rand::random::<u32>()));

    fs::write(&temp_file, format!("{}\n{}", harness_content, test_content)).unwrap();

    let output = Command::new(js_interpreter)
        .arg(&temp_file)
        .output()
        .unwrap();

    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    fs::remove_file(&temp_file).unwrap_or(());

    if stdout.contains("JAWSM parsing error") {
        return (if metadata.negative.map_or(false, |n| n.phase == "parse" && n.r#type == "SyntaxError") {
            0
        } else {
            100
        }, None);
    }

    if let Some(caps) = stderr
        .lines()
        .find(|line| line.contains("thread 'main' panicked at"))
    {
        let panic_location = caps.split("src/").nth(1).unwrap_or("unknown");
        return (exit_code, Some(panic_location.to_string()));
    }

    (exit_code, None)
}

fn parse_test_metadata(content: &str) -> TestMetadata {
    if let Some(yaml_str) = content
        .split("/*---")
        .nth(1)
        .unwrap()
        .split("---*/")
        .next()
    {
        if let Ok(docs) = YamlLoader::load_from_str(yaml_str) {
            if let Some(doc) = docs.first() {
                let negative = doc["negative"].as_hash().map(|h| NegativeMetadata {
                    phase: h.get(&Yaml::String("phase".to_string())).and_then(Yaml::as_str).unwrap_or("").to_string(),
                    r#type: h.get(&Yaml::String("type".to_string())).and_then(Yaml::as_str).unwrap_or("").to_string(),
                });
                let features = doc["features"]
                    .as_vec()
                    .map(|v| v.iter().filter_map(|y| y.as_str().map(|s| s.to_string())).collect())
                    .unwrap_or_default();
                return TestMetadata { negative, features };
            }
        }
    }
    TestMetadata {
        negative: None,
        features: vec![],
    }
}