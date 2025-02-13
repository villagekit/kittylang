use prettydiff::diff_lines;
use std::fs;
use std::panic::{catch_unwind, RefUnwindSafe};
use std::path::Path;

/// Represents a single test case extracted from a file.
#[derive(Debug, Clone)]
struct TestCase {
    pub title: String,
    pub source: String,
    pub expected: String,
}

/// Runs all tests found in files under the directory `corpus`.
///
/// Each test file is expected to be formatted like this:
///
/// ```text
/// ==================
/// Some test name
/// ==================
/// <source code>
/// ---
/// <expected output>
/// ```
///
/// There may be more than one test case in a file (each starting with a header block).
///
/// The caller provides a function `test_fn` that takes the source code and returns
/// the output (for example, a string representing the parse tree).
pub fn run_tests(test_fn: impl Fn(&str) -> String + RefUnwindSafe) {
    let mut has_failures = false;

    // Read all files in the "corpus" directory and process them.
    for entry_result in fs::read_dir("corpus").expect("Cannot read corpus directory") {
        let entry = entry_result.expect("Invalid directory entry");
        let path = entry
            .path()
            .canonicalize()
            .expect("Cannot canonicalize path");

        println!(
            "\n==== RUNNING TEST FILE: {:?} ====\n",
            path.file_stem().unwrap_or_default()
        );

        // If the file has at least one failing test, track that.
        if run_test_file(&test_fn, &path) {
            has_failures = true;
        }
    }

    // Fail the entire run if any test in any file failed.
    assert!(!has_failures, "Some tests failed");
}

/// Runs all test cases in a single file, returning `true` if any tests fail.
fn run_test_file(test_fn: &(impl Fn(&str) -> String + RefUnwindSafe), path: &Path) -> bool {
    let mut file_has_failures = false;

    // Read the file text.
    let file_text =
        fs::read_to_string(path).unwrap_or_else(|err| panic!("{}: {}", path.display(), err));

    // Parse the file into a vector of `TestCase` structs.
    let test_cases = parse_test_cases(&file_text);

    // Run each test case individually.
    for test_case in test_cases {
        if run_single_test_case(test_fn, &test_case, path) {
            file_has_failures = true;
        }
    }

    file_has_failures
}

/// Runs a single test case and prints diagnostic information if it fails.
/// Returns `true` if this test fails, otherwise `false`.
fn run_single_test_case(
    test_fn: &(impl Fn(&str) -> String + RefUnwindSafe),
    test_case: &TestCase,
    path: &Path,
) -> bool {
    println!("--- Test case {} ---", test_case.title);
    let result = catch_unwind(|| test_fn(&test_case.source));

    match result {
        Ok(actual) => {
            if actual != test_case.expected {
                println!(
                    "Test case {} failed in file {:?}:\n",
                    test_case.title,
                    path.file_name().unwrap_or_default()
                );
                println!(
                    "len, expected: {}, actual: {}",
                    test_case.expected.len(),
                    actual.len()
                );
                println!("Diff:\n{}\n", diff_lines(&test_case.expected, &actual));
                return true; // Test failed
            }
        }
        Err(_) => {
            println!(
                "Test case {} panicked in file {:?}",
                test_case.title,
                path.file_name().unwrap_or_default()
            );
            return true; // Test failed (panicked)
        }
    }

    false // Test succeeded
}

/// Parses the given file text into a list of `TestCase` items.
///
/// The expected file format is:
///
/// 1. A header block (three lines):
///    - A line of only '=' characters.
///    - A title (any text).
///    - Another line of only '=' characters.
/// 2. A blank line (optional in some cases).
/// 3. A source code block (one or more lines) that ends at a line exactly `---`.
/// 4. A line containing exactly `---`.
/// 5. An expected output block (until the next header block or EOF).
///
/// Multiple test cases may appear in a single file.
fn parse_test_cases(file_text: &str) -> Vec<TestCase> {
    let lines: Vec<&str> = file_text.lines().collect();
    let mut tests = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        // 1. A test case must start with a header line of "=" characters.
        if !is_header_line(lines[i]) {
            panic!(
                "Expected a header line (only '=' characters) at the start of a test case, but got: {}",
                lines[i],
            );
        }
        i += 1;

        // 2. The next line is the test case title.
        if i >= lines.len() {
            panic!("Missing title line after header line.");
        }
        let title = lines[i].to_string();
        i += 1;

        // 3. The next line should again be a header line of "=" characters.
        if i >= lines.len() || !is_header_line(lines[i]) {
            panic!(
                "Expected a closing header line after the title '{}'.",
                title
            );
        }
        i += 1;

        // 4. Gather the source lines until we encounter the delimiter "---".
        let mut source_lines = Vec::new();
        while i < lines.len() && lines[i].trim() != "---" {
            source_lines.push(lines[i]);
            i += 1;
        }
        if i >= lines.len() {
            panic!(
                "Expected '---' delimiter for test case '{}', but reached EOF.",
                title
            );
        }
        // Skip the "---" line
        i += 1;

        // 5. Gather the expected output until we see the next header line or EOF.
        let start_expected = i;
        while i < lines.len() && !is_header_line(lines[i]) {
            i += 1;
        }
        let expected_lines = &lines[start_expected..i];
        let expected = expected_lines.join("\n").trim().to_string();

        // Construct the source string with a trailing newline (if desired).
        let mut source = source_lines.join("\n");
        source.push('\n');

        tests.push(TestCase {
            title,
            source,
            expected,
        });
    }

    tests
}

/// Returns true if the given line (after trimming) is non-empty and consists only of '=' characters.
fn is_header_line(line: &str) -> bool {
    let trimmed = line.trim();
    !trimmed.is_empty() && trimmed.chars().all(|c| c == '=')
}
