use prettydiff::diff_lines;
use std::fs;
use std::panic::RefUnwindSafe;

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
    let mut did_any_test_fail = false;

    for entry in fs::read_dir("corpus").expect("Cannot read corpus directory") {
        let entry = entry.expect("Invalid directory entry");
        let test_path = entry
            .path()
            .canonicalize()
            .expect("Cannot canonicalize path");

        println!(
            "\n==== RUNNING TEST FILE: {:?} ====\n",
            test_path.file_stem().unwrap_or_default()
        );

        let file_text = fs::read_to_string(&test_path)
            .unwrap_or_else(|err| panic!("{}: {}", test_path.display(), err));

        // Parse the file into a vector of (source, expected) test cases.
        let test_cases = parse_test_cases(&file_text);

        for TestCase {
            title,
            source,
            expected,
        } in test_cases.into_iter()
        {
            println!("--- Test case {} ---", title);
            let result = std::panic::catch_unwind(|| test_fn(&source));
            match result {
                Ok(actual) => {
                    if actual != expected {
                        did_any_test_fail = true;
                        println!("Test case {} failed in file {:?}:\n", title, test_path);
                        println!(
                            "len, expected: {}, actual: {}",
                            expected.len(),
                            actual.len()
                        );
                        println!("Diff:\n{}\n", diff_lines(&expected, &actual));

                        // println!("Source:\n{}\n", source);
                        // println!("Expected:\n{}\n", expected);
                        // println!("Got:\n{}\n", actual);
                    }
                }
                Err(_) => {
                    did_any_test_fail = true;
                    println!("Test case {} panicked in file {:?}", title, test_path);
                }
            }
        }
    }

    assert!(!did_any_test_fail, "Some tests failed");
}

#[derive(Debug, Clone)]
struct TestCase {
    pub title: String,
    pub source: String,
    pub expected: String,
}

/// Returns a vector of (source, expected) pairs by parsing the file text.
///
/// The expected file format is as follows:
///
/// 1. A header block (three lines):
///    - A line of only '=' characters.
///    - A title (any text).
///    - Another line of only '=' characters.
/// 2. A blank line.
/// 3. A source code block (one or more lines) that goes until a line that is exactly `---`.
/// 4. A line containing exactly `---`.
/// 5. An expected output block (until the next header block or EOF).
///
/// If there is more than one test case in the file, then after the expected output
/// of one test case the next header block begins.
fn parse_test_cases(file: &str) -> Vec<TestCase> {
    let mut tests = Vec::new();
    let mut lines = file.lines().peekable();

    while let Some(line) = lines.peek() {
        // A test case is expected to start with a header block.
        if !is_header_line(line) {
            panic!(
                "Expected a header line (a line of '=' characters) at the beginning of a test case, but got: {}", line
            );
        }

        // Consume the header block: three lines.
        let header_line1 = lines.next().unwrap();
        let title = lines
            .next()
            .unwrap_or_else(|| panic!("Missing title line after header line: {}", header_line1))
            .to_string();
        let header_line2 = lines
            .next()
            .unwrap_or_else(|| panic!("Missing closing header line after title"));
        if !is_header_line(header_line1) || !is_header_line(header_line2) {
            panic!("Header block malformed");
        }

        // Collect source code lines until we hit a delimiter line that is exactly `---`
        let mut source_lines = Vec::new();
        while let Some(l) = lines.peek() {
            if l.trim() == "---" {
                break;
            }
            source_lines.push(lines.next().unwrap());
        }
        let mut source = source_lines.join("\n");
        source.push('\n');

        // Expect a line that is exactly `---` as a delimiter.
        match lines.next() {
            Some(delim) if delim.trim() == "---" => { /* OK */ }
            Some(other) => panic!("Expected delimiter '---' but found: {}", other),
            None => panic!("Expected delimiter '---' but reached end of file"),
        }

        // Collect expected output lines until we hit the next header block or EOF.
        let mut expected_lines = Vec::new();
        while let Some(&l) = lines.peek() {
            if is_header_line(l) {
                // Start of a new test case.
                break;
            }
            expected_lines.push(lines.next().unwrap());
        }
        let expected = expected_lines.join("\n").trim().to_string();

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
