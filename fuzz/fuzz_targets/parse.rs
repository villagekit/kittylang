//! Fuzz target: `kitty_parser::parse` never panics on any valid UTF-8
//! input. Bytes that are not UTF-8 are skipped, since the parser takes a
//! `&str`. The tree and the errors are dropped; a panic or an abort is the
//! only failure.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let _ = kitty_parser::parse(source);
    }
});
