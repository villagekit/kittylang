//! The examples test: every program in `examples/` parsed end to end, with
//! the parse errors it produces recorded per file.
//!
//! The expectations are the fixture guard for the grammar. Each grammar
//! change that is meant to shrink them is reviewed and then recorded with
//! `UPDATE_EXPECT=1`. The six files are named here on purpose, so a new
//! example is added to this list by hand.
//!
//! Every error that remains sits at a site an open design plan owns, and
//! the test says which: each file names its sites as line ranges with
//! the plan's prefix, and each error is rendered with that prefix beside
//! it. An error at a line no plan owns fails the test, as does a site no
//! error falls in, so the table stays true as the grammar and the
//! examples move.

use std::ops::RangeInclusive;

use expect_test::{expect, Expect};

use crate::parse;

/// A site an open design plan owns: the source lines, one-based and
/// inclusive, and the plan's prefix. A site is one line, or the head of
/// a block with the lines its error cascade reaches.
type Site = (RangeInclusive<u32>, &'static str);

/// Parses `source` and renders each parse error on its own line, through
/// `ParseError`'s `Display`, followed by the prefix of the design plan
/// whose site holds the line the error starts on. An error at the end of
/// the input counts on the last line with text.
///
/// The expectation is compared first, so `UPDATE_EXPECT` records the
/// errors in one pass; an error no site holds is rendered as such, and
/// then it fails the test, as does a site no error falls in.
fn check_errors(source: &str, sites: &[Site], expected: Expect) {
    let parsed = parse(source);
    let mut hit = vec![false; sites.len()];
    let mut unowned = Vec::new();
    let mut actual = String::new();
    for error in &parsed.errors {
        let line = line_of(source, u32::from(error.range().start()) as usize);
        match sites.iter().position(|(lines, _)| lines.contains(&line)) {
            Some(index) => {
                hit[index] = true;
                let (_, prefix) = sites[index];
                actual.push_str(&format!("{error}  # design plan {prefix}\n"));
            }
            None => {
                if unowned.last() != Some(&line) {
                    unowned.push(line);
                }
                actual.push_str(&format!("{error}  # no design plan owns line {line}\n"));
            }
        }
    }
    expected.assert_eq(&actual);
    assert!(
        unowned.is_empty(),
        "no design plan owns the errors at lines {unowned:?}"
    );
    for ((lines, prefix), hit) in sites.iter().zip(hit) {
        assert!(
            hit,
            "design plan {prefix} owns lines {lines:?}, but no error is there"
        );
    }
}

/// The one-based line that `offset` falls on. An offset at the end of
/// the input lands on the last line with text, whatever trails it.
fn line_of(source: &str, offset: usize) -> u32 {
    let before = if offset == source.len() {
        source.trim_end()
    } else {
        &source[..offset]
    };
    before.matches('\n').count() as u32 + 1
}

#[test]
#[should_panic(expected = "no design plan owns the errors at lines [2]")]
fn an_error_no_plan_owns_fails() {
    check_errors(
        "struct S\n  prop x = 1\n",
        &[],
        expect![[r#"
            error at 18..19: expected ‘:’, but found ‘=’  # no design plan owns line 2
            error at 20..21: expected type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’, but found number  # no design plan owns line 2
        "#]],
    );
}

#[test]
#[should_panic(expected = "owns lines 2..=3, but no error is there")]
fn a_site_with_no_error_fails() {
    check_errors("fn f() => 1\n", &[(2..=3, "00000000")], expect![""]);
}

#[test]
fn three_d_math_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/3d-math.kitty"),
        &[],
        expect![""],
    );
}

#[test]
fn three_d_object_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/3d-object.kitty"),
        &[(17..=19, "e6a33eab")],
        expect![[r#"
            error at 318..319: expected ‘=’, but found ‘.’  # design plan e6a33eab
            error at 349..350: expected ‘=’, but found ‘.’  # design plan e6a33eab
            error at 380..381: expected ‘=’, but found ‘.’  # design plan e6a33eab
        "#]],
    );
}

#[test]
fn units_lists_its_parse_errors() {
    check_errors(include_str!("../../examples/units.kitty"), &[], expect![""]);
}

#[test]
fn assembly_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/assembly.kitty"),
        &[
            (5..=6, "dd325e81"),
            (8..=11, "dd325e81"),
            (13..=13, "dd325e81"),
            (18..=18, "3738718c"),
        ],
        expect![[r#"
            error at 95..96: expected dedent, but found ‘(’  # design plan dd325e81
            error at 106..107: expected ‘for’, but found ‘)’  # design plan dd325e81
            error at 110..114: expected type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’, but found ‘case’  # design plan dd325e81
            error at 115..123: expected indent, but found type-id  # design plan dd325e81
            error at 123..124: expected dedent, but found ‘(’  # design plan dd325e81
            error at 137..138: expected ‘for’, but found ‘)’  # design plan dd325e81
            error at 140..140: expected type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’, but found dedent  # design plan dd325e81
            error at 140: missing indent  # design plan dd325e81
            error at 140: missing dedent  # design plan dd325e81
            error at 175..176: expected dedent, but found ‘(’  # design plan dd325e81
            error at 176..180: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id  # design plan dd325e81
            error at 180..181: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’  # design plan dd325e81
            error at 184..188: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘case’  # design plan dd325e81
            error at 189..195: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id  # design plan dd325e81
            error at 195..196: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’  # design plan dd325e81
            error at 196..200: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’  # design plan dd325e81
            error at 200..201: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’  # design plan dd325e81
            error at 204..208: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘case’  # design plan dd325e81
            error at 209..213: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id  # design plan dd325e81
            error at 215..215: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent  # design plan dd325e81
            error at 234..235: expected indent, but found ‘(’  # design plan dd325e81
            error at 235..239: expected dedent, but found type-id  # design plan dd325e81
            error at 239..240: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’  # design plan dd325e81
            error at 240..249: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id  # design plan dd325e81
            error at 249..250: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’  # design plan dd325e81
            error at 250..251: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’  # design plan dd325e81
            error at 327: missing indent  # design plan 3738718c
            error at 327: missing dedent  # design plan 3738718c
        "#]],
    );
}

#[test]
fn chair_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/chair.kitty"),
        &[
            (56..=56, "e6a33eab"),
            (61..=62, "e6a33eab"),
            (68..=68, "e6a33eab"),
            (73..=73, "e6a33eab"),
            (78..=78, "e6a33eab"),
            (83..=83, "e6a33eab"),
            (88..=88, "e6a33eab"),
            (93..=93, "e6a33eab"),
            (98..=98, "e6a33eab"),
            (103..=103, "e6a33eab"),
        ],
        expect![[r#"
            error at 1520..1529: expected value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 1643..1645: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘if’  # design plan e6a33eab
            error at 1674..1683: expected value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 1838..1846: expected ‘then’, but found type-id  # design plan e6a33eab
            error at 1846..1847: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘.’  # design plan e6a33eab
            error at 1847..1848: expected ‘else’, ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 1913..1921: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 2001..2009: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 2095..2103: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 2202..2210: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 2290..2298: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 2391..2399: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
            error at 2479..2487: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id  # design plan e6a33eab
        "#]],
    );
}

#[test]
fn sample_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/sample.kitty"),
        &[],
        expect![""],
    );
}
