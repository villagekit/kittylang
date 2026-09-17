//! The examples test: every program in `examples/` parsed end to end, with
//! the parse errors it produces recorded per file. Three programs make the
//! parser panic today; those cases pin the panic instead of the errors.
//!
//! The expectations are the fixture guard for the grammar. Each grammar
//! change that is meant to shrink them is reviewed and then recorded with
//! `UPDATE_EXPECT=1`. The six files are named here on purpose, so a new
//! example is added to this list by hand.

use expect_test::{expect, Expect};

use crate::parse;

/// Parses `source` and renders each parse error on its own line, through
/// `ParseError`'s `Display`.
fn check_errors(source: &str, expected: Expect) {
    let parsed = parse(source);
    let actual: String = parsed
        .errors
        .iter()
        .map(|error| format!("{error}\n"))
        .collect();
    expected.assert_eq(&actual);
}

#[test]
fn three_d_math_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/3d-math.kitty"),
        expect![[r#"
            error at 23..26: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 121..128: expected type-id, but found value-id
            error at 148..155: expected type-id, but found value-id
            error at 175..182: expected type-id, but found value-id
            error at 203..204: expected ‘=>’, but found ‘:’
            error at 207..209: expected dedent, but found ‘=>’
            error at 212..214: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 214..217: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 218..222: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 223..224: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘{’
            error at 225..226: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 226..227: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 228..229: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 229..230: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 231..232: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 233..234: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘}’
            error at 235..236: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 237..241: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 246..247: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 247..248: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 249..250: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
            error at 251..252: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 253..254: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 255..256: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 257..258: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
            error at 259..260: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 261..262: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 263..264: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 265..266: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
            error at 267..268: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 268..269: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 269..270: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 270..274: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 274..275: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 275..276: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 278..278: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 278..278: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
        "#]],
    );
}

#[test]
fn three_d_object_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/3d-object.kitty"),
        expect![[r#"
            error at 25..29: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 48..49: expected indent, but found value-id
            error at 49..50: expected dedent, but found ‘-’
            error at 50..54: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 55..57: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 57..64: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 67..77: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 79..79: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 245..246: expected ‘=>’, but found ‘:’
            error at 252..254: expected dedent, but found ‘=>’
            error at 257..259: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 259..263: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 268..270: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 270..273: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘...’
            error at 273..277: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 284..295: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 295..296: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 297..304: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 311..313: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 313..317: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 317..318: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 318..329: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 329..330: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 330..331: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 332..333: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 334..335: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 344..348: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 348..349: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 349..360: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 360..361: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 361..362: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 363..364: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 365..366: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 375..379: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 379..380: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 380..391: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 391..392: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 392..393: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 394..395: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 396..397: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 399..399: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 399..399: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 399..399: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 399..399: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 443..445: expected ‘=>’, but found indent
            error at 454..456: expected dedent, but found indent
            error at 456..467: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 467..468: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 469..476: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 476..477: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 477..483: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 483..484: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 484..485: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 485..492: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 492..493: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 493..494: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 501..509: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 509..510: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 511..521: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 521..522: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 522..529: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 529..530: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 530..531: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 538..543: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 543..544: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 545..552: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 552..553: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 553..560: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 560..561: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 561..562: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 564..564: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 564..564: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 564..564: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 626..633: expected type-id, but found value-id
            error at 690..691: expected dedent, but found ‘:’
            error at 692..696: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 697..699: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=>’
            error at 702..704: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 704..708: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 713..715: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 715..718: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘...’
            error at 718..722: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 729..738: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 738..739: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 740..744: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 744..745: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 745..754: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 754..755: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 755..764: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 764..765: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 765..766: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 766..767: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 768..769: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 769..770: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 771..772: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 772..773: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 774..774: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 774..774: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 774..774: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
        "#]],
    );
}

#[test]
fn units_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/units.kitty"),
        expect![[r#"
            error at 81..82: expected dedent, but found ‘:’
            error at 83..87: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 87..88: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 88..94: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 96..96: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 124..125: expected dedent, but found ‘:’
            error at 126..130: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 132..132: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 225..226: expected dedent, but found ‘{’
            error at 227..228: expected dedent, but found value-id
            error at 228..229: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 230..234: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 234..235: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 235..236: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 237..238: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 239..244: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 244..245: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 245..246: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 247..248: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘}’
            error at 250..250: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 250..250: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 372..376: expected value-id, but found ‘from’
            error at 477..481: expected value-id, but found ‘from’
        "#]],
    );
}

// TODO(cc): the three examples below hit the parameter-list assertion in
// `grammar/function.rs`; plan 277624e4f5fe (The parser never panics) turns
// them into expectations like the ones above. The pinned message is that
// `assert!`'s stringified expression, so any other panic fails the test;
// rewriting the assertion means rewriting the pin.

#[test]
#[should_panic(expected = "assertion failed: p.at(TokenKind::ParenOpen)")]
fn assembly_panics_in_the_parser() {
    parse(include_str!("../../examples/assembly.kitty"));
}

#[test]
#[should_panic(expected = "assertion failed: p.at(TokenKind::ParenOpen)")]
fn chair_panics_in_the_parser() {
    parse(include_str!("../../examples/chair.kitty"));
}

#[test]
#[should_panic(expected = "assertion failed: p.at(TokenKind::ParenOpen)")]
fn sample_panics_in_the_parser() {
    parse(include_str!("../../examples/sample.kitty"));
}
