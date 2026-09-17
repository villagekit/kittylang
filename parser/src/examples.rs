//! The examples test: every program in `examples/` parsed end to end, with
//! the parse errors it produces recorded per file.
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

#[test]
fn assembly_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/assembly.kitty"),
        expect![[r#"
            error at 27..32: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 32..33: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 33..34: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 34..35: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 60..64: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 95..96: expected dedent, but found ‘(’
            error at 106..107: expected ‘for’, but found ‘)’
            error at 110..114: expected type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’, but found ‘case’
            error at 115..123: expected indent, but found type-id
            error at 123..124: expected dedent, but found ‘(’
            error at 137..138: expected ‘for’, but found ‘)’
            error at 140..140: expected type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’, but found dedent
            error at 140: missing indent
            error at 140: missing dedent
            error at 175..176: expected dedent, but found ‘(’
            error at 176..180: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 180..181: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 184..188: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘case’
            error at 189..195: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 195..196: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 196..200: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 200..201: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 204..208: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘case’
            error at 209..213: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 215..215: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 234..235: expected indent, but found ‘(’
            error at 235..239: expected dedent, but found type-id
            error at 239..240: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 240..249: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 249..250: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 250..251: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 285: missing ‘[’ or ‘(’
            error at 285..286: expected dedent, but found ‘:’
            error at 287..292: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 294..294: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 321: missing indent
            error at 321: missing dedent
        "#]],
    );
}

#[test]
fn chair_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/chair.kitty"),
        expect![[r#"
            error at 27..34: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 34..35: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 35..36: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 71..78: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 78..79: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 79..80: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 121..125: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 125..126: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 126..135: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 135..136: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 136..137: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 161..167: expected dedent, but found ‘error’
            error at 167..168: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 168..180: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 180..181: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 184..190: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 190..191: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 191..194: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 194..195: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 196..197: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 197..198: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 199..202: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 202..203: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 204..206: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 206..207: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 208..212: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 212..213: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 214..215: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 215..216: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 219..223: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
            error at 224..234: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 234..235: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 236..242: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 246..252: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 252..253: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 253..265: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 265..266: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 269..275: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 275..276: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 276..279: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 279..280: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 281..282: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 282..283: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 284..287: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 287..288: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 289..291: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 291..292: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 295..299: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
            error at 300..310: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 310..311: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 312..318: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 322..328: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 328..329: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 329..342: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 342..343: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 346..358: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 358..359: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 359..410: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 410..411: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 414..420: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 420..421: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 421..424: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 424..425: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 426..427: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 427..428: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 429..432: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 432..433: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 434..436: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 436..437: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 440..444: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
            error at 445..456: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 456..457: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 458..464: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 468..474: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 474..475: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 475..489: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 489..490: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 493..497: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
            error at 498..517: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 517..518: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 519..526: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 530..536: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 536..537: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 537..550: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 550..551: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 554..566: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 566..567: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 567..620: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 620..621: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 624..633: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 633..634: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 637: missing value-id
            error at 671..672: expected ‘(’, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 675..681: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 681..682: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 682..685: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 685..686: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 687..688: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 688..689: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 690..693: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 693..694: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 695..697: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 697..698: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 701..705: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
            error at 706..717: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 717..718: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 719..725: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 729..735: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 735..736: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 736..760: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 760..761: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 780..781: expected ‘=>’, but found ‘:’
            error at 787..789: expected ‘[’, ‘.[’, ‘.’, ‘(’, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=>’
            error at 792..794: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 794..798: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 803..805: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 805..816: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 817..818: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 819..821: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 828..838: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 839..840: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 841..843: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 850..861: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 862..863: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 864..866: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 873..883: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 884..885: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 886..888: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 895..914: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 915..916: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 917..922: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 926..926: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 926..926: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 926..932: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 932..933: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 933..954: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found string
            error at 954..955: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 984..985: expected ‘=>’, but found ‘:’
            error at 991..993: expected ‘[’, ‘.[’, ‘.’, ‘(’, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=>’
            error at 996..998: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 998..1002: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 1007..1009: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1009..1012: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘...’
            error at 1012..1016: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 1016..1017: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1017..1024: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1024..1025: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 1025..1026: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 1033..1052: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1053..1054: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 1055..1059: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1061..1061: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1061..1061: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1061..1061: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1101..1102: expected ‘=>’, but found ‘:’
            error at 1109..1111: expected dedent, but found ‘=>’
            error at 1114..1116: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1116..1119: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 1120..1124: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1125..1129: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
            error at 1134..1136: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1136..1146: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1153..1163: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1170..1181: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1188..1199: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1206..1225: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1230..1230: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1230..1232: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1237..1240: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 1241..1258: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1259..1260: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 1261..1263: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1264..1283: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1284..1288: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘then’
            error at 1289..1300: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1301..1302: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1303..1314: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1315..1319: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘else’
            error at 1320..1331: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1332..1334: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1339..1342: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 1343..1361: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1362..1363: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 1364..1366: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1367..1386: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1387..1391: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘then’
            error at 1392..1393: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1393..1394: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1395..1399: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘else’
            error at 1400..1401: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1402..1404: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1409..1412: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 1413..1429: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1430..1431: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 1432..1434: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1435..1454: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1455..1459: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘then’
            error at 1460..1470: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1471..1472: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1473..1474: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1475..1479: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘else’
            error at 1480..1490: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1491..1493: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1499..1504: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1509..1511: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1511..1520: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1520..1521: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1521..1523: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1530..1532: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1532..1533: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1533..1534: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1535..1536: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1536..1537: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1537..1538: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1539..1549: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1549..1550: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1559..1560: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1560..1561: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1562..1563: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1563..1581: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1581..1582: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1583..1599: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1599..1600: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1609..1610: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1610..1611: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1612..1616: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1616..1622: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1630..1630: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1630..1632: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1633..1652: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1659..1661: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1661..1670: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1670..1671: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1671..1673: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1682..1684: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1684..1685: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1685..1686: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1687..1688: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1688..1689: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1689..1690: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1691..1701: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1701..1702: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1713..1714: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1714..1715: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1716..1726: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1727..1728: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1729..1730: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1741..1742: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1742..1743: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1744..1745: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1745..1756: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1757..1758: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1759..1760: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1760..1761: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1762..1773: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1774..1775: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1776..1777: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1778..1779: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1780..1791: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1791..1792: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1803..1806: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1806..1807: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1808..1809: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 1809..1812: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1812..1813: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 1821..1821: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1821..1821: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1821..1829: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1829..1830: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1830..1831: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1838..1840: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1840..1841: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1841..1842: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1843..1844: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1853..1854: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1854..1855: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1856..1857: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1866..1867: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1867..1868: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1869..1870: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1870..1871: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1871..1872: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1873..1884: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1884..1885: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1893..1893: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1893..1901: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1901..1902: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1902..1903: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1910..1912: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1912..1913: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1913..1914: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1915..1925: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1926..1927: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1928..1929: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1938..1939: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1939..1940: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1941..1942: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1951..1952: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1952..1953: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1954..1955: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1955..1956: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1956..1957: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1958..1969: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1969..1970: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1978..1978: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1978..1986: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1986..1987: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1987..1988: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1995..1997: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1997..1998: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1998..1999: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2000..2001: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2010..2011: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2011..2012: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2013..2023: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2024..2025: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2026..2027: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2036..2037: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2037..2038: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2039..2040: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2040..2041: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2041..2042: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2043..2060: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2060..2061: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2069..2069: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2069..2077: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2077..2078: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2078..2079: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2086..2088: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2088..2089: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2089..2090: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2091..2101: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2102..2103: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2104..2105: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2114..2115: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2115..2116: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2117..2127: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2128..2129: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2130..2131: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2140..2141: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2141..2142: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2143..2144: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2144..2145: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2145..2146: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2147..2164: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2164..2165: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2173..2173: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2173..2181: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2181..2182: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2182..2183: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2190..2192: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2192..2193: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2193..2194: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2195..2196: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2196..2197: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2197..2198: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2199..2209: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2209..2210: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2219..2220: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2220..2221: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2222..2223: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2232..2233: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2233..2234: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2235..2246: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2247..2248: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2249..2250: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2258..2258: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2258..2266: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2266..2267: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2267..2268: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2275..2277: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2277..2278: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2278..2279: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2280..2281: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2281..2282: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2282..2283: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2284..2294: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2294..2295: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2304..2305: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2305..2306: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2307..2317: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2318..2319: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2320..2321: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2330..2331: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2331..2332: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2333..2344: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2345..2346: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2347..2348: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2356..2356: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2356..2364: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2364..2365: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2365..2366: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2373..2375: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2375..2376: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2376..2377: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2378..2379: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2388..2389: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2389..2390: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2391..2392: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2392..2393: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2393..2394: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2395..2405: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2405..2406: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2415..2416: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2416..2417: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2418..2429: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2430..2431: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2432..2433: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2441..2441: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2441..2449: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2449..2450: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2450..2451: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2458..2460: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2460..2461: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2461..2462: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2463..2473: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2474..2475: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2476..2477: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2486..2487: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2487..2488: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2489..2490: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2490..2491: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2491..2492: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2493..2503: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2503..2504: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2513..2514: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2514..2515: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2516..2527: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2528..2529: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2530..2531: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2537..2537: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2537..2537: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2572..2572: expected ‘(’, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2572..2572: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
        "#]],
    );
}

#[test]
fn sample_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/sample.kitty"),
        expect![[r#"
            error at 53..56: expected ‘in’, but found ‘let’
            error at 61..62: expected dedent, but found ‘=’
            error at 66: missing value-id
            error at 84..87: expected ‘(’, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘(’, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 88..89: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 90..91: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 92..95: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 95..96: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 96..99: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 99..100: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 100..101: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 101..102: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 103..105: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 105..106: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 106..107: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 108..109: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 109..110: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 113..116: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 116..117: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 117..118: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 118..119: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 120..123: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 123..124: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 125..125: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
        "#]],
    );
}
