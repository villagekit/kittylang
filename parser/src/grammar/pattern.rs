use kitty_syntax::{NodeKind, TokenKind};

use super::r#type::{type_path, TYPE_PATH_FIRST};
use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

pub(crate) fn pattern(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let lhs = pattern_single(p, recovery)?;

    if !p.at(TokenKind::Or) {
        return None;
    }

    let m = lhs.precede(p);
    while p.bump_if_at(TokenKind::Or) {
        pattern(p, recovery);
    }
    Some(m.complete(p, NodeKind::PatternOr))
}

pub(crate) fn pattern_single(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::IdentifierValue) {
        pattern_name(p, recovery)
    } else if p.at(TokenKind::Underscore) {
        pattern_wildcard(p)
    } else if p.at_set(PATTERN_LITERAL_FIRST) {
        pattern_literal(p)
    } else if p.at(TokenKind::ParenOpen) {
        pattern_tuple(p, recovery)
    } else if p.at_set(TYPE_PATH_FIRST) {
        pattern_type(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

pub(crate) fn pattern_wildcard(p: &mut Parser) -> CompletedMarker {
    // `pattern_single` dispatches here on `_`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Underscore));
    p.mark_kind(NodeKind::PatternWildcard)
}

const PATTERN_LITERAL_FIRST: [TokenKind; 2] = [TokenKind::Number, TokenKind::String];

pub(crate) fn pattern_literal(p: &mut Parser) -> CompletedMarker {
    // `pattern_single` dispatches here on a literal.
    debug_assert!(p
        .peek()
        .is_some_and(|kind| PATTERN_LITERAL_FIRST.contains(&kind)));
    let m = p.start();
    p.bump(); // Consume <number> or <string>
    m.complete(p, NodeKind::PatternLiteral)
}

pub(crate) fn pattern_tuple(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `pattern_single` dispatches here on `(`.
    debug_assert_eq!(p.peek(), Some(TokenKind::ParenOpen));
    let m = p.start();
    p.bump(); // Consume '('
    loop {
        pattern(p, recovery);
        if !p.bump_if_at(TokenKind::Comma) {
            break;
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::PatternTuple)
}

pub(crate) fn pattern_type(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `pattern_single` dispatches here on the start of a type path.
    debug_assert!(p.peek().is_some_and(|kind| TYPE_PATH_FIRST.contains(&kind)));
    let m = p.start();
    type_path(p, recovery);
    if p.at_set([TokenKind::ParenOpen, TokenKind::BraceOpen]) {
        pattern_type_arg_list(p, recovery);
    }
    m.complete(p, NodeKind::PatternType)
}

/// The fields of a constructor pattern: `( )` for positional fields
/// then named ones, `{ }` for named fields with shorthand.
pub(crate) fn pattern_type_arg_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `pattern_type` dispatches here on `(` or `{`.
    debug_assert!(matches!(
        p.peek(),
        Some(TokenKind::ParenOpen | TokenKind::BraceOpen)
    ));
    let m = p.start();
    if p.at(TokenKind::ParenOpen) {
        pattern_type_arg_list_parens(p, recovery);
    } else {
        pattern_type_arg_list_braces(p, recovery);
    }
    m.complete(p, NodeKind::PatternTypeArgList)
}

fn pattern_type_arg_list_parens(p: &mut Parser, recovery: TokenSet) {
    let recovery_arg = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    p.bump(); // Consume '('.
    'all: {
        if p.at(TokenKind::ParenClose) {
            break 'all; // End all fields
        }
        // First process positional fields
        'positional: loop {
            if p.at(TokenKind::IdentifierValue) && p.lookahead_at(1, TokenKind::Equal) {
                break 'positional; // End positional fields
            }

            pattern_type_arg_positional(p, recovery_arg);

            if !p.bump_if_at(TokenKind::Comma) {
                break 'all; // End all fields
            }
        }
        // Then process labelled fields
        loop {
            pattern_type_arg_labelled(p, recovery_arg);

            if !p.bump_if_at(TokenKind::Comma) {
                break 'all;
            }
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
}

fn pattern_type_arg_list_braces(p: &mut Parser, recovery: TokenSet) {
    let recovery_arg = recovery.union([TokenKind::Comma, TokenKind::BraceClose]);
    p.bump(); // Consume '{'.
    if !p.at(TokenKind::BraceClose) {
        loop {
            pattern_type_field(p, recovery_arg);
            if !p.bump_if_at(TokenKind::Comma) {
                break;
            }
        }
    }
    p.expect(TokenKind::BraceClose, recovery);
}

/// E.g. `let Thing(name, description) = thing`
fn pattern_type_arg_positional(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    m.complete(p, NodeKind::PatternTypeArgPositional)
}

/// E.g. `let Thing(name = title, description = desc) = thing`
///
/// Only the first labelled arg is known to start with a label; the ones
/// after a comma may start with anything, so the label is expected, not
/// assumed. In `( )` a name alone is positional, so the binding is
/// required.
fn pattern_type_arg_labelled(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    p.expect(TokenKind::Equal, recovery);
    p.expect(TokenKind::IdentifierValue, recovery);
    m.complete(p, NodeKind::PatternTypeArgLabelled)
}

/// A field in `{ }`: `name` binds the field of that name, `name =
/// binding` binds it to another name. E.g. `let Self { x, y = a } =
/// self`. The node is the labelled one either way; a shorthand holds one
/// identifier.
fn pattern_type_field(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    // A bare name ends at the comma or the brace. The caller's recovery
    // set is not the test: `let` recovers at `=`, the rename's own token.
    if !p.at_set([TokenKind::Comma, TokenKind::BraceClose]) && !p.at_end() {
        p.expect(TokenKind::Equal, recovery);
        p.expect(TokenKind::IdentifierValue, recovery);
    }
    m.complete(p, NodeKind::PatternTypeArgLabelled)
}

fn pattern_name(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    m.complete(p, NodeKind::PatternName)
}

#[cfg(test)]
mod tests {
    use super::{pattern, Parser, TokenSet};
    use crate::check_grammar;
    use expect_test::expect;
    use kitty_cst::Pattern;

    fn check(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            pattern(p, TokenSet::NONE);
        };
        check_grammar::<Pattern>(grammar, input, expected);
    }

    #[test]
    fn empty_input_is_a_missing_pattern() {
        check(
            "",
            expect![[r#"
                Missing@0..0
                error at 0: missing value-id, _, number, string, ‘(’, type-id, or ‘Self’"#]],
        );
    }

    #[test]
    fn self_type_is_a_type_pattern() {
        check(
            "Self",
            expect![[r#"
            PatternType@0..4
              TypeReference@0..4
                SelfUpper@0..4 "Self""#]],
        );
    }

    #[test]
    fn self_type_with_args_is_a_type_pattern() {
        check(
            "Self(x)",
            expect![[r#"
            PatternType@0..7
              TypeReference@0..4
                SelfUpper@0..4 "Self"
              PatternTypeArgList@4..7
                ParenOpen@4..5 "("
                PatternTypeArgPositional@5..6
                  IdentifierValue@5..6 "x"
                ParenClose@6..7 ")""#]],
        );
    }

    #[test]
    fn type_pattern_labelled_arg_after_a_labelled_arg_recovers() {
        check(
            "Thing(a = b, 1)",
            expect![[r#"
                PatternType@0..15
                  TypeReference@0..5
                    IdentifierType@0..5 "Thing"
                  PatternTypeArgList@5..15
                    ParenOpen@5..6 "("
                    PatternTypeArgLabelled@6..11
                      IdentifierValue@6..7 "a"
                      Whitespace@7..8 " "
                      Equal@8..9 "="
                      Whitespace@9..10 " "
                      IdentifierValue@10..11 "b"
                    Comma@11..12 ","
                    Whitespace@12..13 " "
                    PatternTypeArgLabelled@13..14
                      Error@13..14
                        Number@13..14 "1"
                      Missing@14..14
                      Missing@14..14
                    ParenClose@14..15 ")"
                error at 13..14: expected value-id, but found number
                error at 14: missing ‘=’
                error at 14: missing value-id"#]],
        );
    }

    #[test]
    fn pattern_name() {
        // Happy path
        check(
            "thing",
            expect![[r#"
                PatternName@0..5
                  IdentifierValue@0..5 "thing""#]],
        );
    }

    #[test]
    fn pattern_wildcard() {
        // Happy path
        check(
            "_",
            expect![[r#"
                PatternWildcard@0..1
                  Underscore@0..1 "_""#]],
        );
    }

    #[test]
    fn pattern_literal_string() {
        // Happy path
        check(
            "\"apple\"",
            expect![[r#"
                PatternLiteral@0..7
                  String@0..7 "\"apple\"""#]],
        );
    }

    #[test]
    fn pattern_literal_number_or() {
        // Happy path
        check(
            "2 | 3 | 4",
            expect![[r#"
                PatternLiteral@0..2
                  Number@0..1 "2"
                  Whitespace@1..2 " ""#]],
        );
    }

    #[test]
    fn pattern_tuple() {
        // Happy path
        check(
            "(a, b)",
            expect![[r#"
                PatternTuple@0..6
                  ParenOpen@0..1 "("
                  PatternName@1..2
                    IdentifierValue@1..2 "a"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  PatternName@4..5
                    IdentifierValue@4..5 "b"
                  ParenClose@5..6 ")""#]],
        );
    }

    #[test]
    fn pattern_type_no_args() {
        // Happy path
        check(
            "Thing",
            expect![[r#"
                PatternType@0..5
                  TypeReference@0..5
                    IdentifierType@0..5 "Thing""#]],
        );
    }

    #[test]
    fn pattern_type_positional_args() {
        // Happy path
        check(
            "Thing(name, description)",
            expect![[r#"
                PatternType@0..24
                  TypeReference@0..5
                    IdentifierType@0..5 "Thing"
                  PatternTypeArgList@5..24
                    ParenOpen@5..6 "("
                    PatternTypeArgPositional@6..10
                      IdentifierValue@6..10 "name"
                    Comma@10..11 ","
                    Whitespace@11..12 " "
                    PatternTypeArgPositional@12..23
                      IdentifierValue@12..23 "description"
                    ParenClose@23..24 ")""#]],
        );
    }

    #[test]
    fn pattern_type_labelled_args() {
        // Happy path
        check(
            "Thing(name = title, description = desc)",
            expect![[r#"
                PatternType@0..39
                  TypeReference@0..5
                    IdentifierType@0..5 "Thing"
                  PatternTypeArgList@5..39
                    ParenOpen@5..6 "("
                    PatternTypeArgLabelled@6..18
                      IdentifierValue@6..10 "name"
                      Whitespace@10..11 " "
                      Equal@11..12 "="
                      Whitespace@12..13 " "
                      IdentifierValue@13..18 "title"
                    Comma@18..19 ","
                    Whitespace@19..20 " "
                    PatternTypeArgLabelled@20..38
                      IdentifierValue@20..31 "description"
                      Whitespace@31..32 " "
                      Equal@32..33 "="
                      Whitespace@33..34 " "
                      IdentifierValue@34..38 "desc"
                    ParenClose@38..39 ")""#]],
        );
    }

    #[test]
    fn pattern_type_mixed_arg() {
        // Happy path
        check(
            "Thing(name, description = desc, age = a)",
            expect![[r#"
                PatternType@0..40
                  TypeReference@0..5
                    IdentifierType@0..5 "Thing"
                  PatternTypeArgList@5..40
                    ParenOpen@5..6 "("
                    PatternTypeArgPositional@6..10
                      IdentifierValue@6..10 "name"
                    Comma@10..11 ","
                    Whitespace@11..12 " "
                    PatternTypeArgLabelled@12..30
                      IdentifierValue@12..23 "description"
                      Whitespace@23..24 " "
                      Equal@24..25 "="
                      Whitespace@25..26 " "
                      IdentifierValue@26..30 "desc"
                    Comma@30..31 ","
                    Whitespace@31..32 " "
                    PatternTypeArgLabelled@32..39
                      IdentifierValue@32..35 "age"
                      Whitespace@35..36 " "
                      Equal@36..37 "="
                      Whitespace@37..38 " "
                      IdentifierValue@38..39 "a"
                    ParenClose@39..40 ")""#]],
        );
    }

    #[test]
    fn pattern_type_or() {
        // Happy path
        check(
            "This | That",
            expect![[r#"
                PatternType@0..5
                  TypeReference@0..4
                    IdentifierType@0..4 "This"
                  Whitespace@4..5 " ""#]],
        );
    }

    #[test]
    fn pattern_type_brace_shorthand() {
        check(
            "Self { x, y, z }",
            expect![[r#"
                PatternType@0..16
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Whitespace@4..5 " "
                  PatternTypeArgList@5..16
                    BraceOpen@5..6 "{"
                    Whitespace@6..7 " "
                    PatternTypeArgLabelled@7..8
                      IdentifierValue@7..8 "x"
                    Comma@8..9 ","
                    Whitespace@9..10 " "
                    PatternTypeArgLabelled@10..11
                      IdentifierValue@10..11 "y"
                    Comma@11..12 ","
                    Whitespace@12..13 " "
                    PatternTypeArgLabelled@13..14
                      IdentifierValue@13..14 "z"
                    Whitespace@14..15 " "
                    BraceClose@15..16 "}""#]],
        );
    }

    #[test]
    fn pattern_type_brace_rename() {
        check(
            "Self { x = a }",
            expect![[r#"
                PatternType@0..14
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Whitespace@4..5 " "
                  PatternTypeArgList@5..14
                    BraceOpen@5..6 "{"
                    Whitespace@6..7 " "
                    PatternTypeArgLabelled@7..12
                      IdentifierValue@7..8 "x"
                      Whitespace@8..9 " "
                      Equal@9..10 "="
                      Whitespace@10..11 " "
                      IdentifierValue@11..12 "a"
                    Whitespace@12..13 " "
                    BraceClose@13..14 "}""#]],
        );
    }

    #[test]
    fn pattern_type_brace_field_with_a_colon_recovers() {
        check(
            "Self { x: a }",
            expect![[r#"
                PatternType@0..13
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Whitespace@4..5 " "
                  PatternTypeArgList@5..13
                    BraceOpen@5..6 "{"
                    Whitespace@6..7 " "
                    PatternTypeArgLabelled@7..11
                      IdentifierValue@7..8 "x"
                      Error@8..9
                        Colon@8..9 ":"
                      Whitespace@9..10 " "
                      IdentifierValue@10..11 "a"
                    Whitespace@11..12 " "
                    BraceClose@12..13 "}"
                error at 8..9: expected ‘=’, but found ‘:’"#]],
        );
    }
}
