use kitty_syntax::{NodeKind, TokenKind};

use super::r#type::{type_path, TYPE_PATH_FIRST};
use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

const PATTERN_FIRST: [TokenKind; 8] = [
    TokenKind::IdentifierValue,
    TokenKind::Underscore,
    TokenKind::Boolean,
    TokenKind::Number,
    TokenKind::String,
    TokenKind::ParenOpen,
    TokenKind::IdentifierType,
    TokenKind::SelfUpper,
];

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
    assert!(p.at(TokenKind::Underscore));
    p.mark_kind(NodeKind::PatternWildcard)
}

const PATTERN_LITERAL_FIRST: [TokenKind; 3] =
    [TokenKind::Boolean, TokenKind::Number, TokenKind::String];

pub(crate) fn pattern_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_set(PATTERN_LITERAL_FIRST));
    let m = p.start();
    p.bump(); // Consume <boolean>, <number>, or <string>
    m.complete(p, NodeKind::PatternLiteral)
}

pub(crate) fn pattern_tuple(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
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
    assert!(p.at(TokenKind::IdentifierType));
    let m = p.start();
    type_path(p, recovery);
    if p.at(TokenKind::ParenOpen) {
        pattern_type_arg_list(p, recovery);
    }
    m.complete(p, NodeKind::PatternType)
}

pub(crate) fn pattern_type_arg_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
    let recovery_type_arg_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    p.bump(); // Consume '('.
    'all: {
        if p.at(TokenKind::ParenClose) {
            break 'all; // End all fields
        }
        // First process positional fields
        'positional: loop {
            if p.at(TokenKind::IdentifierValue) && p.lookahead_at(1, TokenKind::Colon) {
                break 'positional; // End positional fields
            }

            pattern_type_arg_positional(p, recovery_type_arg_list);

            if !p.at(TokenKind::Comma) {
                break 'all; // End all fields
            }
            p.bump(); // Consume ','
        }
        // Then process labelled fields
        loop {
            pattern_type_arg_labelled(p, recovery_type_arg_list);

            if !p.at(TokenKind::Comma) {
                break 'all;
            }
            p.bump(); // Consume ','
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::PatternTypeArgList)
}

/// E.g. `let Thing(name, description) = thing`
fn pattern_type_arg_positional(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    m.complete(p, NodeKind::PatternTypeArgPositional)
}

/// E.g. `let Thing(name: title, description:) = thing`
fn pattern_type_arg_labelled(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::IdentifierValue));
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    p.expect(TokenKind::Colon, recovery);
    if !(p.at(TokenKind::Comma) || p.at(TokenKind::ParenClose)) {
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
            "Thing(name: title, description:)",
            expect![[r#"
                PatternType@0..32
                  TypeReference@0..5
                    IdentifierType@0..5 "Thing"
                  PatternTypeArgList@5..32
                    ParenOpen@5..6 "("
                    PatternTypeArgLabelled@6..17
                      IdentifierValue@6..10 "name"
                      Colon@10..11 ":"
                      Whitespace@11..12 " "
                      IdentifierValue@12..17 "title"
                    Comma@17..18 ","
                    Whitespace@18..19 " "
                    PatternTypeArgLabelled@19..31
                      IdentifierValue@19..30 "description"
                      Colon@30..31 ":"
                    ParenClose@31..32 ")""#]],
        );
    }

    #[test]
    fn pattern_type_mixed_arg() {
        // Happy path
        check(
            "Thing(name, description: desc, age:)",
            expect![[r#"
                PatternType@0..36
                  TypeReference@0..5
                    IdentifierType@0..5 "Thing"
                  PatternTypeArgList@5..36
                    ParenOpen@5..6 "("
                    PatternTypeArgPositional@6..10
                      IdentifierValue@6..10 "name"
                    Comma@10..11 ","
                    Whitespace@11..12 " "
                    PatternTypeArgLabelled@12..29
                      IdentifierValue@12..23 "description"
                      Colon@23..24 ":"
                      Whitespace@24..25 " "
                      IdentifierValue@25..29 "desc"
                    Comma@29..30 ","
                    Whitespace@30..31 " "
                    PatternTypeArgLabelled@31..35
                      IdentifierValue@31..34 "age"
                      Colon@34..35 ":"
                    ParenClose@35..36 ")""#]],
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
}
