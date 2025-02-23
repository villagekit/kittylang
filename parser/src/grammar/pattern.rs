use kitty_syntax::{NodeKind, TokenKind};

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

const PATTERN_FIRST: [TokenKind; 2] = [TokenKind::Identifier, TokenKind::SelfUpper];

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

// TODO pattern_identifier
// - how to distinguish between pattern type and pattern identifier?
// - e.g. Thing.Cat vs thing

pub(crate) fn pattern_single(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Underscore) {
        pattern_wildcard(p)
    } else if p.at_set(PATTERN_LITERAL_FIRST) {
        pattern_literal(p)
    } else if p.at(TokenKind::ParenOpen) {
        pattern_tuple(p, recovery)
    } else if p.at(TokenKind::Identifier) {
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
    if p.at(TokenKind::Boolean) {
        p.mark_kind(NodeKind::BooleanLiteral);
    } else if p.at(TokenKind::Number) {
        p.mark_kind(NodeKind::NumberLiteral);
    } else if p.at(TokenKind::String) {
        p.mark_kind(NodeKind::StringLiteral);
    }
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
    assert!(p.at(TokenKind::Identifier));
    let m = p.start();
    type_path(p, recovery);
    if p.at(TokenKind::ParenOpen) {
        pattern_field_list(p, recovery);
    }
    m.complete(p, NodeKind::PatternType)
}

pub(crate) fn pattern_field_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
    let recovery_field_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    p.bump(); // Consume '('.
    'all: {
        if p.at(TokenKind::ParenClose) {
            break 'all; // End all fields
        }
        // First process positional fields
        'positional: loop {
            if p.at(TokenKind::Identifier) && p.lookahead_at(1, TokenKind::Colon) {
                break 'positional; // End positional fields
            }

            pattern_field_positional(p, recovery_field_list);

            if !p.at(TokenKind::Comma) {
                break 'all; // End all fields
            }
            p.bump(); // Consume ','
        }
        // Then process labelled fields
        loop {
            pattern_field_labelled(p, recovery_field_list);

            if !p.at(TokenKind::Comma) {
                break 'all;
            }
            p.bump(); // Consume ','
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::PatternFieldList)
}

fn pattern_field_positional(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    // expr(p, recovery);
    m.complete(p, NodeKind::PatternFieldPositional)
}

fn pattern_field_labelled(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Identifier));
    let m = p.start();
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::Colon, recovery);
    // expr(p, recovery);
    m.complete(p, NodeKind::PatternFieldLabelled)
}
