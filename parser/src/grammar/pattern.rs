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

// TODO pattern_identifier
// - how to distinguish between pattern type and pattern identifier?
// - e.g. Thing.Cat vs thing

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
            if p.at(TokenKind::IdentifierValue) && p.lookahead_at(1, TokenKind::Colon) {
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
    m.complete(p, NodeKind::PatternTypeArgList)
}

// TODO handle this with ':' to rename
fn pattern_field_positional(p: &mut Parser, _recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    // expr(p, recovery);
    m.complete(p, NodeKind::PatternTypeArgPositional)
}

// TODO handle this like Julia does (?)
fn pattern_field_labelled(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::IdentifierValue));
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    p.expect(TokenKind::Colon, recovery);
    // expr(p, recovery);
    m.complete(p, NodeKind::PatternTypeArgLabelled)
}

fn pattern_name(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::IdentifierValue, recovery);
    m.complete(p, NodeKind::PatternName)
}
