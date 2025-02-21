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

pub(crate) fn pattern_single(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Underscore) {
        let type_pattern =
        pattern_wildcard(p)?
    } else if p.at(TokenKind::Number) {
        type_function(p, recovery)
    } else if p.at(TokenKind::Impl) {
        type_impl_trait(p, recovery)
    } else if p.at(TokenKind::ParenOpen) {
        pattern_tuple(p, recovery)
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

const PATTERN_LITERAL_FIRST: [TokenKind; 3] = [TokenKind::Boolean, TokenKind::Number, TokenKind::String];

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

pub(crate) fn pattern_tuple(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {}

pub(crate) fn pattern_type(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {}

pub(crate) fn pattern_type_field_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {}
pub(crate) fn pattern_type_field_item(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {}
pub(crate) fn pattern_type_field_item_positional(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {}
pub(crate) fn pattern_type_field_item_labelled(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {}
