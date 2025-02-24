use kitty_syntax::{NodeKind, TokenKind};

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

pub(crate) const CONSTANT_NAME_FIRST: [TokenKind; 1] = [TokenKind::IdentifierType];

/// Constant name
pub(crate) fn constant_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::IdentifierConstant) {
        Some(p.mark_kind(NodeKind::ConstantName))
    } else {
        p.error(recovery);
        None
    }
}

pub(crate) const VARIABLE_NAME_FIRST: [TokenKind; 2] =
    [TokenKind::IdentifierType, TokenKind::SelfLower];

/// Variable name
pub(crate) fn variable_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set(VARIABLE_NAME_FIRST) {
        Some(p.mark_kind(NodeKind::VariableName))
    } else {
        p.error(recovery);
        None
    }
}

pub(crate) const TYPE_NAME_FIRST: [TokenKind; 2] =
    [TokenKind::IdentifierType, TokenKind::SelfUpper];

/// Type name
pub(crate) fn type_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set(TYPE_NAME_FIRST) {
        Some(p.mark_kind(NodeKind::TypeName))
    } else {
        p.error(recovery);
        None
    }
}

pub(crate) const TRAIT_NAME_FIRST: [TokenKind; 1] = [TokenKind::IdentifierType];

/// Trait name
pub(crate) fn trait_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::IdentifierType) {
        Some(p.mark_kind(NodeKind::TraitName))
    } else {
        p.error(recovery);
        None
    }
}
