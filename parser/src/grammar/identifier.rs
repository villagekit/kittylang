use kitty_syntax::{NodeKind, TokenKind};

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

/// Constant name
pub(crate) fn constant_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::IdentifierConstant) {
        Some(p.mark_kind(NodeKind::ConstantName))
    } else {
        p.error(recovery);
        None
    }
}

/// Constant reference
pub(crate) fn constant_reference(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::IdentifierConstant) {
        Some(p.mark_kind(NodeKind::ConstantReference))
    } else {
        p.error(recovery);
        None
    }
}

/// Variable name
pub(crate) fn variable_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set([TokenKind::IdentifierType, TokenKind::SelfLower]) {
        Some(p.mark_kind(NodeKind::VariableName))
    } else {
        p.error(recovery);
        None
    }
}

/// Variable reference
pub(crate) fn variable_reference(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set([TokenKind::IdentifierType, TokenKind::SelfLower]) {
        Some(p.mark_kind(NodeKind::VariableReference))
    } else {
        p.error(recovery);
        None
    }
}
/// Type name
pub(crate) fn type_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::IdentifierType) {
        Some(p.mark_kind(NodeKind::TypeName))
    } else {
        p.error(recovery);
        None
    }
}

/// Type reference
pub(crate) fn type_reference(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set([TokenKind::IdentifierType, TokenKind::SelfUpper]) {
        Some(p.mark_kind(NodeKind::TypeReference))
    } else {
        p.error(recovery);
        None
    }
}

/// Trait name
pub(crate) fn trait_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::IdentifierType) {
        Some(p.mark_kind(NodeKind::TraitName))
    } else {
        p.error(recovery);
        None
    }
}

/// Type reference
pub(crate) fn trait_reference(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::IdentifierType) {
        Some(p.mark_kind(NodeKind::TraitReference))
    } else {
        p.error(recovery);
        None
    }
}
