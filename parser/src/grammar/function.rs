use kitty_syntax::{NodeKind, TokenKind};

use super::{
    expression::expression,
    r#type::{generic_param_list, generic_where_clause, type_annotation},
};
use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

pub(crate) fn function_declaration_option_name_body(
    p: &mut Parser,
    recovery: TokenSet,
    required_name: bool,
    required_body: bool,
) -> CompletedMarker {
    // `declaration` and its item rules, and `expression_primary`,
    // dispatch here on `fn`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Fn));
    let m = p.start();
    p.bump(); // Consume 'fn'
    if required_name || p.at(TokenKind::IdentifierValue) {
        // A name left out is missing, not an error that eats the `(`
        // after it.
        p.expect(
            TokenKind::IdentifierValue,
            recovery.union([TokenKind::BracketOpen, TokenKind::ParenOpen]),
        );
    }
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    function_param_list(p, recovery);
    if p.at(TokenKind::Where) {
        generic_where_clause(p, recovery);
    }
    if required_body || p.at(TokenKind::FatArrow) {
        p.expect(TokenKind::FatArrow, recovery);
        function_body(p, recovery);
    }
    m.complete(p, NodeKind::DeclarationFunction)
}

/// The list node is made even when the `(` is not there, so a function's
/// shape holds; the node then holds only the `Missing` or `Error` node
/// the recovery rule gives.
fn function_param_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let recovery_param_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    if p.at(TokenKind::ParenOpen) {
        p.bump(); // Consume '('
        if !p.at(TokenKind::ParenClose) {
            loop {
                function_param(p, recovery_param_list);
                if !p.bump_if_at(TokenKind::Comma) {
                    break;
                }
            }
        }
        p.expect(TokenKind::ParenClose, recovery);
    } else {
        // The list is missing when what may follow it comes next: a
        // return type, a `where` clause or a body.
        p.error(recovery.union([TokenKind::Colon, TokenKind::Where, TokenKind::FatArrow]));
    }
    m.complete(p, NodeKind::FunctionParamList)
}

fn function_param(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    // Parse label
    function_param_label(p, recovery);
    if p.at(TokenKind::Colon) {
        // Parse type
        p.expect(TokenKind::Colon, recovery);
        type_annotation(p, recovery);
    }
    // Parse default value
    if p.at(TokenKind::Equal) {
        p.bump(); // Consume '='
        expression(p, recovery);
    }
    m.complete(p, NodeKind::FunctionParam)
}

pub(crate) fn function_arg_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `expression_apply` dispatches here on `(`.
    debug_assert_eq!(p.peek(), Some(TokenKind::ParenOpen));
    let recovery_arg_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    p.bump(); // Consume '('.
    'all: {
        if p.at(TokenKind::ParenClose) {
            break 'all; // End all args
        }
        // First process positional args
        'positional: loop {
            if p.at_set(FUNCTION_PARAM_LABEL_FIRST) && p.lookahead_at(1, TokenKind::Colon) {
                break 'positional; // End positional args
            }

            function_positional_arg(p, recovery_arg_list);

            if !p.at(TokenKind::Comma) {
                break 'all; // End all args
            }
            p.bump(); // Consume ','
        }
        // Then process labelled args
        loop {
            function_labelled_arg(p, recovery_arg_list);

            if !p.at(TokenKind::Comma) {
                break 'all;
            }
            p.bump(); // Consume ','
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::FunctionArgList)
}

fn function_positional_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionArgPositional)
}

/// Only the first labelled arg is known to start with a label; the ones
/// after a comma may start with anything, so `function_param_label`
/// records the error.
fn function_labelled_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    function_param_label(p, recovery);
    p.expect(TokenKind::Colon, recovery);
    if !(p.at(TokenKind::Comma) || p.at(TokenKind::ParenClose)) {
        expression(p, recovery);
    }
    m.complete(p, NodeKind::FunctionArgLabelled)
}

const FUNCTION_PARAM_LABEL_FIRST: [TokenKind; 2] =
    [TokenKind::IdentifierValue, TokenKind::SelfLower];

fn function_param_label(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set(FUNCTION_PARAM_LABEL_FIRST) {
        Some(p.mark_kind(NodeKind::FunctionParamLabel))
    } else {
        p.error(recovery);
        None
    }
}

fn function_body(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionBody)
}
