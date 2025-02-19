use kitty_syntax::{NodeKind, TokenKind};

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

use super::{expr::expr, r#type::type_annotation};

pub(crate) fn function_param_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
    let recovery_param_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    p.bump(); // Consume '('
    if !p.at(TokenKind::ParenClose) {
        loop {
            function_param(p, recovery_param_list);
            if !p.bump_if_at(TokenKind::Comma) {
                break;
            }
            p.bump(); // Consume ','
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::FunctionParamList)
}

fn function_param(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    // Parse name
    p.expect(TokenKind::Identifier, recovery);
    // Parse type
    p.expect(TokenKind::Colon, recovery);
    type_annotation(p, recovery);
    // Parse default value
    if p.at(TokenKind::Equal) {
        p.bump(); // Consume '='
        expr(p, recovery);
    }
    m.complete(p, NodeKind::FunctionParam)
}

pub(crate) fn function_arg_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
    let recovery_arg_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    p.bump(); // Consume '('.
    'all: {
        if p.at(TokenKind::ParenClose) {
            break 'all; // End all args
        }
        // First process positional args
        'positional: loop {
            if p.at(TokenKind::Identifier) && p.lookahead_at(1, TokenKind::Colon) {
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
    expr(p, recovery);
    m.complete(p, NodeKind::FunctionArgPositional)
}

fn function_labelled_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Identifier));
    let m = p.start();
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::Colon, recovery);
    expr(p, recovery);
    m.complete(p, NodeKind::FunctionArgLabelled)
}

pub(crate) fn function_body(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    expr(p, recovery);
    m.complete(p, NodeKind::FunctionBody)
}
