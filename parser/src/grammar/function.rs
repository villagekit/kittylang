use kitty_syntax::{NodeKind, TokenKind};

use super::{
    expression::expression,
    identifier::{value_name, VALUE_NAME_FIRST},
    pattern::pattern,
    r#type::{generic_param_list, generic_where_clause, type_annotation},
};
use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

pub(crate) fn function_declaration_optional_name_types_body(
    p: &mut Parser,
    recovery: TokenSet,
    required_name: bool,
    required_types: bool,
    required_body: bool,
) -> CompletedMarker {
    assert!(p.at(TokenKind::Fn));
    let m = p.start();
    p.bump(); // Consume 'fn'
    if required_name || p.at_set(VALUE_NAME_FIRST) {
        value_name(p, recovery);
    }
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    function_param_list_optional_types(p, recovery, required_types);
    if p.at(TokenKind::Where) {
        generic_where_clause(p, recovery);
    }
    if required_body || p.at(TokenKind::FatArrow) {
        p.expect(TokenKind::FatArrow, recovery);
        function_body(p, recovery);
    }
    m.complete(p, NodeKind::DeclarationFunction)
}

fn function_param_list_optional_types(
    p: &mut Parser,
    recovery: TokenSet,
    required_types: bool,
) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
    let recovery_param_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    p.bump(); // Consume '('
    if !p.at(TokenKind::ParenClose) {
        loop {
            function_param(p, recovery_param_list, required_types);
            if !p.bump_if_at(TokenKind::Comma) {
                break;
            }
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::FunctionParamList)
}

fn function_param(p: &mut Parser, recovery: TokenSet, required_types: bool) -> CompletedMarker {
    let m = p.start();
    // Parse name
    pattern(p, recovery);
    if required_types || p.at(TokenKind::Colon) {
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
            if p.at_set(VALUE_NAME_FIRST) && p.lookahead_at(1, TokenKind::Colon) {
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

fn function_labelled_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at_set(VALUE_NAME_FIRST));
    let m = p.start();
    value_name(p, recovery);
    p.expect(TokenKind::Colon, recovery);
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionArgLabelled)
}

fn function_body(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionBody)
}
