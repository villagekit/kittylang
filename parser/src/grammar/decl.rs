use kitty_syntax::{NodeKind, TokenKind};

use crate::{
    grammar::function::{function_body, function_param_list},
    marker::CompletedMarker,
    parser::Parser,
    token_set::TokenSet,
};

use super::{expr::expr, r#type::type_annotation};

pub(crate) const TOP_ITEM_FIRST: [TokenKind; 7] = [
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Enum,
    TokenKind::Struct,
    TokenKind::Trait,
    TokenKind::Impl,
];

pub(crate) fn top_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Type) {
        type_decl(p, recovery)
    } else if p.at(TokenKind::Const) {
        constant_decl(p, recovery)
    } else if p.at(TokenKind::Fn) {
        function_decl(p, recovery)
    } else if p.at(TokenKind::Enum) {
        enum_decl(p, recovery)
    } else if p.at(TokenKind::Struct) {
        struct_decl(p, recovery)
    } else if p.at(TokenKind::Trait) {
        trait_decl(p, recovery)
    } else if p.at(TokenKind::Impl) {
        impl_trait_decl(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

/// Type alias declaration
fn type_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Type));
    let m = p.start();
    p.bump(); // Consume 'type'
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::Colon, recovery);
    type_annotation(p, recovery);
    m.complete(p, NodeKind::TypeDecl)
}

/// Constant declaration
fn constant_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Const));
    let m = p.start();
    p.bump(); // Consume 'const'
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        type_annotation(p, recovery);
    }
    p.expect(TokenKind::Equal, recovery);
    expr(p, recovery);
    m.complete(p, NodeKind::ConstantDecl)
}

/// Function declaration
fn function_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Fn));
    let m = p.start();
    p.bump(); // Consume 'fn'
    p.expect(TokenKind::Identifier, recovery);
    function_param_list(p, recovery);
    p.expect(TokenKind::FatArrow, recovery);
    function_body(p, recovery);
    m.complete(p, NodeKind::FunctionDecl)
}

/// Function declaration
fn struct_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Struct));
    let recovery_struct = recovery.union(STRUCT_ITEM_FIRST).union([TokenKind::Dedent]);
    let m = p.start();
    p.bump(); // Consume 'struct'
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::Indent, recovery);
    while p.at_set(STRUCT_ITEM_FIRST) {
        struct_item(p, recovery_struct);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::StructDecl)
}

const STRUCT_ITEM_FIRST: [TokenKind; 3] = [TokenKind::Const, TokenKind::Fn, TokenKind::Prop];

fn struct_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Const) {
        constant_decl(p, recovery)
    } else if p.at(TokenKind::Fn) {
        function_decl(p, recovery)
    } else if p.at(TokenKind::Prop) {
        prop_decl(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn prop_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Prop));
    let m = p.start();
    p.bump(); // Consume 'prop'
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::Colon, recovery);
    type_annotation(p, recovery);
    if p.at(TokenKind::Equal) {
        p.bump(); // Consume '='
        expr(p, recovery);
    }
    m.complete(p, NodeKind::PropDecl)
}
