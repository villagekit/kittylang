use kitty_syntax::{NodeKind, TokenKind};

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

use super::declaration::{declaration, DECLARATION_FIRST};

const MODULE_ITEM_FIRST: [TokenKind; 2] = [TokenKind::Import, TokenKind::Export];

pub(crate) fn module(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    let recovery = TokenSet::new(MODULE_ITEM_FIRST).union(DECLARATION_FIRST);

    while !p.at_end() {
        module_item(p, recovery);
    }

    m.complete(p, NodeKind::Module)
}

fn module_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Import) {
        module_import(p, recovery)
    } else if p.at(TokenKind::Export) {
        module_export(p, recovery)
    } else if p.at_set(DECLARATION_FIRST) {
        module_local(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn module_import(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Import));
    let m = p.start();
    p.bump(); // Consume 'import'
    if p.at(TokenKind::From) {
        // import from @std/math
        //   sin
        //   tan
        //   cos

        p.bump(); // Consume 'from'
        p.expect(TokenKind::Package, recovery);

        p.expect(TokenKind::Indent, recovery);
        while p.at_set(IMPORT_ALIAS_FIRST) {
            import_alias(p, recovery.union([TokenKind::Dedent]));
        }
        p.expect(TokenKind::Dedent, recovery);
    } else {
        // import sin, tan, cos from @std/math

        while p.at_set(IMPORT_ALIAS_FIRST) {
            import_alias(
                p,
                recovery.union([TokenKind::From, TokenKind::Comma, TokenKind::Package]),
            );

            if !p.bump_if_at(TokenKind::Comma) {
                break;
            }
        }
        p.expect(TokenKind::From, recovery.union([TokenKind::Package]));
        p.expect(TokenKind::Package, recovery);
    }
    m.complete(p, NodeKind::ModuleImport)
}

const IMPORT_ALIAS_FIRST: [TokenKind; 2] = [TokenKind::IdentifierValue, TokenKind::IdentifierType];

fn import_alias(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::IdentifierValue) {
        import_alias_kind(
            p,
            recovery,
            TokenKind::IdentifierValue,
            NodeKind::ImportAliasValue,
        )
    } else if p.at(TokenKind::IdentifierType) {
        import_alias_kind(
            p,
            recovery,
            TokenKind::IdentifierType,
            NodeKind::ImportAliasType,
        )
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn import_alias_kind(
    p: &mut Parser,
    recovery: TokenSet,
    token: TokenKind,
    node: NodeKind,
) -> CompletedMarker {
    let m = p.start();
    p.expect(token, recovery);
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        p.expect(token, recovery)
    }
    m.complete(p, node)
}

fn module_export(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Export));
    let m = p.start();
    p.bump(); // Consume 'export'
    declaration(p, recovery);
    m.complete(p, NodeKind::ModuleExport)
}

fn module_local(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    declaration(p, recovery);
    m.complete(p, NodeKind::ModuleLocal)
}
