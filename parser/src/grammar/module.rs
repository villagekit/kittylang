use kitty_syntax::{NodeKind, TokenKind};

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

use super::decl::{decl_item, TOP_ITEM_FIRST};

const MODULE_ITEM_FIRST: [TokenKind; 2] = [TokenKind::Import, TokenKind::Export];

pub(crate) fn module(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    let recovery = TokenSet::new(MODULE_ITEM_FIRST).union(TOP_ITEM_FIRST);

    while !p.at_end() {
        module_item(p, recovery);
    }

    m.complete(p, NodeKind::Module)
}

fn module_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Import) {
        import_item(p, recovery)
    } else if p.at(TokenKind::Export) {
        export_item(p, recovery)
    } else if p.at_set(TOP_ITEM_FIRST) {
        local_item(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn import_item(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
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
    m.complete(p, NodeKind::ImportItem)
}

const IMPORT_ALIAS_FIRST: [TokenKind; 1] = [TokenKind::Identifier];

fn import_alias(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        p.expect(TokenKind::Identifier, recovery)
    }
    m.complete(p, NodeKind::ImportAlias)
}

fn export_item(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Export));
    let m = p.start();
    p.bump(); // Consume 'export'
    decl_item(p, recovery);
    m.complete(p, NodeKind::ExportItem)
}

fn local_item(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    decl_item(p, recovery);
    m.complete(p, NodeKind::LocalItem)
}
