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
    // `module_item` dispatches here on `import`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Import));
    let m = p.start();
    p.bump(); // Consume 'import'
    if p.at(TokenKind::From) {
        // import from @std/math
        //   sin
        //   tan
        //   cos

        p.bump(); // Consume 'from'
        p.expect(
            TokenKind::Package,
            recovery.union([TokenKind::Colon, TokenKind::Indent]),
        );
        if p.at(TokenKind::Colon) {
            import_version(p, recovery.union([TokenKind::Indent]));
        }

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
        p.expect(TokenKind::Package, recovery.union([TokenKind::Colon]));
        if p.at(TokenKind::Colon) {
            import_version(p, recovery);
        }
    }
    m.complete(p, NodeKind::ModuleImport)
}

/// The `:` and number after a package: `@std/assembly:1`. What the
/// number means is undecided; the tree keeps it.
fn import_version(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `module_import` dispatches here on `:`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Colon));
    let m = p.start();
    p.bump(); // Consume ':'
    p.expect(TokenKind::Number, recovery);
    m.complete(p, NodeKind::ImportVersion)
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
    // `module_item` dispatches here on `export`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Export));
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

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use kitty_cst::Module;

    use super::{module, Parser};
    use crate::check_grammar;

    fn check(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            module(p);
        };
        check_grammar::<Module>(grammar, input, expected);
    }

    #[test]
    fn import_with_version() {
        check(
            "import Assembly from @std/assembly:1",
            expect![[r#"
                Module@0..36
                  ModuleImport@0..36
                    Import@0..6 "import"
                    Whitespace@6..7 " "
                    ImportAliasType@7..15
                      IdentifierType@7..15 "Assembly"
                    Whitespace@15..16 " "
                    From@16..20 "from"
                    Whitespace@20..21 " "
                    Package@21..34 "@std/assembly"
                    ImportVersion@34..36
                      Colon@34..35 ":"
                      Number@35..36 "1""#]],
        );
    }

    #[test]
    fn import_block_with_version() {
        check(
            "import from @std/assembly:1\n  Assembly\n",
            expect![[r#"
                Module@0..39
                  ModuleImport@0..39
                    Import@0..6 "import"
                    Whitespace@6..7 " "
                    From@7..11 "from"
                    Whitespace@11..12 " "
                    Package@12..25 "@std/assembly"
                    ImportVersion@25..27
                      Colon@25..26 ":"
                      Number@26..27 "1"
                    Newline@27..28 "\n"
                    Indent@28..30 "  "
                    ImportAliasType@30..38
                      IdentifierType@30..38 "Assembly"
                    Newline@38..39 "\n"
                    Dedent@39..39 """#]],
        );
    }

    #[test]
    fn import_version_missing_number() {
        // Unhappy path: a `:` with no version after it.
        check(
            "import Assembly from @std/assembly:",
            expect![[r#"
                Module@0..35
                  ModuleImport@0..35
                    Import@0..6 "import"
                    Whitespace@6..7 " "
                    ImportAliasType@7..15
                      IdentifierType@7..15 "Assembly"
                    Whitespace@15..16 " "
                    From@16..20 "from"
                    Whitespace@20..21 " "
                    Package@21..34 "@std/assembly"
                    ImportVersion@34..35
                      Colon@34..35 ":"
                      Missing@35..35
                error at 35: missing number"#]],
        );
    }

    #[test]
    fn attribute_with_nothing_after_it() {
        // Unhappy path: the attribute sits beside a `Missing` declaration.
        check(
            "@label(\"x\")",
            expect![[r#"
                Module@0..11
                  ModuleLocal@0..11
                    Attribute@0..11
                      At@0..1 "@"
                      IdentifierValue@1..6 "label"
                      FunctionArgList@6..11
                        ParenOpen@6..7 "("
                        FunctionArgPositional@7..10
                          ExpressionLiteral@7..10
                            String@7..10 "\"x\""
                        ParenClose@10..11 ")"
                    Missing@11..11
                error at 11: missing ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’"#]],
        );
    }
}
