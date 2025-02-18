use kitty_lexer::TokenKind;
use kitty_syntax::NodeKind;

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

/// A qualified type
pub(crate) fn type_path(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let mut lhs = type_name(p, recovery);

    loop {
        if p.at(TokenKind::BraceOpen) {
            lhs = type_generic(p, lhs, recovery);
        } else if p.at(TokenKind::DotBraceOpen) {
            lhs = type_projection(p, lhs, recovery);
        } else if p.at(TokenKind::Dot) {
            lhs = type_association(p, lhs, recovery);
        } else {
            break;
        }
    }

    Some(lhs)
}

/// Named type
fn type_name(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Identifier, recovery);
    m.complete(p, NodeKind::TypeName)
}

/// Parametrized type with type arguments (e.g. `List[Number]`, same as `Vec<f64>` in Rust)
fn type_generic(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::BraceOpen));
    let m = lhs.precede(p);
    p.bump(); // Consume '['.
    if !p.at_end() && !p.at(TokenKind::BraceClose) {
        loop {
            type_path(p, recovery);
            if !p.at(TokenKind::Comma) {
                break;
            }
            p.bump(); // Consume comma.
        }
    }
    p.expect(TokenKind::BraceClose, recovery);
    m.complete(p, NodeKind::TypeGeneric)
}

/// Trait projection (e.g. `T.[Iterator]`, same as `<T as Iterator>` in Rust)
fn type_projection(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::DotBraceOpen));
    let m = lhs.precede(p);
    p.bump(); // Consume '.['.
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::BraceClose, recovery);
    m.complete(p, NodeKind::TypeProjection)
}

/// Associated type access (e.g. `Iterator.Item`, same as `Iterator::Item` in Rust)
fn type_association(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Dot));
    let m = lhs.precede(p);
    p.bump(); // Consume '.'.
    p.expect(TokenKind::Identifier, recovery);
    m.complete(p, NodeKind::TypeAssociation)
}

/// A type description.
pub(crate) fn type_annotation(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Identifier) {
        type_path(p, recovery)?
    } else if p.at(TokenKind::ParenOpen) {
        type_tuple(p, recovery)
    } else if p.at(TokenKind::Fn) {
        type_function(p, recovery)
    } else if p.at(TokenKind::Impl) {
        type_trait(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

/// A tuple type (e.g. `(Number, String)`)
fn type_tuple(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::ParenOpen, recovery);
    type_path(p, recovery);
    while p.at(TokenKind::Comma) {
        p.bump();
        type_path(p, recovery);
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::ParenExpr)
}

/// A function type (e.g. `Fn (Number, String) -> Boolean`)
fn type_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::FnUpper, recovery);
    p.expect(TokenKind::ParenOpen, recovery);
    type_annotation(p, recovery);
    while p.at(TokenKind::Comma) {
        p.bump();
        type_annotation(p, recovery);
    }
    p.expect(TokenKind::ParenClose, recovery);
    type_path(p, recovery);
    m.complete(p, NodeKind::TypeFunction)
}

fn type_trait(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Impl, recovery);
    type_path(p, recovery);
    m.complete(p, NodeKind::TypeTrait)
}

#[cfg(test)]
mod tests {
    use super::{type_annotation, type_path, Parser, TokenSet};
    use crate::check_grammar;
    use expect_test::expect;
    use kitty_cst::{TypeAnnotation, TypePath};

    fn check_type_path(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            type_path(p, TokenSet::NONE);
        };
        check_grammar::<TypePath>(grammar, input, expected);
    }

    fn check_type_annotation(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            type_annotation(p, TokenSet::NONE);
        };
        check_grammar::<TypeAnnotation>(grammar, input, expected);
    }

    #[test]
    fn type_path_name() {
        check_type_path(
            "Number",
            expect![[r#"
                TypeName@0..6
                  Identifier@0..6 "Number""#]],
        );
    }

    #[test]
    fn type_annotation_name() {
        check_type_annotation(
            "Number",
            expect![[r#"
                TypeName@0..6
                  Identifier@0..6 "Number""#]],
        );
    }
}
