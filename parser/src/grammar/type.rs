use kitty_lexer::TokenKind;
use kitty_syntax::NodeKind;

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

const TYPE_PATH_FIRST: [TokenKind; 1] = [TokenKind::Identifier];

/// A qualified type
pub(crate) fn type_path(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let mut lhs = type_name(p, recovery);

    loop {
        if p.at(TokenKind::BracketOpen) {
            lhs = type_generic(p, lhs, recovery);
        } else if p.at(TokenKind::DotBracketOpen) {
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
    assert!(p.at(TokenKind::BracketOpen));
    let recovery_generic = recovery
        .union(TYPE_PATH_FIRST)
        .union([TokenKind::BracketClose]);
    let m = lhs.precede(p);
    p.bump(); // Consume '['.
    if !p.at_end() && !p.at(TokenKind::BracketClose) {
        loop {
            type_path(p, recovery_generic);
            if !p.at(TokenKind::Comma) {
                break;
            }
            p.bump(); // Consume comma.
        }
    }
    p.expect(TokenKind::BracketClose, recovery);
    m.complete(p, NodeKind::TypeGeneric)
}

/// Trait projection (e.g. `T.[Iterator]`, same as `<T as Iterator>` in Rust)
fn type_projection(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::DotBracketOpen));
    let recovery_projection = recovery.union([TokenKind::BracketClose]);
    let m = lhs.precede(p);
    p.bump(); // Consume '.['.
    p.expect(TokenKind::Identifier, recovery_projection);
    p.expect(TokenKind::BracketClose, recovery);
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

const TYPE_ANNOTATION_FIRST: [TokenKind; 4] = [
    TokenKind::Identifier,
    TokenKind::ParenOpen,
    TokenKind::FnUpper,
    TokenKind::Impl,
];

/// A type description.
pub(crate) fn type_annotation(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Identifier) {
        type_path(p, recovery)?
    } else if p.at(TokenKind::ParenOpen) {
        type_tuple(p, recovery)
    } else if p.at(TokenKind::FnUpper) {
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
    assert!(p.at(TokenKind::ParenOpen));
    let recovery_tuple = recovery
        .union(TYPE_ANNOTATION_FIRST)
        .union([TokenKind::ParenClose]);
    let m = p.start();
    p.bump(); // Consume '('
    if !p.at(TokenKind::ParenClose) {
        type_annotation(p, recovery_tuple);
        while p.at(TokenKind::Comma) {
            p.bump();
            type_annotation(p, recovery_tuple);
        }
    }
    p.expect(TokenKind::ParenClose, recovery_tuple);
    m.complete(p, NodeKind::TypeTuple)
}

/// A function type (e.g. `Fn (Number, String) -> Boolean`)
fn type_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::FnUpper));
    let recovery_function = recovery.union(TYPE_ANNOTATION_FIRST);
    let m = p.start();
    p.bump(); // Consume 'Fn"
    p.expect(
        TokenKind::ParenOpen,
        recovery_function.union([TokenKind::ParenClose, TokenKind::Arrow]),
    );
    if !p.at(TokenKind::ParenClose) {
        type_annotation(
            p,
            recovery_function.union([TokenKind::ParenClose, TokenKind::Arrow]),
        );
        while p.at(TokenKind::Comma) {
            p.bump();
            type_annotation(
                p,
                recovery_function.union([TokenKind::ParenClose, TokenKind::Arrow]),
            );
        }
    }
    p.expect(
        TokenKind::ParenClose,
        recovery_function.union([TokenKind::Arrow]),
    );
    p.expect(TokenKind::Arrow, recovery_function);
    type_annotation(p, recovery_function);
    m.complete(p, NodeKind::TypeFunction)
}

fn type_trait(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Impl));
    let recovery_trait = recovery.union(TYPE_PATH_FIRST);
    let m = p.start();
    p.bump(); // Consume 'impl'
    type_path(p, recovery_trait);
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

    #[test]
    fn type_generic_single_arg() {
        // Happy path: a generic type with one type argument.
        check_type_path(
            "List[Number]",
            expect![[r#"
                TypeGeneric@0..12
                  TypeName@0..4
                    Identifier@0..4 "List"
                  BracketOpen@4..5 "["
                  TypeName@5..11
                    Identifier@5..11 "Number"
                  BracketClose@11..12 "]""#]],
        );
    }

    #[test]
    fn type_generic_multiple_args() {
        // Happy path: a generic type with two type arguments.
        check_type_path(
            "Map[Key, Value]",
            expect![[r#"
                TypeGeneric@0..15
                  TypeName@0..3
                    Identifier@0..3 "Map"
                  BracketOpen@3..4 "["
                  TypeName@4..7
                    Identifier@4..7 "Key"
                  Comma@7..8 ","
                  Whitespace@8..9 " "
                  TypeName@9..14
                    Identifier@9..14 "Value"
                  BracketClose@14..15 "]""#]],
        );
    }

    #[test]
    fn type_projection_happy() {
        // Happy path: trait projection on a type.
        check_type_path(
            "T.[Iterator]",
            expect![[r#"
                TypeProjection@0..12
                  TypeName@0..1
                    Identifier@0..1 "T"
                  DotBracketOpen@1..3 ".["
                  Identifier@3..11 "Iterator"
                  BracketClose@11..12 "]""#]],
        );
    }

    #[test]
    fn type_association_happy() {
        // Happy path: associated type access.
        check_type_path(
            "Iterator.Item",
            expect![[r#"
                TypeAssociation@0..13
                  TypeName@0..8
                    Identifier@0..8 "Iterator"
                  Dot@8..9 "."
                  Identifier@9..13 "Item""#]],
        );
    }

    #[test]
    fn type_chain_generic_association() {
        // Happy path: chaining a generic and an association.
        check_type_path(
            "Result[Ok, Err].Error",
            expect![[r#"
                TypeAssociation@0..21
                  TypeGeneric@0..15
                    TypeName@0..6
                      Identifier@0..6 "Result"
                    BracketOpen@6..7 "["
                    TypeName@7..9
                      Identifier@7..9 "Ok"
                    Comma@9..10 ","
                    Whitespace@10..11 " "
                    TypeName@11..14
                      Identifier@11..14 "Err"
                    BracketClose@14..15 "]"
                  Dot@15..16 "."
                  Identifier@16..21 "Error""#]],
        );
    }

    #[test]
    fn type_tuple_happy() {
        // Happy path: a tuple type.
        check_type_annotation(
            "(Number, String)",
            expect![[r#"
                TypeTuple@0..16
                  ParenOpen@0..1 "("
                  TypeName@1..7
                    Identifier@1..7 "Number"
                  Comma@7..8 ","
                  Whitespace@8..9 " "
                  TypeName@9..15
                    Identifier@9..15 "String"
                  ParenClose@15..16 ")""#]],
        );
    }

    #[test]
    fn type_function_happy() {
        // Happy path: a function type.
        check_type_annotation(
            "Fn(Number, String) -> Boolean",
            expect![[r#"
                TypeFunction@0..29
                  FnUpper@0..2 "Fn"
                  ParenOpen@2..3 "("
                  TypeName@3..9
                    Identifier@3..9 "Number"
                  Comma@9..10 ","
                  Whitespace@10..11 " "
                  TypeName@11..17
                    Identifier@11..17 "String"
                  ParenClose@17..18 ")"
                  Whitespace@18..19 " "
                  Arrow@19..21 "->"
                  Whitespace@21..22 " "
                  TypeName@22..29
                    Identifier@22..29 "Boolean""#]],
        );
    }

    #[test]
    fn type_trait_happy() {
        // Happy path: a trait type.
        check_type_annotation(
            "impl Number",
            expect![[r#"
                TypeTrait@0..11
                  Impl@0..4 "impl"
                  Whitespace@4..5 " "
                  TypeName@5..11
                    Identifier@5..11 "Number""#]],
        );
    }

    #[test]
    fn type_generic_missing_brace_close() {
        // Unhappy path: generic type missing the closing ']' (BracketClose).
        check_type_path(
            "List[Number",
            expect![[r#"
                TypeGeneric@0..11
                  TypeName@0..4
                    Identifier@0..4 "List"
                  BracketOpen@4..5 "["
                  TypeName@5..11
                    Identifier@5..11 "Number"
                  Missing@11..11
                error at 11: missing ‘]’"#]],
        );
    }

    #[test]
    fn type_projection_missing_identifier() {
        // Unhappy path: projection missing the identifier.
        check_type_path(
            "T.[]",
            expect![[r#"
                TypeProjection@0..4
                  TypeName@0..1
                    Identifier@0..1 "T"
                  DotBracketOpen@1..3 ".["
                  Missing@3..3
                  BracketClose@3..4 "]"
                error at 3: missing identifier"#]],
        );
    }

    #[test]
    fn type_projection_missing_brace_close() {
        // Unhappy path: projection missing the closing brace.
        check_type_path(
            "T.[Iterator",
            expect![[r#"
                TypeProjection@0..11
                  TypeName@0..1
                    Identifier@0..1 "T"
                  DotBracketOpen@1..3 ".["
                  Identifier@3..11 "Iterator"
                  Missing@11..11
                error at 11: missing ‘]’"#]],
        );
    }

    #[test]
    fn type_association_missing_identifier() {
        // Unhappy path: association missing an identifier after the dot.
        check_type_path(
            "Iterator.",
            expect![[r#"
                TypeAssociation@0..9
                  TypeName@0..8
                    Identifier@0..8 "Iterator"
                  Dot@8..9 "."
                  Missing@9..9
                error at 9: missing identifier"#]],
        );
    }

    #[test]
    fn type_tuple_missing_paren_close() {
        // Unhappy path: tuple type missing the closing parenthesis.
        check_type_annotation(
            "(Number, String",
            expect![[r#"
                TypeTuple@0..15
                  ParenOpen@0..1 "("
                  TypeName@1..7
                    Identifier@1..7 "Number"
                  Comma@7..8 ","
                  Whitespace@8..9 " "
                  TypeName@9..15
                    Identifier@9..15 "String"
                  Missing@15..15
                error at 15: missing ‘)’"#]],
        );
    }

    #[test]
    fn type_function_missing_return_type() {
        // Unhappy path: function type with no return type after the argument list.
        // TODO handle this better
        check_type_annotation(
            "Fn(Number, String)",
            expect![[r#"
                TypeFunction@0..18
                  FnUpper@0..2 "Fn"
                  ParenOpen@2..3 "("
                  TypeName@3..9
                    Identifier@3..9 "Number"
                  Comma@9..10 ","
                  Whitespace@10..11 " "
                  TypeName@11..17
                    Identifier@11..17 "String"
                  ParenClose@17..18 ")"
                  Missing@18..18
                  Missing@18..18
                error at 18: missing ‘->’
                error at 18: missing ‘->’, identifier, ‘(’, ‘Fn’, or ‘impl’"#]],
        );
    }

    #[test]
    fn type_function_missing_argument() {
        // Unhappy path: function type with an empty argument list where an argument is expected.
        check_type_annotation(
            "Fn()Boolean",
            expect![[r#"
                TypeFunction@0..11
                  FnUpper@0..2 "Fn"
                  ParenOpen@2..3 "("
                  ParenClose@3..4 ")"
                  Missing@4..4
                  TypeName@4..11
                    Identifier@4..11 "Boolean"
                error at 4: missing ‘->’"#]],
        );
    }

    #[test]
    fn type_trait_missing_type_path() {
        // Unhappy path: trait type missing the type after the 'Impl' keyword.
        check_type_annotation(
            "impl",
            expect![[r#"
                TypeTrait@0..4
                  Impl@0..4 "impl"
                  TypeName@4..4
                    Missing@4..4
                error at 4: missing identifier"#]],
        );
    }

    #[test]
    fn type_complex_nested() {
        // Happy path: a complex nested type combining generics and associations.
        check_type_path(
            "Option[Result[Number, String]].Item",
            expect![[r#"
                TypeAssociation@0..35
                  TypeGeneric@0..30
                    TypeName@0..6
                      Identifier@0..6 "Option"
                    BracketOpen@6..7 "["
                    TypeGeneric@7..29
                      TypeName@7..13
                        Identifier@7..13 "Result"
                      BracketOpen@13..14 "["
                      TypeName@14..20
                        Identifier@14..20 "Number"
                      Comma@20..21 ","
                      Whitespace@21..22 " "
                      TypeName@22..28
                        Identifier@22..28 "String"
                      BracketClose@28..29 "]"
                    BracketClose@29..30 "]"
                  Dot@30..31 "."
                  Identifier@31..35 "Item""#]],
        );
    }

    #[test]
    fn type_complex_nested_whitespace() {
        // Happy path with extra whitespace and multiple chained operators (generic, projection, association).
        check_type_annotation(
            " Outer [ Inner [ Number ] ] .[ Trait ] .Member ",
            expect![[r#"
                TypeAssociation@0..47
                  Whitespace@0..1 " "
                  TypeProjection@1..38
                    TypeGeneric@1..27
                      TypeName@1..6
                        Identifier@1..6 "Outer"
                      Whitespace@6..7 " "
                      BracketOpen@7..8 "["
                      Whitespace@8..9 " "
                      TypeGeneric@9..25
                        TypeName@9..14
                          Identifier@9..14 "Inner"
                        Whitespace@14..15 " "
                        BracketOpen@15..16 "["
                        Whitespace@16..17 " "
                        TypeName@17..23
                          Identifier@17..23 "Number"
                        Whitespace@23..24 " "
                        BracketClose@24..25 "]"
                      Whitespace@25..26 " "
                      BracketClose@26..27 "]"
                    Whitespace@27..28 " "
                    DotBracketOpen@28..30 ".["
                    Whitespace@30..31 " "
                    Identifier@31..36 "Trait"
                    Whitespace@36..37 " "
                    BracketClose@37..38 "]"
                  Whitespace@38..39 " "
                  Dot@39..40 "."
                  Identifier@40..46 "Member"
                  Whitespace@46..47 " ""#]],
        );
    }

    /*
    #[test]
    fn type_annotation_invalid_start() {
        // Unhappy path: an invalid starting token for a type annotation.
        check_type_annotation(
            "@",
            expect![[r#"
            // (auto-generated snapshot showing an error about expected type start)
        "#]],
        );
    }
    */

    #[test]
    fn type_path_multiple_errors() {
        // Unhappy path: multiple errors in one input—
        // generic missing closing brace and an association missing the identifier.
        check_type_path(
            "Map[Key, Value].",
            expect![[r#"
                TypeAssociation@0..16
                  TypeGeneric@0..15
                    TypeName@0..3
                      Identifier@0..3 "Map"
                    BracketOpen@3..4 "["
                    TypeName@4..7
                      Identifier@4..7 "Key"
                    Comma@7..8 ","
                    Whitespace@8..9 " "
                    TypeName@9..14
                      Identifier@9..14 "Value"
                    BracketClose@14..15 "]"
                  Dot@15..16 "."
                  Missing@16..16
                error at 16: missing identifier"#]],
        );
    }
}
