use kitty_lexer::TokenKind;
use kitty_syntax::NodeKind;

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

const TYPE_PATH_FIRST: [TokenKind; 2] = [TokenKind::Identifier, TokenKind::SelfUpper];

/// A qualified type
pub(crate) fn type_path(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let mut lhs = type_name(p, recovery)?;

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
fn type_name(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set([TokenKind::Identifier, TokenKind::SelfUpper]) {
        Some(p.mark_kind(NodeKind::TypeName))
    } else {
        p.error(recovery);
        None
    }
}

/// Parametrized type with type arguments (e.g. `List[Number]`, same as `Vec<f64>` in Rust)
fn type_generic(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::BracketOpen));
    let m = lhs.precede(p);
    generic_arg_list(p, recovery);
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

const TYPE_ANNOTATION_FIRST: [TokenKind; 5] = [
    TokenKind::Identifier,
    TokenKind::SelfUpper,
    TokenKind::ParenOpen,
    TokenKind::FnUpper,
    TokenKind::Impl,
];

/// A type description.
pub(crate) fn type_annotation(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at_set(TYPE_PATH_FIRST) {
        type_path(p, recovery)?
    } else if p.at(TokenKind::ParenOpen) {
        type_tuple(p, recovery)
    } else if p.at(TokenKind::FnUpper) {
        type_function(p, recovery)
    } else if p.at(TokenKind::Impl) {
        type_impl_trait(p, recovery)
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
        loop {
            type_annotation(p, recovery_tuple);
            if !p.bump_if_at(TokenKind::Comma) {
                break;
            }
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
        loop {
            type_annotation(
                p,
                recovery_function.union([TokenKind::ParenClose, TokenKind::Arrow]),
            );
            if !p.bump_if_at(TokenKind::Comma) {
                break;
            }
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

fn type_impl_trait(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Impl));
    let recovery_trait = recovery.union(TYPE_PATH_FIRST);
    let m = p.start();
    p.bump(); // Consume 'impl'
    type_path(p, recovery_trait);
    m.complete(p, NodeKind::TypeTrait)
}

pub(crate) fn generic_param_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::BracketOpen));
    let recovery_param_list = recovery
        .union(TYPE_PATH_FIRST)
        .union([TokenKind::Comma, TokenKind::BracketClose]);
    let m = p.start();
    p.bump(); // Consume '['
    if !p.at(TokenKind::BracketClose) {
        loop {
            generic_param(p, recovery_param_list);
            if !p.bump_if_at(TokenKind::Comma) {
                break;
            }
        }
    }
    p.expect(TokenKind::BracketClose, recovery);
    m.complete(p, NodeKind::GenericParamList)
}

fn generic_param(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    // Parse generic type name
    p.expect(TokenKind::Identifier, recovery);
    // Parse generic type bounds
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        type_bound_list(p, recovery);
    }
    // Parse default concrete type
    if p.at(TokenKind::Equal) {
        p.bump(); // Consume '='
        type_path(p, recovery);
    }
    m.complete(p, NodeKind::GenericParam)
}

pub(crate) fn generic_arg_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::BracketOpen));
    let recovery_arg_list = recovery
        .union(TYPE_PATH_FIRST)
        .union([TokenKind::Comma, TokenKind::BracketClose]);
    let m = p.start();
    p.bump(); // Consume '['.
    'all: {
        if p.at(TokenKind::BracketClose) {
            break 'all; // End all args
        }
        // First process positional args
        'positional: loop {
            if p.at(TokenKind::Identifier) && p.lookahead_at(1, TokenKind::Colon) {
                break 'positional; // End positional args
            }

            generic_positional_arg(p, recovery_arg_list);

            if !p.at(TokenKind::Comma) {
                break 'all; // End all args
            }
            p.bump(); // Consume ','
        }
        // Then process labelled args
        loop {
            generic_labelled_arg(p, recovery_arg_list);

            if !p.at(TokenKind::Comma) {
                break 'all;
            }
            p.bump(); // Consume ','
        }
    }
    p.expect(TokenKind::BracketClose, recovery);
    m.complete(p, NodeKind::GenericArgList)
}

fn generic_positional_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    type_annotation(p, recovery);
    m.complete(p, NodeKind::GenericArgPositional)
}

fn generic_labelled_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Identifier));
    let m = p.start();
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::Colon, recovery);
    type_annotation(p, recovery);
    m.complete(p, NodeKind::GenericArgLabelled)
}

pub(crate) fn type_bound_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    loop {
        type_bound(p, recovery);
        if !p.bump_if_at(TokenKind::Plus) {
            break;
        }
    }
    m.complete(p, NodeKind::TypeBoundList)
}

fn type_bound(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Identifier, recovery);
    m.complete(p, NodeKind::TypeBound)
}

pub(crate) fn where_clause(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Where));
    let recovery_where = recovery.union([TokenKind::Dedent]);
    let m = p.start();
    p.bump(); // Consume 'where'
    p.expect(TokenKind::Indent, recovery);
    while !p.at(TokenKind::Dedent) {
        where_bound(p, recovery_where);
    }
    p.bump(); // Consume <dedent>
    m.complete(p, NodeKind::WhereClause)
}

pub(crate) fn where_bound(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Identifier, recovery);
    p.expect(TokenKind::Colon, recovery);
    type_bound_list(p, recovery);
    m.complete(p, NodeKind::WhereBound)
}

#[cfg(test)]
mod tests {
    use super::{generic_param_list, type_annotation, type_path, Parser, TokenSet};
    use crate::check_grammar;
    use expect_test::expect;
    use kitty_cst::{GenericParamList, TypeAnnotation, TypePath};

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

    fn check_generic_param_list(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            generic_param_list(p, TokenSet::NONE);
        };
        check_grammar::<GenericParamList>(grammar, input, expected);
    }

    #[test]
    fn type_path_name() {
        // Happy path
        check_type_path(
            "Number",
            expect![[r#"
                TypeName@0..6
                  Identifier@0..6 "Number""#]],
        );
    }

    #[test]
    fn type_annotation_name() {
        // Happy path
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
                  GenericArgList@4..12
                    BracketOpen@4..5 "["
                    GenericArgPositional@5..11
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
                  GenericArgList@3..15
                    BracketOpen@3..4 "["
                    GenericArgPositional@4..7
                      TypeName@4..7
                        Identifier@4..7 "Key"
                    Comma@7..8 ","
                    Whitespace@8..9 " "
                    GenericArgPositional@9..14
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
                    GenericArgList@6..15
                      BracketOpen@6..7 "["
                      GenericArgPositional@7..9
                        TypeName@7..9
                          Identifier@7..9 "Ok"
                      Comma@9..10 ","
                      Whitespace@10..11 " "
                      GenericArgPositional@11..14
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
                  GenericArgList@4..11
                    BracketOpen@4..5 "["
                    GenericArgPositional@5..11
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
                error at 18: missing identifier, ‘Self’, ‘(’, ‘Fn’, or ‘impl’"#]],
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
                  Missing@4..4
                error at 4: missing identifier or ‘Self’"#]],
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
                    GenericArgList@6..30
                      BracketOpen@6..7 "["
                      GenericArgPositional@7..29
                        TypeGeneric@7..29
                          TypeName@7..13
                            Identifier@7..13 "Result"
                          GenericArgList@13..29
                            BracketOpen@13..14 "["
                            GenericArgPositional@14..20
                              TypeName@14..20
                                Identifier@14..20 "Number"
                            Comma@20..21 ","
                            Whitespace@21..22 " "
                            GenericArgPositional@22..28
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
        // Happy path: type with extra whitespace and multiple chained operators (generic, projection, association).
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
                      GenericArgList@7..27
                        BracketOpen@7..8 "["
                        Whitespace@8..9 " "
                        GenericArgPositional@9..25
                          TypeGeneric@9..25
                            TypeName@9..14
                              Identifier@9..14 "Inner"
                            Whitespace@14..15 " "
                            GenericArgList@15..25
                              BracketOpen@15..16 "["
                              Whitespace@16..17 " "
                              GenericArgPositional@17..23
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
                    GenericArgList@3..15
                      BracketOpen@3..4 "["
                      GenericArgPositional@4..7
                        TypeName@4..7
                          Identifier@4..7 "Key"
                      Comma@7..8 ","
                      Whitespace@8..9 " "
                      GenericArgPositional@9..14
                        TypeName@9..14
                          Identifier@9..14 "Value"
                      BracketClose@14..15 "]"
                  Dot@15..16 "."
                  Missing@16..16
                error at 16: missing identifier"#]],
        );
    }

    #[test]
    fn generic_param_single_bound() {
        // Happy path: A generic parameter with a single type bound.
        check_generic_param_list(
            "[T: Display]",
            expect![[r#"
                GenericParamList@0..12
                  BracketOpen@0..1 "["
                  GenericParam@1..11
                    Identifier@1..2 "T"
                    Colon@2..3 ":"
                    Whitespace@3..4 " "
                    TypeBoundList@4..11
                      TypeBound@4..11
                        Identifier@4..11 "Display"
                  BracketClose@11..12 "]""#]],
        );
    }

    #[test]
    fn generic_param_multiple() {
        // Happy path: A generic parameter list with one parameter that has a bound
        // and another that provides a default concrete type.
        check_generic_param_list(
            "[T: Display, U = Number]",
            expect![[r#"
                GenericParamList@0..24
                  BracketOpen@0..1 "["
                  GenericParam@1..11
                    Identifier@1..2 "T"
                    Colon@2..3 ":"
                    Whitespace@3..4 " "
                    TypeBoundList@4..11
                      TypeBound@4..11
                        Identifier@4..11 "Display"
                  Comma@11..12 ","
                  Whitespace@12..13 " "
                  GenericParam@13..23
                    Identifier@13..14 "U"
                    Whitespace@14..15 " "
                    Equal@15..16 "="
                    Whitespace@16..17 " "
                    TypeName@17..23
                      Identifier@17..23 "Number"
                  BracketClose@23..24 "]""#]],
        );
    }

    #[test]
    fn generic_param_default_only() {
        // Happy path: A generic parameter that only provides a default type.
        check_generic_param_list(
            "[T=Number]",
            expect![[r#"
                GenericParamList@0..10
                  BracketOpen@0..1 "["
                  GenericParam@1..9
                    Identifier@1..2 "T"
                    Equal@2..3 "="
                    TypeName@3..9
                      Identifier@3..9 "Number"
                  BracketClose@9..10 "]""#]],
        );
    }

    #[test]
    fn generic_param_missing_bound() {
        // Unhappy path: the parameter has a colon but no bound.
        check_generic_param_list(
            "[T:]",
            expect![[r#"
                GenericParamList@0..4
                  BracketOpen@0..1 "["
                  GenericParam@1..3
                    Identifier@1..2 "T"
                    Colon@2..3 ":"
                    TypeBoundList@3..3
                      TypeBound@3..3
                        Missing@3..3
                  BracketClose@3..4 "]"
                error at 3: missing identifier"#]],
        );
    }

    #[test]
    fn generic_param_missing_default() {
        // Unhappy path: the parameter has an equal sign but no default type.
        check_generic_param_list(
            "[T=]",
            expect![[r#"
                GenericParamList@0..4
                  BracketOpen@0..1 "["
                  GenericParam@1..3
                    Identifier@1..2 "T"
                    Equal@2..3 "="
                    Missing@3..3
                  BracketClose@3..4 "]"
                error at 3: missing identifier or ‘Self’"#]],
        );
    }

    #[test]
    fn generic_arg_labelled() {
        // Happy path: A generic type with a single labelled generic argument.
        check_type_path(
            "Foo[bar: Number]",
            expect![[r#"
                TypeGeneric@0..16
                  TypeName@0..3
                    Identifier@0..3 "Foo"
                  GenericArgList@3..16
                    BracketOpen@3..4 "["
                    GenericArgLabelled@4..15
                      Identifier@4..7 "bar"
                      Colon@7..8 ":"
                      Whitespace@8..9 " "
                      TypeName@9..15
                        Identifier@9..15 "Number"
                    BracketClose@15..16 "]""#]],
        );
    }

    #[test]
    fn generic_arg_mixed() {
        // Happy path: A generic type with a positional generic argument followed by a labelled one.
        check_type_path(
            "Foo[Number, bar: String]",
            expect![[r#"
                TypeGeneric@0..24
                  TypeName@0..3
                    Identifier@0..3 "Foo"
                  GenericArgList@3..24
                    BracketOpen@3..4 "["
                    GenericArgPositional@4..10
                      TypeName@4..10
                        Identifier@4..10 "Number"
                    Comma@10..11 ","
                    Whitespace@11..12 " "
                    GenericArgLabelled@12..23
                      Identifier@12..15 "bar"
                      Colon@15..16 ":"
                      Whitespace@16..17 " "
                      TypeName@17..23
                        Identifier@17..23 "String"
                    BracketClose@23..24 "]""#]],
        );
    }

    #[test]
    fn generic_arg_labelled_missing_colon() {
        // Unhappy path: labelled generic argument missing the colon.
        check_type_path(
            "Foo[bar Number]",
            expect![[r#"
                TypeGeneric@0..14
                  TypeName@0..3
                    Identifier@0..3 "Foo"
                  GenericArgList@3..14
                    BracketOpen@3..4 "["
                    GenericArgPositional@4..7
                      TypeName@4..7
                        Identifier@4..7 "bar"
                    Whitespace@7..8 " "
                    Error@8..14
                      Identifier@8..14 "Number"
                error at 8..14: expected ‘]’, but found identifier"#]],
        );
    }
}
