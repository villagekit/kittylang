use kitty_syntax::{NodeKind, TokenKind};

use crate::{
    marker::{CompletedMarker, Marker},
    parser::Parser,
    token_set::TokenSet,
};

use super::{
    expression::expression,
    function::{function_arg_list, function_declaration, FunctionForm, FUNCTION_ARG_LIST_FIRST},
    r#type::{
        generic_bound_list, generic_param_list, generic_where_clause, type_annotation, type_path,
    },
};

/// The tokens that start a declaration: its keyword, or the `@` of an
/// attribute on the line before it.
pub(crate) const DECLARATION_FIRST: [TokenKind; 8] = [
    TokenKind::At,
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Enum,
    TokenKind::Struct,
    TokenKind::Trait,
    TokenKind::Impl,
];

pub(crate) fn declaration(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let m = p.start();
    attributes(p, recovery);
    let cm = if p.at(TokenKind::Type) {
        declaration_type(p, recovery, m)
    } else if p.at(TokenKind::Const) {
        declaration_constant(p, recovery, m)
    } else if p.at(TokenKind::Fn) {
        declaration_function(p, recovery, m)
    } else if p.at(TokenKind::Enum) {
        declaration_enum(p, recovery, m)
    } else if p.at(TokenKind::Struct) {
        declaration_struct(p, recovery, m)
    } else if p.at(TokenKind::Trait) {
        declaration_trait(p, recovery, m)
    } else if p.at(TokenKind::Impl) {
        declaration_impl_trait(p, recovery, m)
    } else {
        m.abandon(p);
        p.error(recovery);
        return None;
    };
    Some(cm)
}

/// Zero or more attributes before a declaration, each its own node. The
/// caller has started the declaration's marker, so they become the
/// declaration's first children; with no declaration after them the
/// caller abandons the marker and they sit beside the `Missing` node.
fn attributes(p: &mut Parser, recovery: TokenSet) {
    while p.at(TokenKind::At) {
        attribute(p, recovery);
    }
}

/// `@name`, with an argument list in any of the call forms when one
/// follows. A name left out is missing, not an error that eats the `(`
/// after it.
fn attribute(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `attributes` dispatches here on `@`.
    debug_assert_eq!(p.peek(), Some(TokenKind::At));
    let m = p.start();
    p.bump(); // Consume '@'
    p.expect(
        TokenKind::IdentifierValue,
        recovery.union(FUNCTION_ARG_LIST_FIRST),
    );
    if p.at_set(FUNCTION_ARG_LIST_FIRST) {
        function_arg_list(p, recovery);
    }
    m.complete(p, NodeKind::Attribute)
}

/// Type alias declaration
fn declaration_type(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    // `declaration` and `impl_trait_item` dispatch here on `type`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Type));
    let recovery_type = recovery.union([TokenKind::Equal, TokenKind::Colon]);
    p.bump(); // Consume 'type'
    p.expect(TokenKind::IdentifierType, recovery_type);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery_type);
    }
    p.expect(TokenKind::Equal, recovery);
    type_annotation(p, recovery);
    m.complete(p, NodeKind::DeclarationType)
}

/// Constant declaration
fn declaration_constant(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    declaration_constant_optional_type_value(p, recovery, m, false, true)
}

fn declaration_constant_optional_type_value(
    p: &mut Parser,
    recovery: TokenSet,
    m: Marker,
    has_type: bool,
    has_value: bool,
) -> CompletedMarker {
    // `declaration` and the item rules dispatch here on `const`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Const));
    p.bump(); // Consume 'const'
    p.expect(TokenKind::IdentifierValue, recovery);
    if has_type || p.at(TokenKind::Colon) {
        p.expect(TokenKind::Colon, recovery);
        type_annotation(p, recovery);
    }
    if has_value || p.at(TokenKind::Equal) {
        p.expect(TokenKind::Equal, recovery);
        expression(p, recovery);
    }
    m.complete(p, NodeKind::DeclarationConstant)
}

fn declaration_function(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    function_declaration(p, recovery, FunctionForm::Declaration, m)
}

fn declaration_struct(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    // `declaration` dispatches here on `struct`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Struct));
    let recovery_struct = recovery.union(STRUCT_ITEM_FIRST).union([TokenKind::Dedent]);
    p.bump(); // Consume 'struct'
    p.expect(TokenKind::IdentifierType, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    p.expect(TokenKind::Indent, recovery);
    if p.at(TokenKind::Where) {
        generic_where_clause(p, recovery);
    }
    while p.at_set(STRUCT_ITEM_FIRST) {
        struct_item(p, recovery_struct);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::DeclarationStruct)
}

const STRUCT_ITEM_FIRST: [TokenKind; 4] = [
    TokenKind::At,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Prop,
];

fn struct_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let m = p.start();
    attributes(p, recovery);
    let cm = if p.at(TokenKind::Const) {
        declaration_constant(p, recovery, m)
    } else if p.at(TokenKind::Fn) {
        declaration_function(p, recovery, m)
    } else if p.at(TokenKind::Prop) {
        declaration_prop(p, recovery, m)
    } else {
        m.abandon(p);
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn declaration_prop(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    declaration_prop_optional_type_value(p, recovery, m, true, false)
}

fn declaration_prop_optional_type_value(
    p: &mut Parser,
    recovery: TokenSet,
    m: Marker,
    has_type: bool,
    has_value: bool,
) -> CompletedMarker {
    // The item rules dispatch here on `prop`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Prop));
    p.bump(); // Consume 'prop'
    p.expect(TokenKind::IdentifierValue, recovery);
    if has_type || p.at(TokenKind::Colon) {
        p.expect(TokenKind::Colon, recovery);
        type_annotation(p, recovery);
    }
    if has_value || p.at(TokenKind::Equal) {
        p.expect(TokenKind::Equal, recovery);
        expression(p, recovery);
    }
    m.complete(p, NodeKind::DeclarationProp)
}

fn declaration_enum(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    // `declaration` dispatches here on `enum`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Enum));
    let recovery_enum = recovery.union(ENUM_ITEM_FIRST).union([TokenKind::Dedent]);
    p.bump(); // Consume 'enum'
    p.expect(TokenKind::IdentifierType, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    p.expect(TokenKind::Indent, recovery);
    if p.at(TokenKind::Where) {
        generic_where_clause(p, recovery);
    }
    while p.at_set(ENUM_ITEM_FIRST) {
        enum_item(p, recovery_enum);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::DeclarationEnum)
}

const ENUM_ITEM_FIRST: [TokenKind; 4] = [
    TokenKind::At,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Case,
];

fn enum_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let m = p.start();
    attributes(p, recovery);
    let cm = if p.at(TokenKind::Const) {
        declaration_constant(p, recovery, m)
    } else if p.at(TokenKind::Fn) {
        declaration_function(p, recovery, m)
    } else if p.at(TokenKind::Case) {
        enum_case(p, recovery, m)
    } else {
        m.abandon(p);
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn enum_case(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    // `enum_item` dispatches here on `case`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Case));
    p.bump(); // Consume 'case'
    p.expect(TokenKind::IdentifierType, recovery);
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        type_annotation(p, recovery);
    }
    m.complete(p, NodeKind::EnumCase)
}

fn declaration_trait(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    // `declaration` dispatches here on `trait`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Trait));
    let recovery_trait = recovery.union(TRAIT_ITEM_FIRST).union([TokenKind::Dedent]);
    p.bump(); // Consume 'trait'
    p.expect(TokenKind::IdentifierType, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    p.expect(TokenKind::Indent, recovery);
    if p.at(TokenKind::Where) {
        generic_where_clause(p, recovery);
    }
    while p.at_set(TRAIT_ITEM_FIRST) {
        trait_item(p, recovery_trait);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::DeclarationTrait)
}

const TRAIT_ITEM_FIRST: [TokenKind; 5] = [
    TokenKind::At,
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Prop,
];

fn trait_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let m = p.start();
    attributes(p, recovery);
    let cm = if p.at(TokenKind::Type) {
        trait_type(p, recovery, m)
    } else if p.at(TokenKind::Const) {
        trait_constant(p, recovery, m)
    } else if p.at(TokenKind::Fn) {
        trait_function(p, recovery, m)
    } else if p.at(TokenKind::Prop) {
        declaration_prop(p, recovery, m)
    } else {
        m.abandon(p);
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn trait_type(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    // `trait_item` dispatches here on `type`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Type));
    p.bump(); // Consume 'type'
    p.expect(TokenKind::IdentifierType, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        generic_bound_list(p, recovery);
    }
    if p.at(TokenKind::Equal) {
        p.bump(); // Consume '='
        type_annotation(p, recovery);
    }
    m.complete(p, NodeKind::DeclarationType)
}

fn trait_constant(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    declaration_constant_optional_type_value(p, recovery, m, false, false)
}

fn trait_function(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    function_declaration(p, recovery, FunctionForm::TraitDeclaration, m)
}

fn declaration_impl_trait(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    // `declaration` dispatches here on `impl`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Impl));
    let recovery_trait = recovery
        .union(IMPL_TRAIT_ITEM_FIRST)
        .union([TokenKind::Dedent]);
    p.bump(); // Consume 'impl'
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    type_path(p, recovery);
    p.expect(TokenKind::For, recovery);
    type_annotation(p, recovery);
    p.expect(TokenKind::Indent, recovery);
    if p.at(TokenKind::Where) {
        generic_where_clause(p, recovery);
    }
    while p.at_set(IMPL_TRAIT_ITEM_FIRST) {
        impl_trait_item(p, recovery_trait);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::DeclarationImplTrait)
}

const IMPL_TRAIT_ITEM_FIRST: [TokenKind; 5] = [
    TokenKind::At,
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Prop,
];

fn impl_trait_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let m = p.start();
    attributes(p, recovery);
    let cm = if p.at(TokenKind::Type) {
        declaration_type(p, recovery, m)
    } else if p.at(TokenKind::Const) {
        trait_impl_constant(p, recovery, m)
    } else if p.at(TokenKind::Fn) {
        trait_impl_function(p, recovery, m)
    } else if p.at(TokenKind::Prop) {
        trait_impl_prop(p, recovery, m)
    } else {
        m.abandon(p);
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn trait_impl_constant(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    declaration_constant_optional_type_value(p, recovery, m, false, true)
}

fn trait_impl_function(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    function_declaration(p, recovery, FunctionForm::Declaration, m)
}

fn trait_impl_prop(p: &mut Parser, recovery: TokenSet, m: Marker) -> CompletedMarker {
    declaration_prop_optional_type_value(p, recovery, m, false, true)
}

#[cfg(test)]
mod tests {
    use super::{declaration, Parser, TokenSet};
    use crate::check_grammar;
    use expect_test::expect;
    use indoc::indoc;
    use kitty_cst::Declaration;

    fn check(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            declaration(p, TokenSet::NONE);
        };
        check_grammar::<Declaration>(grammar, input, expected);
    }

    #[test]
    fn function_without_a_parameter_list_is_missing_one() {
        check(
            "fn f => 1",
            expect![[r#"
                DeclarationFunction@0..9
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..4 "f"
                  Whitespace@4..5 " "
                  FunctionParamList@5..5
                    Missing@5..5
                  FatArrow@5..7 "=>"
                  Whitespace@7..8 " "
                  FunctionBody@8..9
                    ExpressionLiteral@8..9
                      Number@8..9 "1"
                error at 5: missing ‘[’ or ‘(’"#]],
        );
    }

    #[test]
    fn function_with_a_stray_token_for_its_parameter_list_recovers() {
        check(
            "fn f 1 => x",
            expect![[r#"
                DeclarationFunction@0..11
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..4 "f"
                  Whitespace@4..5 " "
                  FunctionParamList@5..6
                    Error@5..6
                      Number@5..6 "1"
                  Whitespace@6..7 " "
                  FatArrow@7..9 "=>"
                  Whitespace@9..10 " "
                  FunctionBody@10..11
                    ExpressionReference@10..11
                      IdentifierValue@10..11 "x"
                error at 5..6: expected ‘[’ or ‘(’, but found number"#]],
        );
    }

    #[test]
    fn function_without_a_name_recovers_at_the_parameter_list() {
        check(
            "fn (x) => x",
            expect![[r#"
                DeclarationFunction@0..11
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Missing@3..3
                  FunctionParamList@3..6
                    ParenOpen@3..4 "("
                    FunctionParam@4..5
                      FunctionParamLabel@4..5
                        IdentifierValue@4..5 "x"
                    ParenClose@5..6 ")"
                  Whitespace@6..7 " "
                  FatArrow@7..9 "=>"
                  Whitespace@9..10 " "
                  FunctionBody@10..11
                    ExpressionReference@10..11
                      IdentifierValue@10..11 "x"
                error at 3: missing value-id or ‘from’"#]],
        );
    }

    #[test]
    fn trait_function_without_a_parameter_list_recovers() {
        check(
            indoc! {"
                trait Assembly
                    fn parts: Parts
            "},
            expect![[r#"
                DeclarationTrait@0..35
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  IdentifierType@6..14 "Assembly"
                  Newline@14..15 "\n"
                  Indent@15..19 "    "
                  DeclarationFunction@19..34
                    Fn@19..21 "fn"
                    Whitespace@21..22 " "
                    IdentifierValue@22..27 "parts"
                    FunctionParamList@27..27
                      Missing@27..27
                    Colon@27..28 ":"
                    Whitespace@28..29 " "
                    FunctionReturnType@29..34
                      TypeReference@29..34
                        IdentifierType@29..34 "Parts"
                  Newline@34..35 "\n"
                  Dedent@35..35 ""
                error at 27: missing ‘[’ or ‘(’"#]],
        );
    }

    #[test]
    fn top_type() {
        // Happy path
        check(
            "type ThingList = List[Thing]",
            expect![[r#"
                DeclarationType@0..28
                  Type@0..4 "type"
                  Whitespace@4..5 " "
                  IdentifierType@5..14 "ThingList"
                  Whitespace@14..15 " "
                  Equal@15..16 "="
                  Whitespace@16..17 " "
                  TypeGeneric@17..28
                    TypeReference@17..21
                      IdentifierType@17..21 "List"
                    GenericArgList@21..28
                      BracketOpen@21..22 "["
                      GenericArgPositional@22..27
                        TypeReference@22..27
                          IdentifierType@22..27 "Thing"
                      BracketClose@27..28 "]""#]],
        );
    }

    #[test]
    fn top_const() {
        // Happy path
        check(
            "const x: Number = 42",
            expect![[r#"
                DeclarationConstant@0..20
                  Const@0..5 "const"
                  Whitespace@5..6 " "
                  IdentifierValue@6..7 "x"
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  TypeReference@9..15
                    IdentifierType@9..15 "Number"
                  Whitespace@15..16 " "
                  Equal@16..17 "="
                  Whitespace@17..18 " "
                  ExpressionLiteral@18..20
                    Number@18..20 "42""#]],
        );
    }

    #[test]
    fn top_const_no_type() {
        // Happy path: Constant declaration without an explicit type.
        check(
            "const y = 100",
            expect![[r#"
                DeclarationConstant@0..13
                  Const@0..5 "const"
                  Whitespace@5..6 " "
                  IdentifierValue@6..7 "y"
                  Whitespace@7..8 " "
                  Equal@8..9 "="
                  Whitespace@9..10 " "
                  ExpressionLiteral@10..13
                    Number@10..13 "100""#]],
        );
    }

    #[test]
    fn top_enum() {
        // Happy path
        check(
            indoc! {"
                enum Option[T]
                    case Some: T
                    case None
            "},
            expect![[r#"
                DeclarationEnum@0..46
                  Enum@0..4 "enum"
                  Whitespace@4..5 " "
                  IdentifierType@5..11 "Option"
                  GenericParamList@11..14
                    BracketOpen@11..12 "["
                    GenericParam@12..13
                      IdentifierType@12..13 "T"
                    BracketClose@13..14 "]"
                  Newline@14..15 "\n"
                  Indent@15..19 "    "
                  EnumCase@19..31
                    Case@19..23 "case"
                    Whitespace@23..24 " "
                    IdentifierType@24..28 "Some"
                    Colon@28..29 ":"
                    Whitespace@29..30 " "
                    TypeReference@30..31
                      IdentifierType@30..31 "T"
                  Newline@31..32 "\n"
                  Whitespace@32..36 "    "
                  EnumCase@36..45
                    Case@36..40 "case"
                    Whitespace@40..41 " "
                    IdentifierType@41..45 "None"
                  Newline@45..46 "\n"
                  Dedent@46..46 """#]],
        );
    }

    #[test]
    fn fn_named_from() {
        // `from` is a keyword, and a function name: `impl From` needs it.
        check(
            "fn from(value) => value",
            expect![[r#"
                DeclarationFunction@0..23
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  From@3..7 "from"
                  FunctionParamList@7..14
                    ParenOpen@7..8 "("
                    FunctionParam@8..13
                      FunctionParamLabel@8..13
                        IdentifierValue@8..13 "value"
                    ParenClose@13..14 ")"
                  Whitespace@14..15 " "
                  FatArrow@15..17 "=>"
                  Whitespace@17..18 " "
                  FunctionBody@18..23
                    ExpressionReference@18..23
                      IdentifierValue@18..23 "value""#]],
        );
    }

    #[test]
    fn top_fn_no_indent() {
        // Happy path
        check(
            "fn foo(x: Number) => x",
            expect![[r#"
                DeclarationFunction@0..22
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..6 "foo"
                  FunctionParamList@6..17
                    ParenOpen@6..7 "("
                    FunctionParam@7..16
                      FunctionParamLabel@7..8
                        IdentifierValue@7..8 "x"
                      Colon@8..9 ":"
                      Whitespace@9..10 " "
                      TypeReference@10..16
                        IdentifierType@10..16 "Number"
                    ParenClose@16..17 ")"
                  Whitespace@17..18 " "
                  FatArrow@18..20 "=>"
                  Whitespace@20..21 " "
                  FunctionBody@21..22
                    ExpressionReference@21..22
                      IdentifierValue@21..22 "x""#]],
        );
    }

    #[test]
    fn function_with_a_return_type() {
        check(
            "fn foo(x: Number): Number => x",
            expect![[r#"
                DeclarationFunction@0..30
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..6 "foo"
                  FunctionParamList@6..17
                    ParenOpen@6..7 "("
                    FunctionParam@7..16
                      FunctionParamLabel@7..8
                        IdentifierValue@7..8 "x"
                      Colon@8..9 ":"
                      Whitespace@9..10 " "
                      TypeReference@10..16
                        IdentifierType@10..16 "Number"
                    ParenClose@16..17 ")"
                  Colon@17..18 ":"
                  Whitespace@18..19 " "
                  FunctionReturnType@19..25
                    TypeReference@19..25
                      IdentifierType@19..25 "Number"
                  Whitespace@25..26 " "
                  FatArrow@26..28 "=>"
                  Whitespace@28..29 " "
                  FunctionBody@29..30
                    ExpressionReference@29..30
                      IdentifierValue@29..30 "x""#]],
        );
    }

    #[test]
    fn trait_function_ends_at_its_return_type() {
        check(
            indoc! {"
                trait Default
                    fn default(): Self
            "},
            expect![[r#"
                DeclarationTrait@0..37
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  IdentifierType@6..13 "Default"
                  Newline@13..14 "\n"
                  Indent@14..18 "    "
                  DeclarationFunction@18..36
                    Fn@18..20 "fn"
                    Whitespace@20..21 " "
                    IdentifierValue@21..28 "default"
                    FunctionParamList@28..30
                      ParenOpen@28..29 "("
                      ParenClose@29..30 ")"
                    Colon@30..31 ":"
                    Whitespace@31..32 " "
                    FunctionReturnType@32..36
                      TypeReference@32..36
                        SelfUpper@32..36 "Self"
                  Newline@36..37 "\n"
                  Dedent@37..37 """#]],
        );
    }

    #[test]
    fn trait_function_ends_at_its_where_clause() {
        check(
            indoc! {"
                trait Add
                    fn add(self, other: Self) where
                        Self: Add
            "},
            expect![[r#"
                DeclarationTrait@0..64
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  IdentifierType@6..9 "Add"
                  Newline@9..10 "\n"
                  Indent@10..14 "    "
                  DeclarationFunction@14..64
                    Fn@14..16 "fn"
                    Whitespace@16..17 " "
                    IdentifierValue@17..20 "add"
                    FunctionParamList@20..39
                      ParenOpen@20..21 "("
                      FunctionParam@21..25
                        FunctionParamLabel@21..25
                          SelfLower@21..25 "self"
                      Comma@25..26 ","
                      Whitespace@26..27 " "
                      FunctionParam@27..38
                        FunctionParamLabel@27..32
                          IdentifierValue@27..32 "other"
                        Colon@32..33 ":"
                        Whitespace@33..34 " "
                        TypeReference@34..38
                          SelfUpper@34..38 "Self"
                      ParenClose@38..39 ")"
                    Whitespace@39..40 " "
                    GenericWhereClause@40..64
                      Where@40..45 "where"
                      Newline@45..46 "\n"
                      Whitespace@46..50 "    "
                      Indent@50..54 "    "
                      GenericWhereBound@54..63
                        TypeReference@54..58
                          SelfUpper@54..58 "Self"
                        Colon@58..59 ":"
                        Whitespace@59..60 " "
                        GenericBoundList@60..63
                          GenericBound@60..63
                            IdentifierType@60..63 "Add"
                      Newline@63..64 "\n"
                      Dedent@64..64 ""
                  Dedent@64..64 """#]],
        );
    }

    #[test]
    fn function_with_a_return_type_left_out_recovers() {
        check(
            "fn foo(): => 1",
            expect![[r#"
                DeclarationFunction@0..14
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..6 "foo"
                  FunctionParamList@6..8
                    ParenOpen@6..7 "("
                    ParenClose@7..8 ")"
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  FunctionReturnType@10..10
                    Missing@10..10
                  FatArrow@10..12 "=>"
                  Whitespace@12..13 " "
                  FunctionBody@13..14
                    ExpressionLiteral@13..14
                      Number@13..14 "1"
                error at 10: missing type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’"#]],
        );
    }

    #[test]
    fn function_with_a_return_type_and_no_fat_arrow_recovers() {
        check(
            indoc! {"
                fn foo(): Number
                    1
            "},
            expect![[r#"
                DeclarationFunction@0..23
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..6 "foo"
                  FunctionParamList@6..8
                    ParenOpen@6..7 "("
                    ParenClose@7..8 ")"
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  FunctionReturnType@10..16
                    TypeReference@10..16
                      IdentifierType@10..16 "Number"
                  Newline@16..17 "\n"
                  Error@17..21
                    Indent@17..21 "    "
                  FunctionBody@21..22
                    ExpressionLiteral@21..22
                      Number@21..22 "1"
                  Newline@22..23 "\n"
                error at 17..21: expected ‘=>’, but found indent"#]],
        );
    }

    #[test]
    fn top_fn_with_indent() {
        // Happy path
        check(
            indoc! {"
                fn foo(x: Number) =>
                    x
            "},
            expect![[r#"
                DeclarationFunction@0..27
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..6 "foo"
                  FunctionParamList@6..17
                    ParenOpen@6..7 "("
                    FunctionParam@7..16
                      FunctionParamLabel@7..8
                        IdentifierValue@7..8 "x"
                      Colon@8..9 ":"
                      Whitespace@9..10 " "
                      TypeReference@10..16
                        IdentifierType@10..16 "Number"
                    ParenClose@16..17 ")"
                  Whitespace@17..18 " "
                  FatArrow@18..20 "=>"
                  Newline@20..21 "\n"
                  FunctionBody@21..27
                    ExpressionBlock@21..27
                      Indent@21..25 "    "
                      ExpressionReference@25..26
                        IdentifierValue@25..26 "x"
                      Newline@26..27 "\n"
                      Dedent@27..27 """#]],
        );
    }

    #[test]
    fn top_struct() {
        // Happy path
        check(
            indoc! {"
                struct Person
                    prop name: String
                    prop age: Number
                    prop is_alive: Boolean = True
            "},
            expect![[r#"
                DeclarationStruct@0..91
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  IdentifierType@7..13 "Person"
                  Newline@13..14 "\n"
                  Indent@14..18 "    "
                  DeclarationProp@18..35
                    Prop@18..22 "prop"
                    Whitespace@22..23 " "
                    IdentifierValue@23..27 "name"
                    Colon@27..28 ":"
                    Whitespace@28..29 " "
                    TypeReference@29..35
                      IdentifierType@29..35 "String"
                  Newline@35..36 "\n"
                  Whitespace@36..40 "    "
                  DeclarationProp@40..56
                    Prop@40..44 "prop"
                    Whitespace@44..45 " "
                    IdentifierValue@45..48 "age"
                    Colon@48..49 ":"
                    Whitespace@49..50 " "
                    TypeReference@50..56
                      IdentifierType@50..56 "Number"
                  Newline@56..57 "\n"
                  Whitespace@57..61 "    "
                  DeclarationProp@61..90
                    Prop@61..65 "prop"
                    Whitespace@65..66 " "
                    IdentifierValue@66..74 "is_alive"
                    Colon@74..75 ":"
                    Whitespace@75..76 " "
                    TypeReference@76..83
                      IdentifierType@76..83 "Boolean"
                    Whitespace@83..84 " "
                    Equal@84..85 "="
                    Whitespace@85..86 " "
                    TypeReference@86..90
                      IdentifierType@86..90 "True"
                  Newline@90..91 "\n"
                  Dedent@91..91 """#]],
        );
    }

    #[test]
    fn top_trait() {
        // Happy path
        check(
            indoc! {"
                trait Add
                    fn add(self, other: Self)
            "},
            expect![[r#"
                DeclarationTrait@0..40
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  IdentifierType@6..9 "Add"
                  Newline@9..10 "\n"
                  Indent@10..14 "    "
                  DeclarationFunction@14..39
                    Fn@14..16 "fn"
                    Whitespace@16..17 " "
                    IdentifierValue@17..20 "add"
                    FunctionParamList@20..39
                      ParenOpen@20..21 "("
                      FunctionParam@21..25
                        FunctionParamLabel@21..25
                          SelfLower@21..25 "self"
                      Comma@25..26 ","
                      Whitespace@26..27 " "
                      FunctionParam@27..38
                        FunctionParamLabel@27..32
                          IdentifierValue@27..32 "other"
                        Colon@32..33 ":"
                        Whitespace@33..34 " "
                        TypeReference@34..38
                          SelfUpper@34..38 "Self"
                      ParenClose@38..39 ")"
                  Newline@39..40 "\n"
                  Dedent@40..40 """#]],
        );
    }

    #[test]
    fn top_impl() {
        // Happy path
        check(
            indoc! {"
                impl MyTrait for MyStruct
                    fn my_method() =>
                        1
            "},
            expect![[r#"
                DeclarationImplTrait@0..58
                  Impl@0..4 "impl"
                  Whitespace@4..5 " "
                  TypeReference@5..12
                    IdentifierType@5..12 "MyTrait"
                  Whitespace@12..13 " "
                  For@13..16 "for"
                  Whitespace@16..17 " "
                  TypeReference@17..25
                    IdentifierType@17..25 "MyStruct"
                  Newline@25..26 "\n"
                  Indent@26..30 "    "
                  DeclarationFunction@30..58
                    Fn@30..32 "fn"
                    Whitespace@32..33 " "
                    IdentifierValue@33..42 "my_method"
                    FunctionParamList@42..44
                      ParenOpen@42..43 "("
                      ParenClose@43..44 ")"
                    Whitespace@44..45 " "
                    FatArrow@45..47 "=>"
                    Newline@47..48 "\n"
                    Whitespace@48..52 "    "
                    FunctionBody@52..58
                      ExpressionBlock@52..58
                        Indent@52..56 "    "
                        ExpressionLiteral@56..57
                          Number@56..57 "1"
                        Newline@57..58 "\n"
                        Dedent@58..58 ""
                  Dedent@58..58 """#]],
        );
    }

    /*
    #[test]
    fn top_item_unhappy() {
        // This input does not start with any of the expected tokens so it should trigger an error.
        check(
            "garbage",
            expect![[r#"
                ERROR@0..7
                  <error: unexpected token>
            "#]],
        );
    }
    */

    #[test]
    fn type_decl_missing_identifier() {
        // Unhappy path: Missing identifier after the 'type' keyword.
        check(
            "type = List[Thing]",
            expect![[r#"
                DeclarationType@0..18
                  Type@0..4 "type"
                  Whitespace@4..5 " "
                  Missing@5..5
                  Equal@5..6 "="
                  Whitespace@6..7 " "
                  TypeGeneric@7..18
                    TypeReference@7..11
                      IdentifierType@7..11 "List"
                    GenericArgList@11..18
                      BracketOpen@11..12 "["
                      GenericArgPositional@12..17
                        TypeReference@12..17
                          IdentifierType@12..17 "Thing"
                      BracketClose@17..18 "]"
                error at 5: missing type-id"#]],
        );
    }

    #[test]
    fn trait_type_decl() {
        // Happy path: Trait with an associated type declaration (using '=' in impls).
        check(
            indoc! {"
                trait MyTrait
                    type Assoc = Concrete
            "},
            expect![[r#"
                DeclarationTrait@0..40
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  IdentifierType@6..13 "MyTrait"
                  Newline@13..14 "\n"
                  Indent@14..18 "    "
                  DeclarationType@18..39
                    Type@18..22 "type"
                    Whitespace@22..23 " "
                    IdentifierType@23..28 "Assoc"
                    Whitespace@28..29 " "
                    Equal@29..30 "="
                    Whitespace@30..31 " "
                    TypeReference@31..39
                      IdentifierType@31..39 "Concrete"
                  Newline@39..40 "\n"
                  Dedent@40..40 """#]],
        );
    }

    #[test]
    fn trait_const_decl() {
        // Happy path: Trait with a constant declaration that only specifies a type.
        check(
            indoc! {"
                trait MyTrait
                    const pi: Number
            "},
            expect![[r#"
                DeclarationTrait@0..35
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  IdentifierType@6..13 "MyTrait"
                  Newline@13..14 "\n"
                  Indent@14..18 "    "
                  DeclarationConstant@18..34
                    Const@18..23 "const"
                    Whitespace@23..24 " "
                    IdentifierValue@24..26 "pi"
                    Colon@26..27 ":"
                    Whitespace@27..28 " "
                    TypeReference@28..34
                      IdentifierType@28..34 "Number"
                  Newline@34..35 "\n"
                  Dedent@35..35 """#]],
        );
    }

    #[test]
    fn impl_all_items() {
        // Happy path: Impl block containing every possible impl item: type, constant, function, and property.
        check(
            indoc! {"
                impl MyTrait for MyStruct
                    type Assoc = Concrete
                    const version = 1
                    fn do_something() => 0
                    prop field = default
            "},
            expect![[r#"
                DeclarationImplTrait@0..126
                  Impl@0..4 "impl"
                  Whitespace@4..5 " "
                  TypeReference@5..12
                    IdentifierType@5..12 "MyTrait"
                  Whitespace@12..13 " "
                  For@13..16 "for"
                  Whitespace@16..17 " "
                  TypeReference@17..25
                    IdentifierType@17..25 "MyStruct"
                  Newline@25..26 "\n"
                  Indent@26..30 "    "
                  DeclarationType@30..51
                    Type@30..34 "type"
                    Whitespace@34..35 " "
                    IdentifierType@35..40 "Assoc"
                    Whitespace@40..41 " "
                    Equal@41..42 "="
                    Whitespace@42..43 " "
                    TypeReference@43..51
                      IdentifierType@43..51 "Concrete"
                  Newline@51..52 "\n"
                  Whitespace@52..56 "    "
                  DeclarationConstant@56..73
                    Const@56..61 "const"
                    Whitespace@61..62 " "
                    IdentifierValue@62..69 "version"
                    Whitespace@69..70 " "
                    Equal@70..71 "="
                    Whitespace@71..72 " "
                    ExpressionLiteral@72..73
                      Number@72..73 "1"
                  Newline@73..74 "\n"
                  Whitespace@74..78 "    "
                  DeclarationFunction@78..100
                    Fn@78..80 "fn"
                    Whitespace@80..81 " "
                    IdentifierValue@81..93 "do_something"
                    FunctionParamList@93..95
                      ParenOpen@93..94 "("
                      ParenClose@94..95 ")"
                    Whitespace@95..96 " "
                    FatArrow@96..98 "=>"
                    Whitespace@98..99 " "
                    FunctionBody@99..100
                      ExpressionLiteral@99..100
                        Number@99..100 "0"
                  Newline@100..101 "\n"
                  Whitespace@101..105 "    "
                  DeclarationProp@105..125
                    Prop@105..109 "prop"
                    Whitespace@109..110 " "
                    IdentifierValue@110..115 "field"
                    Whitespace@115..116 " "
                    Equal@116..117 "="
                    Whitespace@117..118 " "
                    ExpressionReference@118..125
                      IdentifierValue@118..125 "default"
                  Newline@125..126 "\n"
                  Dedent@126..126 """#]],
        );
    }

    #[test]
    fn trait_unknown_item() {
        // Unhappy path: A trait that contains an unrecognized token, which should trigger an error.
        // TODO handle error better
        check(
            indoc! {"
                trait Broken
                    unknown_token
            "},
            expect![[r#"
                DeclarationTrait@0..31
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  IdentifierType@6..12 "Broken"
                  Newline@12..13 "\n"
                  Indent@13..17 "    "
                  Error@17..30
                    IdentifierValue@17..30 "unknown_token"
                  Newline@30..31 "\n"
                error at 17..30: expected dedent, but found value-id"#]],
        );
    }

    #[test]
    fn struct_incomplete_fn() {
        // Unhappy path: A struct with a function declaration missing a complete parameter list or body.
        check(
            indoc! {"
                struct Incomplete
                    fn missing_brace(
            "},
            expect![[r#"
                DeclarationStruct@0..40
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  IdentifierType@7..17 "Incomplete"
                  Newline@17..18 "\n"
                  Indent@18..22 "    "
                  DeclarationFunction@22..40
                    Fn@22..24 "fn"
                    Whitespace@24..25 " "
                    IdentifierValue@25..38 "missing_brace"
                    FunctionParamList@38..40
                      ParenOpen@38..39 "("
                      Newline@39..40 "\n"
                      FunctionParam@40..40
                        Missing@40..40
                      Missing@40..40
                    Missing@40..40
                    FunctionBody@40..40
                      Missing@40..40
                  Dedent@40..40 ""
                error at 40: missing ‘)’, value-id, or ‘self’
                error at 40: missing ‘)’
                error at 40: missing ‘=>’
                error at 40: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn struct_multiple_items() {
        // Happy path: A struct with multiple items: constant, function, and property.
        check(
            indoc! {"
                struct Complex
                    const a = 10
                    fn do_it() => A
                    prop b: Number = 20
            "},
            expect![[r#"
                DeclarationStruct@0..76
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  IdentifierType@7..14 "Complex"
                  Newline@14..15 "\n"
                  Indent@15..19 "    "
                  DeclarationConstant@19..31
                    Const@19..24 "const"
                    Whitespace@24..25 " "
                    IdentifierValue@25..26 "a"
                    Whitespace@26..27 " "
                    Equal@27..28 "="
                    Whitespace@28..29 " "
                    ExpressionLiteral@29..31
                      Number@29..31 "10"
                  Newline@31..32 "\n"
                  Whitespace@32..36 "    "
                  DeclarationFunction@36..51
                    Fn@36..38 "fn"
                    Whitespace@38..39 " "
                    IdentifierValue@39..44 "do_it"
                    FunctionParamList@44..46
                      ParenOpen@44..45 "("
                      ParenClose@45..46 ")"
                    Whitespace@46..47 " "
                    FatArrow@47..49 "=>"
                    Whitespace@49..50 " "
                    FunctionBody@50..51
                      TypeReference@50..51
                        IdentifierType@50..51 "A"
                  Newline@51..52 "\n"
                  Whitespace@52..56 "    "
                  DeclarationProp@56..75
                    Prop@56..60 "prop"
                    Whitespace@60..61 " "
                    IdentifierValue@61..62 "b"
                    Colon@62..63 ":"
                    Whitespace@63..64 " "
                    TypeReference@64..70
                      IdentifierType@64..70 "Number"
                    Whitespace@70..71 " "
                    Equal@71..72 "="
                    Whitespace@72..73 " "
                    ExpressionLiteral@73..75
                      Number@73..75 "20"
                  Newline@75..76 "\n"
                  Dedent@76..76 """#]],
        );
    }

    #[test]
    fn function_with_generic() {
        // Happy path: A function declaration that includes a generic parameter list.
        check(
            "fn foo[T](x: T) => x",
            expect![[r#"
                DeclarationFunction@0..20
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  IdentifierValue@3..6 "foo"
                  GenericParamList@6..9
                    BracketOpen@6..7 "["
                    GenericParam@7..8
                      IdentifierType@7..8 "T"
                    BracketClose@8..9 "]"
                  FunctionParamList@9..15
                    ParenOpen@9..10 "("
                    FunctionParam@10..14
                      FunctionParamLabel@10..11
                        IdentifierValue@10..11 "x"
                      Colon@11..12 ":"
                      Whitespace@12..13 " "
                      TypeReference@13..14
                        IdentifierType@13..14 "T"
                    ParenClose@14..15 ")"
                  Whitespace@15..16 " "
                  FatArrow@16..18 "=>"
                  Whitespace@18..19 " "
                  FunctionBody@19..20
                    ExpressionReference@19..20
                      IdentifierValue@19..20 "x""#]],
        );
    }

    #[test]
    fn attribute_on_a_function() {
        check(
            indoc! {"
                @label(\"Seat\")
                fn f() => 1
            "},
            expect![[r#"
                DeclarationFunction@0..27
                  Attribute@0..14
                    At@0..1 "@"
                    IdentifierValue@1..6 "label"
                    FunctionArgList@6..14
                      ParenOpen@6..7 "("
                      FunctionArgPositional@7..13
                        ExpressionLiteral@7..13
                          String@7..13 "\"Seat\""
                      ParenClose@13..14 ")"
                  Newline@14..15 "\n"
                  Fn@15..17 "fn"
                  Whitespace@17..18 " "
                  IdentifierValue@18..19 "f"
                  FunctionParamList@19..21
                    ParenOpen@19..20 "("
                    ParenClose@20..21 ")"
                  Whitespace@21..22 " "
                  FatArrow@22..24 "=>"
                  Whitespace@24..25 " "
                  FunctionBody@25..26
                    ExpressionLiteral@25..26
                      Number@25..26 "1"
                  Newline@26..27 "\n""#]],
        );
    }

    #[test]
    fn several_attributes_on_a_constant() {
        // A bare `@name` has no argument list.
        check(
            indoc! {"
                @hidden
                @unit(\"mm\")
                const x = 1
            "},
            expect![[r#"
                DeclarationConstant@0..32
                  Attribute@0..7
                    At@0..1 "@"
                    IdentifierValue@1..7 "hidden"
                  Newline@7..8 "\n"
                  Attribute@8..19
                    At@8..9 "@"
                    IdentifierValue@9..13 "unit"
                    FunctionArgList@13..19
                      ParenOpen@13..14 "("
                      FunctionArgPositional@14..18
                        ExpressionLiteral@14..18
                          String@14..18 "\"mm\""
                      ParenClose@18..19 ")"
                  Newline@19..20 "\n"
                  Const@20..25 "const"
                  Whitespace@25..26 " "
                  IdentifierValue@26..27 "x"
                  Whitespace@27..28 " "
                  Equal@28..29 "="
                  Whitespace@29..30 " "
                  ExpressionLiteral@30..31
                    Number@30..31 "1"
                  Newline@31..32 "\n""#]],
        );
    }

    #[test]
    fn attribute_with_a_block_of_arguments() {
        // The block form of the argument list: an indented block after a
        // bare `@name` is its keyword arguments.
        check(
            indoc! {"
                @range
                  min = 5
                  max = 10
                fn f() => 1
            "},
            expect![[r#"
                DeclarationFunction@0..40
                  Attribute@0..28
                    At@0..1 "@"
                    IdentifierValue@1..6 "range"
                    Newline@6..7 "\n"
                    FunctionArgList@7..28
                      Indent@7..9 "  "
                      FunctionArgLabelled@9..16
                        FunctionParamLabel@9..12
                          IdentifierValue@9..12 "min"
                        Whitespace@12..13 " "
                        Equal@13..14 "="
                        Whitespace@14..15 " "
                        ExpressionLiteral@15..16
                          Number@15..16 "5"
                      Newline@16..17 "\n"
                      Whitespace@17..19 "  "
                      FunctionArgLabelled@19..27
                        FunctionParamLabel@19..22
                          IdentifierValue@19..22 "max"
                        Whitespace@22..23 " "
                        Equal@23..24 "="
                        Whitespace@24..25 " "
                        ExpressionLiteral@25..27
                          Number@25..27 "10"
                      Newline@27..28 "\n"
                      Dedent@28..28 ""
                  Fn@28..30 "fn"
                  Whitespace@30..31 " "
                  IdentifierValue@31..32 "f"
                  FunctionParamList@32..34
                    ParenOpen@32..33 "("
                    ParenClose@33..34 ")"
                  Whitespace@34..35 " "
                  FatArrow@35..37 "=>"
                  Whitespace@37..38 " "
                  FunctionBody@38..39
                    ExpressionLiteral@38..39
                      Number@38..39 "1"
                  Newline@39..40 "\n""#]],
        );
    }

    #[test]
    fn attribute_with_a_lambda_argument() {
        check(
            indoc! {"
                @requires(fn (self) => self.should_include_back)
                fn f() => 1
            "},
            expect![[r#"
                DeclarationFunction@0..61
                  Attribute@0..48
                    At@0..1 "@"
                    IdentifierValue@1..9 "requires"
                    FunctionArgList@9..48
                      ParenOpen@9..10 "("
                      FunctionArgPositional@10..47
                        DeclarationFunction@10..47
                          Fn@10..12 "fn"
                          Whitespace@12..13 " "
                          FunctionParamList@13..19
                            ParenOpen@13..14 "("
                            FunctionParam@14..18
                              FunctionParamLabel@14..18
                                SelfLower@14..18 "self"
                            ParenClose@18..19 ")"
                          Whitespace@19..20 " "
                          FatArrow@20..22 "=>"
                          Whitespace@22..23 " "
                          FunctionBody@23..47
                            ExpressionGet@23..47
                              ExpressionReference@23..27
                                SelfLower@23..27 "self"
                              Dot@27..28 "."
                              IdentifierValue@28..47 "should_include_back"
                      ParenClose@47..48 ")"
                  Newline@48..49 "\n"
                  Fn@49..51 "fn"
                  Whitespace@51..52 " "
                  IdentifierValue@52..53 "f"
                  FunctionParamList@53..55
                    ParenOpen@53..54 "("
                    ParenClose@54..55 ")"
                  Whitespace@55..56 " "
                  FatArrow@56..58 "=>"
                  Whitespace@58..59 " "
                  FunctionBody@59..60
                    ExpressionLiteral@59..60
                      Number@59..60 "1"
                  Newline@60..61 "\n""#]],
        );
    }

    #[test]
    fn attributes_on_a_prop() {
        check(
            indoc! {"
                struct S
                  @label(\"Seat width\")
                  @range(min = 5, max = 10)
                  prop seat_width: Number
            "},
            expect![[r#"
                DeclarationStruct@0..86
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  IdentifierType@7..8 "S"
                  Newline@8..9 "\n"
                  Indent@9..11 "  "
                  DeclarationProp@11..85
                    Attribute@11..31
                      At@11..12 "@"
                      IdentifierValue@12..17 "label"
                      FunctionArgList@17..31
                        ParenOpen@17..18 "("
                        FunctionArgPositional@18..30
                          ExpressionLiteral@18..30
                            String@18..30 "\"Seat width\""
                        ParenClose@30..31 ")"
                    Newline@31..32 "\n"
                    Whitespace@32..34 "  "
                    Attribute@34..59
                      At@34..35 "@"
                      IdentifierValue@35..40 "range"
                      FunctionArgList@40..59
                        ParenOpen@40..41 "("
                        FunctionArgLabelled@41..48
                          FunctionParamLabel@41..44
                            IdentifierValue@41..44 "min"
                          Whitespace@44..45 " "
                          Equal@45..46 "="
                          Whitespace@46..47 " "
                          ExpressionLiteral@47..48
                            Number@47..48 "5"
                        Comma@48..49 ","
                        Whitespace@49..50 " "
                        FunctionArgLabelled@50..58
                          FunctionParamLabel@50..53
                            IdentifierValue@50..53 "max"
                          Whitespace@53..54 " "
                          Equal@54..55 "="
                          Whitespace@55..56 " "
                          ExpressionLiteral@56..58
                            Number@56..58 "10"
                        ParenClose@58..59 ")"
                    Newline@59..60 "\n"
                    Whitespace@60..62 "  "
                    Prop@62..66 "prop"
                    Whitespace@66..67 " "
                    IdentifierValue@67..77 "seat_width"
                    Colon@77..78 ":"
                    Whitespace@78..79 " "
                    TypeReference@79..85
                      IdentifierType@79..85 "Number"
                  Newline@85..86 "\n"
                  Dedent@86..86 """#]],
        );
    }

    #[test]
    fn attribute_with_nothing_after_it_in_a_struct() {
        // Unhappy path: the attribute sits beside a `Missing` declaration.
        check(
            indoc! {"
                struct S
                  @label(\"x\")
            "},
            expect![[r#"
                DeclarationStruct@0..23
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  IdentifierType@7..8 "S"
                  Newline@8..9 "\n"
                  Indent@9..11 "  "
                  Attribute@11..22
                    At@11..12 "@"
                    IdentifierValue@12..17 "label"
                    FunctionArgList@17..22
                      ParenOpen@17..18 "("
                      FunctionArgPositional@18..21
                        ExpressionLiteral@18..21
                          String@18..21 "\"x\""
                      ParenClose@21..22 ")"
                  Newline@22..23 "\n"
                  Missing@23..23
                  Dedent@23..23 ""
                error at 23: missing ‘@’, ‘const’, ‘fn’, or ‘prop’"#]],
        );
    }

    #[test]
    fn attribute_without_a_name() {
        // Unhappy path: the name is missing and the arguments still parse.
        check(
            indoc! {"
                @(\"x\")
                fn f() => 1
            "},
            expect![[r#"
                DeclarationFunction@0..19
                  Attribute@0..6
                    At@0..1 "@"
                    Missing@1..1
                    FunctionArgList@1..6
                      ParenOpen@1..2 "("
                      FunctionArgPositional@2..5
                        ExpressionLiteral@2..5
                          String@2..5 "\"x\""
                      ParenClose@5..6 ")"
                  Newline@6..7 "\n"
                  Fn@7..9 "fn"
                  Whitespace@9..10 " "
                  IdentifierValue@10..11 "f"
                  FunctionParamList@11..13
                    ParenOpen@11..12 "("
                    ParenClose@12..13 ")"
                  Whitespace@13..14 " "
                  FatArrow@14..16 "=>"
                  Whitespace@16..17 " "
                  FunctionBody@17..18
                    ExpressionLiteral@17..18
                      Number@17..18 "1"
                  Newline@18..19 "\n"
                error at 1: missing value-id"#]],
        );
    }
}
