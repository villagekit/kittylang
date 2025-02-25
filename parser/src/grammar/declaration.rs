use kitty_syntax::{NodeKind, TokenKind};

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

use super::{
    expression::expression,
    function::function_declaration_option_name_body,
    r#type::{
        generic_bound_list, generic_param_list, generic_where_clause, type_annotation, type_path,
    },
};

pub(crate) const DECLARATION_FIRST: [TokenKind; 7] = [
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Enum,
    TokenKind::Struct,
    TokenKind::Trait,
    TokenKind::Impl,
];

pub(crate) fn declaration(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Type) {
        declaration_type(p, recovery)
    } else if p.at(TokenKind::Const) {
        declaration_constant(p, recovery)
    } else if p.at(TokenKind::Fn) {
        declaration_function(p, recovery)
    } else if p.at(TokenKind::Enum) {
        declaration_enum(p, recovery)
    } else if p.at(TokenKind::Struct) {
        declaration_struct(p, recovery)
    } else if p.at(TokenKind::Trait) {
        declaration_trait(p, recovery)
    } else if p.at(TokenKind::Impl) {
        declaration_impl_trait(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

/// Type alias declaration
fn declaration_type(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Type));
    let recovery_type = recovery.union([TokenKind::Equal, TokenKind::Colon]);
    let m = p.start();
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
fn declaration_constant(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    declaration_constant_optional_type_value(p, recovery, false, true)
}

fn declaration_constant_optional_type_value(
    p: &mut Parser,
    recovery: TokenSet,
    has_type: bool,
    has_value: bool,
) -> CompletedMarker {
    assert!(p.at(TokenKind::Const));
    let m = p.start();
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

fn declaration_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    function_declaration_option_name_body(p, recovery, true, true)
}

fn declaration_struct(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Struct));
    let recovery_struct = recovery.union(STRUCT_ITEM_FIRST).union([TokenKind::Dedent]);
    let m = p.start();
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

const STRUCT_ITEM_FIRST: [TokenKind; 3] = [TokenKind::Const, TokenKind::Fn, TokenKind::Prop];

fn struct_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Const) {
        declaration_constant(p, recovery)
    } else if p.at(TokenKind::Fn) {
        declaration_function(p, recovery)
    } else if p.at(TokenKind::Prop) {
        declaration_prop(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn declaration_prop(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    declaration_prop_optional_type_value(p, recovery, true, false)
}

fn declaration_prop_optional_type_value(
    p: &mut Parser,
    recovery: TokenSet,
    has_type: bool,
    has_value: bool,
) -> CompletedMarker {
    assert!(p.at(TokenKind::Prop));
    let m = p.start();
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

fn declaration_enum(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Enum));
    let recovery_enum = recovery.union(ENUM_ITEM_FIRST).union([TokenKind::Dedent]);
    let m = p.start();
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

const ENUM_ITEM_FIRST: [TokenKind; 3] = [TokenKind::Const, TokenKind::Fn, TokenKind::Case];

fn enum_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Const) {
        declaration_constant(p, recovery)
    } else if p.at(TokenKind::Fn) {
        declaration_function(p, recovery)
    } else if p.at(TokenKind::Case) {
        enum_case(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn enum_case(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Case));
    let m = p.start();
    p.bump(); // Consume 'case'
    p.expect(TokenKind::IdentifierType, recovery);
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        type_annotation(p, recovery);
    }
    m.complete(p, NodeKind::EnumCase)
}

fn declaration_trait(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Trait));
    let recovery_trait = recovery.union(TRAIT_ITEM_FIRST).union([TokenKind::Dedent]);
    let m = p.start();
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

const TRAIT_ITEM_FIRST: [TokenKind; 4] = [
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Prop,
];

fn trait_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Type) {
        trait_type(p, recovery)
    } else if p.at(TokenKind::Const) {
        trait_constant(p, recovery)
    } else if p.at(TokenKind::Fn) {
        trait_function(p, recovery)
    } else if p.at(TokenKind::Prop) {
        declaration_prop(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn trait_type(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Type));
    let m = p.start();
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

fn trait_constant(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    declaration_constant_optional_type_value(p, recovery, false, false)
}

fn trait_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    function_declaration_option_name_body(p, recovery, true, false)
}

fn declaration_impl_trait(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Impl));
    let recovery_trait = recovery
        .union(IMPL_TRAIT_ITEM_FIRST)
        .union([TokenKind::Dedent]);
    let m = p.start();
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

const IMPL_TRAIT_ITEM_FIRST: [TokenKind; 4] = [
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Prop,
];

fn impl_trait_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Type) {
        declaration_type(p, recovery)
    } else if p.at(TokenKind::Const) {
        trait_impl_constant(p, recovery)
    } else if p.at(TokenKind::Fn) {
        trait_impl_function(p, recovery)
    } else if p.at(TokenKind::Prop) {
        trait_impl_prop(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn trait_impl_constant(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    declaration_constant_optional_type_value(p, recovery, false, true)
}

fn trait_impl_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    function_declaration_option_name_body(p, recovery, true, true)
}

fn trait_impl_prop(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    declaration_prop_optional_type_value(p, recovery, false, true)
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
                    ExpressionLiteral@86..90
                      Boolean@86..90 "True"
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
                error at 40: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
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
}
