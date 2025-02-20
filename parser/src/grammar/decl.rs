use kitty_syntax::{NodeKind, TokenKind};

use crate::{
    grammar::r#type::where_clause, marker::CompletedMarker, parser::Parser, token_set::TokenSet,
};

use super::{
    expr::expr,
    function::function_decl_optional_name_types_body,
    r#type::{generic_param_list, type_annotation, type_bound_list, type_path},
};

pub(crate) const TOP_ITEM_FIRST: [TokenKind; 7] = [
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Enum,
    TokenKind::Struct,
    TokenKind::Trait,
    TokenKind::Impl,
];

pub(crate) fn decl_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
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
    let recovery_type = recovery.union([TokenKind::Equal]);
    let m = p.start();
    p.bump(); // Consume 'type'
    p.expect(TokenKind::Identifier, recovery_type);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery_type);
    }
    p.expect(TokenKind::Equal, recovery);
    type_annotation(p, recovery);
    m.complete(p, NodeKind::TypeDecl)
}

/// Constant declaration
fn constant_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    constant_decl_optional_type_value(p, recovery, false, true)
}

fn function_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    function_decl_optional_name_types_body(p, recovery, true, true, true)
}

fn constant_decl_optional_type_value(
    p: &mut Parser,
    recovery: TokenSet,
    has_type: bool,
    has_value: bool,
) -> CompletedMarker {
    assert!(p.at(TokenKind::Const));
    let m = p.start();
    p.bump(); // Consume 'const'
    p.expect(TokenKind::Identifier, recovery);
    if has_type || p.at(TokenKind::Colon) {
        p.expect(TokenKind::Colon, recovery);
        type_annotation(p, recovery);
    }
    if has_value || p.at(TokenKind::Equal) {
        p.expect(TokenKind::Equal, recovery);
        expr(p, recovery);
    }
    m.complete(p, NodeKind::ConstantDecl)
}

fn struct_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Struct));
    let recovery_struct = recovery.union(STRUCT_ITEM_FIRST).union([TokenKind::Dedent]);
    let m = p.start();
    p.bump(); // Consume 'struct'
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    p.expect(TokenKind::Indent, recovery);
    if p.at(TokenKind::Where) {
        where_clause(p, recovery);
    }
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
    prop_decl_optional_type_value(p, recovery, true, false)
}

fn prop_decl_optional_type_value(
    p: &mut Parser,
    recovery: TokenSet,
    has_type: bool,
    has_value: bool,
) -> CompletedMarker {
    assert!(p.at(TokenKind::Prop));
    let m = p.start();
    p.bump(); // Consume 'prop'
    p.expect(TokenKind::Identifier, recovery);
    if has_type || p.at(TokenKind::Colon) {
        p.expect(TokenKind::Colon, recovery);
        type_annotation(p, recovery);
    }
    if has_value || p.at(TokenKind::Equal) {
        p.expect(TokenKind::Equal, recovery);
        expr(p, recovery);
    }
    m.complete(p, NodeKind::PropDecl)
}

fn enum_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Enum));
    let recovery_enum = recovery.union(ENUM_ITEM_FIRST).union([TokenKind::Dedent]);
    let m = p.start();
    p.bump(); // Consume 'struct'
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    p.expect(TokenKind::Indent, recovery);
    if p.at(TokenKind::Where) {
        where_clause(p, recovery);
    }
    while p.at_set(ENUM_ITEM_FIRST) {
        enum_item(p, recovery_enum);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::EnumDecl)
}

const ENUM_ITEM_FIRST: [TokenKind; 3] = [TokenKind::Const, TokenKind::Fn, TokenKind::Case];

fn enum_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Const) {
        constant_decl(p, recovery)
    } else if p.at(TokenKind::Fn) {
        function_decl(p, recovery)
    } else if p.at(TokenKind::Case) {
        enum_case_decl(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

fn enum_case_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Case));
    let m = p.start();
    p.bump(); // Consume 'case'
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        type_annotation(p, recovery);
    }
    m.complete(p, NodeKind::EnumCase)
}

fn trait_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Trait));
    let recovery_trait = recovery.union(TRAIT_ITEM_FIRST).union([TokenKind::Dedent]);
    let m = p.start();
    p.bump(); // Consume 'struct'
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    p.expect(TokenKind::Indent, recovery);
    if p.at(TokenKind::Where) {
        where_clause(p, recovery);
    }
    while p.at_set(TRAIT_ITEM_FIRST) {
        trait_item(p, recovery_trait);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::TraitDecl)
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
        prop_decl(p, recovery)
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
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        type_bound_list(p, recovery);
    }
    if p.at(TokenKind::Equal) {
        p.bump(); // Consume '='
        type_annotation(p, recovery);
    }
    m.complete(p, NodeKind::TypeDecl)
}

fn trait_constant(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    constant_decl_optional_type_value(p, recovery, false, false)
}

fn trait_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    function_decl_optional_name_types_body(p, recovery, true, true, false)
}

fn impl_trait_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
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
        where_clause(p, recovery);
    }
    while p.at_set(IMPL_TRAIT_ITEM_FIRST) {
        impl_trait_item(p, recovery_trait);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::ImplTraitDecl)
}

const IMPL_TRAIT_ITEM_FIRST: [TokenKind; 4] = [
    TokenKind::Type,
    TokenKind::Const,
    TokenKind::Fn,
    TokenKind::Prop,
];

fn impl_trait_item(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Type) {
        type_decl(p, recovery)
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
    constant_decl_optional_type_value(p, recovery, false, true)
}

fn trait_impl_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    function_decl_optional_name_types_body(p, recovery, true, false, true)
}

fn trait_impl_prop(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    prop_decl_optional_type_value(p, recovery, false, true)
}

#[cfg(test)]
mod tests {
    use super::{decl_item, Parser, TokenSet};
    use crate::check_grammar;
    use expect_test::expect;
    use indoc::indoc;
    use kitty_cst::DeclItem;

    fn check(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            decl_item(p, TokenSet::NONE);
        };
        check_grammar::<DeclItem>(grammar, input, expected);
    }

    #[test]
    fn top_type() {
        // Happy path
        check(
            "type ThingList = List[Thing]",
            expect![[r#"
                TypeDecl@0..28
                  Type@0..4 "type"
                  Whitespace@4..5 " "
                  Identifier@5..14 "ThingList"
                  Whitespace@14..15 " "
                  Equal@15..16 "="
                  Whitespace@16..17 " "
                  TypeGeneric@17..28
                    TypeName@17..21
                      Identifier@17..21 "List"
                    GenericArgList@21..28
                      BracketOpen@21..22 "["
                      GenericArgPositional@22..27
                        TypeName@22..27
                          Identifier@22..27 "Thing"
                      BracketClose@27..28 "]""#]],
        );
    }

    #[test]
    fn top_const() {
        // Happy path
        check(
            "const x: Number = 42",
            expect![[r#"
                ConstantDecl@0..20
                  Const@0..5 "const"
                  Whitespace@5..6 " "
                  Identifier@6..7 "x"
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  TypeName@9..15
                    Identifier@9..15 "Number"
                  Whitespace@15..16 " "
                  Equal@16..17 "="
                  Whitespace@17..18 " "
                  NumberLiteral@18..20
                    Number@18..20 "42""#]],
        );
    }

    #[test]
    fn top_const_no_type() {
        // Happy path: Constant declaration without an explicit type.
        check(
            "const y = 100",
            expect![[r#"
                ConstantDecl@0..13
                  Const@0..5 "const"
                  Whitespace@5..6 " "
                  Identifier@6..7 "y"
                  Whitespace@7..8 " "
                  Equal@8..9 "="
                  Whitespace@9..10 " "
                  NumberLiteral@10..13
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
                EnumDecl@0..46
                  Enum@0..4 "enum"
                  Whitespace@4..5 " "
                  Identifier@5..11 "Option"
                  GenericParamList@11..14
                    BracketOpen@11..12 "["
                    GenericParam@12..13
                      Identifier@12..13 "T"
                    BracketClose@13..14 "]"
                  Newline@14..15 "\n"
                  Indent@15..19 "    "
                  EnumCase@19..31
                    Case@19..23 "case"
                    Whitespace@23..24 " "
                    Identifier@24..28 "Some"
                    Colon@28..29 ":"
                    Whitespace@29..30 " "
                    TypeName@30..31
                      Identifier@30..31 "T"
                  Newline@31..32 "\n"
                  Whitespace@32..36 "    "
                  EnumCase@36..45
                    Case@36..40 "case"
                    Whitespace@40..41 " "
                    Identifier@41..45 "None"
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
                FunctionDecl@0..22
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Identifier@3..6 "foo"
                  FunctionParamList@6..17
                    ParenOpen@6..7 "("
                    FunctionParam@7..16
                      Identifier@7..8 "x"
                      Colon@8..9 ":"
                      Whitespace@9..10 " "
                      TypeName@10..16
                        Identifier@10..16 "Number"
                    ParenClose@16..17 ")"
                  Whitespace@17..18 " "
                  FatArrow@18..20 "=>"
                  Whitespace@20..21 " "
                  FunctionBody@21..22
                    VariableRef@21..22
                      Identifier@21..22 "x""#]],
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
                FunctionDecl@0..27
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Identifier@3..6 "foo"
                  FunctionParamList@6..17
                    ParenOpen@6..7 "("
                    FunctionParam@7..16
                      Identifier@7..8 "x"
                      Colon@8..9 ":"
                      Whitespace@9..10 " "
                      TypeName@10..16
                        Identifier@10..16 "Number"
                    ParenClose@16..17 ")"
                  Whitespace@17..18 " "
                  FatArrow@18..20 "=>"
                  Newline@20..21 "\n"
                  FunctionBody@21..27
                    BlockExpr@21..27
                      Indent@21..25 "    "
                      VariableRef@25..26
                        Identifier@25..26 "x"
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
                StructDecl@0..91
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Identifier@7..13 "Person"
                  Newline@13..14 "\n"
                  Indent@14..18 "    "
                  PropDecl@18..35
                    Prop@18..22 "prop"
                    Whitespace@22..23 " "
                    Identifier@23..27 "name"
                    Colon@27..28 ":"
                    Whitespace@28..29 " "
                    TypeName@29..35
                      Identifier@29..35 "String"
                  Newline@35..36 "\n"
                  Whitespace@36..40 "    "
                  PropDecl@40..56
                    Prop@40..44 "prop"
                    Whitespace@44..45 " "
                    Identifier@45..48 "age"
                    Colon@48..49 ":"
                    Whitespace@49..50 " "
                    TypeName@50..56
                      Identifier@50..56 "Number"
                  Newline@56..57 "\n"
                  Whitespace@57..61 "    "
                  PropDecl@61..90
                    Prop@61..65 "prop"
                    Whitespace@65..66 " "
                    Identifier@66..74 "is_alive"
                    Colon@74..75 ":"
                    Whitespace@75..76 " "
                    TypeName@76..83
                      Identifier@76..83 "Boolean"
                    Whitespace@83..84 " "
                    Equal@84..85 "="
                    Whitespace@85..86 " "
                    BooleanLiteral@86..90
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
                TraitDecl@0..40
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  Identifier@6..9 "Add"
                  Newline@9..10 "\n"
                  Indent@10..14 "    "
                  FunctionDecl@14..39
                    Fn@14..16 "fn"
                    Whitespace@16..17 " "
                    Identifier@17..20 "add"
                    FunctionParamList@20..39
                      ParenOpen@20..21 "("
                      FunctionParam@21..25
                        SelfLower@21..25 "self"
                      Comma@25..26 ","
                      Whitespace@26..27 " "
                      FunctionParam@27..38
                        Identifier@27..32 "other"
                        Colon@32..33 ":"
                        Whitespace@33..34 " "
                        TypeName@34..38
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
                ImplTraitDecl@0..58
                  Impl@0..4 "impl"
                  Whitespace@4..5 " "
                  TypeName@5..12
                    Identifier@5..12 "MyTrait"
                  Whitespace@12..13 " "
                  For@13..16 "for"
                  Whitespace@16..17 " "
                  TypeName@17..25
                    Identifier@17..25 "MyStruct"
                  Newline@25..26 "\n"
                  Indent@26..30 "    "
                  FunctionDecl@30..58
                    Fn@30..32 "fn"
                    Whitespace@32..33 " "
                    Identifier@33..42 "my_method"
                    FunctionParamList@42..44
                      ParenOpen@42..43 "("
                      ParenClose@43..44 ")"
                    Whitespace@44..45 " "
                    FatArrow@45..47 "=>"
                    Newline@47..48 "\n"
                    Whitespace@48..52 "    "
                    FunctionBody@52..58
                      BlockExpr@52..58
                        Indent@52..56 "    "
                        NumberLiteral@56..57
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
                TypeDecl@0..18
                  Type@0..4 "type"
                  Whitespace@4..5 " "
                  Missing@5..5
                  Equal@5..6 "="
                  Whitespace@6..7 " "
                  TypeGeneric@7..18
                    TypeName@7..11
                      Identifier@7..11 "List"
                    GenericArgList@11..18
                      BracketOpen@11..12 "["
                      GenericArgPositional@12..17
                        TypeName@12..17
                          Identifier@12..17 "Thing"
                      BracketClose@17..18 "]"
                error at 5: missing identifier"#]],
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
                TraitDecl@0..40
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  Identifier@6..13 "MyTrait"
                  Newline@13..14 "\n"
                  Indent@14..18 "    "
                  TypeDecl@18..39
                    Type@18..22 "type"
                    Whitespace@22..23 " "
                    Identifier@23..28 "Assoc"
                    Whitespace@28..29 " "
                    Equal@29..30 "="
                    Whitespace@30..31 " "
                    TypeName@31..39
                      Identifier@31..39 "Concrete"
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
                    const PI: Number
            "},
            expect![[r#"
                TraitDecl@0..35
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  Identifier@6..13 "MyTrait"
                  Newline@13..14 "\n"
                  Indent@14..18 "    "
                  ConstantDecl@18..34
                    Const@18..23 "const"
                    Whitespace@23..24 " "
                    Identifier@24..26 "PI"
                    Colon@26..27 ":"
                    Whitespace@27..28 " "
                    TypeName@28..34
                      Identifier@28..34 "Number"
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
                    const VERSION = 1
                    fn do_something() => 0
                    prop field = default
            "},
            expect![[r#"
                ImplTraitDecl@0..126
                  Impl@0..4 "impl"
                  Whitespace@4..5 " "
                  TypeName@5..12
                    Identifier@5..12 "MyTrait"
                  Whitespace@12..13 " "
                  For@13..16 "for"
                  Whitespace@16..17 " "
                  TypeName@17..25
                    Identifier@17..25 "MyStruct"
                  Newline@25..26 "\n"
                  Indent@26..30 "    "
                  TypeDecl@30..51
                    Type@30..34 "type"
                    Whitespace@34..35 " "
                    Identifier@35..40 "Assoc"
                    Whitespace@40..41 " "
                    Equal@41..42 "="
                    Whitespace@42..43 " "
                    TypeName@43..51
                      Identifier@43..51 "Concrete"
                  Newline@51..52 "\n"
                  Whitespace@52..56 "    "
                  ConstantDecl@56..73
                    Const@56..61 "const"
                    Whitespace@61..62 " "
                    Identifier@62..69 "VERSION"
                    Whitespace@69..70 " "
                    Equal@70..71 "="
                    Whitespace@71..72 " "
                    NumberLiteral@72..73
                      Number@72..73 "1"
                  Newline@73..74 "\n"
                  Whitespace@74..78 "    "
                  FunctionDecl@78..100
                    Fn@78..80 "fn"
                    Whitespace@80..81 " "
                    Identifier@81..93 "do_something"
                    FunctionParamList@93..95
                      ParenOpen@93..94 "("
                      ParenClose@94..95 ")"
                    Whitespace@95..96 " "
                    FatArrow@96..98 "=>"
                    Whitespace@98..99 " "
                    FunctionBody@99..100
                      NumberLiteral@99..100
                        Number@99..100 "0"
                  Newline@100..101 "\n"
                  Whitespace@101..105 "    "
                  PropDecl@105..125
                    Prop@105..109 "prop"
                    Whitespace@109..110 " "
                    Identifier@110..115 "field"
                    Whitespace@115..116 " "
                    Equal@116..117 "="
                    Whitespace@117..118 " "
                    VariableRef@118..125
                      Identifier@118..125 "default"
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
                TraitDecl@0..31
                  Trait@0..5 "trait"
                  Whitespace@5..6 " "
                  Identifier@6..12 "Broken"
                  Newline@12..13 "\n"
                  Indent@13..17 "    "
                  Error@17..30
                    Identifier@17..30 "unknown_token"
                  Newline@30..31 "\n"
                error at 17..30: expected dedent, but found identifier"#]],
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
                StructDecl@0..40
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Identifier@7..17 "Incomplete"
                  Newline@17..18 "\n"
                  Indent@18..22 "    "
                  FunctionDecl@22..40
                    Fn@22..24 "fn"
                    Whitespace@24..25 " "
                    Identifier@25..38 "missing_brace"
                    FunctionParamList@38..40
                      ParenOpen@38..39 "("
                      Newline@39..40 "\n"
                      FunctionParam@40..40
                        Missing@40..40
                        Missing@40..40
                        Missing@40..40
                      Missing@40..40
                    Missing@40..40
                    FunctionBody@40..40
                      Missing@40..40
                  Dedent@40..40 ""
                error at 40: missing identifier
                error at 40: missing ‘:’
                error at 40: missing identifier, ‘Self’, ‘(’, ‘Fn’, or ‘impl’
                error at 40: missing ‘)’
                error at 40: missing ‘=>’
                error at 40: missing ‘+’, ‘-’, ‘not’, identifier, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’"#]],
        );
    }

    #[test]
    fn struct_multiple_items() {
        // Happy path: A struct with multiple items: constant, function, and property.
        check(
            indoc! {"
                struct Complex
                    const A = 10
                    fn do_it() => A
                    prop b: Number = 20
            "},
            expect![[r#"
                StructDecl@0..76
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Identifier@7..14 "Complex"
                  Newline@14..15 "\n"
                  Indent@15..19 "    "
                  ConstantDecl@19..31
                    Const@19..24 "const"
                    Whitespace@24..25 " "
                    Identifier@25..26 "A"
                    Whitespace@26..27 " "
                    Equal@27..28 "="
                    Whitespace@28..29 " "
                    NumberLiteral@29..31
                      Number@29..31 "10"
                  Newline@31..32 "\n"
                  Whitespace@32..36 "    "
                  FunctionDecl@36..51
                    Fn@36..38 "fn"
                    Whitespace@38..39 " "
                    Identifier@39..44 "do_it"
                    FunctionParamList@44..46
                      ParenOpen@44..45 "("
                      ParenClose@45..46 ")"
                    Whitespace@46..47 " "
                    FatArrow@47..49 "=>"
                    Whitespace@49..50 " "
                    FunctionBody@50..51
                      VariableRef@50..51
                        Identifier@50..51 "A"
                  Newline@51..52 "\n"
                  Whitespace@52..56 "    "
                  PropDecl@56..75
                    Prop@56..60 "prop"
                    Whitespace@60..61 " "
                    Identifier@61..62 "b"
                    Colon@62..63 ":"
                    Whitespace@63..64 " "
                    TypeName@64..70
                      Identifier@64..70 "Number"
                    Whitespace@70..71 " "
                    Equal@71..72 "="
                    Whitespace@72..73 " "
                    NumberLiteral@73..75
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
                FunctionDecl@0..20
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Identifier@3..6 "foo"
                  GenericParamList@6..9
                    BracketOpen@6..7 "["
                    GenericParam@7..8
                      Identifier@7..8 "T"
                    BracketClose@8..9 "]"
                  FunctionParamList@9..15
                    ParenOpen@9..10 "("
                    FunctionParam@10..14
                      Identifier@10..11 "x"
                      Colon@11..12 ":"
                      Whitespace@12..13 " "
                      TypeName@13..14
                        Identifier@13..14 "T"
                    ParenClose@14..15 ")"
                  Whitespace@15..16 " "
                  FatArrow@16..18 "=>"
                  Whitespace@18..19 " "
                  FunctionBody@19..20
                    VariableRef@19..20
                      Identifier@19..20 "x""#]],
        );
    }
}
