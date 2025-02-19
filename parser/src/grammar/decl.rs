use kitty_syntax::{NodeKind, TokenKind};

use crate::{
    grammar::{
        function::{function_body, function_param_list_optional_types},
        r#type::{generic_param_list, type_bound_list, type_path},
    },
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
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    p.expect(TokenKind::Equal, recovery);
    type_annotation(p, recovery);
    m.complete(p, NodeKind::TypeDecl)
}

/// Constant declaration
fn constant_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    constant_decl_optional_type_value(p, recovery, false, true)
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

/// Function declaration
fn function_decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    function_decl_optional_types_body(p, recovery, true, true)
}

fn function_decl_optional_types_body(
    p: &mut Parser,
    recovery: TokenSet,
    has_types: bool,
    has_body: bool,
) -> CompletedMarker {
    assert!(p.at(TokenKind::Fn));
    let m = p.start();
    p.bump(); // Consume 'fn'
    p.expect(TokenKind::Identifier, recovery);
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    function_param_list_optional_types(p, recovery, has_types);
    if has_body || p.at(TokenKind::FatArrow) {
        p.expect(TokenKind::FatArrow, recovery);
        function_body(p, recovery);
    }
    m.complete(p, NodeKind::FunctionDecl)
}

/// Function declaration
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
    while p.at_set(TRAIT_ITEM_FIRST) {
        trait_item(p, recovery_trait);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::TraitDecl)
}

const TRAIT_ITEM_FIRST: [TokenKind; 3] = [TokenKind::Const, TokenKind::Fn, TokenKind::Prop];

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
    p.expect(TokenKind::Colon, recovery);
    type_bound_list(p, recovery);
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
    function_decl_optional_types_body(p, recovery, true, false)
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
    while p.at_set(IMPL_TRAIT_ITEM_FIRST) {
        impl_trait_item(p, recovery_trait);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::ImplTraitDecl)
}

const IMPL_TRAIT_ITEM_FIRST: [TokenKind; 3] = [TokenKind::Const, TokenKind::Fn, TokenKind::Prop];

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
    function_decl_optional_types_body(p, recovery, false, true)
}

fn trait_impl_prop(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    prop_decl_optional_type_value(p, recovery, false, true)
}

#[cfg(test)]
mod tests {
    use super::{top_item, Parser, TokenSet};
    use crate::check_grammar;
    use expect_test::expect;
    use indoc::indoc;
    use kitty_cst::TopItem;

    fn check(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            top_item(p, TokenSet::NONE);
        };
        check_grammar::<TopItem>(grammar, input, expected);
    }

    #[test]
    fn top_type() {
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
    fn top_enum() {
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
}
