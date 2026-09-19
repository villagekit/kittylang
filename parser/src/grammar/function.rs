use kitty_syntax::{NodeKind, TokenKind};

use super::{
    expression::{expression, EXPRESSION_FIRST},
    r#type::{generic_param_list, generic_where_clause, type_annotation},
    NAME_FIRST,
};
use crate::{
    marker::{CompletedMarker, Marker},
    parser::Parser,
    token_set::TokenSet,
};

/// Which parts a function rule requires or allows, by where it stands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FunctionForm {
    /// A declaration in a module, struct, enum or impl: named, with an
    /// optional return type and a required body.
    Declaration,
    /// A declaration in a trait: the same, but the body is optional and
    /// a declaration with none ends at its parameter list, return type
    /// or `where` clause.
    TraitDeclaration,
    /// A lambda in an expression: the name is optional, there is no
    /// return type and the body is required.
    Lambda,
}

impl FunctionForm {
    fn requires_name(self) -> bool {
        self != Self::Lambda
    }

    fn takes_return_type(self) -> bool {
        self != Self::Lambda
    }

    fn requires_body(self) -> bool {
        self != Self::TraitDeclaration
    }
}

/// The marker `m` is the caller's, started before any attributes, so
/// they are children of the function node.
pub(crate) fn function_declaration(
    p: &mut Parser,
    recovery: TokenSet,
    form: FunctionForm,
    m: Marker,
) -> CompletedMarker {
    // `declaration` and its item rules, and `expression_primary`,
    // dispatch here on `fn`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Fn));
    p.bump(); // Consume 'fn'
    if p.at_set(NAME_FIRST) {
        p.bump();
    } else if form.requires_name() {
        // A name left out is missing, not an error that eats the `(`
        // after it.
        p.error(recovery.union([TokenKind::BracketOpen, TokenKind::ParenOpen]));
    }
    if p.at(TokenKind::BracketOpen) {
        generic_param_list(p, recovery);
    }
    function_param_list(p, recovery);
    if form.takes_return_type() && p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'
        function_return_type(p, recovery);
    }
    if p.at(TokenKind::Where) {
        // The clause ends at `=>`, so a body after a malformed clause is
        // a body, not more bounds.
        generic_where_clause(p, recovery.union([TokenKind::FatArrow]));
    }
    if form.requires_body() || p.at(TokenKind::FatArrow) {
        p.expect(TokenKind::FatArrow, recovery);
        function_body(p, recovery);
    }
    m.complete(p, NodeKind::DeclarationFunction)
}

/// The type after the `:` that follows the parameter list. A type left
/// out recovers at the `where` clause or the body.
fn function_return_type(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    type_annotation(p, recovery.union([TokenKind::Where, TokenKind::FatArrow]));
    m.complete(p, NodeKind::FunctionReturnType)
}

/// The list node is made even when the `(` is not there, so a function's
/// shape holds; the node then holds only the `Missing` or `Error` node
/// the recovery rule gives.
fn function_param_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let recovery_param_list = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    let m = p.start();
    if p.at(TokenKind::ParenOpen) {
        p.bump(); // Consume '('
        if !p.at(TokenKind::ParenClose) {
            loop {
                function_param(p, recovery_param_list);
                if !p.bump_if_at(TokenKind::Comma) {
                    break;
                }
            }
        }
        p.expect(TokenKind::ParenClose, recovery);
    } else {
        // The list is missing when what may follow it comes next: a
        // return type, a `where` clause or a body.
        p.error(recovery.union([TokenKind::Colon, TokenKind::Where, TokenKind::FatArrow]));
    }
    m.complete(p, NodeKind::FunctionParamList)
}

fn function_param(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    // Parse label
    function_param_label(p, recovery);
    if p.at(TokenKind::Colon) {
        // Parse type
        p.expect(TokenKind::Colon, recovery);
        type_annotation(p, recovery);
    }
    // Parse default value
    if p.at(TokenKind::Equal) {
        p.bump(); // Consume '='
        expression(p, recovery);
    }
    m.complete(p, NodeKind::FunctionParam)
}

/// The first tokens of an argument list: the three call forms.
pub(crate) const FUNCTION_ARG_LIST_FIRST: [TokenKind; 3] = [
    TokenKind::ParenOpen,
    TokenKind::BraceOpen,
    TokenKind::Indent,
];

/// An argument list in one of its three forms: `( )` for positional
/// then keyword arguments, `{ }` for keyword arguments, or a block of
/// keyword arguments one per line. A spread may stand in any of them.
pub(crate) fn function_arg_list(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `expression_apply` dispatches here on `(`, `{` or an indent.
    debug_assert!(p
        .peek()
        .is_some_and(|kind| FUNCTION_ARG_LIST_FIRST.contains(&kind)));
    let m = p.start();
    if p.at(TokenKind::ParenOpen) {
        function_arg_list_parens(p, recovery);
    } else if p.at(TokenKind::BraceOpen) {
        function_arg_list_braces(p, recovery);
    } else {
        function_arg_list_block(p, recovery);
    }
    m.complete(p, NodeKind::FunctionArgList)
}

/// Positional arguments, then keyword arguments once a `label =` is
/// seen. `label :` counts as well: it is the spelling `=` replaced, so
/// the argument is read as a keyword argument and the error sits at the
/// `:`, as it does in `{ }` and in a block.
fn function_arg_list_parens(p: &mut Parser, recovery: TokenSet) {
    let recovery_arg = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    p.bump(); // Consume '('.
    if !p.at(TokenKind::ParenClose) {
        let mut keyword = false;
        loop {
            if !keyword
                && p.at_set(FUNCTION_PARAM_LABEL_FIRST)
                && (p.lookahead_at(1, TokenKind::Equal) || p.lookahead_at(1, TokenKind::Colon))
            {
                keyword = true;
            }
            if keyword {
                function_keyword_arg(p, recovery_arg);
            } else if p.at(TokenKind::Ellipses) {
                function_spread_arg(p, recovery_arg);
            } else {
                function_positional_arg(p, recovery_arg);
            }
            if !function_arg_separator(p, TokenKind::ParenClose, recovery_arg) {
                break;
            }
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
}

fn function_arg_list_braces(p: &mut Parser, recovery: TokenSet) {
    let recovery_arg = recovery.union([TokenKind::Comma, TokenKind::BraceClose]);
    p.bump(); // Consume '{'.
    if !p.at(TokenKind::BraceClose) {
        loop {
            function_keyword_arg(p, recovery_arg);
            if !function_arg_separator(p, TokenKind::BraceClose, recovery_arg) {
                break;
            }
        }
    }
    p.expect(TokenKind::BraceClose, recovery);
}

/// After an argument in `( )` or `{ }`: whether another argument
/// follows. A `,` says one does; the `close`, a token the caller
/// recovers at, or the end of the input says none does. Any other token
/// is skipped as one `Error` node so the list can go on, and another
/// argument follows unless the list then ends.
fn function_arg_separator(p: &mut Parser, close: TokenKind, recovery_arg: TokenSet) -> bool {
    if p.bump_if_at(TokenKind::Comma) {
        return true;
    }
    if p.at(close) || p.at_recovery(recovery_arg) {
        return false;
    }
    p.error(recovery_arg);
    !(p.at(close) || p.at_recovery(recovery_arg))
}

/// The lines are separated by newlines, which are trivia, so the loop
/// runs until the dedent or a token the caller recovers at.
fn function_arg_list_block(p: &mut Parser, recovery: TokenSet) {
    let recovery_arg = recovery.union([TokenKind::Dedent]);
    p.bump(); // Consume the indent.
    while !p.at_recovery(recovery_arg) {
        function_keyword_arg(p, recovery_arg);
    }
    p.expect(TokenKind::Dedent, recovery);
}

fn function_positional_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionArgPositional)
}

/// An argument where only a keyword argument or a spread may stand: in
/// `{ }`, in a block, or after the first keyword argument in `( )`.
///
/// A positional argument here is still parsed whole, under one error,
/// so a line in the wrong place is one error rather than one per token.
/// Whether a block may carry positional lines is design plan
/// e6a33eab19d4's to decide; until then they are errors.
fn function_keyword_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    if p.at_set(FUNCTION_PARAM_LABEL_FIRST) {
        function_labelled_arg(p, recovery)
    } else if p.at(TokenKind::Ellipses) {
        function_spread_arg(p, recovery)
    } else if p.peek_in(EXPRESSION_FIRST) {
        p.error_misplaced();
        function_positional_arg(p, recovery)
    } else {
        let m = p.start();
        p.error(recovery);
        m.complete(p, NodeKind::FunctionArgLabelled)
    }
}

/// `name = value`. The value is required: `name` alone is positional.
fn function_labelled_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `function_keyword_arg` dispatches here on a label.
    debug_assert!(p
        .peek()
        .is_some_and(|kind| FUNCTION_PARAM_LABEL_FIRST.contains(&kind)));
    let m = p.start();
    function_param_label(p, recovery);
    p.expect(TokenKind::Equal, recovery);
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionArgLabelled)
}

/// `...value`: the fields of `value` supplied as keyword arguments.
fn function_spread_arg(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // The argument rules dispatch here on `...`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Ellipses));
    let m = p.start();
    p.bump(); // Consume '...'.
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionArgSpread)
}

const FUNCTION_PARAM_LABEL_FIRST: [TokenKind; 2] =
    [TokenKind::IdentifierValue, TokenKind::SelfLower];

fn function_param_label(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if p.at_set(FUNCTION_PARAM_LABEL_FIRST) {
        Some(p.mark_kind(NodeKind::FunctionParamLabel))
    } else {
        p.error(recovery);
        None
    }
}

fn function_body(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    expression(p, recovery);
    m.complete(p, NodeKind::FunctionBody)
}
