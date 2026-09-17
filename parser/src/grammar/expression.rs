use kitty_syntax::{NodeKind, TokenKind};

use super::{
    function::{function_arg_list, function_declaration, FunctionForm, FUNCTION_ARG_LIST_FIRST},
    pattern::pattern,
    r#type::{type_annotation, type_path_in_expression, TYPE_PATH_FIRST},
    NAME_FIRST,
};
use crate::{marker::CompletedMarker, token_set::TokenSet, Parser};

/// Parse an expression.
pub(crate) fn expression(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    expression_pratt(p, recovery, 0, BlockArgs::Allowed)
}

/// Parse an expression that stops short of an indented block, for a rule
/// that owns the block after its expression, such as `match`. Elsewhere
/// an indented block after an operand is its argument list.
fn expression_before_block(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    expression_pratt(p, recovery, 0, BlockArgs::Forbidden)
}

/// Whether an indented block after an operand is its argument list.
///
/// The restriction holds through operators, unary operands and the tail
/// of an `if` or a `let`, and lifts inside brackets and blocks, where a
/// nested expression starts afresh. A lambda body is not restricted: it
/// is parsed by the function rule, and a lambda has no use as a
/// scrutinee.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BlockArgs {
    Allowed,
    Forbidden,
}

/// Parse an expression with a given minimum binding power.
/// (Also known as a Pratt parser.)
fn expression_pratt(
    p: &mut Parser,
    recovery: TokenSet,
    min_bp: u8,
    block_args: BlockArgs,
) -> Option<CompletedMarker> {
    // First, parse a left-hand side (unary operator or primary) expression.
    let mut lhs = expression_lhs(p, recovery, block_args)?;

    loop {
        // Check for postfix operators inline.
        let at_arg_list = match block_args {
            BlockArgs::Allowed => p.at_set(FUNCTION_ARG_LIST_FIRST),
            BlockArgs::Forbidden => p.at_set([TokenKind::ParenOpen, TokenKind::BraceOpen]),
        };
        if at_arg_list {
            lhs = expression_apply(p, lhs, recovery);
            continue;
        }
        if p.at(TokenKind::Dot) {
            lhs = expression_get(p, lhs, recovery);
            continue;
        }

        // Then, check if a binary operator follows.
        let (left_bp, right_bp) = match binary_binding_power(p) {
            Some(bp) => bp,
            None => break,
        };

        // If the operator's left binding power is lower than what we're expecting, stop.
        if left_bp < min_bp {
            break;
        }

        p.bump(); // Consume the operator.

        let m = lhs.precede(p);
        let rhs = expression_pratt(p, recovery, right_bp, block_args);
        lhs = m.complete(p, NodeKind::ExpressionBinary);

        if rhs.is_none() {
            break;
        }
    }

    Some(lhs)
}

/// Parse a left-hand side expression, which may be a unary operator or a primary.
fn expression_lhs(
    p: &mut Parser,
    recovery: TokenSet,
    block_args: BlockArgs,
) -> Option<CompletedMarker> {
    if let Some(bp) = unary_binding_power(p) {
        // A unary operator is present.
        // The node is made whether or not the operand parses, as the
        // binary rule does, so the marker is always completed.
        let m = p.start();
        p.bump(); // Consume the unary operator token.
        expression_pratt(p, recovery, bp, block_args);
        return Some(m.complete(p, NodeKind::ExpressionUnary));
    }
    expression_primary(p, recovery, block_args)
}

/// The tokens an expression can start with: what `expression_lhs` and
/// `expression_primary` dispatch on, for a rule that must know whether
/// an expression follows before it commits to one.
pub(crate) const EXPRESSION_FIRST: TokenSet = TokenSet::new([
    TokenKind::Plus,
    TokenKind::Minus,
    TokenKind::Not,
    TokenKind::IdentifierValue,
    TokenKind::SelfLower,
    TokenKind::IdentifierType,
    TokenKind::SelfUpper,
    TokenKind::Number,
    TokenKind::String,
    TokenKind::ParenOpen,
    TokenKind::Indent,
    TokenKind::Fn,
    TokenKind::Let,
    TokenKind::If,
    TokenKind::Match,
]);

/// Parse a primary expression.
fn expression_primary(
    p: &mut Parser,
    recovery: TokenSet,
    block_args: BlockArgs,
) -> Option<CompletedMarker> {
    let cm = if p.at_set(EXPRESSION_REFERENCE_FIRST) {
        expression_reference(p)
    } else if p.at_set(TYPE_PATH_FIRST) {
        type_path_in_expression(p, recovery)?
    } else if p.at_set(LITERAL_FIRST) {
        expression_literal(p)
    } else if p.at(TokenKind::ParenOpen) {
        expression_tuple(p, recovery)
    } else if p.at(TokenKind::Indent) {
        expression_block(p, recovery)
    } else if p.at(TokenKind::Fn) {
        expression_function(p, recovery)
    } else if p.at(TokenKind::Let) {
        expression_let(p, recovery, block_args)
    } else if p.at(TokenKind::If) {
        expression_if(p, recovery, block_args)
    } else if p.at(TokenKind::Match) {
        expression_match(p, recovery)
    } else {
        // The branches above are `EXPRESSION_FIRST`; keep the two in step.
        debug_assert!(!p.peek_in(EXPRESSION_FIRST));
        p.error(recovery);
        return None;
    };
    Some(cm)
}

pub(crate) const EXPRESSION_REFERENCE_FIRST: [TokenKind; 2] =
    [TokenKind::IdentifierValue, TokenKind::SelfLower];

fn expression_reference(p: &mut Parser) -> CompletedMarker {
    // `expression_primary` dispatches here on a value name or `self`.
    debug_assert!(p
        .peek()
        .is_some_and(|kind| EXPRESSION_REFERENCE_FIRST.contains(&kind)));
    p.mark_kind(NodeKind::ExpressionReference)
}

const LITERAL_FIRST: [TokenKind; 2] = [TokenKind::Number, TokenKind::String];

fn expression_literal(p: &mut Parser) -> CompletedMarker {
    // `expression_primary` dispatches here on a literal.
    debug_assert!(p.peek().is_some_and(|kind| LITERAL_FIRST.contains(&kind)));
    p.mark_kind(NodeKind::ExpressionLiteral)
}

/// Parse an apply expression given an existing `lhs`: the callee and
/// its argument list in any of the three forms.
fn expression_apply(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    // `expression_pratt` dispatches here on `(`, `{` or an indent, which
    // `function_arg_list` asserts.
    let m = lhs.precede(p);
    function_arg_list(p, recovery);
    m.complete(p, NodeKind::ExpressionApply)
}

/// Parse a field access (get expression) given an existing `lhs`.
fn expression_get(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    // `expression_pratt` dispatches here on `.`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Dot));
    let m = lhs.precede(p);
    p.bump(); // Consume '.'.
    if p.at_set(NAME_FIRST) {
        p.bump();
    } else {
        p.error(recovery);
    }
    m.complete(p, NodeKind::ExpressionGet)
}

/// Parse a tuple expression.
fn expression_tuple(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `expression_primary` dispatches here on `(`.
    debug_assert_eq!(p.peek(), Some(TokenKind::ParenOpen));
    let m = p.start();
    p.bump(); // Consume '('
    let recovery_tuple = recovery.union([TokenKind::Comma, TokenKind::ParenClose]);
    while !p.at(TokenKind::ParenClose) {
        expression(p, recovery_tuple);
        if !p.bump_if_at(TokenKind::Comma) {
            break;
        }
    }
    p.expect(TokenKind::ParenClose, recovery);
    m.complete(p, NodeKind::ExpressionTuple)
}

/// Parse a block expression: `{ ... }` but with indents
fn expression_block(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let recovery_block = recovery.union([TokenKind::Indent, TokenKind::Dedent]);
    let m = p.start();
    p.expect(TokenKind::Indent, recovery_block);
    expression(p, recovery_block);
    p.expect(TokenKind::Dedent, recovery_block);
    m.complete(p, NodeKind::ExpressionBlock)
}

/// Parse a function expression: `fn <identifier>(<identifier>: <type annotation>) => <expr>`
fn expression_function(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    // `expression_primary` dispatches here on `fn`.
    debug_assert_eq!(p.peek(), Some(TokenKind::Fn));
    let m = p.start();
    function_declaration(p, recovery, FunctionForm::Lambda, m)
}

/// Parse a let expression: `let <identifier> = <expr> in <expr>`
///
/// The body is the tail, so it keeps the caller's block restriction.
fn expression_let(p: &mut Parser, recovery: TokenSet, block_args: BlockArgs) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Let, recovery);
    pattern(p, recovery.union([TokenKind::Equal, TokenKind::In]));
    if p.at(TokenKind::Colon) {
        p.bump(); // Consume ':'.
        type_annotation(p, recovery);
    }
    p.expect(TokenKind::Equal, recovery.union([TokenKind::In]));
    // The value of the variable
    expression(p, recovery);
    p.expect(TokenKind::In, recovery);
    // The body of the let scope
    expression_pratt(p, recovery, 0, block_args);
    m.complete(p, NodeKind::ExpressionLet)
}

/// Parse an if expression: `if <cond> then <expr> [else <expr>]`
///
/// Either branch may be the tail, so both keep the caller's block
/// restriction.
fn expression_if(p: &mut Parser, recovery: TokenSet, block_args: BlockArgs) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::If, recovery);
    expression(p, recovery.union([TokenKind::Then])); // condition
    p.expect(TokenKind::Then, recovery);
    expression_pratt(p, recovery.union([TokenKind::Else]), 0, block_args); // then body
    if p.at(TokenKind::Else) {
        p.bump(); // Consume 'else'.
        expression_pratt(p, recovery, 0, block_args); // else body
    }
    m.complete(p, NodeKind::ExpressionIf)
}

/// Parse an match expression: `match <value> { pattern => expression, ... }`
fn expression_match(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Match, recovery);
    // The block after the scrutinee is the arms, never its arguments.
    expression_before_block(p, recovery);
    p.expect(TokenKind::Indent, recovery);
    // The arms end at the dedent, or where an arm would consume nothing.
    let recovery_arms = recovery.union([TokenKind::Dedent]);
    while !p.at_recovery(recovery_arms) {
        match_arm(p, recovery_arms);
    }
    p.expect(TokenKind::Dedent, recovery);
    m.complete(p, NodeKind::ExpressionMatch)
}

fn match_arm(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    pattern(p, recovery.union([TokenKind::FatArrow]));
    p.expect(TokenKind::FatArrow, recovery);
    expression(p, recovery);
    m.complete(p, NodeKind::MatchArm)
}

/// Return the binding power for a unary operator at the current token, if any.
fn unary_binding_power(p: &mut Parser) -> Option<u8> {
    if p.at_set([TokenKind::Plus, TokenKind::Minus, TokenKind::Not]) {
        Some(15)
    } else {
        None
    }
}

/// Return the binding power for a binary operator at the current token, if any.
fn binary_binding_power(p: &mut Parser) -> Option<(u8, u8)> {
    // Binding power for binary operations, in order of highest to lowest.
    //
    // Multiplicative (Factor) operators
    if p.at_set([TokenKind::Multiply, TokenKind::Divide, TokenKind::Rem]) {
        Some((13, 14))
    }
    // Additive (Term) operators
    else if p.at_set([TokenKind::Plus, TokenKind::Minus]) {
        Some((11, 12))
    }
    // Bitwise shift
    // Comparison
    else if p.at_set([
        TokenKind::Less,
        TokenKind::LessEqual,
        TokenKind::Greater,
        TokenKind::GreaterEqual,
    ]) {
        Some((9, 10))
    }
    // Equality
    else if p.at_set([TokenKind::EqualEqual, TokenKind::NotEqual]) {
        Some((7, 8))
    }
    // Bitwise And
    // Bitwise Xor
    // Bitwise Or
    // Logical And
    else if p.at(TokenKind::And) {
        Some((5, 6))
    }
    // Logical Xor
    else if p.at(TokenKind::Xor) {
        Some((3, 4))
    }
    // Logical Or
    else if p.at(TokenKind::Or) {
        Some((1, 2))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::{expression, Parser, TokenSet};
    use crate::check_grammar;
    use expect_test::expect;
    use indoc::indoc;
    use kitty_cst::Expression;

    fn check(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            expression(p, TokenSet::NONE);
        };
        check_grammar::<Expression>(grammar, input, expected);
    }

    #[test]
    fn unary_operator_without_an_operand_is_missing() {
        check(
            "-",
            expect![[r#"
                ExpressionUnary@0..1
                  Minus@0..1 "-"
                  Missing@1..1
                error at 1: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn lambda_in_an_argument() {
        check(
            "f(fn (x) => x)",
            expect![[r#"
            ExpressionApply@0..14
              ExpressionReference@0..1
                IdentifierValue@0..1 "f"
              FunctionArgList@1..14
                ParenOpen@1..2 "("
                FunctionArgPositional@2..13
                  DeclarationFunction@2..13
                    Fn@2..4 "fn"
                    Whitespace@4..5 " "
                    FunctionParamList@5..8
                      ParenOpen@5..6 "("
                      FunctionParam@6..7
                        FunctionParamLabel@6..7
                          IdentifierValue@6..7 "x"
                      ParenClose@7..8 ")"
                    Whitespace@8..9 " "
                    FatArrow@9..11 "=>"
                    Whitespace@11..12 " "
                    FunctionBody@12..13
                      ExpressionReference@12..13
                        IdentifierValue@12..13 "x"
                ParenClose@13..14 ")""#]],
        );
    }

    #[test]
    fn labelled_arg_after_a_labelled_arg_recovers() {
        check(
            "f(a = 1, 2)",
            expect![[r#"
                ExpressionApply@0..11
                  ExpressionReference@0..1
                    IdentifierValue@0..1 "f"
                  FunctionArgList@1..11
                    ParenOpen@1..2 "("
                    FunctionArgLabelled@2..7
                      FunctionParamLabel@2..3
                        IdentifierValue@2..3 "a"
                      Whitespace@3..4 " "
                      Equal@4..5 "="
                      Whitespace@5..6 " "
                      ExpressionLiteral@6..7
                        Number@6..7 "1"
                    Comma@7..8 ","
                    Whitespace@8..9 " "
                    FunctionArgPositional@9..10
                      ExpressionLiteral@9..10
                        Number@9..10 "2"
                    ParenClose@10..11 ")"
                error at 9..10: expected value-id, ‘self’, or ‘...’, but found number"#]],
        );
    }

    #[test]
    fn number() {
        // Happy path
        check(
            "123",
            expect![[r#"
                ExpressionLiteral@0..3
                  Number@0..3 "123""#]],
        );
    }

    #[test]
    fn number_preceded_by_whitespace() {
        // Happy path
        check(
            "   9876",
            expect![[r#"
                ExpressionLiteral@0..7
                  Whitespace@0..3 "   "
                  Number@3..7 "9876""#]],
        );
    }

    #[test]
    fn number_followed_by_whitespace() {
        // Happy path
        check(
            "999   ",
            expect![[r#"
                ExpressionLiteral@0..6
                  Number@0..3 "999"
                  Whitespace@3..6 "   ""#]],
        );
    }

    #[test]
    fn number_surrounded_by_whitespace() {
        // Happy path
        check(
            " 123     ",
            expect![[r#"
                ExpressionLiteral@0..9
                  Whitespace@0..1 " "
                  Number@1..4 "123"
                  Whitespace@4..9 "     ""#]],
        );
    }

    #[test]
    fn variable_ref() {
        // Happy path
        check(
            "counter",
            expect![[r#"
                ExpressionReference@0..7
                  IdentifierValue@0..7 "counter""#]],
        );
    }

    #[test]
    fn simple_infix_expression() {
        // Happy path
        check(
            "1+2",
            expect![[r#"
                ExpressionBinary@0..3
                  ExpressionLiteral@0..1
                    Number@0..1 "1"
                  Plus@1..2 "+"
                  ExpressionLiteral@2..3
                    Number@2..3 "2""#]],
        );
    }

    #[test]
    fn left_associative_infix_expression() {
        // Happy path
        check(
            "1+2+3+4",
            expect![[r#"
                ExpressionBinary@0..7
                  ExpressionBinary@0..5
                    ExpressionBinary@0..3
                      ExpressionLiteral@0..1
                        Number@0..1 "1"
                      Plus@1..2 "+"
                      ExpressionLiteral@2..3
                        Number@2..3 "2"
                    Plus@3..4 "+"
                    ExpressionLiteral@4..5
                      Number@4..5 "3"
                  Plus@5..6 "+"
                  ExpressionLiteral@6..7
                    Number@6..7 "4""#]],
        );
    }

    #[test]
    fn infix_expression_with_mixed_binding_power() {
        // Happy path
        check(
            "1+2*3-4",
            expect![[r#"
                ExpressionBinary@0..7
                  ExpressionBinary@0..5
                    ExpressionLiteral@0..1
                      Number@0..1 "1"
                    Plus@1..2 "+"
                    ExpressionBinary@2..5
                      ExpressionLiteral@2..3
                        Number@2..3 "2"
                      Multiply@3..4 "*"
                      ExpressionLiteral@4..5
                        Number@4..5 "3"
                  Minus@5..6 "-"
                  ExpressionLiteral@6..7
                    Number@6..7 "4""#]],
        );
    }

    #[test]
    fn infix_expression_with_whitespace() {
        // Happy path
        check(
            " 1 +   2* 3 ",
            expect![[r#"
                ExpressionBinary@0..12
                  Whitespace@0..1 " "
                  ExpressionLiteral@1..2
                    Number@1..2 "1"
                  Whitespace@2..3 " "
                  Plus@3..4 "+"
                  Whitespace@4..7 "   "
                  ExpressionBinary@7..11
                    ExpressionLiteral@7..8
                      Number@7..8 "2"
                    Multiply@8..9 "*"
                    Whitespace@9..10 " "
                    ExpressionLiteral@10..11
                      Number@10..11 "3"
                  Whitespace@11..12 " ""#]],
        );
    }

    #[test]
    fn infix_expression_interspersed_with_newlines() {
        // Happy path
        check(
            "
1 +
1
+ 1",
            expect![[r#"
                ExpressionBinary@0..10
                  Newline@0..1 "\n"
                  ExpressionBinary@1..6
                    ExpressionLiteral@1..2
                      Number@1..2 "1"
                    Whitespace@2..3 " "
                    Plus@3..4 "+"
                    Newline@4..5 "\n"
                    ExpressionLiteral@5..6
                      Number@5..6 "1"
                  Newline@6..7 "\n"
                  Plus@7..8 "+"
                  Whitespace@8..9 " "
                  ExpressionLiteral@9..10
                    Number@9..10 "1""#]],
        );
    }

    #[test]
    fn infix_expression_interspersed_with_blocks_and_comments() {
        // Happy path
        check(
            "
1 +
  1 # Add one
  + 10 # Add ten",
            expect![[r##"
                ExpressionBinary@0..35
                  Newline@0..1 "\n"
                  ExpressionLiteral@1..2
                    Number@1..2 "1"
                  Whitespace@2..3 " "
                  Plus@3..4 "+"
                  Newline@4..5 "\n"
                  ExpressionBlock@5..35
                    Indent@5..7 "  "
                    ExpressionBinary@7..25
                      ExpressionLiteral@7..8
                        Number@7..8 "1"
                      Whitespace@8..9 " "
                      Comment@9..18 "# Add one"
                      Newline@18..19 "\n"
                      Whitespace@19..21 "  "
                      Plus@21..22 "+"
                      Whitespace@22..23 " "
                      ExpressionLiteral@23..25
                        Number@23..25 "10"
                    Whitespace@25..26 " "
                    Comment@26..35 "# Add ten"
                    Dedent@35..35 """##]],
        );
    }

    #[test]
    fn do_not_operator_if_getting_rhs_failed() {
        // Unhappy path
        check(
            "(1+",
            expect![[r#"
                ExpressionTuple@0..3
                  ParenOpen@0..1 "("
                  ExpressionBinary@1..3
                    ExpressionLiteral@1..2
                      Number@1..2 "1"
                    Plus@2..3 "+"
                    Missing@3..3
                  Missing@3..3
                error at 3: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
                error at 3: missing ‘)’"#]],
        );
    }

    #[test]
    fn negation() {
        // Happy path
        check(
            "-10",
            expect![[r#"
                ExpressionUnary@0..3
                  Minus@0..1 "-"
                  ExpressionLiteral@1..3
                    Number@1..3 "10""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_binary_operators() {
        // Happy path
        check(
            "-20+20",
            expect![[r#"
                ExpressionBinary@0..6
                  ExpressionUnary@0..3
                    Minus@0..1 "-"
                    ExpressionLiteral@1..3
                      Number@1..3 "20"
                  Plus@3..4 "+"
                  ExpressionLiteral@4..6
                    Number@4..6 "20""#]],
        );
    }

    #[test]
    fn nested_parentheses() {
        // Happy path
        check(
            "((((((10))))))",
            expect![[r#"
                ExpressionTuple@0..14
                  ParenOpen@0..1 "("
                  ExpressionTuple@1..13
                    ParenOpen@1..2 "("
                    ExpressionTuple@2..12
                      ParenOpen@2..3 "("
                      ExpressionTuple@3..11
                        ParenOpen@3..4 "("
                        ExpressionTuple@4..10
                          ParenOpen@4..5 "("
                          ExpressionTuple@5..9
                            ParenOpen@5..6 "("
                            ExpressionLiteral@6..8
                              Number@6..8 "10"
                            ParenClose@8..9 ")"
                          ParenClose@9..10 ")"
                        ParenClose@10..11 ")"
                      ParenClose@11..12 ")"
                    ParenClose@12..13 ")"
                  ParenClose@13..14 ")""#]],
        );
    }

    #[test]
    fn parentheses_affect_precedence() {
        // Happy path
        check(
            "5*(2+1)",
            expect![[r#"
                ExpressionBinary@0..7
                  ExpressionLiteral@0..1
                    Number@0..1 "5"
                  Multiply@1..2 "*"
                  ExpressionTuple@2..7
                    ParenOpen@2..3 "("
                    ExpressionBinary@3..6
                      ExpressionLiteral@3..4
                        Number@3..4 "2"
                      Plus@4..5 "+"
                      ExpressionLiteral@5..6
                        Number@5..6 "1"
                    ParenClose@6..7 ")""#]],
        );
    }

    #[test]
    fn paren_expression_missing_closing_paren() {
        // Unhappy path
        check(
            "(foo",
            expect![[r#"
                ExpressionTuple@0..4
                  ParenOpen@0..1 "("
                  ExpressionReference@1..4
                    IdentifierValue@1..4 "foo"
                  Missing@4..4
                error at 4: missing ‘)’"#]],
        );
    }

    #[test]
    fn keyword_arg_with_a_colon_in_parens_recovers() {
        // Unhappy path: `name: value` where `name = value` was meant, among
        // positional arguments. Each `:` is one error at the `:`.
        check(
            "f(a: 1, b: 2)",
            expect![[r#"
                ExpressionApply@0..13
                  ExpressionReference@0..1
                    IdentifierValue@0..1 "f"
                  FunctionArgList@1..13
                    ParenOpen@1..2 "("
                    FunctionArgLabelled@2..6
                      FunctionParamLabel@2..3
                        IdentifierValue@2..3 "a"
                      Error@3..4
                        Colon@3..4 ":"
                      Whitespace@4..5 " "
                      ExpressionLiteral@5..6
                        Number@5..6 "1"
                    Comma@6..7 ","
                    Whitespace@7..8 " "
                    FunctionArgLabelled@8..12
                      FunctionParamLabel@8..9
                        IdentifierValue@8..9 "b"
                      Error@9..10
                        Colon@9..10 ":"
                      Whitespace@10..11 " "
                      ExpressionLiteral@11..12
                        Number@11..12 "2"
                    ParenClose@12..13 ")"
                error at 3..4: expected ‘=’, but found ‘:’
                error at 9..10: expected ‘=’, but found ‘:’"#]],
        );
    }

    #[test]
    fn stray_token_between_paren_args_is_one_error_each() {
        // Unhappy path: a stray token after an argument in `( )` is one
        // error and the list goes on.
        check(
            "f(1 : 2, 3)",
            expect![[r#"
                ExpressionApply@0..11
                  ExpressionReference@0..1
                    IdentifierValue@0..1 "f"
                  FunctionArgList@1..11
                    ParenOpen@1..2 "("
                    FunctionArgPositional@2..3
                      ExpressionLiteral@2..3
                        Number@2..3 "1"
                    Whitespace@3..4 " "
                    Error@4..5
                      Colon@4..5 ":"
                    Whitespace@5..6 " "
                    FunctionArgPositional@6..7
                      ExpressionLiteral@6..7
                        Number@6..7 "2"
                    Comma@7..8 ","
                    Whitespace@8..9 " "
                    FunctionArgPositional@9..10
                      ExpressionLiteral@9..10
                        Number@9..10 "3"
                    ParenClose@10..11 ")"
                error at 4..5: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘,’, or ‘)’, but found ‘:’"#]],
        );
    }

    #[test]
    fn stray_token_between_brace_args_is_one_error_each() {
        // Unhappy path: a stray token after an argument in `{ }` is one
        // error and the list goes on.
        check(
            "f { a = 1 : b = 2 }",
            expect![[r#"
                ExpressionApply@0..19
                  ExpressionReference@0..1
                    IdentifierValue@0..1 "f"
                  Whitespace@1..2 " "
                  FunctionArgList@2..19
                    BraceOpen@2..3 "{"
                    Whitespace@3..4 " "
                    FunctionArgLabelled@4..9
                      FunctionParamLabel@4..5
                        IdentifierValue@4..5 "a"
                      Whitespace@5..6 " "
                      Equal@6..7 "="
                      Whitespace@7..8 " "
                      ExpressionLiteral@8..9
                        Number@8..9 "1"
                    Whitespace@9..10 " "
                    Error@10..11
                      Colon@10..11 ":"
                    Whitespace@11..12 " "
                    FunctionArgLabelled@12..17
                      FunctionParamLabel@12..13
                        IdentifierValue@12..13 "b"
                      Whitespace@13..14 " "
                      Equal@14..15 "="
                      Whitespace@15..16 " "
                      ExpressionLiteral@16..17
                        Number@16..17 "2"
                    Whitespace@17..18 " "
                    BraceClose@18..19 "}"
                error at 10..11: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘,’, or ‘}’, but found ‘:’"#]],
        );
    }

    #[test]
    fn call_expression_missing_closing_paren() {
        // Unhappy path: call expression with a missing closing ')'
        check(
            "foo(",
            expect![[r#"
                ExpressionApply@0..4
                  ExpressionReference@0..3
                    IdentifierValue@0..3 "foo"
                  FunctionArgList@3..4
                    ParenOpen@3..4 "("
                    FunctionArgPositional@4..4
                      Missing@4..4
                    Missing@4..4
                error at 4: missing ‘)’, value-id, ‘self’, ‘...’, ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
                error at 4: missing ‘)’"#]],
        );
    }

    #[test]
    fn get_expression_named_from() {
        // `from` is a keyword, and a field name after `.`, as it is a
        // function name.
        check(
            "converter.from",
            expect![[r#"
                ExpressionGet@0..14
                  ExpressionReference@0..9
                    IdentifierValue@0..9 "converter"
                  Dot@9..10 "."
                  From@10..14 "from""#]],
        );
    }

    #[test]
    fn get_expression_missing_identifier() {
        // Unhappy path: get expression missing the identifier after the dot.
        check(
            "foo.",
            expect![[r#"
                ExpressionGet@0..4
                  ExpressionReference@0..3
                    IdentifierValue@0..3 "foo"
                  Dot@3..4 "."
                  Missing@4..4
                error at 4: missing value-id or ‘from’"#]],
        );
    }

    #[test]
    fn value_segment_after_a_type_path() {
        // `N.default()`: the type path ends at `N`, and `.default` is a
        // field get on it, so the call reaches the associated function.
        check(
            "N.default()",
            expect![[r#"
                ExpressionApply@0..11
                  ExpressionGet@0..9
                    TypeReference@0..1
                      IdentifierType@0..1 "N"
                    Dot@1..2 "."
                    IdentifierValue@2..9 "default"
                  FunctionArgList@9..11
                    ParenOpen@9..10 "("
                    ParenClose@10..11 ")""#]],
        );
    }

    #[test]
    fn value_segment_after_self_type() {
        // `Self` starts a type path as a type name does.
        check(
            "Self.regular()",
            expect![[r#"
                ExpressionApply@0..14
                  ExpressionGet@0..12
                    TypeReference@0..4
                      SelfUpper@0..4 "Self"
                    Dot@4..5 "."
                    IdentifierValue@5..12 "regular"
                  FunctionArgList@12..14
                    ParenOpen@12..13 "("
                    ParenClose@13..14 ")""#]],
        );
    }

    #[test]
    fn value_segment_named_from() {
        // `from` is a keyword, and a value segment after a type path, as
        // it is a field name, so `Length.from(5)` reaches the `From`
        // method.
        check(
            "Length.from(5)",
            expect![[r#"
                ExpressionApply@0..14
                  ExpressionGet@0..11
                    TypeReference@0..6
                      IdentifierType@0..6 "Length"
                    Dot@6..7 "."
                    From@7..11 "from"
                  FunctionArgList@11..14
                    ParenOpen@11..12 "("
                    FunctionArgPositional@12..13
                      ExpressionLiteral@12..13
                        Number@12..13 "5"
                    ParenClose@13..14 ")""#]],
        );
    }

    #[test]
    fn value_segment_after_a_generic_type() {
        // The path may end in a generic argument list before the value
        // segment, as `3d-object.kitty` writes it.
        check(
            "Vector3[Length].default()",
            expect![[r#"
                ExpressionApply@0..25
                  ExpressionGet@0..23
                    TypeGeneric@0..15
                      TypeReference@0..7
                        IdentifierType@0..7 "Vector3"
                      GenericArgList@7..15
                        BracketOpen@7..8 "["
                        GenericArgPositional@8..14
                          TypeReference@8..14
                            IdentifierType@8..14 "Length"
                        BracketClose@14..15 "]"
                    Dot@15..16 "."
                    IdentifierValue@16..23 "default"
                  FunctionArgList@23..25
                    ParenOpen@23..24 "("
                    ParenClose@24..25 ")""#]],
        );
    }

    #[test]
    fn type_segment_then_a_value_segment() {
        // `Type.Assoc` is the type path; `.value` ends it.
        check(
            "Type.Assoc.value",
            expect![[r#"
                ExpressionGet@0..16
                  TypeAssociation@0..10
                    TypeReference@0..4
                      IdentifierType@0..4 "Type"
                    Dot@4..5 "."
                    IdentifierType@5..10 "Assoc"
                  Dot@10..11 "."
                  IdentifierValue@11..16 "value""#]],
        );
    }

    #[test]
    fn two_type_segments_stay_a_type_path() {
        // `GridBeam.Z` is one type path: a type name after `.` continues
        // it, so the call applies the path.
        check(
            "GridBeam.Z(x = 0)",
            expect![[r#"
                ExpressionApply@0..17
                  TypeAssociation@0..10
                    TypeReference@0..8
                      IdentifierType@0..8 "GridBeam"
                    Dot@8..9 "."
                    IdentifierType@9..10 "Z"
                  FunctionArgList@10..17
                    ParenOpen@10..11 "("
                    FunctionArgLabelled@11..16
                      FunctionParamLabel@11..12
                        IdentifierValue@11..12 "x"
                      Whitespace@12..13 " "
                      Equal@13..14 "="
                      Whitespace@14..15 " "
                      ExpressionLiteral@15..16
                        Number@15..16 "0"
                    ParenClose@16..17 ")""#]],
        );
    }

    #[test]
    fn type_path_with_a_dot_and_nothing_after_it_recovers() {
        // Unhappy path: the `.` may be a value or a type segment; the
        // expression rule takes it and reports the value segment.
        check(
            "N.",
            expect![[r#"
                ExpressionGet@0..2
                  TypeReference@0..1
                    IdentifierType@0..1 "N"
                  Dot@1..2 "."
                  Missing@2..2
                error at 2: missing value-id or ‘from’"#]],
        );
    }

    #[test]
    fn let_expression_missing_identifier() {
        // Unhappy path: let expression missing an identifier after the 'let' keyword.
        check(
            "let = 1 in 2",
            expect![[r#"
                ExpressionLet@0..12
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  Missing@4..4
                  Equal@4..5 "="
                  Whitespace@5..6 " "
                  ExpressionLiteral@6..7
                    Number@6..7 "1"
                  Whitespace@7..8 " "
                  In@8..10 "in"
                  Whitespace@10..11 " "
                  ExpressionLiteral@11..12
                    Number@11..12 "2"
                error at 4: missing value-id, _, number, string, ‘(’, type-id, or ‘Self’"#]],
        );
    }

    #[test]
    fn let_expression_missing_equal() {
        // Unhappy path: let expression missing the '=' token.
        check(
            "let x 1 in 2",
            expect![[r#"
                ExpressionLet@0..12
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  PatternName@4..5
                    IdentifierValue@4..5 "x"
                  Whitespace@5..6 " "
                  Error@6..7
                    Number@6..7 "1"
                  Whitespace@7..8 " "
                  Error@8..10
                    In@8..10 "in"
                  Whitespace@10..11 " "
                  Error@11..12
                    Number@11..12 "2"
                  Missing@12..12
                error at 6..7: expected ‘=’, but found number
                error at 8..10: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘in’
                error at 11..12: expected ‘in’, but found number
                error at 12: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn let_expression_missing_expression() {
        // Unhappy path: let expression missing an expression after the '='.
        check(
            "let x =  in 2",
            expect![[r#"
                ExpressionLet@0..13
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  PatternName@4..5
                    IdentifierValue@4..5 "x"
                  Whitespace@5..6 " "
                  Equal@6..7 "="
                  Whitespace@7..9 "  "
                  Error@9..11
                    In@9..11 "in"
                  Whitespace@11..12 " "
                  Error@12..13
                    Number@12..13 "2"
                  Missing@13..13
                error at 9..11: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘in’
                error at 12..13: expected ‘in’, but found number
                error at 13: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn let_expression_missing_in() {
        // Unhappy path: let expression missing the 'in' keyword.
        check(
            "let x = 1 2",
            expect![[r#"
                ExpressionLet@0..11
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  PatternName@4..5
                    IdentifierValue@4..5 "x"
                  Whitespace@5..6 " "
                  Equal@6..7 "="
                  Whitespace@7..8 " "
                  ExpressionLiteral@8..9
                    Number@8..9 "1"
                  Whitespace@9..10 " "
                  Error@10..11
                    Number@10..11 "2"
                  Missing@11..11
                error at 10..11: expected ‘in’, but found number
                error at 11: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn if_expression_missing_condition() {
        // Unhappy path: if expression missing the condition between 'if' and 'then'
        check(
            "if then 1 else 2",
            expect![[r#"
                ExpressionIf@0..16
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  Missing@3..3
                  Then@3..7 "then"
                  Whitespace@7..8 " "
                  ExpressionLiteral@8..9
                    Number@8..9 "1"
                  Whitespace@9..10 " "
                  Else@10..14 "else"
                  Whitespace@14..15 " "
                  ExpressionLiteral@15..16
                    Number@15..16 "2"
                error at 3: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn if_expression_missing_then_body() {
        // Unhappy path: if expression missing the then–body (and there is no else branch).
        check(
            "if 1 then",
            expect![[r#"
                ExpressionIf@0..9
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  ExpressionLiteral@3..4
                    Number@3..4 "1"
                  Whitespace@4..5 " "
                  Then@5..9 "then"
                  Missing@9..9
                error at 9: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn call_expression_trailing_comma() {
        // Unhappy path: call expression has a trailing comma with no expression following it.
        check(
            "foo(1,)",
            expect![[r#"
                ExpressionApply@0..7
                  ExpressionReference@0..3
                    IdentifierValue@0..3 "foo"
                  FunctionArgList@3..7
                    ParenOpen@3..4 "("
                    FunctionArgPositional@4..5
                      ExpressionLiteral@4..5
                        Number@4..5 "1"
                    Comma@5..6 ","
                    FunctionArgPositional@6..6
                      Missing@6..6
                    ParenClose@6..7 ")"
                error at 6: missing value-id, ‘self’, ‘...’, ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    /*
    #[test]
    fn invalid_token() {
        // "@" is not a valid token to start an expression.
        check(
            "@",
            expect![[r#"
            error at 0..1: expected ‘+’, ‘-’, ‘not’, identifier, boolean, number, string, ‘(’, indent, ‘let’ or ‘if’
        "#]],
        );
    }
    */

    #[test]
    fn if_expression_with_nested_error() {
        // Unhappy path: if expression contains a binary expression error inside the if–condition.
        check(
            "if (1+) then 2",
            expect![[r#"
                ExpressionIf@0..14
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  ExpressionTuple@3..7
                    ParenOpen@3..4 "("
                    ExpressionBinary@4..6
                      ExpressionLiteral@4..5
                        Number@4..5 "1"
                      Plus@5..6 "+"
                      Missing@6..6
                    ParenClose@6..7 ")"
                  Whitespace@7..8 " "
                  Then@8..12 "then"
                  Whitespace@12..13 " "
                  ExpressionLiteral@13..14
                    Number@13..14 "2"
                error at 6: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn let_expression_type() {
        // Happy path
        check(
            "let x: Number = 10 in x + 20",
            expect![[r#"
                ExpressionLet@0..28
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  PatternName@4..5
                    IdentifierValue@4..5 "x"
                  Colon@5..6 ":"
                  Whitespace@6..7 " "
                  TypeReference@7..13
                    IdentifierType@7..13 "Number"
                  Whitespace@13..14 " "
                  Equal@14..15 "="
                  Whitespace@15..16 " "
                  ExpressionLiteral@16..18
                    Number@16..18 "10"
                  Whitespace@18..19 " "
                  In@19..21 "in"
                  Whitespace@21..22 " "
                  ExpressionBinary@22..28
                    ExpressionReference@22..23
                      IdentifierValue@22..23 "x"
                    Whitespace@23..24 " "
                    Plus@24..25 "+"
                    Whitespace@25..26 " "
                    ExpressionLiteral@26..28
                      Number@26..28 "20""#]],
        );
    }

    #[test]
    fn function_expr() {
        // Happy path
        check(
            indoc! {"
                let add = fn (a: Number, b: Number) => a + b in
                add(10, 20)
            "},
            expect![[r#"
                ExpressionLet@0..60
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  PatternName@4..7
                    IdentifierValue@4..7 "add"
                  Whitespace@7..8 " "
                  Equal@8..9 "="
                  Whitespace@9..10 " "
                  DeclarationFunction@10..44
                    Fn@10..12 "fn"
                    Whitespace@12..13 " "
                    FunctionParamList@13..35
                      ParenOpen@13..14 "("
                      FunctionParam@14..23
                        FunctionParamLabel@14..15
                          IdentifierValue@14..15 "a"
                        Colon@15..16 ":"
                        Whitespace@16..17 " "
                        TypeReference@17..23
                          IdentifierType@17..23 "Number"
                      Comma@23..24 ","
                      Whitespace@24..25 " "
                      FunctionParam@25..34
                        FunctionParamLabel@25..26
                          IdentifierValue@25..26 "b"
                        Colon@26..27 ":"
                        Whitespace@27..28 " "
                        TypeReference@28..34
                          IdentifierType@28..34 "Number"
                      ParenClose@34..35 ")"
                    Whitespace@35..36 " "
                    FatArrow@36..38 "=>"
                    Whitespace@38..39 " "
                    FunctionBody@39..44
                      ExpressionBinary@39..44
                        ExpressionReference@39..40
                          IdentifierValue@39..40 "a"
                        Whitespace@40..41 " "
                        Plus@41..42 "+"
                        Whitespace@42..43 " "
                        ExpressionReference@43..44
                          IdentifierValue@43..44 "b"
                  Whitespace@44..45 " "
                  In@45..47 "in"
                  Newline@47..48 "\n"
                  ExpressionApply@48..59
                    ExpressionReference@48..51
                      IdentifierValue@48..51 "add"
                    FunctionArgList@51..59
                      ParenOpen@51..52 "("
                      FunctionArgPositional@52..54
                        ExpressionLiteral@52..54
                          Number@52..54 "10"
                      Comma@54..55 ","
                      Whitespace@55..56 " "
                      FunctionArgPositional@56..58
                        ExpressionLiteral@56..58
                          Number@56..58 "20"
                      ParenClose@58..59 ")"
                  Newline@59..60 "\n""#]],
        );
    }

    #[test]
    fn expression_match() {
        // Happy path
        check(
            indoc! {"
                match maybe_thing
                    Some(thing) => ()
                    None => ()
            "},
            expect![[r#"
                ExpressionMatch@0..55
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  ExpressionReference@6..17
                    IdentifierValue@6..17 "maybe_thing"
                  Newline@17..18 "\n"
                  Indent@18..22 "    "
                  MatchArm@22..39
                    PatternType@22..33
                      TypeReference@22..26
                        IdentifierType@22..26 "Some"
                      PatternTypeArgList@26..33
                        ParenOpen@26..27 "("
                        PatternTypeArgPositional@27..32
                          IdentifierValue@27..32 "thing"
                        ParenClose@32..33 ")"
                    Whitespace@33..34 " "
                    FatArrow@34..36 "=>"
                    Whitespace@36..37 " "
                    ExpressionTuple@37..39
                      ParenOpen@37..38 "("
                      ParenClose@38..39 ")"
                  Newline@39..40 "\n"
                  Whitespace@40..44 "    "
                  MatchArm@44..54
                    PatternType@44..48
                      TypeReference@44..48
                        IdentifierType@44..48 "None"
                    Whitespace@48..49 " "
                    FatArrow@49..51 "=>"
                    Whitespace@51..52 " "
                    ExpressionTuple@52..54
                      ParenOpen@52..53 "("
                      ParenClose@53..54 ")"
                  Newline@54..55 "\n"
                  Dedent@55..55 """#]],
        );
    }

    #[test]
    fn keyword_args_in_parens() {
        check(
            "f(x = 1)",
            expect![[r#"
                ExpressionApply@0..8
                  ExpressionReference@0..1
                    IdentifierValue@0..1 "f"
                  FunctionArgList@1..8
                    ParenOpen@1..2 "("
                    FunctionArgLabelled@2..7
                      FunctionParamLabel@2..3
                        IdentifierValue@2..3 "x"
                      Whitespace@3..4 " "
                      Equal@4..5 "="
                      Whitespace@5..6 " "
                      ExpressionLiteral@6..7
                        Number@6..7 "1"
                    ParenClose@7..8 ")""#]],
        );
    }

    #[test]
    fn keyword_args_in_braces() {
        check(
            "Self { x = 1 }",
            expect![[r#"
                ExpressionApply@0..14
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Whitespace@4..5 " "
                  FunctionArgList@5..14
                    BraceOpen@5..6 "{"
                    Whitespace@6..7 " "
                    FunctionArgLabelled@7..12
                      FunctionParamLabel@7..8
                        IdentifierValue@7..8 "x"
                      Whitespace@8..9 " "
                      Equal@9..10 "="
                      Whitespace@10..11 " "
                      ExpressionLiteral@11..12
                        Number@11..12 "1"
                    Whitespace@12..13 " "
                    BraceClose@13..14 "}""#]],
        );
    }

    #[test]
    fn keyword_args_in_a_block() {
        check(
            indoc! {"
                Self
                  x = 1
                  y = 2
            "},
            expect![[r#"
                ExpressionApply@0..21
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Newline@4..5 "\n"
                  FunctionArgList@5..21
                    Indent@5..7 "  "
                    FunctionArgLabelled@7..12
                      FunctionParamLabel@7..8
                        IdentifierValue@7..8 "x"
                      Whitespace@8..9 " "
                      Equal@9..10 "="
                      Whitespace@10..11 " "
                      ExpressionLiteral@11..12
                        Number@11..12 "1"
                    Newline@12..13 "\n"
                    Whitespace@13..15 "  "
                    FunctionArgLabelled@15..20
                      FunctionParamLabel@15..16
                        IdentifierValue@15..16 "y"
                      Whitespace@16..17 " "
                      Equal@17..18 "="
                      Whitespace@18..19 " "
                      ExpressionLiteral@19..20
                        Number@19..20 "2"
                    Newline@20..21 "\n"
                    Dedent@21..21 """#]],
        );
    }

    #[test]
    fn spread_among_keyword_args() {
        check(
            "Self { ...self, x = 1 }",
            expect![[r#"
                ExpressionApply@0..23
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Whitespace@4..5 " "
                  FunctionArgList@5..23
                    BraceOpen@5..6 "{"
                    Whitespace@6..7 " "
                    FunctionArgSpread@7..14
                      Ellipses@7..10 "..."
                      ExpressionReference@10..14
                        SelfLower@10..14 "self"
                    Comma@14..15 ","
                    Whitespace@15..16 " "
                    FunctionArgLabelled@16..21
                      FunctionParamLabel@16..17
                        IdentifierValue@16..17 "x"
                      Whitespace@17..18 " "
                      Equal@18..19 "="
                      Whitespace@19..20 " "
                      ExpressionLiteral@20..21
                        Number@20..21 "1"
                    Whitespace@21..22 " "
                    BraceClose@22..23 "}""#]],
        );
    }

    #[test]
    fn spread_in_a_block() {
        check(
            indoc! {"
                Self
                  ...self
                  x = 1
            "},
            expect![[r#"
                ExpressionApply@0..23
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Newline@4..5 "\n"
                  FunctionArgList@5..23
                    Indent@5..7 "  "
                    FunctionArgSpread@7..14
                      Ellipses@7..10 "..."
                      ExpressionReference@10..14
                        SelfLower@10..14 "self"
                    Newline@14..15 "\n"
                    Whitespace@15..17 "  "
                    FunctionArgLabelled@17..22
                      FunctionParamLabel@17..18
                        IdentifierValue@17..18 "x"
                      Whitespace@18..19 " "
                      Equal@19..20 "="
                      Whitespace@20..21 " "
                      ExpressionLiteral@21..22
                        Number@21..22 "1"
                    Newline@22..23 "\n"
                    Dedent@23..23 """#]],
        );
    }

    #[test]
    fn keyword_arg_with_a_colon_recovers() {
        check(
            "Self { x: 1 }",
            expect![[r#"
                ExpressionApply@0..13
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Whitespace@4..5 " "
                  FunctionArgList@5..13
                    BraceOpen@5..6 "{"
                    Whitespace@6..7 " "
                    FunctionArgLabelled@7..11
                      FunctionParamLabel@7..8
                        IdentifierValue@7..8 "x"
                      Error@8..9
                        Colon@8..9 ":"
                      Whitespace@9..10 " "
                      ExpressionLiteral@10..11
                        Number@10..11 "1"
                    Whitespace@11..12 " "
                    BraceClose@12..13 "}"
                error at 8..9: expected ‘=’, but found ‘:’"#]],
        );
    }

    #[test]
    fn positional_line_in_a_block_is_an_error() {
        check(
            indoc! {"
                Parts
                  Beam.Z(1)
                  x = 1
            "},
            expect![[r#"
                ExpressionApply@0..26
                  TypeReference@0..5
                    IdentifierType@0..5 "Parts"
                  Newline@5..6 "\n"
                  FunctionArgList@6..26
                    Indent@6..8 "  "
                    FunctionArgPositional@8..17
                      ExpressionApply@8..17
                        TypeAssociation@8..14
                          TypeReference@8..12
                            IdentifierType@8..12 "Beam"
                          Dot@12..13 "."
                          IdentifierType@13..14 "Z"
                        FunctionArgList@14..17
                          ParenOpen@14..15 "("
                          FunctionArgPositional@15..16
                            ExpressionLiteral@15..16
                              Number@15..16 "1"
                          ParenClose@16..17 ")"
                    Newline@17..18 "\n"
                    Whitespace@18..20 "  "
                    FunctionArgLabelled@20..25
                      FunctionParamLabel@20..21
                        IdentifierValue@20..21 "x"
                      Whitespace@21..22 " "
                      Equal@22..23 "="
                      Whitespace@23..24 " "
                      ExpressionLiteral@24..25
                        Number@24..25 "1"
                    Newline@25..26 "\n"
                    Dedent@26..26 ""
                error at 8..12: expected value-id, ‘self’, or ‘...’, but found type-id"#]],
        );
    }

    #[test]
    fn keyword_arg_without_a_value_is_missing() {
        check(
            "f(x = )",
            expect![[r#"
                ExpressionApply@0..7
                  ExpressionReference@0..1
                    IdentifierValue@0..1 "f"
                  FunctionArgList@1..7
                    ParenOpen@1..2 "("
                    FunctionArgLabelled@2..6
                      FunctionParamLabel@2..3
                        IdentifierValue@2..3 "x"
                      Whitespace@3..4 " "
                      Equal@4..5 "="
                      Whitespace@5..6 " "
                      Missing@6..6
                    ParenClose@6..7 ")"
                error at 6: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }

    #[test]
    fn positional_arg_in_braces_is_an_error() {
        check(
            "Self { 1, x = 2 }",
            expect![[r#"
                ExpressionApply@0..17
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Whitespace@4..5 " "
                  FunctionArgList@5..17
                    BraceOpen@5..6 "{"
                    Whitespace@6..7 " "
                    FunctionArgPositional@7..8
                      ExpressionLiteral@7..8
                        Number@7..8 "1"
                    Comma@8..9 ","
                    Whitespace@9..10 " "
                    FunctionArgLabelled@10..15
                      FunctionParamLabel@10..11
                        IdentifierValue@10..11 "x"
                      Whitespace@11..12 " "
                      Equal@12..13 "="
                      Whitespace@13..14 " "
                      ExpressionLiteral@14..15
                        Number@14..15 "2"
                    Whitespace@15..16 " "
                    BraceClose@16..17 "}"
                error at 7..8: expected ‘}’, value-id, ‘self’, or ‘...’, but found number"#]],
        );
    }

    #[test]
    fn stray_token_in_a_block_is_one_error() {
        check(
            indoc! {"
                Self
                  , x = 1
            "},
            expect![[r#"
                ExpressionApply@0..15
                  TypeReference@0..4
                    SelfUpper@0..4 "Self"
                  Newline@4..5 "\n"
                  FunctionArgList@5..15
                    Indent@5..7 "  "
                    FunctionArgLabelled@7..8
                      Error@7..8
                        Comma@7..8 ","
                    Whitespace@8..9 " "
                    FunctionArgLabelled@9..14
                      FunctionParamLabel@9..10
                        IdentifierValue@9..10 "x"
                      Whitespace@10..11 " "
                      Equal@11..12 "="
                      Whitespace@12..13 " "
                      ExpressionLiteral@13..14
                        Number@13..14 "1"
                    Newline@14..15 "\n"
                    Dedent@15..15 ""
                error at 7..8: expected value-id, ‘self’, or ‘...’, but found ‘,’"#]],
        );
    }

    #[test]
    fn let_pattern_with_a_renamed_field() {
        // `let` recovers at `=`, which must not read the rename as a
        // shorthand field.
        check(
            "let Self { x = a } = self in a",
            expect![[r#"
                ExpressionLet@0..30
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  PatternType@4..18
                    TypeReference@4..8
                      SelfUpper@4..8 "Self"
                    Whitespace@8..9 " "
                    PatternTypeArgList@9..18
                      BraceOpen@9..10 "{"
                      Whitespace@10..11 " "
                      PatternTypeArgLabelled@11..16
                        IdentifierValue@11..12 "x"
                        Whitespace@12..13 " "
                        Equal@13..14 "="
                        Whitespace@14..15 " "
                        IdentifierValue@15..16 "a"
                      Whitespace@16..17 " "
                      BraceClose@17..18 "}"
                  Whitespace@18..19 " "
                  Equal@19..20 "="
                  Whitespace@20..21 " "
                  ExpressionReference@21..25
                    SelfLower@21..25 "self"
                  Whitespace@25..26 " "
                  In@26..28 "in"
                  Whitespace@28..29 " "
                  ExpressionReference@29..30
                    IdentifierValue@29..30 "a""#]],
        );
    }

    #[test]
    fn match_scrutinee_ends_before_the_arms() {
        check(
            indoc! {"
                match a + b
                  _ => 1
            "},
            expect![[r#"
                ExpressionMatch@0..21
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  ExpressionBinary@6..11
                    ExpressionReference@6..7
                      IdentifierValue@6..7 "a"
                    Whitespace@7..8 " "
                    Plus@8..9 "+"
                    Whitespace@9..10 " "
                    ExpressionReference@10..11
                      IdentifierValue@10..11 "b"
                  Newline@11..12 "\n"
                  Indent@12..14 "  "
                  MatchArm@14..20
                    PatternWildcard@14..15
                      Underscore@14..15 "_"
                    Whitespace@15..16 " "
                    FatArrow@16..18 "=>"
                    Whitespace@18..19 " "
                    ExpressionLiteral@19..20
                      Number@19..20 "1"
                  Newline@20..21 "\n"
                  Dedent@21..21 """#]],
        );
    }

    #[test]
    fn match_scrutinee_tail_ends_before_the_arms() {
        check(
            indoc! {"
                match if a then b else c
                  _ => 1
            "},
            expect![[r#"
                ExpressionMatch@0..34
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  ExpressionIf@6..24
                    If@6..8 "if"
                    Whitespace@8..9 " "
                    ExpressionReference@9..10
                      IdentifierValue@9..10 "a"
                    Whitespace@10..11 " "
                    Then@11..15 "then"
                    Whitespace@15..16 " "
                    ExpressionReference@16..17
                      IdentifierValue@16..17 "b"
                    Whitespace@17..18 " "
                    Else@18..22 "else"
                    Whitespace@22..23 " "
                    ExpressionReference@23..24
                      IdentifierValue@23..24 "c"
                  Newline@24..25 "\n"
                  Indent@25..27 "  "
                  MatchArm@27..33
                    PatternWildcard@27..28
                      Underscore@27..28 "_"
                    Whitespace@28..29 " "
                    FatArrow@29..31 "=>"
                    Whitespace@31..32 " "
                    ExpressionLiteral@32..33
                      Number@32..33 "1"
                  Newline@33..34 "\n"
                  Dedent@34..34 """#]],
        );
    }

    #[test]
    fn match_with_no_arms_ends_at_the_input() {
        check(
            "match x",
            expect![[r#"
                ExpressionMatch@0..7
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  ExpressionReference@6..7
                    IdentifierValue@6..7 "x"
                  Missing@7..7
                  Missing@7..7
                error at 7: missing indent
                error at 7: missing dedent"#]],
        );
    }
}
