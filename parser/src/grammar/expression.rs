use kitty_syntax::{NodeKind, TokenKind};

use super::{
    function::{function_arg_list, function_declaration_option_name_body},
    pattern::pattern,
    r#type::{type_annotation, type_path, TYPE_PATH_FIRST},
};
use crate::{marker::CompletedMarker, token_set::TokenSet, Parser};

/// Parse an expression.
#[allow(dead_code)]
pub(crate) fn expression(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    expression_pratt(p, recovery, 0)
}

/// Parse an expression with a given minimum binding power.
/// (Also known as a Pratt parser.)
fn expression_pratt(p: &mut Parser, recovery: TokenSet, min_bp: u8) -> Option<CompletedMarker> {
    // First, parse a left-hand side (unary operator or primary) expression.
    let mut lhs = expression_lhs(p, recovery)?;

    loop {
        // Check for postfix operators inline.
        if p.at(TokenKind::ParenOpen) {
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
        let rhs = expression_pratt(p, recovery, right_bp);
        lhs = m.complete(p, NodeKind::ExpressionBinary);

        if rhs.is_none() {
            break;
        }
    }

    Some(lhs)
}

/// Parse a left-hand side expression, which may be a unary operator or a primary.
fn expression_lhs(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    if let Some(bp) = unary_binding_power(p) {
        // A unary operator is present.
        let m = p.start();
        p.bump(); // Consume the unary operator token.
        expression_pratt(p, recovery, bp)?;
        return Some(m.complete(p, NodeKind::ExpressionUnary));
    }
    expression_primary(p, recovery)
}

/// Parse a primary expression.
fn expression_primary(p: &mut Parser, recovery: TokenSet) -> Option<CompletedMarker> {
    let cm = if p.at_set(EXPRESSION_REFERENCE_FIRST) {
        expression_reference(p)
    } else if p.at_set(TYPE_PATH_FIRST) {
        type_path(p, recovery)?
    } else if p.at_set(LITERAL_FIRST) {
        expression_literal(p)
    } else if p.at(TokenKind::ParenOpen) {
        expression_tuple(p, recovery)
    } else if p.at(TokenKind::Indent) {
        expression_block(p, recovery)
    } else if p.at(TokenKind::Fn) {
        expression_function(p, recovery)
    } else if p.at(TokenKind::Let) {
        expression_let(p, recovery)
    } else if p.at(TokenKind::If) {
        expression_if(p, recovery)
    } else {
        p.error(recovery);
        return None;
    };
    Some(cm)
}

pub(crate) const EXPRESSION_REFERENCE_FIRST: [TokenKind; 2] =
    [TokenKind::IdentifierValue, TokenKind::SelfLower];

fn expression_reference(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_set(EXPRESSION_REFERENCE_FIRST));
    p.mark_kind(NodeKind::ExpressionReference)
}

const LITERAL_FIRST: [TokenKind; 3] = [TokenKind::Boolean, TokenKind::Number, TokenKind::String];

fn expression_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_set(LITERAL_FIRST));
    p.mark_kind(NodeKind::ExpressionLiteral)
}

/// Parse an apply expression given an existing `lhs`.
fn expression_apply(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
    let m = lhs.precede(p);
    function_arg_list(p, recovery);
    m.complete(p, NodeKind::ExpressionApply)
}

/// Parse a field access (get expression) given an existing `lhs`.
fn expression_get(p: &mut Parser, lhs: CompletedMarker, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::Dot));
    let m = lhs.precede(p);
    p.bump(); // Consume '.'.
    p.expect(TokenKind::IdentifierValue, recovery);
    m.complete(p, NodeKind::ExpressionGet)
}

/// Parse a tuple expression.
fn expression_tuple(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::ParenOpen));
    let m = p.start();
    p.bump(); // Consume ')'
    loop {
        expression(p, recovery);
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
    assert!(p.at(TokenKind::Fn));
    function_declaration_option_name_body(p, recovery, false, true)
}

/// Parse a let expression: `let <identifier> = <expr> in <expr>`
fn expression_let(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
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
    expression(p, recovery);
    m.complete(p, NodeKind::ExpressionLet)
}

/// Parse an if–expression: `if <cond> { ... } [else { ... }]`
fn expression_if(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::If, recovery);
    expression(p, recovery.union([TokenKind::Then])); // condition
    p.expect(TokenKind::Then, recovery);
    expression(p, recovery.union([TokenKind::Else])); // then body
    if p.at(TokenKind::Else) {
        p.bump(); // Consume 'else'.
        expression(p, recovery); // else body
    }
    m.complete(p, NodeKind::ExpressionIf)
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
                error at 3: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’
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
                error at 4: missing ‘)’, value-id, ‘self’, ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’
                error at 4: missing ‘)’"#]],
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
                error at 4: missing value-id"#]],
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
                error at 4: missing value-id, _, boolean, number, string, ‘(’, type-id, or ‘Self’"#]],
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
                error at 8..10: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’, but found ‘in’
                error at 11..12: expected ‘in’, but found number
                error at 12: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’"#]],
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
                error at 9..11: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’, but found ‘in’
                error at 12..13: expected ‘in’, but found number
                error at 13: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’"#]],
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
                error at 11: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’"#]],
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
                error at 3: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’"#]],
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
                error at 9: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’"#]],
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
                error at 6: missing value-id, ‘self’, ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’"#]],
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
                  ExpressionTuple@3..8
                    ParenOpen@3..4 "("
                    ExpressionBinary@4..7
                      ExpressionLiteral@4..5
                        Number@4..5 "1"
                      Plus@5..6 "+"
                      Error@6..7
                        ParenClose@6..7 ")"
                    Whitespace@7..8 " "
                    Missing@8..8
                  Then@8..12 "then"
                  Whitespace@12..13 " "
                  ExpressionLiteral@13..14
                    Number@13..14 "2"
                error at 6..7: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, boolean, number, string, ‘(’, indent, ‘fn’, ‘let’, or ‘if’, but found ‘)’
                error at 8: missing ‘)’"#]],
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
}
