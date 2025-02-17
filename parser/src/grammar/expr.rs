use kitty_syntax::{NodeKind, TokenKind};

use super::*;
use crate::{marker::CompletedMarker, token_set::TokenSet};

/// Entry point: parse an expression.
#[allow(dead_code)]
pub(crate) fn parse_expr(p: &mut Parser) -> Option<CompletedMarker> {
    parse_expr_with_bp(p, 0)
}

/// Parse an expression with a given right binding power.
fn parse_expr_with_bp(p: &mut Parser, min_bp: u8) -> Option<CompletedMarker> {
    // First, parse a left-hand side (unary operator or primary) expression.
    let mut lhs = parse_lhs(p)?;

    loop {
        // Check for postfix operators inline.
        if p.at(TokenKind::ParenOpen) {
            lhs = parse_call_expr(p, lhs);
            continue;
        }
        if p.at(TokenKind::Dot) {
            lhs = parse_get_expr(p, lhs);
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
        let rhs = parse_expr_with_bp(p, right_bp);
        lhs = m.complete(p, NodeKind::BinaryExpr);

        if rhs.is_none() {
            break;
        }
    }

    Some(lhs)
}

/// Parse a left-hand side expression, which may be a unary operator or a primary.
fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    if let Some(bp) = unary_binding_power(p) {
        // A unary operator is present.
        let m = p.start();
        p.bump(); // Consume the unary operator token.
        parse_expr_with_bp(p, bp)?;
        return Some(m.complete(p, NodeKind::UnaryExpr));
    }
    parse_primary(p)
}

/// Parse a primary expression.
fn parse_primary(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Identifier) {
        parse_kind(p, NodeKind::VariableRef)
    } else if p.at(TokenKind::Boolean) {
        parse_kind(p, NodeKind::BooleanLiteral)
    } else if p.at(TokenKind::Number) {
        parse_kind(p, NodeKind::NumberLiteral)
    } else if p.at(TokenKind::String) {
        parse_kind(p, NodeKind::StringLiteral)
    } else if p.at(TokenKind::ParenOpen) {
        parse_paren_expr(p)
    } else if p.at(TokenKind::Indent) {
        parse_block_expr(p)
    } else if p.at(TokenKind::Let) {
        parse_let_expr(p)
    } else if p.at(TokenKind::If) {
        parse_if_expr(p)
    } else {
        p.error();
        return None;
    };
    Some(cm)
}

fn parse_kind(p: &mut Parser, kind: NodeKind) -> CompletedMarker {
    let m = p.start();
    p.bump();
    m.complete(p, kind)
}

/// Parse a call expression given an existing `lhs`.
fn parse_call_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    p.bump(); // Consume '('.
    if !p.at(TokenKind::ParenClose) {
        loop {
            parse_expr(p);
            if !p.at(TokenKind::Comma) {
                break;
            }
            p.bump(); // Consume comma.
        }
    }
    p.expect(TokenKind::ParenClose);
    m.complete(p, NodeKind::CallExpr)
}

/// Parse a field access (get expression) given an existing `lhs`.
fn parse_get_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    p.bump(); // Consume '.'.
    p.expect(TokenKind::Identifier);
    m.complete(p, NodeKind::GetExpr)
}

/// Parse a parenthesized expression (or tuple expression).
fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::ParenOpen);
    parse_expr(p);
    if p.at(TokenKind::Comma) {
        // More than one expression makes it a tuple.
        while p.at(TokenKind::Comma) {
            p.bump();
            parse_expr(p);
        }
        p.expect(TokenKind::ParenClose);
        return m.complete(p, NodeKind::TupleExpr);
    }
    p.expect(TokenKind::ParenClose);
    m.complete(p, NodeKind::ParenExpr)
}

/// Parse a block expression: `{ ... }` but with indents
fn parse_block_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Indent);
    parse_expr(p);
    p.expect(TokenKind::Dedent);
    m.complete(p, NodeKind::BlockExpr)
}

/// Parse a let–expression: `let <identifier> = <expr> in <body>`
fn parse_let_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Let);
    p.expect(TokenKind::Identifier);
    p.expect(TokenKind::Equal);
    // The value of the variable
    parse_expr(p);
    p.expect(TokenKind::In);
    // The body of the let scope
    parse_expr(p);
    m.complete(p, NodeKind::LetExpr)
}

/// Parse an if–expression: `if <cond> { ... } [else { ... }]`
fn parse_if_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::If);
    parse_expr(p); // condition
    p.expect(TokenKind::Then);
    parse_expr(p); // then body
    if p.at(TokenKind::Else) {
        p.bump(); // Consume 'else'.
        parse_expr(p); // else body
    }
    m.complete(p, NodeKind::IfExpr)
}

/// Return the binding power for a unary operator at the current token, if any.
fn unary_binding_power(p: &mut Parser) -> Option<u8> {
    if p.at_set(&TokenSet::new([
        TokenKind::Minus,
        TokenKind::Plus,
        TokenKind::Not,
    ])) {
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
    if p.at_set(&TokenSet::new([
        TokenKind::Multiply,
        TokenKind::Divide,
        TokenKind::Rem,
    ])) {
        Some((13, 14))
    }
    // Additive (Term) operators
    else if p.at_set(&TokenSet::new([TokenKind::Plus, TokenKind::Minus])) {
        Some((11, 12))
    }
    // Bitwise shift
    // Comparison
    else if p.at_set(&TokenSet::new([
        TokenKind::Less,
        TokenKind::LessEqual,
        TokenKind::Greater,
        TokenKind::GreaterEqual,
    ])) {
        Some((9, 10))
    }
    // Equality
    else if p.at_set(&TokenSet::new([TokenKind::EqualEqual, TokenKind::NotEqual])) {
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
    use super::{parse_expr, Parser};
    use crate::check_grammar;
    use expect_test::expect;
    use kitty_cst::Expr;

    fn check(input: &str, expected: expect_test::Expect) {
        let grammar = |p: &mut Parser| {
            parse_expr(p);
        };
        check_grammar::<Expr>(grammar, input, expected);
    }

    #[test]
    fn parse_number() {
        check(
            "123",
            expect![[r#"
                NumberLiteral@0..3
                  Number@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_number_preceded_by_whitespace() {
        check(
            "   9876",
            expect![[r#"
                NumberLiteral@0..7
                  Whitespace@0..3 "   "
                  Number@3..7 "9876""#]],
        );
    }

    #[test]
    fn parse_number_followed_by_whitespace() {
        check(
            "999   ",
            expect![[r#"
                NumberLiteral@0..6
                  Number@0..3 "999"
                  Whitespace@3..6 "   ""#]],
        );
    }

    #[test]
    fn parse_number_surrounded_by_whitespace() {
        check(
            " 123     ",
            expect![[r#"
                NumberLiteral@0..9
                  Whitespace@0..1 " "
                  Number@1..4 "123"
                  Whitespace@4..9 "     ""#]],
        );
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
                VariableRef@0..7
                  Identifier@0..7 "counter""#]],
        );
    }

    #[test]
    fn parse_simple_infix_expression() {
        check(
            "1+2",
            expect![[r#"
                BinaryExpr@0..3
                  NumberLiteral@0..1
                    Number@0..1 "1"
                  Plus@1..2 "+"
                  NumberLiteral@2..3
                    Number@2..3 "2""#]],
        );
    }

    #[test]
    fn parse_left_associative_infix_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
                BinaryExpr@0..7
                  BinaryExpr@0..5
                    BinaryExpr@0..3
                      NumberLiteral@0..1
                        Number@0..1 "1"
                      Plus@1..2 "+"
                      NumberLiteral@2..3
                        Number@2..3 "2"
                    Plus@3..4 "+"
                    NumberLiteral@4..5
                      Number@4..5 "3"
                  Plus@5..6 "+"
                  NumberLiteral@6..7
                    Number@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_infix_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
                BinaryExpr@0..7
                  BinaryExpr@0..5
                    NumberLiteral@0..1
                      Number@0..1 "1"
                    Plus@1..2 "+"
                    BinaryExpr@2..5
                      NumberLiteral@2..3
                        Number@2..3 "2"
                      Multiply@3..4 "*"
                      NumberLiteral@4..5
                        Number@4..5 "3"
                  Minus@5..6 "-"
                  NumberLiteral@6..7
                    Number@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_infix_expression_with_whitespace() {
        check(
            " 1 +   2* 3 ",
            expect![[r#"
                BinaryExpr@0..12
                  Whitespace@0..1 " "
                  NumberLiteral@1..2
                    Number@1..2 "1"
                  Whitespace@2..3 " "
                  Plus@3..4 "+"
                  Whitespace@4..7 "   "
                  BinaryExpr@7..11
                    NumberLiteral@7..8
                      Number@7..8 "2"
                    Multiply@8..9 "*"
                    Whitespace@9..10 " "
                    NumberLiteral@10..11
                      Number@10..11 "3"
                  Whitespace@11..12 " ""#]],
        );
    }

    #[test]
    fn parse_infix_expression_interspersed_with_newlines() {
        check(
            "
1 +
1
+ 1",
            expect![[r#"
                BinaryExpr@0..10
                  Newline@0..1 "\n"
                  BinaryExpr@1..6
                    NumberLiteral@1..2
                      Number@1..2 "1"
                    Whitespace@2..3 " "
                    Plus@3..4 "+"
                    Newline@4..5 "\n"
                    NumberLiteral@5..6
                      Number@5..6 "1"
                  Newline@6..7 "\n"
                  Plus@7..8 "+"
                  Whitespace@8..9 " "
                  NumberLiteral@9..10
                    Number@9..10 "1""#]],
        );
    }

    #[test]
    fn parse_infix_expression_interspersed_with_blocks_and_comments() {
        check(
            "
1 +
  1 # Add one
  + 10 # Add ten",
            expect![[r##"
                BinaryExpr@0..35
                  Newline@0..1 "\n"
                  NumberLiteral@1..2
                    Number@1..2 "1"
                  Whitespace@2..3 " "
                  Plus@3..4 "+"
                  Newline@4..5 "\n"
                  BlockExpr@5..35
                    Indent@5..7 "  "
                    BinaryExpr@7..25
                      NumberLiteral@7..8
                        Number@7..8 "1"
                      Whitespace@8..9 " "
                      Comment@9..18 "# Add one"
                      Newline@18..19 "\n"
                      Whitespace@19..21 "  "
                      Plus@21..22 "+"
                      Whitespace@22..23 " "
                      NumberLiteral@23..25
                        Number@23..25 "10"
                    Whitespace@25..26 " "
                    Comment@26..35 "# Add ten"
                    Dedent@35..35 """##]],
        );
    }

    #[test]
    fn do_not_parse_operator_if_getting_rhs_failed() {
        check(
            "(1+",
            expect![[r#"
                ParenExpr@0..3
                  ParenOpen@0..1 "("
                  BinaryExpr@1..3
                    NumberLiteral@1..2
                      Number@1..2 "1"
                    Plus@2..3 "+"
                error at 2..3: expected ‘+’, ‘-’, ‘not’, identifier, boolean, number, string, ‘(’, indent, ‘let’ or ‘if’
                error at 2..3: expected ‘)’"#]],
        );
    }

    #[test]
    fn parse_negation() {
        check(
            "-10",
            expect![[r#"
                UnaryExpr@0..3
                  Minus@0..1 "-"
                  NumberLiteral@1..3
                    Number@1..3 "10""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_binary_operators() {
        check(
            "-20+20",
            expect![[r#"
                BinaryExpr@0..6
                  UnaryExpr@0..3
                    Minus@0..1 "-"
                    NumberLiteral@1..3
                      Number@1..3 "20"
                  Plus@3..4 "+"
                  NumberLiteral@4..6
                    Number@4..6 "20""#]],
        );
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "((((((10))))))",
            expect![[r#"
                ParenExpr@0..14
                  ParenOpen@0..1 "("
                  ParenExpr@1..13
                    ParenOpen@1..2 "("
                    ParenExpr@2..12
                      ParenOpen@2..3 "("
                      ParenExpr@3..11
                        ParenOpen@3..4 "("
                        ParenExpr@4..10
                          ParenOpen@4..5 "("
                          ParenExpr@5..9
                            ParenOpen@5..6 "("
                            NumberLiteral@6..8
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
        check(
            "5*(2+1)",
            expect![[r#"
                BinaryExpr@0..7
                  NumberLiteral@0..1
                    Number@0..1 "5"
                  Multiply@1..2 "*"
                  ParenExpr@2..7
                    ParenOpen@2..3 "("
                    BinaryExpr@3..6
                      NumberLiteral@3..4
                        Number@3..4 "2"
                      Plus@4..5 "+"
                      NumberLiteral@5..6
                        Number@5..6 "1"
                    ParenClose@6..7 ")""#]],
        );
    }

    #[test]
    fn parse_unclosed_parentheses() {
        check(
            "(foo",
            expect![[r#"
                ParenExpr@0..4
                  ParenOpen@0..1 "("
                  VariableRef@1..4
                    Identifier@1..4 "foo"
                error at 1..4: expected ‘)’"#]],
        );
    }
}
