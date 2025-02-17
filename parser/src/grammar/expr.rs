use kitty_syntax::{NodeKind, TokenKind};

use super::*;
use crate::marker::CompletedMarker;

/// Entry point: parse an expression.
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
        let parsed_rhs = parse_expr_with_bp(p, right_bp).is_some();
        lhs = m.complete(p, NodeKind::BinaryExpr);

        if !parsed_rhs {
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
    if p.at(TokenKind::Identifier) {
        return parse_kind(p, NodeKind::VariableRef);
    } else if p.at(TokenKind::Boolean) {
        return parse_kind(p, NodeKind::BooleanLiteral);
    } else if p.at(TokenKind::Number) {
        return parse_kind(p, NodeKind::NumberLiteral);
    } else if p.at(TokenKind::String) {
        return parse_kind(p, NodeKind::StringLiteral);
    } else if p.at(TokenKind::ParenOpen) {
        return parse_paren_expr(p);
    } else if p.at(TokenKind::Indent) {
        return parse_block_expr(p);
    } else if p.at(TokenKind::Let) {
        return parse_let_expr(p);
    } else if p.at(TokenKind::If) {
        return parse_if_expr(p);
    }
    p.error();
    None
}

fn parse_kind(p: &mut Parser, kind: NodeKind) -> Option<CompletedMarker> {
    let m = p.start();
    p.bump();
    Some(m.complete(p, kind))
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
fn parse_paren_expr(p: &mut Parser) -> Option<CompletedMarker> {
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
        return Some(m.complete(p, NodeKind::TupleExpr));
    }
    p.expect(TokenKind::ParenClose);
    Some(m.complete(p, NodeKind::ParenExpr))
}

/// Parse a block expression: `{ ... }`
fn parse_block_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();
    p.expect(TokenKind::Indent);
    while !p.at(TokenKind::Dedent) && !p.at_end() {
        parse_expr(p);
    }
    p.expect(TokenKind::Dedent);
    Some(m.complete(p, NodeKind::BlockExpr))
}

/// Parse a let–expression: `let <identifier> = <expr>`
fn parse_let_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();
    p.expect(TokenKind::Let);
    p.expect(TokenKind::Identifier);
    p.expect(TokenKind::Equal);
    // The value of the variable
    parse_expr(p);
    p.expect(TokenKind::Newline);
    // After the let should be an expression, to be the body of the let scope.
    parse_expr(p);
    Some(m.complete(p, NodeKind::LetExpr))
}

/// Parse an if–expression: `if <cond> { ... } [else { ... }]`
fn parse_if_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();
    p.expect(TokenKind::If);
    parse_expr(p); // condition
    parse_block_expr(p); // then–block
    if p.at(TokenKind::Else) {
        p.bump(); // Consume 'else'.
        parse_block_expr(p); // else–block
    }
    Some(m.complete(p, NodeKind::IfExpr))
}

/// Return the binding power for a unary operator at the current token, if any.
fn unary_binding_power(p: &mut Parser) -> Option<u8> {
    if p.at(TokenKind::Minus) || p.at(TokenKind::Plus) || p.at(TokenKind::Not) {
        Some(13)
    } else {
        None
    }
}

/// Return the binding power for a binary operator at the current token, if any.
fn binary_binding_power(p: &mut Parser) -> Option<(u8, u8)> {
    if p.at(TokenKind::Plus) || p.at(TokenKind::Minus) {
        Some((11, 12))
    } else if p.at(TokenKind::Multiply) || p.at(TokenKind::Divide) || p.at(TokenKind::Rem) {
        Some((9, 10))
    } else if p.at(TokenKind::Less)
        || p.at(TokenKind::LessEqual)
        || p.at(TokenKind::Greater)
        || p.at(TokenKind::GreaterEqual)
    {
        Some((7, 8))
    } else if p.at(TokenKind::EqualEqual) || p.at(TokenKind::NotEqual) {
        Some((5, 6))
    } else if p.at(TokenKind::And) {
        Some((3, 4))
    } else if p.at(TokenKind::Or) || p.at(TokenKind::Xor) {
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
                BinaryExpr@0..5
                  BinaryExpr@0..3
                    NumberLiteral@0..1
                      Number@0..1 "1"
                    Plus@1..2 "+"
                    NumberLiteral@2..3
                      Number@2..3 "2"
                  Multiply@3..4 "*"
                  NumberLiteral@4..5
                    Number@4..5 "3""#]],
        );
    }

    #[test]
    fn parse_infix_expression_with_whitespace() {
        check(
            " 1 +   2* 3 ",
            expect![[r#"
                BinaryExpr@0..12
                  Whitespace@0..1 " "
                  BinaryExpr@1..8
                    NumberLiteral@1..2
                      Number@1..2 "1"
                    Whitespace@2..3 " "
                    Plus@3..4 "+"
                    Whitespace@4..7 "   "
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
    fn parse_infix_expression_interspersed_with_comments() {
        check(
            "
1
  + 1 # Add one
  + 10 # Add ten",
            expect![[r#"
                NumberLiteral@0..3
                  Newline@0..1 "\n"
                  Number@1..2 "1"
                  Newline@2..3 "\n""#]],
        );
    }

    #[test]
    fn do_not_parse_operator_if_gettting_rhs_failed() {
        check(
            "(1+",
            expect![[r#"
                ParenExpr@0..3
                  ParenOpen@0..1 "("
                  BinaryExpr@1..3
                    NumberLiteral@1..2
                      Number@1..2 "1"
                    Plus@2..3 "+"ParseError { expected: [Minus, Plus, Not, Identifier, Boolean, Number, String, ParenOpen, Indent, Let, If], found: None, range: 2..3 }
                ParseError { expected: [Minus, Plus, Not, Identifier, Boolean, Number, String, ParenOpen, Indent, Let, If, Comma, ParenClose], found: None, range: 2..3 }
            "#]],
        );
    }

    #[test]
    fn parse_negation() {
        check(
            "-10",
            expect![[r#"
                NumberLiteral@0..3
                  Number@0..3 "-10""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_binary_operators() {
        check(
            "-20+20",
            expect![[r#"
                BinaryExpr@0..6
                  NumberLiteral@0..3
                    Number@0..3 "-20"
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
                    Identifier@1..4 "foo"ParseError { expected: [ParenOpen, Dot, Plus, Minus, Multiply, Divide, Rem, Less, LessEqual, Greater, GreaterEqual, EqualEqual, NotEqual, And, Or, Xor, Comma, ParenClose], found: None, range: 1..4 }
            "#]],
        );
    }
}
