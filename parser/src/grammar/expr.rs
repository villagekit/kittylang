use kitty_syntax::TokenKind;

use crate::{marker::CompletedMarker, token_set::TokenSet};

use super::*;

pub(super) fn parse_expr(
    p: &mut Parser,
    expected_syntax_name: &'static str,
) -> Option<CompletedMarker> {
    parse_expr_bp(p, 0, TokenSet::NONE, expected_syntax_name)
}

fn parse_expr_bp(
    p: &mut Parser,
    minimum_bp: u8,
    recovery_set: TokenSet,
    expected_syntax_name: &'static str,
) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p, recovery_set, expected_syntax_name)?;

    loop {
        lhs = parse_post_operators(p, recovery_set, lhs, false, false);

        let (left_bp, right_bp) = if p.at(TokenKind::Or) {
            (1, 2)
        } else if p.at(TokenKind::And) {
            (3, 4)
        } else if p.at_set(TokenSet::new([TokenKind::EqualEqual, TokenKind::NotEqual])) {
            (5, 6)
        } else if p.at_set(TokenSet::new([TokenKind::Plus, TokenKind::Minus])) {
            (7, 8)
        } else if p.at_set(TokenSet::new([
            TokenKind::Multiply,
            TokenKind::Divide,
            TokenKind::Rem,
        ])) {
            (9, 10)
        } else {
            break;
        };

        if left_bp < minimum_bp {
            break;
        }

        p.bump(); // bump operator

        let m = lhs.precede(p);
        parse_expr_bp(p, right_bp, recovery_set, "operand");
        lhs = m.complete(p, NodeKind::BinaryExpr);
    }

    Some(lhs)
}

fn parse_lhs(
    p: &mut Parser,
    recovery_set: TokenSet,
    expected_syntax_name: &'static str,
) -> Option<CompletedMarker> {
    let _guard = p.expected_syntax_name(expected_syntax_name);

    if p.at(TokenKind::Let) {
        parse_let_expr(p, recovery_set)
    } else if p.at(TokenKind::If) {
        parse_if_expr(p, recovery_set)
    } else if p.at(TokenKind::Match) {
        parse_match_expr(p, recovery_set)
    } else if p.at(TokenKind::Ident) {
        parse_identifier_expr(p)
    } else if p.at_set(TokenSet::new([
        TokenKind::Boolean,
        TokenKind::Number,
        TokenKind::String,
    ])) {
        parse_literal_expr(p)
    } else if p.at(TokenKind::LParen) {
        parse_paren_expr(p, recovery_set)
    } else if p.at(TokenKind::LBrace) {
        parse_block_expr(p, recovery_set)
    } else {
        return p.error_with_recovery_set(recovery_set);
    }
}

// Let Expression
fn parse_let_expr(p: &mut Parser, recovery_set: TokenSet) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Let));

    let m = p.start();
    p.bump(); // "let" keyword

    let name = parse_identifier_expr(p)?;
    p.expect_with_no_skip(TokenKind::Equal);
    let value = parse_expr(p, "expression")?;

    m.complete(p, NodeKind::LetExpr)
}

// If Expression
fn parse_if_expr(p: &mut Parser, recovery_set: TokenSet) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::If));

    let m = p.start();
    p.bump(); // "if"

    let condition = parse_expr(p, "condition")?;
    p.expect_with_no_skip(TokenKind::LBrace);
    let then_branch = parse_expr(p, "then branch")?;

    let else_branch = if p.at(TokenKind::Else) {
        p.bump(); // "else"
        Some(parse_expr(p, "else branch")?)
    } else {
        None
    };

    m.complete(p, NodeKind::IfExpr)
}

// Match Expression
fn parse_match_expr(p: &mut Parser, recovery_set: TokenSet) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Match));

    let m = p.start();
    p.bump(); // "match"

    let expr = parse_expr(p, "match expression")?;
    p.expect_with_no_skip(TokenKind::LBrace);

    let mut arms = Vec::new();
    while !p.at(TokenKind::RBrace) {
        let arm = parse_match_arm(p, recovery_set)?;
        arms.push(arm);
    }
    p.bump(); // closing '}'

    m.complete(p, NodeKind::MatchExpr)
}

// Match Arm
fn parse_match_arm(p: &mut Parser, recovery_set: TokenSet) -> Option<CompletedMarker> {
    let m = p.start();
    let pattern = parse_pattern(p)?;
    p.expect_with_no_skip(TokenKind::FatArrow);
    let expr = parse_expr(p, "match arm expression")?;

    m.complete(p, NodeKind::MatchArm)
}

// Pattern
fn parse_pattern(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Ident) {
        parse_identifier_expr(p)
    } else if p.at_set(TokenSet::new([
        TokenKind::Boolean,
        TokenKind::Number,
        TokenKind::String,
    ])) {
        parse_literal_expr(p)
    } else {
        p.error()
    }
}

// Literal Expression
fn parse_literal_expr(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Boolean) {
        parse_boolean_literal(p)
    } else if p.at(TokenKind::Number) {
        parse_number_literal(p)
    } else if p.at(TokenKind::String) {
        parse_string_literal(p)
    } else {
        p.error()
    }
}

// Identifier Expression
fn parse_identifier_expr(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Ident));

    let m = p.start();
    p.bump(); // Identifier
    m.complete(p, NodeKind::IdentifierExpr)
}

// Boolean Literal
fn parse_boolean_literal(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Boolean));

    let m = p.start();
    p.bump(); // Boolean literal
    m.complete(p, NodeKind::LiteralExpr)
}

// Number Literal
fn parse_number_literal(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Number));

    let m = p.start();
    p.bump(); // Number literal
    m.complete(p, NodeKind::LiteralExpr)
}

// String Literal
fn parse_string_literal(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::String));

    let m = p.start();
    p.bump(); // String literal
    m.complete(p, NodeKind::LiteralExpr)
}

// Parentheses Expression
fn parse_paren_expr(p: &mut Parser, recovery_set: TokenSet) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump(); // "("

    let expr = parse_expr(p, "expression")?;
    p.expect_with_no_skip(TokenKind::RParen); // ")"

    m.complete(p, NodeKind::ParenExpr)
}

// Block Expression
fn parse_block_expr(p: &mut Parser, recovery_set: TokenSet) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::LBrace));

    let m = p.start();
    p.bump(); // "{"

    while !p.at(TokenKind::RBrace) {
        parse_expr(p, "block expression");
    }
    p.bump(); // closing '}'

    m.complete(p, NodeKind::BlockExpr)
}

// Binary Operators
fn parse_post_operators(
    p: &mut Parser,
    recovery_set: TokenSet,
    cm: CompletedMarker,
    no_derefs: bool,
    no_dot_instantiation: bool,
) -> CompletedMarker {
    let mut cm = cm;

    loop {
        match p.kind() {
            Some(TokenKind::Plus)
            | Some(TokenKind::Minus)
            | Some(TokenKind::Multiply)
            | Some(TokenKind::Divide)
            | Some(TokenKind::Greater)
            | Some(TokenKind::Less) => {
                let op = cm.precede(p);
                p.bump();
                cm = op.complete(p, NodeKind::BinaryExpr);
            }
            _ => break,
        }
    }

    cm
}
