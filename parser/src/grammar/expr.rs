use kitty_syntax::{NodeKind, TokenKind};

use super::*;
use crate::marker::CompletedMarker;

/// Entry point: parse an expression.
pub(super) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    parse_expr(p, 0)
}

/// Parse an expression with a given minimum binding power.
fn parse_expr(p: &mut Parser, min_bp: u8) -> Option<CompletedMarker> {
    // First, parse a prefix (or primary) expression.
    let mut lhs = parse_prefix(p)?;

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
        parse_expr(p, right_bp)?;
        lhs = m.complete(p, NodeKind::BinaryExpr);
    }

    Some(lhs)
}

/// Parse a prefix expression, which may be a unary operator or simply a primary.
fn parse_prefix(p: &mut Parser) -> Option<CompletedMarker> {
    if let Some(bp) = prefix_binding_power(p) {
        // A prefix operator is present.
        let m = p.start();
        p.bump(); // Consume the prefix token.
        parse_expr(p, bp)?;
        return Some(m.complete(p, NodeKind::UnaryExpr));
    }
    parse_primary(p)
}

/// Parse a primary expression.
fn parse_primary(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Identifier) {
        let m = p.start();
        p.bump();
        return Some(m.complete(p, NodeKind::IdentifierExpr));
    } else if p.at(TokenKind::Number) || p.at(TokenKind::String) || p.at(TokenKind::Boolean) {
        let m = p.start();
        p.bump();
        return Some(m.complete(p, NodeKind::LiteralExpr));
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

/// Parse a call expression given an existing `lhs`.
fn parse_call_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    p.bump(); // Consume '('.
    if !p.at(TokenKind::ParenClose) {
        loop {
            expr(p);
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
    expr(p);
    if p.at(TokenKind::Comma) {
        // More than one expression makes it a tuple.
        while p.at(TokenKind::Comma) {
            p.bump();
            expr(p);
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
        expr(p);
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
    expr(p);
    Some(m.complete(p, NodeKind::LetExpr))
}

/// Parse an if–expression: `if <cond> { ... } [else { ... }]`
fn parse_if_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();
    p.expect(TokenKind::If);
    expr(p); // condition
    parse_block_expr(p); // then–block
    if p.at(TokenKind::Else) {
        p.bump(); // Consume 'else'.
        parse_block_expr(p); // else–block
    }
    Some(m.complete(p, NodeKind::IfExpr))
}

/// Return the binding power for a binary operator at the current token, if any.
/// The returned tuple is (left_binding_power, right_binding_power).
fn binary_binding_power(p: &mut Parser) -> Option<(u8, u8)> {
    if p.at(TokenKind::Plus) || p.at(TokenKind::Minus) {
        Some((9, 10))
    } else if p.at(TokenKind::Multiply) || p.at(TokenKind::Divide) || p.at(TokenKind::Rem) {
        Some((11, 12))
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

/// Return the binding power for a prefix operator at the current token, if any.
fn prefix_binding_power(p: &mut Parser) -> Option<u8> {
    if p.at(TokenKind::Minus) || p.at(TokenKind::Plus) || p.at(TokenKind::Not) {
        Some(13)
    } else {
        None
    }
}
