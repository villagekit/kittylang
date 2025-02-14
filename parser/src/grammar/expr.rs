use kitty_syntax::{NodeKind, TokenKind};

use crate::token_set::TokenSet;
use crate::{marker::CompletedMarker, parser::Parser};

/// FIRST set for expressions.
pub(super) const EXPR_FIRST: TokenSet = TokenSet::new([
    TokenKind::Identifier,
    TokenKind::Number,
    TokenKind::String,
    TokenKind::Boolean,
    TokenKind::ParenOpen,
    TokenKind::BraceClose,
    TokenKind::Let,
    TokenKind::If,
    TokenKind::Match,
    TokenKind::Not,   // for unary operators
    TokenKind::Plus,  // unary plus
    TokenKind::Minus, // unary minus
]);

/// The binding power (precedence) for prefix (unary) operators.
const UNARY_BP: u8 = 5;

/// Entry point to parse an expression.
pub fn parse_expr(p: &mut Parser<'_>, expected: &'static str) -> Option<CompletedMarker> {
    // Use an empty recovery set for top–level expression parsing.
    parse_expr_bp(p, 0, TokenSet::default(), expected)
}

/// Parse an expression using precedence climbing (bp stands for binding power).
fn parse_expr_bp(
    p: &mut Parser<'_>,
    min_bp: u8,
    recovery_set: TokenSet,
    expected: &'static str,
) -> Option<CompletedMarker> {
    // Parse a prefix expression (the "nud", or null denotation).
    let mut lhs = parse_nud(p, recovery_set, expected)?;

    loop {
        // First, check for postfix operators.
        match p.current_token() {
            TokenKind::LParen => {
                lhs = parse_call_or_tuple(p, lhs)?;
                continue;
            }
            TokenKind::Dot => {
                lhs = parse_get_expr(p, lhs)?;
                continue;
            }
            _ => { /* fall through to binary operator check */ }
        }

        // Check if the current token is a binary operator.
        if let Some((left_bp, right_bp, _op)) = maybe_binary_operator(p) {
            // If the left binding power is too low, we break.
            if left_bp < min_bp {
                break;
            }
            // Consume the operator.
            p.bump();

            // Build a binary expression node.
            let m = lhs.precede(p);
            // Recursively parse the right–hand side with the operator's right binding power.
            parse_expr_bp(p, right_bp, recovery_set, "binary operand")?;
            lhs = m.complete(p, NodeKind::BinaryExpr);
            continue;
        }

        break;
    }

    Some(lhs)
}

/// Parse a prefix (or primary) expression.
fn parse_nud(
    p: &mut Parser<'_>,
    recovery_set: TokenSet,
    expected: &'static str,
) -> Option<CompletedMarker> {
    // Set an expected syntax name for better error messages.
    let _guard = p.expected_syntax_name(expected);

    match p.current_token() {
        TokenKind::Identifier => parse_identifier_expr(p),
        TokenKind::Number | TokenKind::String | TokenKind::Boolean => parse_literal_expr(p),
        TokenKind::LParen => parse_paren_or_tuple_expr(p),
        TokenKind::LBrace => parse_block_expr(p),
        TokenKind::Let => parse_let_expr(p),
        TokenKind::If => parse_if_expr(p),
        TokenKind::Match => parse_match_expr(p),
        TokenKind::Not | TokenKind::Plus | TokenKind::Minus => parse_unary_expr(p),
        _ => p.error_with_recovery_set(recovery_set),
    }
}

/// Parse an identifier expression.
fn parse_identifier_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();
    p.bump(); // consume the identifier
    Some(m.complete(p, NodeKind::IdentifierExpr))
}

/// Parse a literal expression (Number, String, or Boolean).
fn parse_literal_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();
    p.bump(); // consume the literal token
    Some(m.complete(p, NodeKind::LiteralExpr))
}

/// Parse a parenthesized expression or a tuple expression.
fn parse_paren_or_tuple_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::LParen));
    let m = p.start();
    p.bump(); // consume '('

    // Handle an empty tuple: "()"
    if p.at(TokenKind::RParen) {
        p.bump();
        return Some(m.complete(p, NodeKind::TupleExpr));
    }

    // Parse the first expression.
    parse_expr(p, "expression")?;

    // If there is a comma, then we have a tuple expression.
    if p.at(TokenKind::Comma) {
        while p.at(TokenKind::Comma) {
            p.bump(); // consume comma
                      // Allow a trailing comma before the closing ')'.
            if p.at(TokenKind::RParen) {
                break;
            }
            parse_expr(p, "expression")?;
        }
        p.expect(TokenKind::RParen);
        Some(m.complete(p, NodeKind::TupleExpr))
    } else {
        // Otherwise, it is a parenthesized expression.
        p.expect(TokenKind::RParen);
        Some(m.complete(p, NodeKind::ParenExpr))
    }
}

/// Parse a block expression.
fn parse_block_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::LBrace));
    let m = p.start();
    p.bump(); // consume '{'

    // In a full parser, you might differentiate between statements and a trailing expression.
    while !p.at(TokenKind::RBrace) && !p.at_eof() {
        parse_expr(p, "expression or statement");
    }

    p.expect(TokenKind::RBrace);
    Some(m.complete(p, NodeKind::BlockExpr))
}

/// Parse a let expression: `let <identifier> = <expr>;`
fn parse_let_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Let));
    let m = p.start();
    p.bump(); // consume 'let'

    if !p.at(TokenKind::Identifier) {
        return p.error("expected identifier after 'let'");
    }
    p.bump(); // consume identifier

    p.expect(TokenKind::Equal);
    parse_expr(p, "let initializer")?;
    // Optionally consume a semicolon.
    if p.at(TokenKind::Semicolon) {
        p.bump();
    }

    Some(m.complete(p, NodeKind::LetExpr))
}

/// Parse an if expression: `if (<condition>) <then_expr> [else <else_expr>]`
fn parse_if_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::If));
    let m = p.start();
    p.bump(); // consume 'if'

    p.expect(TokenKind::LParen);
    parse_expr(p, "condition")?;
    p.expect(TokenKind::RParen);

    parse_expr(p, "then branch")?;
    if p.at(TokenKind::Else) {
        p.bump();
        parse_expr(p, "else branch")?;
    }
    Some(m.complete(p, NodeKind::IfExpr))
}

/// Parse a match expression: `match <expr> { <arms>* }`
fn parse_match_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Match));
    let m = p.start();
    p.bump(); // consume 'match'

    parse_expr(p, "match value")?;
    p.expect(TokenKind::LBrace);

    while !p.at(TokenKind::RBrace) && !p.at_eof() {
        parse_match_arm(p)?;
    }
    p.expect(TokenKind::RBrace);
    Some(m.complete(p, NodeKind::MatchExpr))
}

/// Parse a single match arm.
fn parse_match_arm(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();

    // For demonstration, we use a dummy pattern parser.
    parse_pattern(p, "pattern")?;
    p.expect(TokenKind::FatArrow); // e.g. '=>'
    parse_expr(p, "match arm expression")?;

    // Optional comma between match arms.
    if p.at(TokenKind::Comma) {
        p.bump();
    }
    Some(m.complete(p, NodeKind::MatchArm))
}

/// Parse a unary (prefix) expression.
fn parse_unary_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    // We expect a prefix operator here.
    if p.at(TokenKind::Not) || p.at(TokenKind::Plus) || p.at(TokenKind::Minus) {
        let m = p.start();
        p.bump(); // consume the unary operator
                  // Parse the operand with the designated unary binding power.
        parse_expr_bp(p, UNARY_BP, TokenSet::default(), "unary operand")?;
        return Some(m.complete(p, NodeKind::UnaryExpr));
    }
    // Fallback to a primary expression.
    parse_nud(p, TokenSet::default(), "expression")
}

/// Determine if the current token is a binary operator and return its binding powers.
/// The returned tuple is (left_bp, right_bp, operator).
fn maybe_binary_operator(p: &mut Parser<'_>) -> Option<(u8, u8, TokenKind)> {
    match p.current_token() {
        TokenKind::Or => Some((1, 2, TokenKind::Or)),
        TokenKind::And => Some((2, 3, TokenKind::And)),
        TokenKind::EqualEqual | TokenKind::NotEqual => Some((3, 4, p.current_token())),
        TokenKind::Less | TokenKind::LessEqual | TokenKind::Greater | TokenKind::GreaterEqual => {
            Some((4, 5, p.current_token()))
        }
        TokenKind::Plus | TokenKind::Minus => Some((5, 6, p.current_token())),
        TokenKind::Multiply | TokenKind::Divide | TokenKind::Rem => Some((6, 7, p.current_token())),
        _ => None,
    }
}

/// Parse a call or tuple expression after a primary expression.
/// The `callee` is the expression preceding the '('.
fn parse_call_or_tuple(p: &mut Parser<'_>, callee: CompletedMarker) -> Option<CompletedMarker> {
    let m = callee.precede(p);
    p.bump(); // consume '('

    let mut arg_count = 0;
    if !p.at(TokenKind::RParen) {
        parse_expr(p, "argument")?;
        arg_count += 1;
        while p.at(TokenKind::Comma) {
            p.bump(); // consume comma
                      // Allow trailing comma.
            if p.at(TokenKind::RParen) {
                break;
            }
            parse_expr(p, "argument")?;
            arg_count += 1;
        }
    }
    p.expect(TokenKind::RParen);
    // Decide node kind based on the presence of arguments.
    if arg_count > 0 {
        Some(m.complete(p, NodeKind::CallExpr))
    } else {
        Some(m.complete(p, NodeKind::TupleExpr))
    }
}

/// Parse a get expression (member access): `<receiver> . <identifier>`
fn parse_get_expr(p: &mut Parser<'_>, receiver: CompletedMarker) -> Option<CompletedMarker> {
    let m = receiver.precede(p);
    p.bump(); // consume '.'
    if !p.at(TokenKind::Identifier) {
        return p.error("expected identifier after '.'");
    }
    p.bump();
    Some(m.complete(p, NodeKind::GetExpr))
}

/// Dummy pattern parser.
/// In a real parser, this function would handle various pattern forms.
fn parse_pattern(p: &mut Parser<'_>, _expected: &'static str) -> Option<CompletedMarker> {
    let m = p.start();
    if p.at(TokenKind::Identifier) {
        p.bump();
        // For demonstration, we mark an identifier pattern as a wildcard.
        return Some(m.complete(p, NodeKind::PatternWildcard));
    }
    p.error_with_recovery_set(TokenSet::default())
}
