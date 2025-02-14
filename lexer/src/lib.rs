mod indenter;
mod token;

use indenter::Indenter;
use logos::{Logos, SpannedIter};

pub use crate::token::{Token, TokenKind};

pub fn lex(source: &str) -> impl Iterator<Item = Token> + '_ {
    Lexer::new(source)
}

pub struct Lexer<'src> {
    inner: Indenter<'src, SpannedIter<'src, TokenKind>>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        let tokens = TokenKind::lexer(source).spanned();
        let splitter = Indenter::new(source, tokens);
        Self { inner: splitter }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some((Ok(token), span)) => Some(Token::new(token, span)),
            Some((Err(_), span)) => Some(Token::new(TokenKind::Error, span)),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use super::*;

    #[test]
    fn run_tests() {
        kitty_test_utils::run_tests(|input| {
            lex(input)
                .map(|token| format!("{:?}@{:?}", token.kind, token.range))
                .collect::<Vec<_>>()
                .join("\n")
        });
    }

    fn check_tokens(input: &str, expected: Vec<(TokenKind, Range<u32>)>) {
        let actual: Vec<(TokenKind, Range<u32>)> = lex(input)
            .map(|token| (token.kind, token.range.into()))
            .collect();

        assert_eq!(expected, actual);
    }

    #[test]
    fn lex_indent() {
        let input = r"
fn foo()
  fn bar()
    baz
";
        use TokenKind::*;
        let expected = vec![
            (Newline, 0..1),
            (Fn, 1..3),
            (Whitespace, 3..4),
            (Identifier, 4..7),
            (ParenOpen, 7..8),
            (ParenClose, 8..9),
            (Newline, 9..10),
            (Indent, 10..12),
            (Fn, 12..14),
            (Whitespace, 14..15),
            (Identifier, 15..18),
            (ParenOpen, 18..19),
            (ParenClose, 19..20),
            (Newline, 20..21),
            (Whitespace, 21..23),
            (Indent, 23..25),
            (Identifier, 25..28),
            (Newline, 28..29),
            (Dedent, 29..29),
            (Dedent, 29..29),
        ];
        check_tokens(input, expected);
    }

    #[test]
    fn lex_indent_empty_lines() {
        let input = r"
fn foo()

  baz

";
        use TokenKind::*;
        let expected = vec![
            (Newline, 0..1),
            (Fn, 1..3),
            (Whitespace, 3..4),
            (Identifier, 4..7),
            (ParenOpen, 7..8),
            (ParenClose, 8..9),
            (Newline, 9..10),
            (Newline, 10..11),
            (Indent, 11..13),
            (Identifier, 13..16),
            (Newline, 16..17),
            (Newline, 17..18),
            (Dedent, 18..18),
        ];
        check_tokens(input, expected);
    }

    #[test]
    fn lex_indent_2() {
        let input = r"
fn foo()
  baz
foo()
";
        use TokenKind::*;
        let expected = vec![
            (Newline, 0..1),
            (Fn, 1..3),
            (Whitespace, 3..4),
            (Identifier, 4..7),
            (ParenOpen, 7..8),
            (ParenClose, 8..9),
            (Newline, 9..10),
            (Indent, 10..12),
            (Identifier, 12..15),
            (Newline, 15..16),
            (Dedent, 16..16),
            (Identifier, 16..19),
            (ParenOpen, 19..20),
            (ParenClose, 20..21),
            (Newline, 21..22),
        ];
        check_tokens(input, expected);
    }
}
