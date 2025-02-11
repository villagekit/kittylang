use logos::{Lexer as LogosLexer, Logos, Span};
use std::fmt;

pub struct Lexer<'src> {
    inner: LogosLexer<'src, Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        let extras = LexerExtras::new();
        let inner = Token::lexer_with_extras(input, extras);
        Self { inner }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (Result<Token, <Token as Logos<'src>>::Error>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((token, span)) = self.inner.extras.queued_tokens.pop() {
            return Some((Ok(token), span));
        }

        self.inner.next().map(|token| (token, self.inner.span()))
    }
}

#[derive(Debug)]
pub struct LexerExtras {
    indent_stack: Vec<usize>,
    queued_tokens: Vec<(Token, Span)>,
}

impl LexerExtras {
    fn new() -> Self {
        LexerExtras {
            indent_stack: vec![0],
            queued_tokens: Vec::new(),
        }
    }
}

#[derive(Logos, Debug, Copy, Clone, PartialEq)]
#[logos(extras = LexerExtras)]
pub enum Token {
    #[regex(r"(\r)?\n", handle_newline)]
    Newline,

    #[regex(r"[ \t\f]+")]
    Whitespace,

    // These tokens will be generated manually in our handle_newline callback.
    Indent,
    Dedent,

    #[token("fn")]
    FnKw,

    #[token("let")]
    LetKw,

    #[regex("[A-Za-z_][A-Za-z0-9_]*")]
    Ident,

    #[regex("[0-9]+")]
    Number,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("=")]
    Equals,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[regex("#.*")]
    Comment,
}

impl Token {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Newline => "newline",
            Self::Whitespace => "whitespace",
            Self::Indent => "indent",
            Self::Dedent => "dedent",
            Self::FnKw => "‘fn’",
            Self::LetKw => "‘let’",
            Self::Ident => "identifier",
            Self::Number => "number",
            Self::Plus => "‘+’",
            Self::Minus => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::Equals => "‘=’",
            Self::LParen => "‘(’",
            Self::RParen => "‘)’",
            Self::LBrace => "‘{’",
            Self::RBrace => "‘}’",
            Self::Comment => "comment",
        })
    }
}

/// This callback is called whenever a newline is lexed.
/// It examines the following whitespace (if any) to determine the new indentation level.
fn handle_newline(lex: &mut LogosLexer<Token>) -> Token {
    // Look at the remaining input to count leading whitespace.
    let remainder = lex.remainder();
    let mut indent = 0;

    // For simplicity, assume a space counts as 1 and a tab as 4.
    for ch in remainder.chars() {
        match ch {
            ' ' => indent += 1,
            '\t' => indent += 4,
            _ => break,
        }
    }
    // Advance the lexer past the counted whitespace.
    lex.bump(indent);

    // Get the current indent level (the top of our stack).
    let current_indent = *lex.extras.indent_stack.last().unwrap();

    let current_span = lex.span();

    if indent > current_indent {
        // Increased indent: push new level and queue an Indent token.
        lex.extras.indent_stack.push(indent);
        let indent_span = Span {
            start: current_span.start + current_indent,
            end: current_span.end,
        };
        lex.extras.queued_tokens.push((Token::Indent, indent_span));
    } else if indent < current_indent {
        // Decreased indent: pop until we match the new level, queuing Dedent tokens.
        while let Some(&level) = lex.extras.indent_stack.last() {
            if level > indent {
                lex.extras.indent_stack.pop();
                let dedent_span = Span {
                    start: current_span.end,
                    end: current_span.end,
                };
                lex.extras.queued_tokens.push((Token::Dedent, dedent_span));
            } else {
                break;
            }
        }
    }
    // Always return the newline token.
    Token::Newline
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Lexer;

    fn check(input: &str, expected_token: Token) {
        let mut lexer = Lexer::new(input);

        let (actual_token, _span) = lexer.next().unwrap();
        assert!(actual_token.is_ok());
        assert_eq!(expected_token, actual_token.unwrap());
    }

    #[test]
    fn lex_spaces_and_newlines() {
        check("  \n ", Token::Whitespace);
    }

    #[test]
    fn lex_fn_keyword() {
        check("fn", Token::FnKw);
    }

    #[test]
    fn lex_let_keyword() {
        check("let", Token::LetKw);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check("abcd", Token::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check("ab123cde456", Token::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check("ABCdef", Token::Ident);
    }

    #[test]
    fn lex_single_char_identifier() {
        check("x", Token::Ident);
    }

    #[test]
    fn lex_number() {
        check("123456", Token::Number);
    }

    #[test]
    fn lex_plus() {
        check("+", Token::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", Token::Minus);
    }

    #[test]
    fn lex_star() {
        check("*", Token::Star);
    }

    #[test]
    fn lex_slash() {
        check("/", Token::Slash);
    }

    #[test]
    fn lex_equals() {
        check("=", Token::Equals);
    }

    #[test]
    fn lex_left_parenthesis() {
        check("(", Token::LParen);
    }

    #[test]
    fn lex_right_parenthesis() {
        check(")", Token::RParen);
    }

    #[test]
    fn lex_left_brace() {
        check("{", Token::LBrace);
    }

    #[test]
    fn lex_right_brace() {
        check("}", Token::RBrace);
    }

    #[test]
    fn lex_comment() {
        check("# foo", Token::Comment);
    }
}
