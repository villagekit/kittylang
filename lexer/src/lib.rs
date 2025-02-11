use logos::{Lexer as LogosLexer, Logos, Span};
use std::{cmp::Ordering, fmt};

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
        let inner = &mut self.inner;

        // If we have some tokens queued, return them first.
        if let Some((token, span)) = inner.extras.queued_tokens.pop() {
            return Some((Ok(token), span));
        }

        // Get the next token (and corresponding span).
        let res = inner.next().map(|token| (token, inner.span()));

        // If we have no more tokens but still indented, return dedents.
        if res.is_none() && inner.extras.indent_stack.len() > 1 {
            inner.extras.indent_stack.pop();
            let end = inner.source().len();
            let span = end..end;
            let token = Token::Dedent;
            return Some((Ok(token), span));
        }

        res
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

    match indent.cmp(&current_indent) {
        Ordering::Greater => {
            // Increased indent: push new level and queue an Indent token.
            lex.extras.indent_stack.push(indent);
            let indent_span = Span {
                start: current_span.start + current_indent,
                end: current_span.end,
            };
            lex.extras.queued_tokens.push((Token::Indent, indent_span));
        }
        Ordering::Less => {
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
        Ordering::Equal => {}
    }
    // Always return the newline token.
    Token::Newline
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Lexer;

    fn check_token(input: &str, expected_token: Token) {
        let mut lexer = Lexer::new(input);

        let (actual_token, actual_span) = lexer.next().unwrap();
        assert_eq!(expected_token, actual_token.unwrap());
        assert_eq!(0..input.len(), actual_span);
    }

    fn check_tokens(input: &str, expected_tokens: Vec<(Token, Span)>) {
        let lexer = Lexer::new(input);

        let actual_tokens: Vec<(Token, Span)> =
            lexer.map(|(tok, span)| (tok.unwrap(), span)).collect();

        assert_eq!(expected_tokens, actual_tokens);
    }

    #[test]
    fn lex_spaces_and_newlines() {
        check_token("   ", Token::Whitespace);
    }

    #[test]
    fn lex_fn_keyword() {
        check_token("fn", Token::FnKw);
    }

    #[test]
    fn lex_let_keyword() {
        check_token("let", Token::LetKw);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check_token("abcd", Token::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check_token("ab123cde456", Token::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check_token("ABCdef", Token::Ident);
    }

    #[test]
    fn lex_single_char_identifier() {
        check_token("x", Token::Ident);
    }

    #[test]
    fn lex_number() {
        check_token("123456", Token::Number);
    }

    #[test]
    fn lex_plus() {
        check_token("+", Token::Plus);
    }

    #[test]
    fn lex_minus() {
        check_token("-", Token::Minus);
    }

    #[test]
    fn lex_star() {
        check_token("*", Token::Star);
    }

    #[test]
    fn lex_slash() {
        check_token("/", Token::Slash);
    }

    #[test]
    fn lex_equals() {
        check_token("=", Token::Equals);
    }

    #[test]
    fn lex_left_parenthesis() {
        check_token("(", Token::LParen);
    }

    #[test]
    fn lex_right_parenthesis() {
        check_token(")", Token::RParen);
    }

    #[test]
    fn lex_left_brace() {
        check_token("{", Token::LBrace);
    }

    #[test]
    fn lex_right_brace() {
        check_token("}", Token::RBrace);
    }

    #[test]
    fn lex_comment() {
        check_token("# foo", Token::Comment);
    }

    #[test]
    fn lex_indentation() {
        let input = r"fn foo()
  fn bar()
    baz
";
        let expected = vec![(Token::FnKw, 0..2)];
        check_tokens(input, expected);
    }
}
