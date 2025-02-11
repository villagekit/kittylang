use logos::{Lexer as LogosLexer, Logos, Span};
use std::{cmp::Ordering, fmt};

pub struct Lexer<'src> {
    lex: LogosLexer<'src, Token>,
    last_token: Option<Result<Token, <Token as Logos<'src>>::Error>>,
    indents: Vec<usize>,
    queued_tokens: Vec<(Token, Span)>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        let lex = Token::lexer(input);
        let last_token = None;
        let indents = vec![0];
        let queued_tokens = Vec::new();
        Self {
            lex,
            last_token,
            indents,
            queued_tokens,
        }
    }

    fn handle_whitespace(&mut self) -> (Token, Span) {
        let span = self.lex.span();
        let ws = &self.lex.source()[span.clone()];

        let mut indent = 0;

        for ch in ws.chars() {
            match ch {
                ' ' => indent += 1,
                // TODO handle tabs correctly
                '\t' => indent += 4,
                _ => break,
            }
        }

        // Get the current indent level (the top of our stack).
        let current_indent = *self.indents.last().unwrap();

        match indent.cmp(&current_indent) {
            Ordering::Greater => {
                // Increased indent: push new level and queue an Indent token.
                self.indents.push(indent);
                let indent_span = (span.start + current_indent)..span.end;
                self.queued_tokens.push((Token::Indent, indent_span));
            }
            Ordering::Less => {
                // Decreased indent: pop until we match the new level, queuing Dedent tokens.
                while let Some(&level) = self.indents.last() {
                    if level > indent {
                        self.indents.pop();
                        let dedent_span = span.end..span.end;
                        self.queued_tokens.push((Token::Dedent, dedent_span));
                    } else {
                        break;
                    }
                }
            }
            Ordering::Equal => {}
        }

        // If queued an indent or dedent, return that.
        if let Some((token, span)) = self.queued_tokens.pop() {
            (token, span)
        } else {
            // Otherwise return original whitespace
            (Token::Whitespace, span)
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (Result<Token, <Token as Logos<'src>>::Error>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        // If we have some tokens queued, return them first
        if let Some((token, span)) = self.queued_tokens.pop() {
            return Some((Ok(token), span));
        }

        // Get the next token.
        let token = self.lex.next();

        // If the last token was a newline (or this is the first token)
        //   and the current token is whitespace,
        //   return / queue any indents.
        if (self.last_token.is_none() || self.last_token == Some(Ok(Token::Newline)))
            && token == Some(Ok(Token::Whitespace))
        {
            let (next_token, next_span) = self.handle_whitespace();
            return Some((Ok(next_token), next_span));
        }

        // If we have no more tokens but still indented, return dedents.
        if token.is_none() && self.indents.len() > 1 {
            self.indents.pop();
            let end = self.lex.source().len();
            let span = end..end;
            let token = Token::Dedent;
            return Some((Ok(token), span));
        }

        self.last_token = token;

        token.map(|token| (token, self.lex.span()))
    }
}

#[derive(Logos, Debug, Copy, Clone, PartialEq)]
pub enum Token {
    #[regex(r"[ \t\f]+(\r)?\n")]
    EmptyLine,

    #[regex(r"[ \t\f]+")]
    Whitespace,

    #[regex(r"(\r)?\n")]
    Newline,

    // These tokens will be generated manually by our iterator wrapping Logos.
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
            Self::EmptyLine => "empty-line",
            Self::Whitespace => "whitespace",
            Self::Newline => "newline",
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
    fn lex_empty_line() {
        check_token("   \n", Token::EmptyLine);
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
