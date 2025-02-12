use fastnum::{decimal, D128};
use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, Copy, Clone, PartialEq)]
pub enum Token {
    #[regex(r"[ \t\f]+")]
    Whitespace,
    #[regex(r"(\r)?\n")]
    Newline,

    // These tokens will be generated manually by our indenter wrapping Logos.
    Indent,
    Dedent,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    String,
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")]
    Number,

    #[regex(r"(\p{XID_Start}|_)\p{XID_Continue}*")]
    Identifier,

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,

    #[regex("#.*")]
    Comment,

    #[token("fn")]
    Fn,
    #[token("let")]
    Let,

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
}

impl Token {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Whitespace => "whitespace",
            Self::Newline => "newline",
            Self::Indent => "indent",
            Self::Dedent => "dedent",
            Self::String => "string",
            Self::Number => "number",
            Self::Identifier => "identifier",
            Self::ParenOpen => "‘(’",
            Self::ParenClose => "‘)’",
            Self::BraceOpen => "‘{’",
            Self::BraceClose => "‘}’",
            Self::BracketOpen => "‘[’",
            Self::BracketClose => "‘]’",
            Self::Fn => "‘fn’",
            Self::Let => "‘let’",
            Self::Plus => "‘+’",
            Self::Minus => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::Equals => "‘=’",
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

    #[test]
    fn lex_empty_line() {
        check_token("   ", Token::Whitespace);
    }

    #[test]
    fn lex_fn_keyword() {
        check_token("fn", Token::Fn);
    }

    #[test]
    fn lex_let_keyword() {
        check_token("let", Token::Let);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check_token("abcd", Token::Identifier);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check_token("ab123cde456", Token::Identifier);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check_token("ABCdef", Token::Identifier);
    }

    #[test]
    fn lex_single_char_identifier() {
        check_token("x", Token::Identifier);
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
        check_token("(", Token::ParenOpen);
    }

    #[test]
    fn lex_right_parenthesis() {
        check_token(")", Token::ParenClose);
    }

    #[test]
    fn lex_left_brace() {
        check_token("{", Token::BraceOpen);
    }

    #[test]
    fn lex_right_brace() {
        check_token("}", Token::BraceClose);
    }

    #[test]
    fn lex_comment() {
        check_token("# foo", Token::Comment);
    }
}
