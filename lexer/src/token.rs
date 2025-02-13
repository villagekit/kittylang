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

    #[regex(r"@[a-zA-Z0-9-]+\/[a-zA-Z0-9-]")]
    Package,

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
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("case")]
    Case,

    #[token("type")]
    Type,
    #[token("enum")]
    Enum,
    #[token("struct")]
    Struct,
    #[token("prop")]
    Prop,
    #[token("impl")]
    Impl,
    #[token("trait")]
    Trait,
    #[token("where")]
    Where,
    #[token("for")]
    For,

    #[token("import")]
    Import,
    #[token("export")]
    Export,
    #[token("from")]
    From,

    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("...")]
    Ellipses,
    #[token(".")]
    Dot,
    #[token("=>")]
    FatArrow,

    #[token("+")]
    Cross,
    #[token("-")]
    Dash,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token(">=")]
    GreaterEqual,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEqual,
    #[token("<")]
    Less,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("=")]
    Equal,

    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("xor")]
    Xor,
    #[token("not")]
    Not,
    #[token("rem")]
    Rem,
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
            Self::Package => "package",
            Self::ParenOpen => "‘(’",
            Self::ParenClose => "‘)’",
            Self::BraceOpen => "‘{’",
            Self::BraceClose => "‘}’",
            Self::BracketOpen => "‘[’",
            Self::BracketClose => "‘]’",
            Self::Comment => "comment",
            Self::Fn => "‘fn’",
            Self::Let => "‘let’",
            Self::If => "‘if’",
            Self::Then => "‘then’",
            Self::Else => "‘else’",
            Self::Match => "‘match’",
            Self::Case => "‘case’",
            Self::Type => "‘type’",
            Self::Enum => "‘enum’",
            Self::Struct => "‘struct’",
            Self::Prop => "‘prop’",
            Self::Impl => "‘impl’",
            Self::Trait => "‘trait’",
            Self::Where => "‘where’",
            Self::For => "‘for’",
            Self::Import => "‘import’",
            Self::Export => "‘export’",
            Self::From => "‘from’",
            Self::Comma => "‘,’",
            Self::Colon => "‘:’",
            Self::Ellipses => "‘...’",
            Self::Dot => "‘.’",
            Self::FatArrow => "‘=>’",
            Self::Cross => "‘+’",
            Self::Dash => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::GreaterEqual => "‘>=’",
            Self::Greater => "‘>’",
            Self::LessEqual => "‘<=’",
            Self::Less => "‘<’",
            Self::EqualEqual => "‘==’",
            Self::NotEqual => "‘!=’",
            Self::Equal => "‘=’",
            Self::And => "‘and’",
            Self::Or => "‘or’",
            Self::Xor => "‘xor’",
            Self::Not => "‘not’",
            Self::Rem => "‘rem’",
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
    fn lex_whitespace() {
        check_token("   ", Token::Whitespace);
    }

    #[test]
    fn lex_newline() {
        check_token("\n", Token::Newline);
    }

    #[test]
    fn lex_newline_crlf() {
        check_token("\r\n", Token::Newline);
    }

    // Indent and Dedent are handled manually by the indenter wrapper.

    #[test]
    fn lex_string() {
        check_token("\"hello\"", Token::String);
    }

    #[test]
    fn lex_number() {
        check_token("123456", Token::Number);
    }

    #[test]
    fn lex_identifier() {
        check_token("abcd", Token::Identifier);
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
    fn lex_left_bracket() {
        check_token("[", Token::BracketOpen);
    }

    #[test]
    fn lex_right_bracket() {
        check_token("]", Token::BracketClose);
    }

    #[test]
    fn lex_comment() {
        check_token("# foo", Token::Comment);
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
    fn lex_if_keyword() {
        check_token("if", Token::If);
    }

    #[test]
    fn lex_then_keyword() {
        check_token("then", Token::Then);
    }

    #[test]
    fn lex_else_keyword() {
        check_token("else", Token::Else);
    }

    #[test]
    fn lex_match_keyword() {
        check_token("match", Token::Match);
    }

    #[test]
    fn lex_case_keyword() {
        check_token("case", Token::Case);
    }

    #[test]
    fn lex_type_keyword() {
        check_token("type", Token::Type);
    }

    #[test]
    fn lex_enum_keyword() {
        check_token("enum", Token::Enum);
    }

    #[test]
    fn lex_struct_keyword() {
        check_token("struct", Token::Struct);
    }

    #[test]
    fn lex_prop_keyword() {
        check_token("prop", Token::Prop);
    }

    #[test]
    fn lex_impl_keyword() {
        check_token("impl", Token::Impl);
    }

    #[test]
    fn lex_trait_keyword() {
        check_token("trait", Token::Trait);
    }

    #[test]
    fn lex_where_keyword() {
        check_token("where", Token::Where);
    }

    #[test]
    fn lex_for_keyword() {
        check_token("for", Token::For);
    }

    #[test]
    fn lex_import_keyword() {
        check_token("import", Token::Import);
    }

    #[test]
    fn lex_export_keyword() {
        check_token("export", Token::Export);
    }

    #[test]
    fn lex_from_keyword() {
        check_token("from", Token::From);
    }

    #[test]
    fn lex_comma() {
        check_token(",", Token::Comma);
    }

    #[test]
    fn lex_colon() {
        check_token(":", Token::Colon);
    }

    #[test]
    fn lex_ellipses() {
        check_token("...", Token::Ellipses);
    }

    #[test]
    fn lex_dot() {
        check_token(".", Token::Dot);
    }

    #[test]
    fn lex_fat_arrow() {
        check_token("=>", Token::FatArrow);
    }

    #[test]
    fn lex_plus() {
        check_token("+", Token::Cross);
    }

    #[test]
    fn lex_minus() {
        check_token("-", Token::Dash);
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
    fn lex_greater_equal() {
        check_token(">=", Token::GreaterEqual);
    }

    #[test]
    fn lex_greater() {
        check_token(">", Token::Greater);
    }

    #[test]
    fn lex_less_equal() {
        check_token("<=", Token::LessEqual);
    }

    #[test]
    fn lex_less() {
        check_token("<", Token::Less);
    }

    #[test]
    fn lex_equal_equal() {
        check_token("==", Token::EqualEqual);
    }

    #[test]
    fn lex_not_equal() {
        check_token("!=", Token::NotEqual);
    }

    #[test]
    fn lex_equals() {
        check_token("=", Token::Equal);
    }

    #[test]
    fn lex_and_keyword() {
        check_token("and", Token::And);
    }

    #[test]
    fn lex_or_keyword() {
        check_token("or", Token::Or);
    }

    #[test]
    fn lex_xor_keyword() {
        check_token("xor", Token::Xor);
    }

    #[test]
    fn lex_not_keyword() {
        check_token("not", Token::Not);
    }

    #[test]
    fn lex_rem_keyword() {
        check_token("rem", Token::Rem);
    }
}
