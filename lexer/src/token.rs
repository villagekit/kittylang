use logos::{Logos, Span};
use std::fmt;
use text_size::TextRange;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            range: TextRange::new((span.start as u32).into(), (span.end as u32).into()),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind, self.range)
    }
}

#[derive(Logos, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TokenKind {
    #[regex(r"[ \t\f]+")]
    Whitespace,
    #[regex(r"(\r)?\n")]
    Newline,

    // These tokens will be generated manually by our indenter wrapping Logos.
    Indent,
    Dedent,

    #[regex("(True|False)")]
    Boolean,
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    String,
    #[regex(r"(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")]
    Number,

    #[regex(r"[\p{XID_Start}_]\p{XID_Continue}*")]
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
    #[token("Fn")]
    FnUpper,
    #[token("let")]
    Let,
    #[token("in")]
    In,
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

    #[token("self")]
    SelfLower,
    #[token("Self")]
    SelfUpper,

    #[token("type")]
    Type,
    #[token("const")]
    Const,
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
    #[token(".[")]
    DotBracketOpen,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("_", priority = 3)]
    Underscore,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
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

    Error,
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Newline | Self::Comment)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Whitespace => "whitespace",
            Self::Newline => "newline",
            Self::Indent => "indent",
            Self::Dedent => "dedent",
            Self::Boolean => "boolean",
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
            Self::FnUpper => "‘Fn’",
            Self::Let => "‘let’",
            Self::In => "‘in’",
            Self::If => "‘if’",
            Self::Then => "‘then’",
            Self::Else => "‘else’",
            Self::Match => "‘match’",
            Self::Case => "‘case’",
            Self::SelfLower => "‘self’",
            Self::SelfUpper => "‘Self’",
            Self::Type => "‘type’",
            Self::Const => "‘const’",
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
            Self::DotBracketOpen => "‘.[’",
            Self::Dot => "‘.’",
            Self::Arrow => "‘->’",
            Self::FatArrow => "‘=>’",
            Self::Underscore => "_",
            Self::Plus => "‘+’",
            Self::Minus => "‘-’",
            Self::Multiply => "‘*’",
            Self::Divide => "‘/’",
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
            Self::Error => "‘error’",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Lexer;

    fn check_token(input: &str, expected: TokenKind) {
        let mut lexer = Lexer::new(input);
        let actual = lexer.next().unwrap();
        assert_eq!(expected, actual.kind);
        assert_eq!(
            TextRange::new(0.into(), (input.len() as u32).into()),
            actual.range
        );
    }

    #[test]
    fn lex_whitespace() {
        check_token("   ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_newline() {
        check_token("\n", TokenKind::Newline);
    }

    #[test]
    fn lex_newline_crlf() {
        check_token("\r\n", TokenKind::Newline);
    }

    // Indent and Dedent are handled manually by the indenter wrapper.

    #[test]
    fn lex_boolean_true() {
        check_token("True", TokenKind::Boolean);
    }

    #[test]
    fn lex_boolean_false() {
        check_token("False", TokenKind::Boolean);
    }

    #[test]
    fn lex_string() {
        check_token("\"hello\"", TokenKind::String);
    }

    #[test]
    fn lex_number() {
        check_token("123456", TokenKind::Number);
    }

    #[test]
    fn lex_identifier() {
        check_token("abcd", TokenKind::Identifier);
    }

    #[test]
    fn lex_left_parenthesis() {
        check_token("(", TokenKind::ParenOpen);
    }

    #[test]
    fn lex_right_parenthesis() {
        check_token(")", TokenKind::ParenClose);
    }

    #[test]
    fn lex_left_brace() {
        check_token("{", TokenKind::BraceOpen);
    }

    #[test]
    fn lex_right_brace() {
        check_token("}", TokenKind::BraceClose);
    }

    #[test]
    fn lex_left_bracket() {
        check_token("[", TokenKind::BracketOpen);
    }

    #[test]
    fn lex_right_bracket() {
        check_token("]", TokenKind::BracketClose);
    }

    #[test]
    fn lex_comment() {
        check_token("# foo", TokenKind::Comment);
    }

    #[test]
    fn lex_fn_keyword() {
        check_token("fn", TokenKind::Fn);
    }

    #[test]
    fn lex_let_keyword() {
        check_token("let", TokenKind::Let);
    }

    #[test]
    fn lex_if_keyword() {
        check_token("if", TokenKind::If);
    }

    #[test]
    fn lex_then_keyword() {
        check_token("then", TokenKind::Then);
    }

    #[test]
    fn lex_else_keyword() {
        check_token("else", TokenKind::Else);
    }

    #[test]
    fn lex_match_keyword() {
        check_token("match", TokenKind::Match);
    }

    #[test]
    fn lex_case_keyword() {
        check_token("case", TokenKind::Case);
    }

    #[test]
    fn lex_type_keyword() {
        check_token("type", TokenKind::Type);
    }

    #[test]
    fn lex_enum_keyword() {
        check_token("enum", TokenKind::Enum);
    }

    #[test]
    fn lex_struct_keyword() {
        check_token("struct", TokenKind::Struct);
    }

    #[test]
    fn lex_prop_keyword() {
        check_token("prop", TokenKind::Prop);
    }

    #[test]
    fn lex_impl_keyword() {
        check_token("impl", TokenKind::Impl);
    }

    #[test]
    fn lex_trait_keyword() {
        check_token("trait", TokenKind::Trait);
    }

    #[test]
    fn lex_where_keyword() {
        check_token("where", TokenKind::Where);
    }

    #[test]
    fn lex_for_keyword() {
        check_token("for", TokenKind::For);
    }

    #[test]
    fn lex_import_keyword() {
        check_token("import", TokenKind::Import);
    }

    #[test]
    fn lex_export_keyword() {
        check_token("export", TokenKind::Export);
    }

    #[test]
    fn lex_from_keyword() {
        check_token("from", TokenKind::From);
    }

    #[test]
    fn lex_comma() {
        check_token(",", TokenKind::Comma);
    }

    #[test]
    fn lex_colon() {
        check_token(":", TokenKind::Colon);
    }

    #[test]
    fn lex_ellipses() {
        check_token("...", TokenKind::Ellipses);
    }

    #[test]
    fn lex_dot() {
        check_token(".", TokenKind::Dot);
    }

    #[test]
    fn lex_fat_arrow() {
        check_token("=>", TokenKind::FatArrow);
    }

    #[test]
    fn lex_plus() {
        check_token("+", TokenKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check_token("-", TokenKind::Minus);
    }

    #[test]
    fn lex_star() {
        check_token("*", TokenKind::Multiply);
    }

    #[test]
    fn lex_slash() {
        check_token("/", TokenKind::Divide);
    }

    #[test]
    fn lex_greater_equal() {
        check_token(">=", TokenKind::GreaterEqual);
    }

    #[test]
    fn lex_greater() {
        check_token(">", TokenKind::Greater);
    }

    #[test]
    fn lex_less_equal() {
        check_token("<=", TokenKind::LessEqual);
    }

    #[test]
    fn lex_less() {
        check_token("<", TokenKind::Less);
    }

    #[test]
    fn lex_equal_equal() {
        check_token("==", TokenKind::EqualEqual);
    }

    #[test]
    fn lex_not_equal() {
        check_token("!=", TokenKind::NotEqual);
    }

    #[test]
    fn lex_equals() {
        check_token("=", TokenKind::Equal);
    }

    #[test]
    fn lex_and_keyword() {
        check_token("and", TokenKind::And);
    }

    #[test]
    fn lex_or_keyword() {
        check_token("or", TokenKind::Or);
    }

    #[test]
    fn lex_xor_keyword() {
        check_token("xor", TokenKind::Xor);
    }

    #[test]
    fn lex_not_keyword() {
        check_token("not", TokenKind::Not);
    }

    #[test]
    fn lex_rem_keyword() {
        check_token("rem", TokenKind::Rem);
    }
}
