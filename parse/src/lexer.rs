use chumsky::{input::MappedSpan, prelude::*};
use std::fmt;

use kitty_meta::{ErrorReport, Span, Spanned};

// Tokens and lexer

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Ident(&'src str),
    Num(f64),
    Parens(Vec<Spanned<Self>>),

    // Ops
    Eq,
    Plus,
    Asterisk,

    // Keywords
    Let,
    In,
    Fn,
    True,
    False,
}

pub type SpannedToken<'src> = Spanned<Token<'src>>;

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(x) => write!(f, "{x}"),
            Token::Num(x) => write!(f, "{x}"),
            Token::Parens(_) => write!(f, "(...)"),
            Token::Eq => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Asterisk => write!(f, "*"),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Fn => write!(f, "fn"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

pub fn lexer<'src, F>() -> impl Parser<
    'src,
    MappedSpan<Span, &'src str, F>,
    Vec<SpannedToken<'src>>,
    extra::Err<LexerErrorInner<'src>>,
>
where
    F: Fn(SimpleSpan) -> Span + 'src,
{
    recursive(|token| {
        choice((
            // Keywords
            text::ident().map(|s| match s {
                "let" => Token::Let,
                "in" => Token::In,
                "fn" => Token::Fn,
                "true" => Token::True,
                "false" => Token::False,
                s => Token::Ident(s),
            }),
            // Operators
            just("=").to(Token::Eq),
            just("+").to(Token::Plus),
            just("*").to(Token::Asterisk),
            // Numbers
            text::int(10)
                .then(just('.').then(text::digits(10)).or_not())
                .to_slice()
                .map(|s: &str| Token::Num(s.parse().unwrap())),
            token
                .repeated()
                .collect()
                .delimited_by(just('('), just(')'))
                .labelled("token tree")
                .as_context()
                .map(Token::Parens),
        ))
        .map_with(|t, e| (t, e.span()))
        .padded()
    })
    .repeated()
    .collect()
}

pub type LexerErrorInner<'src> = Rich<'src, char, Span>;

pub struct LexerError<'src>(pub LexerErrorInner<'src>);

impl From<LexerError<'_>> for ErrorReport {
    fn from(value: LexerError) -> ErrorReport {
        let error = value.0;
        ErrorReport {
            message: error.reason().to_string(),
            span: *error.span(),
            labels: vec![(
                *error.span(),
                error
                    .found()
                    .map(|c| c.to_string())
                    .unwrap_or_else(|| "end of input".to_string()),
            )]
            .into_iter()
            .chain(
                error
                    .contexts()
                    .map(|(l, _s)| (*error.span(), format!("while parsing this {l}"))),
            )
            .collect(),
            notes: vec![],
        }
    }
}
