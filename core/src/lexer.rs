use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{input::BorrowInput, pratt::*, prelude::*};
use std::{env, fmt, fs};

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

pub type Spanned<T> = (T, SimpleSpan);

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char>>> {
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
