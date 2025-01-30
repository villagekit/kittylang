use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{input::BorrowInput, pratt::*, prelude::*};
use std::{env, fmt, fs};

// AST and parser

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Var(&'src str),
    Num(f64),
    Bool(bool),
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let {
        lhs: Spanned<&'src str>,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
    },
    Apply {
        func: Box<Spanned<Self>>,
        arg: Box<Spanned<Self>>,
    },
    Func {
        arg: Box<Spanned<&'src str>>,
        body: Box<Spanned<Self>>,
    },
}

fn parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Spanned<Expr<'src>>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan, &'src [Spanned<Token<'src>>]) -> I + Clone + 'src,
{
    recursive(|expr| {
        let ident = select_ref! { Token::Ident(x) => *x };
        let atom = choice((
            select_ref! { Token::Num(x) => Expr::Num(*x) },
            just(Token::True).to(Expr::Bool(true)),
            just(Token::False).to(Expr::Bool(false)),
            ident.map(Expr::Var),
            // let x = y in z
            just(Token::Let)
                .ignore_then(ident.map_with(|x, e| (x, e.span())))
                .then_ignore(just(Token::Eq))
                .then(expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map(|((lhs, rhs), then)| Expr::Let {
                    lhs,
                    rhs: Box::new(rhs),
                    then: Box::new(then),
                }),
        ));

        choice((
            atom.map_with(|expr, e| (expr, e.span())),
            // fn x y = z
            just(Token::Fn).ignore_then(
                ident.map_with(|x, e| (x, e.span())).repeated().foldr_with(
                    just(Token::Eq).ignore_then(expr.clone()),
                    |arg, body, e| {
                        (
                            Expr::Func {
                                arg: Box::new(arg),
                                body: Box::new(body),
                            },
                            e.span(),
                        )
                    },
                ),
            ),
            // ( x )
            expr.nested_in(select_ref! { Token::Parens(ts) = e => make_input(e.span(), ts) }),
        ))
        .pratt(vec![
            // Multiply
            infix(left(10), just(Token::Asterisk), |x, _, y, e| {
                (Expr::Mul(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Add

// AST and parser

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Var(&'src str),
    Num(f64),
    Bool(bool),
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let {
        lhs: Spanned<&'src str>,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
    },
    Apply {
        func: Box<Spanned<Self>>,
        arg: Box<Spanned<Self>>,
    },
    Func {
        arg: Box<Spanned<&'src str>>,
        body: Box<Spanned<Self>>,
    },
}

fn parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Spanned<Expr<'src>>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan, &'src [Spanned<Token<'src>>]) -> I + Clone + 'src,
{
    recursive(|expr| {
        let ident = select_ref! { Token::Ident(x) => *x };
        let atom = choice((
            select_ref! { Token::Num(x) => Expr::Num(*x) },
            just(Token::True).to(Expr::Bool(true)),
            just(Token::False).to(Expr::Bool(false)),
            ident.map(Expr::Var),
            // let x = y in z
            just(Token::Let)
                .ignore_then(ident.map_with(|x, e| (x, e.span())))
                .then_ignore(just(Token::Eq))
                .then(expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map(|((lhs, rhs), then)| Expr::Let {
                    lhs,
                    rhs: Box::new(rhs),
                    then: Box::new(then),
                }),
        ));

        choice((
            atom.map_with(|expr, e| (expr, e.span())),
            // fn x y = z
            just(Token::Fn).ignore_then(
                ident.map_with(|x, e| (x, e.span())).repeated().foldr_with(
                    just(Token::Eq).ignore_then(expr.clone()),
                    |arg, body, e| {
                        (
                            Expr::Func {
                                arg: Box::new(arg),
                                body: Box::new(body),
                            },
                            e.span(),
                        )
                    },
                ),
            ),
            // ( x )
            expr.nested_in(select_ref! { Token::Parens(ts) = e => make_input(e.span(), ts) }),
        ))
        .pratt(vec![
            // Multiply
            infix(left(10), just(Token::Asterisk), |x, _, y, e| {
                (Expr::Mul(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Add
            infix(left(9), just(Token::Plus), |x, _, y, e| {
                (Expr::Add(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Calls
            infix(left(1), empty(), |x, _, y, e| {
                (
                    Expr::Apply {
                        func: Box::new(x),
                        arg: Box::new(y),
                    },
                    e.span(),
                )
            })
            .boxed(),
        ])
        .labelled("expression")
        .as_context()
    })
}

fn parse_failure(err: &Rich<impl fmt::Display>, src: &str) -> ! {
    failure(
        err.reason().to_string(),
        (
            err.found()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "end of input".to_string()),
            *err.span(),
        ),
        err.contexts()
            .map(|(l, s)| (format!("while parsing this {l}"), *s)),
        src,
    )
}
