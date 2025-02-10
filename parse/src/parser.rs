use chumsky::{input::BorrowInput, pratt::*, prelude::*};

use crate::{lexer::Token, token::Token};
use kitty_ast::Expr;
use kitty_meta::{ErrorReport, Span, Spanned};

pub fn parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Spanned<Expr<'src>>, extra::Err<ParserErrorInner<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = Span>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(Span, &'src [Spanned<Token<'src>>]) -> I + Clone + 'src,
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

pub type ParserErrorInner<'src> = Rich<'src, Token<'src>, Span>;

pub struct ParserError<'src>(pub ParserErrorInner<'src>);

impl From<ParserError<'_>> for ErrorReport {
    fn from(value: ParserError) -> ErrorReport {
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
