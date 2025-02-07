use chumsky::{input::MappedSpan, input::*, prelude::*};

use kitty_meta::{ErrorReport, Span, Spanned};

use crate::token::{SpannedToken, Token};

/*
pub trait Lexer<'src, F, T>:
    Parser<
        'src,
        MappedSpan<Span, &'src str, F>,
        Vec<SpannedToken<'src>>,
        extra::Err<LexerErrorInner<'src>>,
    > + Sized
    + Clone
where
    F: Fn(SimpleSpan) -> Span + 'src,
{
}
impl<'src, P, F, T> Lexer<'src, F, T> for P
where
    P: Parser<
            'src,
            MappedSpan<Span, &'src str, F>,
            Vec<SpannedToken<'src>>,
            extra::Err<LexerErrorInner<'src>>,
        > + Clone,
    F: Fn(SimpleSpan) -> Span + 'src,
{
}

pub fn lexer<'src, F>() -> impl Lexer<'src, F, SpannedToken<'src>>
where
    F: Fn(SimpleSpan) -> Span + 'src,
*/

/*
pub fn lexer<'src, I>(
) -> impl Parser<'src, I, Vec<SpannedToken<'src>>, extra::Err<LexerErrorInner<'src>>>
where
    I: Input<'src, Cursor = usize, Span = Span, Token = char, MaybeToken = char>
        + SliceInput<'src>
        + StrInput<'src>
        + ValueInput<'src>
        + ExactSizeInput<'src>,
*/
trait MapSpan<'src>: Fn(SimpleSpan) -> Span + 'src {}
impl<'src, T: Fn(SimpleSpan) -> Span + 'src> MapSpan<'src> for T {}

type Input<'src, F: MapSpan<'src>> = MappedSpan<Span, &'src str, F>;
type Output<'src> = Vec<SpannedToken<'src>>;
type Extra<'src> = extra::Err<LexerErrorInner<'src>>;

trait Lexer<'src, T, F: MapSpan<'src>>: Parser<'src, Input<'src, F>, T, Extra<'src>> {}
impl<'src, P, T, F> Lexer<'src, T, F> for P
where
    P: Parser<'src, Input<'src, F>, T, Extra<'src>>,
    F: MapSpan<'src>,
{
}

pub fn lexer<'src, F: MapSpan<'src>>() -> impl Lexer<'src, Vec<SpannedToken<'src>>, F> {
    let boolean = choice((
        just("true").to(Token::Boolean(true)),
        just("false").to(Token::Boolean(false)),
    ))
    .labelled("boolean");

    let token = boolean;

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded()
        .repeated()
        .collect()
}

/*
pub fn lexer<'src, F>() -> impl Parser<
    'src,
    MappedSpan<Span, &'src str, F>,
    Vec<SpannedToken<'src>>,
    extra::Err<LexerErrorInner<'src>>,
>
where
    F: Fn(SimpleSpan) -> Span + 'src,
{
    let block = recursive(|block| {
        let indent = just(' ')
            .repeated()
            .configure(|cfg, parent_indent| cfg.exactly(*parent_indent));

        let expr_stmt = expr.then_ignore(text::newline()).to(Stmt::Expr);
        let control_flow = just("loop:")
            .then(text::newline())
            .ignore_then(block)
            .map(Stmt::Loop);
        let stmt = expr_stmt.or(control_flow);

        text::whitespace()
            .count()
            .ignore_with_ctx(stmt.separated_by(indent).collect())
    });

    block.with_ctx(0)
}

fn line_parser() -> impl Lexer<Vec<SpannedToken>> {
    let boolean = choice((
        just("true").to(Token::Boolean(true)),
        just("false").to(Token::Boolean(false)),
    ))
    .labelled("boolean");

    let number = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .try_map(|s, span| {
            Decimal::from_str(&s).map_err(|e| Simple::custom(span, format!("{}", e)))
        })
        .map(Token::Number)
        .labelled("number");

    let escape = just('\\')
        .ignore_then(
            just('\\')
                .or(just('/'))
                .or(just('"'))
                .or(just('b').to('\x08'))
                .or(just('f').to('\x0C'))
                .or(just('n').to('\n'))
                .or(just('r').to('\r'))
                .or(just('t').to('\t')),
        )
        .labelled("escape");

    // TODO parse string interpolations
    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::String)
        .labelled("string");

    let delimiter = choice((
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        just('[').to(Token::LeftBrack),
        just(']').to(Token::RightBrack),
        just('{').to(Token::LeftBrace),
        just('}').to(Token::RightBrace),
    ))
    .labelled("delimiter");

    let keyword = choice((
        just("if").to(Token::If),
        just("then").to(Token::Then),
        just("else").to(Token::Else),
        just("let").to(Token::Let),
        just("in").to(Token::In),
    ))
    .labelled("keyword");

    let control = choice((
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just('.').to(Token::Dot),
        just("=>").to(Token::FatArrow),
    ))
    .labelled("control");

    let operator = choice((
        just('+').to(Token::Plus),
        just('-').to(Token::Dash),
        just('*').to(Token::Star),
        just('/').to(Token::Slash),
        just('>').to(Token::Greater),
        just(">=").to(Token::GreaterEqual),
        just('<').to(Token::Less),
        just("<=").to(Token::LessEqual),
        just("==").to(Token::Equal),
        just("!=").to(Token::NotEqual),
        just("&&").to(Token::And),
        just("||").to(Token::Or),
        just("^").to(Token::Xor),
        just("!").to(Token::Not),
        just("%").to(Token::Rem),
    ))
    .labelled("operator");

    let identifier = ident().map(Token::Identifier).labelled("identifier");

    let token = choice((
        null, boolean, number, string, delimiter, keyword, control, operator, identifier,
    ))
    .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(Spanned::new)
        .padded()
        .repeated()
        .then_ignore(end())
}

pub fn ident<C: text::Character, E: chumsky::Error<C>>(
) -> impl Parser<C, C::Collection, Error = E> + Copy + Clone {
    filter(|c: &C| c.to_char().is_ascii_alphabetic() || c.to_char() == '_' || c.to_char() == '$')
        .map(Some)
        .chain::<C, Vec<_>, _>(
            filter(|c: &C| c.to_char().is_ascii_alphanumeric() || c.to_char() == '_').repeated(),
        )
        .collect()
}
*/

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
