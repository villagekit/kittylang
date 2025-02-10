use chumsky::{
    combinator::RepeatedCfg,
    input::{self, MapExtra},
    prelude::*,
    primitive::JustCfg,
};

use kitty_meta::{ErrorReport, Span, Spanned};

use crate::{token::SpannedToken, Token};

pub(crate) trait Input<'src>:
    input::Input<'src, Cursor = usize, Span = Span, Token = char, MaybeToken = char>
    + input::SliceInput<'src>
    + input::StrInput<'src>
    + input::ValueInput<'src>
    + input::ExactSizeInput<'src>
{
}
impl<'src, I> Input<'src> for I where
    I: input::Input<'src, Cursor = usize, Span = Span, Token = char, MaybeToken = char>
        + input::SliceInput<'src>
        + input::StrInput<'src>
        + input::ValueInput<'src>
        + input::ExactSizeInput<'src>
{
}

type Output<'src> = Vec<SpannedToken<'src>>;
type Extra<'src> = extra::Full<LexerErrorInner<'src>, (), Context>;

pub(crate) trait Lexer<'src, I: Input<'src>, O>: Parser<'src, I, O, Extra<'src>> {}
impl<'src, P, I, O> Lexer<'src, I, O> for P
where
    I: Input<'src>,
    P: Parser<'src, I, O, Extra<'src>>,
{
}

pub fn lexer<'src, I: Input<'src>>() -> impl Lexer<'src, I, Vec<SpannedToken<'src>>> {
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
fn expression_parser<'src, I: Input<'src>>() -> impl Lexer<'src, I, Vec<SpannedToken<'src>>> {
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
*/

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

#[derive(Debug, Default, Clone)]
pub(crate) struct Context {
    pub parent_indent: String,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Delim {
    Block,
    Paren,
    Brace,
    Bracket,
}

#[derive(Clone, Debug, PartialEq)]
enum TokenTree<Token> {
    Tree(Delim, Vec<Self>),
    Token(Token),
}

fn block_parser<'src, I: Input<'src>, Token: 'src>(
    token_parser: impl Lexer<'src, I, Token> + Clone + 'src,
) -> impl Lexer<'src, I, Vec<TokenTree<Token>>> {
    /*
    let empty_lines = text::inline_whitespace()
        .then_ignore(text::newline())
        .repeated();
    */

    let block_parser = recursive(|block_parser| {
        let same_indent = just("".to_string())
            .configure(|cfg: JustCfg<String>, ctx: &Context| cfg.seq(ctx.parent_indent.clone()))
            .map(|_| ());

        let new_indent = just(" ").or(just("\t")).repeated().at_least(1).collect();

        let tokens = token_parser
            .map(TokenTree::Token)
            .repeated()
            .collect()
            .then_ignore(text::newline());

        let block = new_indent
            .map_with(
                |indent: Vec<&str>, extra: &mut MapExtra<'src, '_, I, Extra<'src>>| Context {
                    parent_indent: extra.ctx().parent_indent.clone() + &indent.join(""),
                },
            )
            .ignore_with_ctx(block_parser);

        let item = tokens.or(block).map(|b| TokenTree::Tree(Delim::Block, b));

        item.separated_by(same_indent).collect()
    });

    block_parser.with_ctx(Context {
        parent_indent: "".into(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_parser() {
        use chumsky::input::Input as ChumskyInput;
        use kitty_meta::SourceId;

        #[derive(Clone, Debug, PartialEq)]
        enum Token {
            BlockStart,
            Expr,
        }

        fn line_parser<'src, I: Input<'src>>() -> impl Lexer<'src, I, Token> + Clone {
            let block_start = just("block:").to(Token::BlockStart);
            let expr = just("expr").to(Token::Expr);
            block_start.or(expr)
        }

        let input = r#"
expr
expr
block:
    expr
    block:
        expr
        block:
            expr
    expr
expr
"#;
        let input = input.map_span(move |span| Span::new(SourceId::empty(), span.into_range()));

        let result = block_parser(line_parser()).padded().parse(input);

        let expected = vec![
            TokenTree::Token(Token::Expr),
            TokenTree::Token(Token::Expr),
            TokenTree::Token(Token::BlockStart),
            TokenTree::Tree(
                Delim::Block,
                vec![
                    TokenTree::Token(Token::Expr),
                    TokenTree::Token(Token::BlockStart),
                    TokenTree::Tree(
                        Delim::Block,
                        vec![
                            TokenTree::Token(Token::Expr),
                            TokenTree::Token(Token::BlockStart),
                            TokenTree::Tree(Delim::Block, vec![TokenTree::Token(Token::Expr)]),
                        ],
                    ),
                    TokenTree::Token(Token::Expr),
                ],
            ),
            TokenTree::Token(Token::Expr),
        ];

        assert_eq!(result.errors().next(), None);
        assert_eq!(result.output(), Some(&expected));
    }
}
