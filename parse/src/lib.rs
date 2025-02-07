mod lexer;
// mod parser;
mod token;

pub use lexer::LexerError;
// pub use parser::ParserError;
pub use token::{SpannedToken, Token};

// use chumsky::input::BorrowInput;
use chumsky::prelude::*;
// use kitty_ast::SpannedExpression;
use kitty_meta::{SourceId, Span};

use crate::lexer::lexer;
// use crate::parser::parser;

pub fn lex<'src>(
    code: &'src str,
    source: &'src SourceId,
) -> (Option<Vec<SpannedToken<'src>>>, Vec<LexerError<'src>>) {
    let code = code.map_span(move |span| Span::new(*source, span.into_range()));
    let (tokens, errors) = lexer().parse(code).into_output_errors();
    let errors: Vec<LexerError> = errors.into_iter().map(LexerError).collect();
    (tokens, errors)
}

/*
fn make_input<'src>(
    eoi: Span,
    tokens: &'src [SpannedToken<'src>],
) -> impl BorrowInput<'src, Token = Token<'src>, Span = Span> {
    tokens.map(eoi, |(t, s)| (t, s))
}

pub fn parse<'src>(
    code: &'src str,
    source: &'src SourceId,
    tokens: &'src [SpannedToken<'src>],
) -> (Option<SpannedExpression<'src>>, Vec<ParserError<'src>>) {
    let len = code.chars().count();
    let eoi = Span::new(*source, 0..len);

    let (expr, errors) = parser(make_input)
        .parse(make_input(eoi, tokens))
        .into_output_errors();
    let errors: Vec<ParserError> = errors.into_iter().map(ParserError).collect();
    (expr, errors)
}
*/
