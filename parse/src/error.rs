use kitty_meta::ErrorReport;

use crate::lexer::LexerError;
use crate::parser::ParserError;

pub enum ParseError<'src> {
    Lexer(LexerError<'src>),
    Compiler(ParserError<'src>),
}

impl From<ParseError<'_>> for ErrorReport {
    fn from(value: ParseError) -> Self {
        match value {
            ParseError::Lexer(error) => ErrorReport {
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
            },
            ParseError::Compiler(error) => ErrorReport {
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
            },
        }
    }
}
