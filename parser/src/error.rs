use kitty_lexer::TokenKind;
use std::fmt;
use text_size::{TextRange, TextSize};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Missing {
        expected: Vec<TokenKind>,
        offset: TextSize,
    },
    Unexpected {
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
        range: TextRange,
    },
}

impl ParseError {
    /// The range of source text the error points at. A missing token has
    /// an empty range at the offset where it was expected.
    pub fn range(&self) -> TextRange {
        match self {
            ParseError::Missing { offset, .. } => TextRange::empty(*offset),
            ParseError::Unexpected { range, .. } => *range,
        }
    }

    /// The error's message without its position: what was expected and,
    /// for an unexpected token, what was found.
    pub fn message(&self) -> String {
        let mut message = String::new();
        match self {
            ParseError::Missing { expected, .. } => {
                message.push_str("missing ");
                write_expected(&mut message, expected);
            }
            ParseError::Unexpected {
                expected, found, ..
            } => {
                message.push_str("expected ");
                write_expected(&mut message, expected);
                if let Some(found) = found {
                    message.push_str(&format!(", but found {found}"));
                }
            }
        }
        message
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Missing { offset, .. } => {
                write!(f, "error at {}: ", u32::from(*offset))?;
            }
            ParseError::Unexpected { range, .. } => {
                write!(
                    f,
                    "error at {}..{}: ",
                    u32::from(range.start()),
                    u32::from(range.end()),
                )?;
            }
        }
        f.write_str(&self.message())
    }
}

/// Writes the expected kinds as a list: `a`, `a or b`, `a, b, or c`.
fn write_expected(out: &mut String, expected: &[TokenKind]) {
    let num_expected = expected.len();
    for (idx, expected_kind) in expected.iter().enumerate() {
        let separator = if idx == 0 {
            ""
        } else if idx == num_expected - 1 {
            if num_expected == 2 {
                " or "
            } else {
                ", or "
            }
        } else {
            ", "
        };
        out.push_str(separator);
        out.push_str(&expected_kind.to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range as StdRange;

    fn check(
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
        range: StdRange<u32>,
        output: &str,
    ) {
        let error = ParseError::Unexpected {
            expected,
            found,
            range: {
                let start = range.start.into();
                let end = range.end.into();
                TextRange::new(start, end)
            },
        };

        assert_eq!(format!("{}", error), output);
    }

    #[test]
    fn a_missing_error_has_an_empty_range_at_its_offset_and_a_message_without_it() {
        let error = ParseError::Missing {
            expected: vec![TokenKind::ParenClose],
            offset: 7.into(),
        };
        assert_eq!(error.range(), TextRange::empty(7.into()));
        assert_eq!(error.message(), "missing ‘)’");
        assert_eq!(format!("{}", error), "error at 7: missing ‘)’");
    }

    #[test]
    fn an_unexpected_error_has_its_range_and_a_message_without_it() {
        let error = ParseError::Unexpected {
            expected: vec![TokenKind::Equal],
            found: Some(TokenKind::IdentifierValue),
            range: TextRange::new(10.into(), 20.into()),
        };
        assert_eq!(error.range(), TextRange::new(10.into(), 20.into()));
        assert_eq!(error.message(), "expected ‘=’, but found value-id");
    }

    #[test]
    fn one_expected_did_find() {
        check(
            vec![TokenKind::Equal],
            Some(TokenKind::IdentifierValue),
            10..20,
            "error at 10..20: expected ‘=’, but found value-id",
        );
    }

    #[test]
    fn one_expected_did_not_find() {
        check(
            vec![TokenKind::ParenClose],
            None,
            5..6,
            "error at 5..6: expected ‘)’",
        );
    }

    #[test]
    fn two_expected_did_find() {
        check(
            vec![TokenKind::Plus, TokenKind::Minus],
            Some(TokenKind::Equal),
            0..1,
            "error at 0..1: expected ‘+’ or ‘-’, but found ‘=’",
        );
    }

    #[test]
    fn multiple_expected_did_find() {
        check(
            vec![
                TokenKind::Number,
                TokenKind::IdentifierValue,
                TokenKind::Minus,
                TokenKind::ParenOpen,
            ],
            Some(TokenKind::Let),
            100..105,
            "error at 100..105: expected number, value-id, ‘-’, or ‘(’, but found ‘let’",
        );
    }
}
