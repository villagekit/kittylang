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

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_expected(f: &mut fmt::Formatter<'_>, expected: &[TokenKind]) -> fmt::Result {
            let num_expected = expected.len();
            let is_first = |idx| idx == 0;
            let is_last = |idx| idx == num_expected - 1;

            for (idx, expected_kind) in expected.iter().enumerate() {
                if is_first(idx) {
                    write!(f, "{}", expected_kind)?;
                } else if num_expected == 2 && is_last(idx) {
                    write!(f, " or {}", expected_kind)?;
                } else if num_expected > 2 && is_last(idx) {
                    write!(f, ", or {}", expected_kind)?;
                } else {
                    write!(f, ", {}", expected_kind)?;
                }
            }

            Ok(())
        }

        match self {
            ParseError::Missing { expected, offset } => {
                write!(f, "error at {}: missing ", u32::from(*offset))?;

                write_expected(f, expected)?;
            }
            ParseError::Unexpected {
                expected,
                found,
                range,
            } => {
                write!(
                    f,
                    "error at {}..{}: expected ",
                    u32::from(range.start()),
                    u32::from(range.end()),
                )?;

                write_expected(f, expected)?;

                if let Some(found) = found {
                    write!(f, ", but found {}", found)?;
                }
            }
        }
        Ok(())
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
    fn one_expected_did_find() {
        check(
            vec![TokenKind::Equal],
            Some(TokenKind::IdentifierVariable),
            10..20,
            "error at 10..20: expected ‘=’, but found identifier",
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
                TokenKind::IdentifierVariable,
                TokenKind::Minus,
                TokenKind::ParenOpen,
            ],
            Some(TokenKind::Let),
            100..105,
            "error at 100..105: expected number, identifier, ‘-’, or ‘(’, but found ‘let’",
        );
    }
}
