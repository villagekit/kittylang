use kitty_lexer::Token;
use kitty_syntax::{NodeKind, TokenKind};
use std::fmt;
use text_size::{TextRange, TextSize};

use crate::event::Event;

pub enum Error {
    Expected { range: TextRange, message: String },
    Missing { offset: TextSize, message: String },
}

pub(crate) struct Parser<'tokens> {
    tokens: &'tokens [Token],
    events: Vec<Event>,
    errors: Vec<Error>,
    cursor: usize,
}

impl<'tokens> Parser<'tokens> {
    pub(crate) fn new(tokens: &'tokens [Token]) -> Self {
        Parser {
            tokens,
            events: Vec::new(),
            errors: Vec::new(),
            cursor: 0,
        }
    }

    pub(crate) fn parse(mut self, grammar: impl Fn(&mut Self)) -> (Vec<Event>, Vec<Error>) {
        grammar(&mut self);

        (self.events, self.errors)
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        self.expect_with_name(kind, &format!("{kind}"));
    }

    pub(crate) fn expect_with_name(&mut self, kind: TokenKind, name: &str) {
        if self.at(kind) {
            self.bump_any();
            return;
        }

        self.error(name);
    }

    pub(crate) fn bump(&mut self, kind: TokenKind) {
        assert!(self.at(kind));
        self.bump_any();
    }

    pub(crate) fn at(&self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }

    pub(crate) fn peek(&self) -> Option<TokenKind> {
        if self.eof() {
            return None;
        }
        Some(self.tokens[self.cursor].kind)
    }

    pub(crate) fn lookahead(&self) -> Option<TokenKind> {
        self.tokens.get(self.cursor + 1).map(|t| t.kind)
    }

    pub(crate) fn error(&mut self, message: &str) {
        if self.at_recovery() {
            self.errors.push(Error::Missing {
                offset: self.tokens[self.cursor - 1].range.end(),
                message: message.to_string(),
            });
            return;
        }

        self.errors.push(Error::Expected {
            range: self.tokens[self.cursor].range,
            message: message.to_string(),
        });
        self.start_node(NodeKind::Error);
        self.bump_any();
        self.finish_node();
    }

    pub(crate) fn error_without_recovery(&mut self, message: &str) {
        self.errors.push(Error::Expected {
            range: self.tokens[self.cursor].range,
            message: message.to_string(),
        });
        self.start_node(NodeKind::Error);
        self.bump_any();
        self.finish_node();
    }

    pub(crate) fn at_recovery(&self) -> bool {
        self.eof()
            || self.at(TokenKind::BraceOpen)
            || self.at(TokenKind::BraceClose)
            || self.at(TokenKind::Struct)
    }

    pub(crate) fn start_node(&mut self, kind: NodeKind) {
        self.events.push(Event::StartNode(kind));
    }

    pub(crate) fn bump_any(&mut self) {
        assert!(!self.eof());
        self.events.push(Event::AddToken);
        self.cursor += 1;
    }

    pub(crate) fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    pub(crate) fn eof(&self) -> bool {
        self.cursor == self.tokens.len()
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Expected { range, message } => {
                write!(f, "error at {:?}: expected {}", range, message)
            }
            Error::Missing { offset, message } => {
                write!(f, "error at {:?}: missing {}", offset, message)
            }
        }
    }
}
