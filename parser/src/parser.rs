use kitty_lexer::Token;
use kitty_syntax::{NodeKind, TokenKind};

use crate::{
    error::ParseError,
    marker::{CompletedMarker, Marker},
    sink::Event,
    source::Source,
    token_set::TokenSet,
};

pub(crate) struct Parser<'t> {
    source: Source<'t>,
    pub(crate) events: Vec<Option<Event>>,
    errors: Vec<ParseError>,
    expected_kinds: Vec<TokenKind>,
}

impl<'t> Parser<'t> {
    pub(crate) fn new(tokens: &'t [Token]) -> Self {
        let source = Source::new(tokens);
        Parser {
            source,
            events: Vec::new(),
            errors: Vec::new(),
            expected_kinds: Vec::new(),
        }
    }

    pub(crate) fn parse(mut self, grammar: impl Fn(&mut Self)) -> (Vec<Event>, Vec<ParseError>) {
        grammar(&mut self);

        let events = self
            .events
            .into_iter()
            .map(|ev| ev.expect("Expected no empty events"))
            .collect();

        (events, self.errors)
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(None);

        Marker::new(pos)
    }

    pub(crate) fn expect(&mut self, kind: TokenKind, recovery: TokenSet) {
        self.expected_kinds.clear();
        if self.at(kind) {
            self.bump();
        } else {
            self.error(recovery);
        }
    }

    pub(crate) fn error(&mut self, recovery_set: TokenSet) -> Option<CompletedMarker> {
        if self.at_set_raw(&recovery_set) || self.at_end() {
            let current_token = self.source.peek_token();
            let offset = if let Some(Token { range, .. }) = current_token {
                range.start()
            } else {
                self.source.last_token_range().unwrap().end()
            };
            self.errors.push(ParseError::Missing {
                expected: self.expected_kinds.clone(),
                offset,
            });

            let marker = self.start();
            let completed = marker.complete(self, NodeKind::Missing);
            Some(completed)
        } else {
            let current_token = self.source.peek_token();
            let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
                (Some(*kind), *range)
            } else {
                // If we’re at the end of the input we use the range of the very last token in the input.
                (None, self.source.last_token_range().unwrap())
            };
            self.errors.push(ParseError::Unexpected {
                expected: self.expected_kinds.clone(),
                found,
                range,
            });

            Some(self.mark_kind(NodeKind::Error))
        }
    }

    pub(crate) fn mark_kind(&mut self, kind: NodeKind) -> CompletedMarker {
        let m = self.start();
        self.bump();
        m.complete(self, kind)
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.bump();
        self.events.push(Some(Event::AddToken));
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    pub(crate) fn at_set<const LEN: usize>(&mut self, set: [TokenKind; LEN]) -> bool {
        self.expected_kinds.extend_from_slice(&set);
        self.at_set_raw(&TokenSet::new(set))
    }

    fn at_set_raw(&mut self, set: &TokenSet) -> bool {
        self.peek().map_or(false, |k| set.contains(k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_token().map(|token| token.kind)
    }

    pub(crate) fn debug_source(&self) {
        self.source.debug()
    }
}
