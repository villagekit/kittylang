use kitty_lexer::Token;
use kitty_syntax::{NodeKind, TokenKind};

use crate::{
    error::ParseError,
    event::Event,
    marker::{CompletedMarker, Marker},
    source::Source,
    token_set::TokenSet,
};

const DEFAULT_RECOVERY_SET: TokenSet =
    TokenSet::new([TokenKind::Newline, TokenKind::Indent, TokenKind::Dedent]);

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

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        self.expect_with_recovery(kind, TokenSet::NONE)
    }

    pub(crate) fn expect_with_recovery(&mut self, kind: TokenKind, recovery: TokenSet) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery(recovery);
        }
    }

    pub(crate) fn error(&mut self) -> Option<CompletedMarker> {
        self.error_with_recovery_raw(DEFAULT_RECOVERY_SET)
    }

    pub(crate) fn error_with_recovery(
        &mut self,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        self.error_with_recovery_raw(recovery_set.union(DEFAULT_RECOVERY_SET))
    }

    pub(crate) fn error_with_recovery_raw(
        &mut self,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        let current_token = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            // If we’re at the end of the input we use the range of the very last token in the
            // input.
            (None, self.source.last_token_range().unwrap())
        };

        self.errors.push(ParseError {
            expected: self.expected_kinds.clone(),
            found,
            range,
        });

        if !self.at_set(&recovery_set) && !self.at_end() {
            let marker = self.start();
            self.bump();
            let completed = marker.complete(self, NodeKind::Error);
            Some(completed)
        } else {
            None
        }
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token();
        self.events.push(Some(Event::AddToken));
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    pub(crate) fn at_set(&mut self, set: &TokenSet) -> bool {
        self.peek().map_or(false, |k| set.contains(k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_token().map(|token| token.kind)
    }
}
