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

        // A `None` event is where a marker was started and then
        // abandoned mid-stream: the node was never made, so the event
        // is dropped and the node's children attach to its parent.
        let events = self.events.into_iter().flatten().collect();

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

    /// Records an error at the current token and returns the node that
    /// stands in for what was expected.
    ///
    /// A token the caller cannot continue from is consumed into an `Error`
    /// node. A token in the recovery set, or the end of the input, is left
    /// for the caller and an empty `Missing` node marks where it was
    /// expected.
    pub(crate) fn error(&mut self, recovery_set: TokenSet) -> CompletedMarker {
        let expected = std::mem::take(&mut self.expected_kinds);

        match self.source.peek_token() {
            Some(&Token { kind, range, .. }) if !recovery_set.contains(kind) => {
                self.errors.push(ParseError::Unexpected {
                    expected,
                    found: Some(kind),
                    range,
                });
                self.mark_kind(NodeKind::Error)
            }
            current => {
                // At the end of the input the `Missing` node sits after
                // the last token, or at the start of an input with none.
                let offset = match current {
                    Some(token) => token.range.start(),
                    None => self.source.end_offset(),
                };
                self.errors.push(ParseError::Missing { expected, offset });
                self.mark_kind_empty(NodeKind::Missing)
            }
        }
    }

    /// Records an `Unexpected` error at the current token and leaves the
    /// token in place. The caller then parses the construct it begins, so
    /// a construct in the wrong place is one error, not one per token.
    ///
    /// At the end of the input this records a `Missing` error instead, as
    /// `error` does; a caller checks `at_recovery` first.
    pub(crate) fn error_misplaced(&mut self) {
        let expected = std::mem::take(&mut self.expected_kinds);
        match self.source.peek_token() {
            Some(&Token { kind, range, .. }) => {
                self.errors.push(ParseError::Unexpected {
                    expected,
                    found: Some(kind),
                    range,
                });
            }
            None => {
                let offset = self.source.end_offset();
                self.errors.push(ParseError::Missing { expected, offset });
            }
        }
    }

    pub(crate) fn mark_kind(&mut self, kind: NodeKind) -> CompletedMarker {
        let m = self.start();
        self.bump();
        m.complete(self, kind)
    }

    pub(crate) fn mark_kind_empty(&mut self, kind: NodeKind) -> CompletedMarker {
        let m = self.start();
        m.complete(self, kind)
    }

    /// Consumes the current token into the tree.
    ///
    /// A rule only bumps a token it has looked at, so the end of the input
    /// is never bumped; if it were, nothing happens.
    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        let consumed = self.source.bump();
        debug_assert!(consumed, "bump at the end of the input");
        if consumed {
            self.events.push(Some(Event::AddToken));
        }
    }

    pub(crate) fn bump_if_at(&mut self, kind: TokenKind) -> bool {
        let is_at_kind = self.at(kind);
        if is_at_kind {
            self.bump();
        }
        is_at_kind
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    pub(crate) fn at_set<const LEN: usize>(&mut self, set: [TokenKind; LEN]) -> bool {
        self.expected_kinds.extend_from_slice(&set);
        self.at_set_raw(&TokenSet::new(set))
    }

    pub(crate) fn lookahead_at(&mut self, nth: usize, kind: TokenKind) -> bool {
        self.source.lookahead_kind(nth) == Some(kind)
    }

    /// Whether the next token is one the caller continues from, or the
    /// input has ended: the place `error` consumes nothing. A loop over
    /// items with no separator ends here, so it always makes progress.
    /// The recovery kinds are not expected kinds, so none is recorded.
    pub(crate) fn at_recovery(&mut self, recovery: TokenSet) -> bool {
        self.peek().is_none_or(|kind| recovery.contains(kind))
    }

    /// Whether the next token is in `set`, without recording the set as
    /// expected: for a rule choosing a path, not a rule expecting a token.
    pub(crate) fn peek_in(&mut self, set: TokenSet) -> bool {
        self.peek().is_some_and(|kind| set.contains(kind))
    }

    fn at_set_raw(&mut self, set: &TokenSet) -> bool {
        self.peek().is_some_and(|k| set.contains(k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }
}
