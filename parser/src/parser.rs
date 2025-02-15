use kitty_lexer::Token;
use kitty_syntax::{NodeKind, TokenKind};
use std::{cell::Cell, rc::Rc};

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

    fn at_set(&mut self, set: &TokenSet) -> bool {
        self.peek().map_or(false, |k| set.contains(k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_token().map(|token| token.kind)
    }

    /*

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        self.expect_with_recovery_set(kind, TokenSet::NONE)
    }

    pub(crate) fn expect_with_recovery_set(&mut self, kind: TokenKind, recovery_set: TokenSet) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set(recovery_set);
        }
    }

    pub(crate) fn expect_with_recovery_set_no_default(
        &mut self,
        kind: TokenKind,
        recovery_set: TokenSet,
    ) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set_no_default(recovery_set);
        }
    }

    pub(crate) fn expect_with_no_skip(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_no_skip();
        }
    }

    pub(crate) fn error_with_recovery_set(
        &mut self,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        self.error_with_recovery_set_no_default(recovery_set.union(DEFAULT_RECOVERY_SET))
    }

    #[allow(unused)]
    pub(crate) fn error(&mut self) -> Option<CompletedMarker> {
        self.error_with_recovery_set_no_default(DEFAULT_RECOVERY_SET)
    }

    pub(crate) fn error_with_no_skip(&mut self) -> Option<CompletedMarker> {
        self.error_with_recovery_set_no_default(TokenSet::ALL)
    }

    pub(crate) fn error_with_skip(&mut self) -> Option<CompletedMarker> {
        self.error_with_recovery_set_no_default(TokenSet::NONE)
    }

    pub(crate) fn error_with_recovery_set_no_default(
        &mut self,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        // we must have been expecting something if there was an error
        let expected_syntax = self.expected_syntax.take().unwrap();
        self.expected_syntax_tracking_state
            .set(ExpectedSyntaxTrackingState::Unnamed);

        if self.at_eof() || self.at_set(recovery_set) {
            let range = self.previous_token_range();
            self.errors.push(SyntaxError {
                expected_syntax,
                kind: SyntaxErrorKind::Missing {
                    offset: range.end(),
                },
            });

            return None;
        }

        let token = self.tokens[self.token_idx];
        self.errors.push(SyntaxError {
            expected_syntax,
            kind: SyntaxErrorKind::UnexpectedToken {
                found: token.kind,
                range: token.range,
            },
        });

        let m = self.start();
        self.bump();
        Some(m.complete(self, NodeKind::Error))
    }

    pub(crate) fn mark_old_error(
        &mut self,
        found: NodeKind,
        start_token_idx: usize,
        end_token_idx: usize,
        expected: ExpectedSyntax,
    ) {
        let start_token = self.tokens[start_token_idx];
        let end_token = self.tokens[end_token_idx];
        let range = start_token.range.cover(end_token.range);
        self.errors.push(SyntaxError {
            expected_syntax: expected,
            kind: SyntaxErrorKind::UnexpectedNode { found, range },
        });
    }

    #[must_use]
    pub(crate) fn expected_syntax_name(&mut self, name: &'static str) -> ExpectedSyntaxGuard {
        self.expected_syntax_tracking_state
            .set(ExpectedSyntaxTrackingState::Named);
        self.expected_syntax = Some(ExpectedSyntax::Named(name));

        ExpectedSyntaxGuard::new(Rc::clone(&self.expected_syntax_tracking_state))
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(None);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        if let ExpectedSyntaxTrackingState::Unnamed = self.expected_syntax_tracking_state.get() {
            self.expected_syntax = Some(ExpectedSyntax::Unnamed(kind));
        }

        self.skip_trivia();
        self.at_raw(kind)
    }

    pub(crate) fn at_ahead(&mut self, offset: usize, set: TokenSet) -> bool {
        let original_token_idx = self.token_idx;

        for _ in 0..offset {
            self.skip_trivia();
            self.token_idx += 1;
            if self.at_eof() {
                self.token_idx = original_token_idx;
                return false;
            }
        }
        let res = self.at_set(set);

        self.token_idx = original_token_idx;

        res
    }

    pub(crate) fn at_eof_ahead(&mut self, offset: usize) -> bool {
        let original_token_idx = self.token_idx;

        for _ in 0..offset {
            self.skip_trivia();
            self.token_idx += 1;
            if self.at_eof() {
                self.token_idx = original_token_idx;
                return true;
            }
        }
        let res = self.at_eof();

        self.token_idx = original_token_idx;

        res
    }

    pub(crate) fn at_eof(&mut self) -> bool {
        self.skip_trivia();
        self.token_idx >= self.tokens.len()
    }

    pub(crate) fn at_default_recovery_set(&mut self) -> bool {
        self.at_set(DEFAULT_RECOVERY_SET)
    }

    pub(crate) fn at_set(&mut self, set: TokenSet) -> bool {
        self.skip_trivia();
        self.peek_raw().is_some_and(|kind| set.contains(kind))
    }

    pub(crate) fn kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_raw()
    }

    pub(crate) fn bump(&mut self) {
        self.clear_expected_syntaxes();
        self.events.push(Some(Event::AddToken));
        self.token_idx += 1;
    }

    fn clear_expected_syntaxes(&mut self) {
        self.expected_syntax = None;
        self.expected_syntax_tracking_state
            .set(ExpectedSyntaxTrackingState::Unnamed);
    }

    fn previous_token_range(&mut self) -> TextRange {
        let mut previous_token_idx = if let Some(idx) = self.token_idx.checked_sub(1) {
            idx
        } else {
            return self.tokens[self.token_idx].range;
        };

        while self.tokens[previous_token_idx].kind.is_trivia() {
            previous_token_idx = if let Some(idx) = previous_token_idx.checked_sub(1) {
                idx
            } else {
                return self.tokens[self.token_idx].range;
            }
        }

        self.tokens[previous_token_idx].range
    }

    pub(crate) fn previous_token_kind(&mut self) -> TokenKind {
        let mut previous_token_idx = if let Some(idx) = self.token_idx.checked_sub(1) {
            idx
        } else {
            return self.tokens[self.token_idx].kind;
        };

        while self.tokens[previous_token_idx].kind.is_trivia() {
            previous_token_idx = if let Some(idx) = previous_token_idx.checked_sub(1) {
                idx
            } else {
                return self.tokens[self.token_idx].kind;
            }
        }

        self.tokens[previous_token_idx].kind
    }

    pub(crate) fn peek(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_raw()
    }

    fn skip_trivia(&mut self) {
        while self.at_trivia() {
            self.token_idx += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_raw().map_or(false, |kind| kind.is_trivia())
    }

    fn at_raw(&self, kind: TokenKind) -> bool {
        self.peek_raw().is_some_and(|k| k == kind)
    }

    fn peek_raw(&self) -> Option<TokenKind> {
        self.tokens.get(self.token_idx).map(|token| token.kind)
    }
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    pub(crate) fn error(&mut self) {
        let current_token = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            // If we’re at the end of the input we use the range of the very last token in the
            // input.
            (None, self.source.last_token_range().unwrap())
        };

        self.events.push(Event::Error(ParseError {
            expected: mem::take(&mut self.expected_kinds),
            found,
            range,
        }));

        if !self.at_set(&RECOVERY_SET) && !self.at_end() {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::Error);
        }
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    fn at_set(&mut self, set: &[TokenKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }
    */
}

pub(crate) struct ExpectedSyntaxGuard {
    expected_syntax_tracking_state: Rc<Cell<ExpectedSyntaxTrackingState>>,
}

impl ExpectedSyntaxGuard {
    fn new(expected_syntax_tracking_state: Rc<Cell<ExpectedSyntaxTrackingState>>) -> Self {
        Self {
            expected_syntax_tracking_state,
        }
    }
}

impl Drop for ExpectedSyntaxGuard {
    fn drop(&mut self) {
        self.expected_syntax_tracking_state
            .set(ExpectedSyntaxTrackingState::Unnamed);
    }
}

#[derive(Debug, Clone, Copy)]
enum ExpectedSyntaxTrackingState {
    Named,
    Unnamed,
}
