use kitty_lexer::{Token, TokenKind};
use text_size::TextSize;

pub(crate) struct Source<'t> {
    tokens: &'t [Token],
    cursor: usize,
}

impl<'t> Source<'t> {
    pub(crate) fn new(tokens: &'t [Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    /// Moves past the current token. Returns `false` at the end of the
    /// input, where the cursor stays put.
    pub(crate) fn bump(&mut self) -> bool {
        self.eat_trivia();
        if self.cursor < self.tokens.len() {
            self.cursor += 1;
            true
        } else {
            false
        }
    }

    pub(crate) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.eat_trivia();
        self.peek_kind_raw()
    }

    pub(crate) fn peek_token(&mut self) -> Option<&Token> {
        self.eat_trivia();
        self.peek_token_raw()
    }

    pub(crate) fn lookahead_kind(&mut self, nth: usize) -> Option<TokenKind> {
        self.tokens[self.cursor..]
            .iter()
            .map(|Token { kind, .. }| kind)
            .filter(|kind| !kind.is_trivia())
            .nth(nth)
            .cloned()
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().is_some_and(TokenKind::is_trivia)
    }

    /// The offset just past the last token, or zero when there are no
    /// tokens.
    pub(crate) fn end_offset(&self) -> TextSize {
        self.tokens
            .last()
            .map_or(TextSize::default(), |Token { range, .. }| range.end())
    }

    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.peek_token_raw().map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }
}
