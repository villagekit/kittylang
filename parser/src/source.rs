use kitty_lexer::{Token, TokenKind};
use text_size::TextRange;

pub(crate) struct Source<'t> {
    tokens: &'t [Token],
    cursor: usize,
}

impl<'t> Source<'t> {
    pub(crate) fn new(tokens: &'t [Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(crate) fn bump(&mut self) {
        self.eat_trivia();
        self.cursor += 1;
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
        self.peek_kind_raw().map_or(false, TokenKind::is_trivia)
    }

    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        self.tokens.last().map(|Token { range, .. }| *range)
    }

    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.peek_token_raw().map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    pub(crate) fn debug(&self) {
        println!("next token: {:?}", self.tokens.get(self.cursor));
        println!("next+1 token: {:?}", self.tokens.get(self.cursor + 1));
        println!("next+2 token: {:?}", self.tokens.get(self.cursor + 2));
    }
}
