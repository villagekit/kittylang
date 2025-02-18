use std::collections::BTreeSet;

use kitty_lexer::TokenKind;

#[derive(Debug, Clone)]
pub(crate) struct TokenSet(BTreeSet<TokenKind>);

impl TokenSet {
    pub(crate) fn new<const LEN: usize>(items: [TokenKind; LEN]) -> Self {
        Self(items.iter().cloned().collect())
    }

    pub(crate) fn none() -> Self {
        Self::new([])
    }

    pub(crate) fn contains(&self, item: &TokenKind) -> bool {
        self.0.contains(item)
    }

    pub(crate) fn union(self, other: &TokenSet) -> Self {
        Self(self.0.union(&other.0).cloned().collect())
    }
}

impl From<&TokenSet> for Vec<TokenKind> {
    fn from(value: &TokenSet) -> Self {
        value.0.iter().cloned().collect()
    }
}
