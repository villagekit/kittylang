use kitty_syntax::TokenKind;

// Each bit represents whether that bit’s TokenKind is in the set.
//
// This is a TokenSet containing the first and third variants of TokenKind
// (regardless of what they may be):
//
//     0000000000000101
//
// Thus, the number of TokenKind variants must not exceed
// the number of bits in TokenSet.
//
// This implementation is mostly stolen from rust-analyzer:
// https://github.com/rust-analyzer/rust-analyzer/blob/b73b321478d3b2a98d380eb79de717e01620c4e9/crates/parser/src/token_set.rs
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct TokenSet(u128);

impl TokenSet {
    pub(crate) const ALL: Self = Self(u128::MAX);
    pub(crate) const NONE: Self = Self(u128::MIN);

    pub(crate) const fn new<const LEN: usize>(kinds: [TokenKind; LEN]) -> Self {
        let mut value = 0;

        let mut idx = 0;
        while idx < kinds.len() {
            value |= mask(kinds[idx]);
            idx += 1;
        }

        Self(value)
    }

    pub(crate) const fn contains(self, kind: TokenKind) -> bool {
        self.0 & mask(kind) != 0
    }

    pub(crate) const fn union<const LEN: usize>(self, other: [TokenKind; LEN]) -> Self {
        Self(self.0 | TokenSet::new(other).0)
    }

    pub(crate) const fn without(self, kind: TokenKind) -> Self {
        Self(self.0 ^ mask(kind))
    }
}

const fn mask(kind: TokenKind) -> u128 {
    1 << (kind as usize)
}

#[cfg(test)]
#[test]
fn it_works() {
    let set = TokenSet::new([TokenKind::Let, TokenKind::Number]);

    assert!(set.contains(TokenKind::Let));
    assert!(set.contains(TokenKind::Number));
    assert!(!set.contains(TokenKind::String));

    let set = set.union([TokenKind::String]);

    assert!(set.contains(TokenKind::Let));
    assert!(set.contains(TokenKind::Number));
    assert!(set.contains(TokenKind::String));
    assert!(!set.contains(TokenKind::IdentifierVariable));
}
