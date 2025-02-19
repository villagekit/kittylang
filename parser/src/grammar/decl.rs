use kitty_syntax::NodeKind;

use crate::{marker::CompletedMarker, parser::Parser, token_set::TokenSet};

pub(crate) fn decl(p: &mut Parser, recovery: TokenSet) -> CompletedMarker {
    p.start().complete(p, NodeKind::TypeDecl)
}
