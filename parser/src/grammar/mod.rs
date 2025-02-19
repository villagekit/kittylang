mod decl;
mod expr;
mod function;
mod r#type;

use decl::{top_item, TOP_ITEM_FIRST};
use kitty_syntax::NodeKind;

use crate::{token_set::TokenSet, Parser};

pub(crate) fn source(p: &mut Parser<'_>) {
    let m = p.start();

    while !p.at_end() {
        top_item(p, TokenSet::new(TOP_ITEM_FIRST));
    }

    m.complete(p, NodeKind::Source);
}
