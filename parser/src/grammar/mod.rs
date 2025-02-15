mod decl;
mod expr;

use kitty_syntax::NodeKind;

use crate::Parser;

pub(crate) fn source(p: &mut Parser<'_>) {
    let m = p.start();

    while !p.at_end() {
        parse_decl(p);
    }

    m.complete(p, NodeKind::Source);
}
