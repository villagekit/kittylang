mod error;
mod event;
mod grammar;
mod marker;
mod parser;
mod token_set;

use kitty_cst::{CstNode, Source};
use kitty_lexer::{lex, Token};
use kitty_syntax::SyntaxTreeBuf;
use std::fmt;

use crate::error::SyntaxError;
use crate::event::{process_events, Event};
use crate::parser::Parser;

pub fn parse(input: &str) -> Parse<Source> {
    let tokens: Vec<Token> = lex(input).collect();
    let (events, errors) = Parser::new(&tokens).parse(grammar::source);
    let tree = process_events(input, &events, &tokens);
    let node = Source::cast(tree.root(), &tree).unwrap();
    Parse { tree, node, errors }
}

pub struct Parse<N: CstNode> {
    pub tree: SyntaxTreeBuf,
    pub node: N,
    pub errors: Vec<SyntaxError>,
}

impl<N: CstNode> fmt::Debug for Parse<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.tree)?;

        for error in &self.errors {
            writeln!(f, "{error:?}")?;
        }

        Ok(())
    }
}
