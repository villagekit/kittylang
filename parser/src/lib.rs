mod error;
mod event;
mod grammar;
mod marker;
mod parser;
mod source;
mod token_set;

use kitty_cst::{CstNode, Source};
use kitty_lexer::{lex, Token};
use kitty_syntax::SyntaxTreeBuf;
use std::fmt;

pub use crate::error::ParseError;
use crate::event::process_events;
use crate::parser::Parser;

pub fn parse(input: &str) -> Parse<Source> {
    parse_grammar(grammar::source, input)
}

pub(crate) fn parse_grammar<Node: CstNode>(
    grammar: impl Fn(&mut Parser<'_>),
    input: &str,
) -> Parse<Node> {
    let tokens: Vec<Token> = lex(input).collect();
    let (events, errors) = Parser::new(&tokens).parse(grammar);
    println!("events: {:?}", events);
    let tree = process_events(input, &events, &tokens);
    let node = Node::cast(tree.root(), &tree).unwrap();
    Parse { tree, node, errors }
}

pub struct Parse<N: CstNode> {
    pub tree: SyntaxTreeBuf,
    pub node: N,
    pub errors: Vec<ParseError>,
}

impl<N: CstNode> fmt::Debug for Parse<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tree = format!("{:#?}", self.tree);
        write!(f, "{}", &tree[0..tree.len() - 1])?;

        for error in &self.errors {
            write!(f, "\n{error:?}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
fn check_grammar<Node: CstNode>(
    grammar: impl Fn(&mut Parser<'_>),
    input: &str,
    expected: expect_test::Expect,
) {
    let result: Parse<Node> = parse_grammar(grammar, input);
    let actual = format!("{:?}", result);
    expected.assert_eq(&actual);
}
