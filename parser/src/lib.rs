mod error;
mod grammar;
mod marker;
mod parser;
mod sink;
mod source;
mod token_set;

use kitty_cst::{CstNode, Module};
use kitty_lexer::{lex, Token};
use kitty_syntax::SyntaxTreeBuf;
use std::fmt;

pub use crate::error::ParseError;
use crate::parser::Parser;
use crate::sink::Sink;

pub fn parse(input: &str) -> Parse<Module> {
    parse_grammar(
        |p: &mut Parser<'_>| {
            grammar::module(p);
        },
        input,
    )
}

pub(crate) fn parse_grammar<Node: CstNode>(
    grammar: impl Fn(&mut Parser<'_>),
    input: &str,
) -> Parse<Node> {
    let tokens: Vec<Token> = lex(input).collect();
    let (events, errors) = Parser::new(&tokens).parse(grammar);
    println!("events: {:?}", events);
    let tree = Sink::new(input, &tokens).process(&events);
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
            write!(f, "\n{error}")?;
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
