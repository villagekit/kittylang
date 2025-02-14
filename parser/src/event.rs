use kitty_lexer::{Token, TokenKind};
use kitty_syntax::{NodeKind, SyntaxBuilder, SyntaxTreeBuf};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Event {
    StartNode(NodeKind),
    AddToken,
    FinishNode,
}

pub fn process_events(input: &str, events: &[Event], tokens: &[Token]) -> SyntaxTreeBuf {
    Sink {
        builder: SyntaxBuilder::new(input),
        tokens,
        cursor: 0,
    }
    .process_events(events)
}

struct Sink<'a> {
    builder: SyntaxBuilder,
    tokens: &'a [Token],
    cursor: usize,
}

impl Sink<'_> {
    fn process_events(mut self, events: &[Event]) -> SyntaxTreeBuf {
        assert_eq!(events[events.len() - 1], Event::FinishNode);

        for idx in 0..events.len() - 1 {
            let event = events[idx];
            let next = events[idx + 1];

            match event {
                Event::StartNode(kind) => self.builder.start_node(kind),
                Event::AddToken => self.add_token(),
                Event::FinishNode => self.builder.finish_node(),
            }

            match next {
                Event::StartNode(_) | Event::AddToken => self.skip_trivia(),
                Event::FinishNode => {}
            }
        }

        self.skip_trivia();
        self.builder.finish_node();

        self.builder.finish()
    }

    fn skip_trivia(&mut self) {
        if self.cursor >= self.tokens.len() {
            return;
        };
        let next_token = self.tokens[self.cursor].kind;
        if next_token.is_trivia() {
            self.add_token();
        };
    }

    fn add_token(&mut self) {
        let token = self.tokens[self.cursor];
        self.builder.add_token(token.kind, token.range);
        self.cursor += 1;
    }
}
