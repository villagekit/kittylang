use kitty_lexer::Token;
use kitty_syntax::{NodeKind, SyntaxBuilder, SyntaxTreeBuf};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Event {
    StartNode(NodeKind),
    AddToken,
    FinishNode,
}

pub(crate) struct Sink<'t> {
    builder: SyntaxBuilder,
    tokens: &'t [Token],
    cursor: usize,
}

impl<'t> Sink<'t> {
    pub(crate) fn new(input: &str, tokens: &'t [Token]) -> Self {
        Self {
            builder: SyntaxBuilder::new(input),
            tokens,
            cursor: 0,
        }
    }
    pub(crate) fn process(mut self, events: &[Event]) -> SyntaxTreeBuf {
        // the first event always starts the root node,
        // and the last event always finishes that node
        assert!(matches!(events.get(0), Some(Event::StartNode { .. })));
        assert!(matches!(events.last(), Some(Event::FinishNode)));

        // We want to avoid nodes having trailing trivia:
        //
        // BinaryExpr
        //   BinaryExpr
        //     [1] [ ] [*] [ ] [2] [ ]
        //   [+] [ ] [3]
        //
        // An error attached to the nested BinaryExpr would include
        // the trailing whitespace in its range:
        //
        // 1 * 2 + 3
        // ^^^^^^

        // we go through all events apart from the last one,
        // since we can’t peek what the next event is when we’re at the last
        // and thus need to handle it specially

        let mut current = events.as_ptr();
        let mut next = unsafe { current.add(1) };
        let last = &events[events.len() - 1] as *const _;

        while current != last {
            self.process_event(unsafe { *current });

            match unsafe { *next } {
                Event::StartNode(_) | Event::AddToken => self.skip_trivia(),
                Event::FinishNode => {}
            }

            current = next;
            next = unsafe { current.add(1) };
        }

        // unconditionally skip any trivia before processing the last event
        // to ensure we don’t miss trailing trivia at the end of the input
        self.skip_trivia();
        self.process_event(unsafe { *last });

        self.builder.finish()
    }

    #[inline(always)]
    fn process_event(&mut self, event: Event) {
        match event {
            Event::StartNode(kind) => self.builder.start_node(kind),
            Event::FinishNode => self.builder.finish_node(),
            Event::AddToken => self.add_token(),
        }
    }

    #[inline(always)]
    fn skip_trivia(&mut self) {
        while self.next_token_is_trivia() {
            self.add_token();
        }
    }

    #[inline(always)]
    fn next_token_is_trivia(&self) -> bool {
        self.tokens
            .get(self.cursor)
            .map_or(false, |tok| tok.kind.is_trivia())
    }

    #[inline(always)]
    fn add_token(&mut self) {
        let token = self.tokens[self.cursor];
        self.builder.add_token(token.kind, token.range);
        self.cursor += 1;
    }
}
