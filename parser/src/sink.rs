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
    /// Builds the tree from the parser's events.
    ///
    /// The events must start and finish exactly one root node, which the
    /// parser's marker discipline guarantees.
    pub(crate) fn process(mut self, events: &[Event]) -> SyntaxTreeBuf {
        debug_assert!(matches!(events.first(), Some(Event::StartNode(_))));
        debug_assert!(matches!(events.last(), Some(Event::FinishNode)));

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
        //
        // So trivia is added only before a token or a node start, never
        // before a node finish. The last event is the root's finish, and
        // the trivia at the end of the input goes before it so the tree
        // stays lossless.
        let mut events = events.iter().peekable();
        while let Some(&event) = events.next() {
            match events.peek() {
                Some(Event::StartNode(_) | Event::AddToken) => {
                    self.process_event(event);
                    self.skip_trivia();
                }
                Some(Event::FinishNode) => self.process_event(event),
                None => {
                    self.skip_trivia();
                    self.process_event(event);
                }
            }
        }

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
            .is_some_and(|tok| tok.kind.is_trivia())
    }

    #[inline(always)]
    fn add_token(&mut self) {
        let token = self.tokens[self.cursor];
        self.builder.add_token(token.kind, token.range);
        self.cursor += 1;
    }
}
