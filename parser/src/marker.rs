use drop_bomb::DebugDropBomb;
use kitty_syntax::NodeKind;

use crate::sink::Event;
use crate::Parser;

/// The start of a node the parser has not finished yet.
///
/// A rule that starts a marker must complete or abandon it before it
/// returns. Leaking one is a grammar bug, never an input the parser can
/// meet: the bomb catches it in a debug build, and a release build drops
/// the marker's `None` event as if the marker had been abandoned.
#[derive(Debug)]
pub(crate) struct Marker {
    pos: usize,
    bomb: DebugDropBomb,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DebugDropBomb::new("Marker must be either completed or abandoned"),
        }
    }

    pub(crate) fn complete(mut self, p: &mut Parser<'_>, kind: NodeKind) -> CompletedMarker {
        self.bomb.defuse();
        let old_event = p.events[self.pos].replace(Event::StartNode(kind));
        debug_assert!(old_event.is_none());
        p.events.push(Some(Event::FinishNode));

        CompletedMarker { pos: self.pos }
    }

    /// Abandons the syntax tree node. All its children
    /// are attached to its parent instead.
    ///
    /// The marker's `None` event is popped when it is the last one;
    /// otherwise it stays and `Parser::parse` drops it.
    pub(crate) fn abandon(mut self, p: &mut Parser<'_>) {
        self.bomb.defuse();
        if self.pos + 1 == p.events.len() {
            let own = p.events.pop();
            debug_assert!(matches!(own, Some(None)));
        }
    }
}

#[derive(Debug)]
pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub(crate) fn precede(self, p: &mut Parser<'_>) -> Marker {
        p.events.insert(self.pos, None);
        Marker::new(self.pos)
    }
}
