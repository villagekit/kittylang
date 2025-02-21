use drop_bomb::DropBomb;
use kitty_syntax::NodeKind;
use std::mem;

use crate::sink::Event;
use crate::Parser;

#[derive(Debug)]
pub(crate) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DropBomb::new("Marker must be either completed or abandoned"),
        }
    }

    pub(crate) fn complete(mut self, p: &mut Parser<'_>, kind: NodeKind) -> CompletedMarker {
        self.bomb.defuse();
        let old_event = mem::replace(&mut p.events[self.pos], Some(Event::StartNode(kind)));
        debug_assert!(old_event.is_none());
        p.events.push(Some(Event::FinishNode));

        CompletedMarker { pos: self.pos }
    }

    /// Abandons the syntax tree node. All its children
    /// are attached to its parent instead.
    pub(crate) fn abandon(mut self, p: &mut Parser<'_>) {
        self.bomb.defuse();
        if self.pos == p.events.len() - 1 {
            assert!(matches!(p.events.pop(), Some(None)));
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
