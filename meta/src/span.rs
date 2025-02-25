use std::fmt;
use text_size::{TextRange, TextSize};

use crate::source::SourceId;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    source: SourceId,
    range: TextRange,
}

impl Span {
    pub fn new(source: SourceId, range: TextRange) -> Self {
        Self { source, range }
    }

    #[cfg(test)]
    pub fn empty() -> Self {
        Self::new(SourceId::empty(), TextRange::empty(0.into()))
    }

    pub fn source(&self) -> SourceId {
        self.source
    }

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn start(&self) -> TextSize {
        self.range.start()
    }

    pub fn end(&self) -> TextSize {
        self.range.end()
    }

    pub fn union(self, other: Self) -> Self {
        assert_eq!(
            self.source, other.source,
            "attempted to union spans with different sources"
        );
        Self {
            range: self.range.cover(other.range()),
            ..self
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.source, self.range())
    }
}

impl ariadne::Span for Span {
    type SourceId = SourceId;

    fn source(&self) -> &SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.range.start().into()
    }
    fn end(&self) -> usize {
        self.range.end().into()
    }
}
