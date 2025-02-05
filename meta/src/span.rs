use chumsky::span::Span as ChumskySpan;
use std::{fmt, ops::Range};

use crate::source::SourceId;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    source: SourceId,
    range: (usize, usize),
}

impl Span {
    #[cfg(test)]
    pub fn empty() -> Self {
        Self::new(SourceId::empty(), 0..0)
    }

    pub fn source(&self) -> SourceId {
        self.source
    }

    pub fn range(&self) -> Range<usize> {
        Range {
            start: self.start(),
            end: self.end(),
        }
    }

    pub fn union(self, other: Self) -> Self {
        assert_eq!(
            self.source, other.source,
            "attempted to union spans with different sources"
        );
        Self {
            range: (self.start().min(other.start()), self.end().max(other.end())),
            ..self
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.source, self.range())
    }
}

impl ChumskySpan for Span {
    type Context = SourceId;
    type Offset = usize;

    fn new(source: SourceId, range: Range<usize>) -> Self {
        assert!(range.start <= range.end);
        Self {
            source,
            range: (range.start, range.end),
        }
    }

    fn context(&self) -> SourceId {
        self.source
    }
    fn start(&self) -> Self::Offset {
        self.range.0
    }
    fn end(&self) -> Self::Offset {
        self.range.1
    }
}

impl ariadne::Span for Span {
    type SourceId = SourceId;

    fn source(&self) -> &SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.range.0
    }
    fn end(&self) -> usize {
        self.range.1
    }
}

pub type Spanned<T> = (T, Span);
