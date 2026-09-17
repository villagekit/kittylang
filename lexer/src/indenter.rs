use logos::{Logos, Span};
use peek_again::Peekable;
use std::{cmp::Ordering, collections::VecDeque};

use crate::token::TokenKind;

pub trait TokenKindIterator<'src>:
    Iterator<Item = (Result<TokenKind, <TokenKind as Logos<'src>>::Error>, Span)>
{
}
impl<'src, I> TokenKindIterator<'src> for I where
    I: Iterator<Item = (Result<TokenKind, <TokenKind as Logos<'src>>::Error>, Span)>
{
}

pub struct Indenter<'src, I: TokenKindIterator<'src>> {
    source: &'src str,
    tokens: Peekable<I>,
    indents: Vec<usize>,
    queued_tokens: VecDeque<(TokenKind, Span)>,
}

impl<'src, I: TokenKindIterator<'src>> Indenter<'src, I> {
    pub fn new(source: &'src str, tokens: I) -> Self {
        let tokens = Peekable::new(tokens);
        let indents = vec![0];
        let queued_tokens = VecDeque::new();
        Self {
            source,
            tokens,
            indents,
            queued_tokens,
        }
    }

    fn get_next_indent_level(&self, ws_span: Span) -> usize {
        indent_level(&self.source[ws_span])
    }

    fn pop_and_queue_dedents(&mut self, indent: usize, dedent_span: Span) {
        // Pop until we match the new level
        while let Some(&level) = self.indents.last() {
            if level > indent {
                self.indents.pop();

                // Queuing Dedent tokens.
                self.queued_tokens
                    .push_back((TokenKind::Dedent, dedent_span.clone()));
            } else {
                break;
            }
        }
    }

    fn pop_dedent(
        &mut self,
        dedent_span: Span,
    ) -> Option<(Result<TokenKind, <TokenKind as Logos<'src>>::Error>, Span)> {
        if self.indents.len() > 1 {
            self.indents.pop();
            Some((Ok(TokenKind::Dedent), dedent_span))
        } else {
            None
        }
    }
}

impl<'src, I: TokenKindIterator<'src>> Iterator for Indenter<'src, I> {
    type Item = (Result<TokenKind, <TokenKind as Logos<'src>>::Error>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        // If we have some tokens queued, return them first
        if let Some((token, span)) = self.queued_tokens.pop_front() {
            return Some((Ok(token), span));
        }

        // Get the next token.
        let next = self.tokens.next();

        match next {
            Some((Ok(token), span)) => {
                let mut peek = self.tokens.peek();
                let ahead = peek.get().cloned();
                let ahead_2 = peek.peek().cloned();

                if token != TokenKind::Newline {
                    // Return the next token
                    return Some((Ok(token), span));
                }

                // If newline followed by an whitespace followed by newline
                if let (
                    Some((Ok(TokenKind::Whitespace), ws_span)),
                    Some((Ok(TokenKind::Newline), _)),
                ) = (ahead.clone(), ahead_2)
                {
                    // Skip and queue the whitespace
                    self.tokens.next();
                    self.queued_tokens
                        .push_back((TokenKind::Whitespace, ws_span));
                }
                // If newline followed by newline
                else if let Some((Ok(TokenKind::Newline), _)) = ahead {
                    // Do nothing
                }
                // If newline followed by a whitespace (but not on an empty line),
                //   Return / queue any indents
                else if let Some((Ok(TokenKind::Whitespace), ws_span)) = ahead {
                    let indent = self.get_next_indent_level(ws_span.clone());

                    // Get the current indent level (the top of our stack).
                    let current_indent = *self.indents.last().unwrap();

                    match indent.cmp(&current_indent) {
                        Ordering::Greater => {
                            // Increased indent:

                            // Skip and queue whitespace as being up to existing indent level
                            self.tokens.next();
                            let split = ws_span.start
                                + split_at_level(&self.source[ws_span.clone()], current_indent);
                            let revised_ws_span = ws_span.start..split;
                            if !revised_ws_span.is_empty() {
                                self.queued_tokens
                                    .push_back((TokenKind::Whitespace, revised_ws_span));
                            }

                            // Push new indent level
                            self.indents.push(indent);

                            // Queue an indent token.
                            let indent_span = split..ws_span.end;
                            self.queued_tokens
                                .push_back((TokenKind::Indent, indent_span));
                        }
                        Ordering::Less => {
                            // Decreased indent:

                            // Skip and queue whitespace as-is
                            self.tokens.next();
                            self.queued_tokens
                                .push_back((TokenKind::Whitespace, ws_span.clone()));

                            let dedent_span = ws_span.end..ws_span.end;
                            self.pop_and_queue_dedents(indent, dedent_span);
                        }
                        Ordering::Equal => {}
                    }
                }
                // If newline followed by not whitespace.
                else if let Some((Ok(_), _)) = ahead {
                    let dedent_span = span.end..span.end;
                    self.pop_and_queue_dedents(0, dedent_span);
                }

                // Return the newline
                Some((Ok(TokenKind::Newline), span))
            }
            Some((Err(error), span)) => Some((Err(error), span)),
            None => {
                let end = self.source.len();
                let dedent_span = end..end;
                self.pop_dedent(dedent_span)
            }
        }
    }
}

/// The indentation level of a whitespace run: the widths of its leading
/// spaces and tabs summed, the count ending at any other character.
fn indent_level(ws: &str) -> usize {
    ws.chars().map_while(char_width).sum()
}

/// The byte length of the longest prefix of a whitespace run made of
/// indentation characters whose level is at most `level`. A split by
/// level alone can land inside a tab, or past the run when tabs are wider
/// than the level, and the `Indent` range that starts there ends before
/// it starts, which `TextRange::new` refuses.
fn split_at_level(ws: &str, level: usize) -> usize {
    let mut seen = 0;
    for (offset, ch) in ws.char_indices() {
        match char_width(ch) {
            Some(width) if seen + width <= level => seen += width,
            _ => return offset,
        }
    }

    ws.len()
}

/// The width one character adds to an indentation level: a space one, a
/// tab four (the measure the lexing spec records under "Non-guarantees"),
/// and `None` for a character that is not indentation.
fn char_width(ch: char) -> Option<usize> {
    match ch {
        ' ' => Some(1),
        '\t' => Some(4),
        _ => None,
    }
}
