use logos::{Logos, Span};
use peek_again::Peekable;
use std::{cmp::Ordering, collections::VecDeque};

use crate::token::Token;

pub trait TokenIterator<'src>:
    Iterator<Item = (Result<Token, <Token as Logos<'src>>::Error>, Span)>
{
}
impl<'src, I> TokenIterator<'src> for I where
    I: Iterator<Item = (Result<Token, <Token as Logos<'src>>::Error>, Span)>
{
}

pub struct Indenter<'src, I: TokenIterator<'src>> {
    source: &'src str,
    tokens: Peekable<I>,
    indents: Vec<usize>,
    queued_tokens: VecDeque<(Token, Span)>,
}

impl<'src, I: TokenIterator<'src>> Indenter<'src, I> {
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
        let ws = &self.source[ws_span];

        let mut indent = 0;

        for ch in ws.chars() {
            match ch {
                ' ' => indent += 1,
                // TODO handle tabs correctly
                '\t' => indent += 4,
                _ => break,
            }
        }

        indent
    }

    fn pop_and_queue_dedents(&mut self, indent: usize, dedent_span: Span) {
        // Pop until we match the new level
        while let Some(&level) = self.indents.last() {
            if level > indent {
                self.indents.pop();

                // Queuing Dedent tokens.
                self.queued_tokens
                    .push_back((Token::Dedent, dedent_span.clone()));
            } else {
                break;
            }
        }
    }

    fn pop_dedent(
        &mut self,
        dedent_span: Span,
    ) -> Option<(Result<Token, <Token as Logos<'src>>::Error>, Span)> {
        if self.indents.len() > 1 {
            self.indents.pop();
            Some((Ok(Token::Dedent), dedent_span))
        } else {
            None
        }
    }
}

impl<'src, I: TokenIterator<'src>> Iterator for Indenter<'src, I> {
    type Item = (Result<Token, <Token as Logos<'src>>::Error>, Span);

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

                if token != Token::Newline {
                    // Return the next token
                    return Some((Ok(token), span));
                }

                // If newline followed by an whitespace followed by newline
                if let (Some((Ok(Token::Whitespace), ws_span)), Some((Ok(Token::Newline), _))) =
                    (ahead.clone(), ahead_2)
                {
                    // Skip and queue the whitespace
                    self.tokens.next();
                    self.queued_tokens.push_back((Token::Whitespace, ws_span));
                }
                // If newline followed by newline
                else if let Some((Ok(Token::Newline), _)) = ahead {
                    // Do nothing
                }
                // If newline followed by a whitespace (but not on an empty line),
                //   Return / queue any indents
                else if let Some((Ok(Token::Whitespace), ws_span)) = ahead {
                    let indent = self.get_next_indent_level(ws_span.clone());

                    // Get the current indent level (the top of our stack).
                    let current_indent = *self.indents.last().unwrap();

                    match indent.cmp(&current_indent) {
                        Ordering::Greater => {
                            // Increased indent:

                            // Skip and queue whitespace as being up to existing indent level
                            self.tokens.next();
                            let revised_ws_span = ws_span.start..(ws_span.start + current_indent);
                            if !revised_ws_span.is_empty() {
                                self.queued_tokens
                                    .push_back((Token::Whitespace, revised_ws_span));
                            }

                            // Push new indent level
                            self.indents.push(indent);

                            // Queue an indent token.
                            let indent_span = (ws_span.start + current_indent)..ws_span.end;
                            self.queued_tokens.push_back((Token::Indent, indent_span));
                        }
                        Ordering::Less => {
                            // Decreased indent:

                            // Skip and queue whitespace as-is
                            self.tokens.next();
                            self.queued_tokens
                                .push_back((Token::Whitespace, ws_span.clone()));

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
                Some((Ok(Token::Newline), span))
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
