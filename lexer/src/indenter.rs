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
}

impl<'src, I: TokenIterator<'src>> Iterator for Indenter<'src, I> {
    type Item = (Result<Token, <Token as Logos<'src>>::Error>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        // If we have some tokens queued, return them first
        if let Some((token, span)) = self.queued_tokens.pop_front() {
            return Some((Ok(token), span));
        }

        // Get the next token.
        match self.tokens.next() {
            Some((Ok(token), span)) => {
                let mut peek = self.tokens.peek();
                let ahead = peek.get().cloned();
                let ahead_2 = peek.peek().cloned();

                // If newline followed by an empty line
                if let (
                    Token::Newline,
                    Some((Ok(Token::Whitespace), ws_span)),
                    Some((Ok(Token::Newline), _)),
                ) = (token, ahead.clone(), ahead_2)
                {
                    // Skip and queue the whitespace
                    self.tokens.next();
                    self.queued_tokens
                        .push_back((Token::Whitespace, ws_span.clone()));

                    // Return the newline
                    Some((Ok(Token::Newline), span))
                }
                // If newline followed by a whitespace (but not on an empty line),
                //   Return / queue any indents
                else if let (Token::Newline, Some((Ok(Token::Whitespace), ws_span))) =
                    (token, ahead)
                {
                    println!("hiiii");

                    let ws = &self.source[ws_span.clone()];

                    let mut indent = 0;

                    for ch in ws.chars() {
                        match ch {
                            ' ' => indent += 1,
                            // TODO handle tabs correctly
                            '\t' => indent += 4,
                            _ => break,
                        }
                    }

                    // Get the current indent level (the top of our stack).
                    let current_indent = *self.indents.last().unwrap();

                    println!("indent: {}, current_indent: {}", indent, current_indent);

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
                            self.queued_tokens.push_back((Token::Whitespace, ws_span));

                            // Pop until we match the new level
                            while let Some(&level) = self.indents.last() {
                                if level > indent {
                                    self.indents.pop();

                                    // Queuing Dedent tokens.
                                    let dedent_span = span.end..span.end;
                                    self.queued_tokens.push_back((Token::Dedent, dedent_span));
                                } else {
                                    break;
                                }
                            }
                        }
                        Ordering::Equal => {}
                    }

                    // Return the newline
                    Some((Ok(Token::Newline), span))
                } else {
                    // Return the next token
                    Some((Ok(token), span))
                }
            }
            Some((Err(error), span)) => Some((Err(error), span)),
            None => {
                // If we have no more tokens but still indented, return dedents.
                if self.indents.len() > 1 {
                    self.indents.pop();
                    let end = self.source.len();
                    let span = end..end;
                    let token = Token::Dedent;
                    Some((Ok(token), span))
                } else {
                    None
                }
            }
        }
    }
}
