---
title: "Open questions and todos"
tags: [design]
---

Carried from the old `NOTES.md` on 2026-09-09. A question here is settled
by a decision item; a todo becomes a plan when it is scoped.

## Open questions

- Use `choice` instead of `enum`?
- Use `{}` instead of `[]` for type arguments? Julia does; `{}` is free
  because there are no blocks or object literals; `[]` could then be field
  get. Note [[ec7345d92813]] chose `[]` for generics.

## Todos

- Write an item tree like rust-analyzer's `hir-def/src/item_tree.rs`.
- Add `++` for append and concatenate.
- Add `value.[Trait]` to cast a value as a trait object, so a trait method
  can be called without ambiguity.

## Log
