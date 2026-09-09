---
title: "Brackets: generics, tuples, lists and indexing"
status: accepted
date: "2025-02-26"
tags: [syntax]
---

## Context

Rust uses `<>` for generics and `[]` for both arrays and indexing; Flix
uses `[]` for generics and lists both. Kitty Lang has no need for `{}`
blocks or object literals, so the bracket pairs can be spent differently.

## Decision

- `[]` for generics: `From[Meter]`, `Vector3[N]`.
- `()` for tuples.
- `List(...)` for lists.
- `list(x)` for indexing.

## Consequences

`<>` is never a bracket, so `<` and `>` are only comparison operators.
Indexing is a call, so a list and a function of one argument read alike.

The README's "Language" section still says `list.get` and `list.set` for
indexing, which this decision replaced; the README is behind, not a
competing decision.

Carried from [sketches/015.md](../sketches/015.md).
