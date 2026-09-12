---
title: An if without else has type Option
status: accepted
tags:
  - types
date: 2026-09-12
---

## Context

Village Kit programs build lists of parts where an item is present only
under a condition: `chair.kitty` writes `if should_include_back` followed
by a part, with no `else`, inside the list of parts. In Rust an `if`
without `else` has type `()`, so that line would be an error and the
example would need an explicit `else None` and an `Option` on every item.

## Decision

An `if` expression without an `else` branch has type `Option[T]`, where
`T` is the type of the `then` branch: `Some(value)` when the condition
holds, `None` otherwise. An `if` with an `else` has the type of its
branches, as before.

## Consequences

The conditional-inclusion idiom reads as written. The `PartsItem.None`
variant in `assembly.kitty` becomes redundant, which the design plan on
tuple structs and `Parts` takes up. The type checker treats an else-less
`if` as sugar for `if c then Some(x) else None`. The alternatives were
the Rust rule (`()`, an error in the idiom) and allowing the form only
where the expected type is `Option[T]`.

Decided in the grilling of 2026-09-12.
