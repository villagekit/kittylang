---
title: Attributes replace metadata comments
status: accepted
tags:
  - syntax
  - lexer
date: 2026-09-12
supersedes: cb55e71a221a
---

## Context

Metadata on a declaration has had three spellings: an indented
`key = value` block under a prop (sketches 004 to 012), the `#{ key =
value }` metadata comment that [[cb55e71a221a]] chose (sketches 013 to
015), and the `@name(args)` attributes the examples adopted when the
parser was written, without a decision. The examples need a closure in
the payload (`@requires(fn (self) => self.should_include_back)`), which a
comment-shaped form was never meant to hold. Two mechanisms for one job
is the one option ruled out.

## Decision

Metadata is written as attributes: `@name(args)` on the line before the
declaration it describes, `args` an ordinary call argument list, so the
payload is a real expression and keyword arguments use `=`
([[95cd2585f916]]). Attribute names are an open set the host validates,
not a set the compiler knows. Metadata comments (`#{ ... }` and
`#={ ... }=#`) are removed; `#` and `#= ... =#` remain plain comments.

## Consequences

Supersedes [[cb55e71a221a]] for metadata; its comment syntax stands. The
lexer gains `@` and loses the metadata comment kinds; the grammar reuses
the call argument list. It is the mainstream spelling (Python decorators,
Java, Kotlin and Swift annotations, Gleam's `@deprecated` and
`@external`). An attribute is data on the item in the HIR; what a host
does with an unknown attribute name is the host's rule.

Decided in the grilling of 2026-09-12.
