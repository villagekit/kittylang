---
title: M3 grilling
status: todo
parent: 436aea0e22af
blocked_by:
  - 0ccbdb209358
  - d0658cb19697
  - 3738718cde03
  - e6a33eab19d4
  - dd325e81ad2c
  - a89ddd383a16
priority: medium
---

The grilling that designs M3, analysis: concrete syntax tree to HIR, name
resolution, the item index, bidirectional type checking, trait
resolution, diagnostics through `ariadne`. Held by [[0ccbdb209358]] and
by the design plans whose answers it consumes. Its inputs: the research
synthesis `research/20260909-rust-like-compiler-architecture-synthesis.md`,
whose section 9 lists twenty open questions; the outcome of
[[a89ddd383a16]] on modules and [[3738718cde03]] on supertraits; and one
answer the grilling of 2026-09-12 gave without a decision item: trait
objects, `impl Trait` as a type with dynamic dispatch, are wanted, and
the user is open to an interpreted design with runtime reflection if
that is the best language.

## Work

Fresh conversation, `grilling` over the inputs above, then `/to-plan`
for the M3 record and `/to-plan-slices` for its slices; `specs/types.md`
and `specs/analysis.md` cut as the answers crystallise.

## Seams under test

None: the outcome is a record.

## Done when

- The M3 record is minted with `parent` [[436aea0e22af]] and its slices
  are in the order of work
- `specs/types.md` and `specs/analysis.md` exist

## Outcome

## Log
