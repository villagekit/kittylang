---
title: "Design: modules and imports"
status: todo
tags:
  - design
blocked_by: 1cee599ce218
priority: medium
---

What a module is and how `import` resolves. The examples import each
other (`@std/units`, `@std/3d-math`, `@villagekit/gridbeam:1`), so M3
cannot analyse one file alone and its design must include a module
boundary from the start (research synthesis of 2026-09-09, question 13).
The grilling of 2026-09-12 deferred this to the M3 conversation.

The options: M3 analyses one source at a time and imports are opaque
holes; M3 takes a set of sources handed in by the embedding, the host
mapping `@std/units` to one of them and the compiler never touching a
filesystem, the `:1` version suffix parsed and ignored until a package
story exists; or a package system with versions now. Languages to read:
Gleam's packages and modules, Rust's crates and paths, Deno and ES
module specifiers, Roc's platforms and packages, Julia's modules.

## Work

In this order, as `plans/README.md` defines a design plan:

1. Opus sub-agents research the question against primary sources: the
   languages named below, read at their references, cited to file and
   section; findings synthesised into `research/` per the `/research`
   skill.
2. The main session proposes designs, each worked through the examples
   in `examples/` so the ergonomics are visible in the playground.
3. Opus sub-agents review the proposals adversarially.
4. A grilling; the user decides. The decision item and the spec change
   ship together.

## Seams under test

None: the outcome is a decision and spec text, not code.

## Done when

- A decision item in `decisions/` is accepted and cited in the Outcome
- `specs/grammar.md` (or `specs/types.md` once it exists) states the rule
- The affected examples are rewritten to it, or a slice is minted to do so

## Outcome

## Log
