---
title: "Design: newlines inside brackets"
status: todo
tags:
  - design
blocked_by: 5c5f4b9256b3
priority: medium
---

What a newline means inside `( )`, `[ ]` or `{ }`, and where a newline
ends an expression outside them (today the parser sees newlines as
trivia, so `let x = f` followed by `(1)` on the next line reads as a
call). Today the indenter
emits indent and dedent tokens for every newline, brackets included, so
`Self {` followed by fields on their own lines cannot parse. The
grilling of 2026-09-12 deferred this until the playground
([[5c5f4b9256b3]]) makes the options visible.

The options on the table: Python's rule, where indentation is suspended
inside brackets and a newline there is whitespace; the current rule,
where brackets are single-line and an indented block is the only
multi-line form; or brackets that may contain an indented block as their
body. The evidence so far: no example spans a bracket over lines; `let
with` exists partly because a multi-line pattern cannot; decision
[[881178303bc8]] already gives every call an indented form. Languages to
read: Python, Nim, Haskell's layout rule, Scala 3's optional braces,
Rhombus and shrubbery.

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
