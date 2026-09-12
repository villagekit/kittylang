---
title: "Design: positional arguments in indented blocks"
status: todo
tags:
  - design
blocked_by: 5c5f4b9256b3
priority: medium
---

Whether an indented block under a call may carry positional arguments.
Decision [[881178303bc8]] says an indented block carries keyword
arguments, but `Vector3` in `3d-object.kitty` and `Parts` in
`chair.kitty` give positional arguments one per line, and the list of
parts with a nested block per part is the Village Kit idiom. The
grilling of 2026-09-12 deferred it until the playground
([[5c5f4b9256b3]]) shows both spellings side by side.

The options: amend the decision so an indented block holds positional
lines or keyword lines, never mixed; or rewrite those two sites with
`( )` (which [[d0658cb19697]] may make multi-line). Interacts with
[[dd325e81ad2c]], since `Parts` is the main positional block. Languages
to read: Nim's command syntax, Haskell's layout, YAML-like block
literals in Rimu and Ruby's block arguments.

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
