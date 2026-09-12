---
title: Kitty Lang to M4
status: doing
tags:
  - epic
---

## Goal

Kitty Lang runs the Village Kit programs in `examples/` end to end: source
text lexed, parsed, analysed and evaluated, every value carrying the span
that made it, from a host program that embeds the compiler as a library.
This epic parents the milestones DESIGN.md names and the tooling that
lets the language be designed by looking at it.

## Scope

The milestone records, each a child of this epic: M1 (the lexer, done
before this store existed), [[1cee599ce218]] (M2, the parser),
[[5c5f4b9256b3]] (the tooling for language design), then M3 (analysis)
and M4 (evaluation), minted by their own grillings. A gate holds each
milestone until the user accepts the one before.

## Seams under test

The `examples/*.kitty` programs, the fixtures every milestone's exit
demo runs.

## Exit demo

Every program in `examples/` evaluates from a host program with no
diagnostics, and a value chosen from the result reports the span of the
expression that made it.

## Out of scope

Editor integration beyond the playground, a formatter, a package
registry: each is a later record once M4 holds.

## Outcome

## Log
