---
title: "Design: tuple structs and Parts"
status: todo
tags:
  - design
blocked_by: 5c5f4b9256b3
priority: medium
---

Whether positional payloads exist, on structs and on enum variants, and
what `Parts` is. `struct Parts(List[PartsItem])` in `assembly.kitty` is
the only tuple struct in the examples, and its variants `case
Stock(impl Stock)`, `case Single(Part)` and `case Nested(Self)` carry
their payload the same positional way, where the grammar today reads
`case Name: Type`; positional construction of any struct already exists
(`Length(0)`), so the form buys only an unnamed field. `Parts` is
constructed as an indented list of items, and `PartsItem.None` exists so
a conditional item can be absent, which [[77ca5300cd95]] now makes
redundant. This is standard-library design as much as language design,
so the grilling of 2026-09-12 deferred it.

The options: keep positional payloads on both, so `case Name(Type)`
enters the grammar; keep them on variants only, `case Name: Type`
becoming `case Name(Type)` or the reverse; drop tuple structs drop them and make `Parts` a struct
with one prop; drop `Parts` and write `fn parts(self): List[Part]`, with
`Part` an enum of `Stock` and `Assembly` and `Option` doing the work of
`None`. Interacts with [[e6a33eab19d4]]. Languages to read: Rust newtypes,
Gleam and Elm records, Swift's absence of the form, and Village Kit's
own `Parts` type in the villagekit repo.

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
