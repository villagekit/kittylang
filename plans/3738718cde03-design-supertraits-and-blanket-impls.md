---
title: "Design: supertraits and blanket impls"
status: todo
tags:
  - design
blocked_by: 5c5f4b9256b3
priority: medium
---

Whether a trait may be implemented for a trait. `assembly.kitty` writes
`impl Object3d for Assembly`, which in Rust is a blanket impl, while
sketch 015 spells the same intent as a supertrait, `trait Stock:
Object3d`. The research synthesis of 2026-09-09 (question 5) makes the
answer load-bearing for M3: with no blanket and no overlapping impls,
impl selection is a hash lookup and coherence is a duplicate-key check.
The grilling of 2026-09-12 deferred it: what the code is trying to say
and what a good language does here both need more thought.

The evidence so far: every Village Kit assembly is a 3d object, which a
supertrait states directly; trait objects (`impl Trait` as a type) are
wanted, so "every `impl Assembly` value is an `Object3d`" must hold for
those too. Languages to read: Rust (supertraits, blanket impls,
coherence and the orphan rule), Swift protocol inheritance and
extensions, Haskell superclasses, Flix traits.

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
