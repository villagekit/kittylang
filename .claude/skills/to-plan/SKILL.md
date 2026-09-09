---
name: to-plan
description: "Turn the settled conversation into a plan record: the design that /to-plan-slices later splits into one-PR slices. No interview, just synthesis."
---

# To Plan

Write the record of the work the conversation settled, as an item in the
`plan` collection. Synthesize what was decided; the deciding is done. If
the design is not settled, say so and suggest a grilling first (the
`grilling` skill).

A record is a milestone, or a feature too big for one PR;
`/to-plan-slices` splits it. Work that fits one PR is written as a slice
directly, in the shape `.kipu/templates/plan.md` gives, and never as a
record: on one-PR work the record's paperwork outweighs its outcome
(kipu's `research/20260903-planning-structure-synthesis.md`, where this
process was worked out).

## Bindings

Nothing here restates the store; read it:

- `plans/README.md`, "Writing a plan": the two shapes, the frontmatter a
  plan carries, prefix versus full id, how to mint an id by hand.
- `.kipu/collections/plan.toml`: the states, the fields, the headings.
- `.kipu/README.md` for the frontmatter and edge rules.
- The most recent record in `plans/` (a plan with a `## Goal`), the example;
  when there is none yet, the shape below is the whole contract.

## Process

1. Orient in the specs and the decisions in the area. A conflict with a
   recorded decision is flagged and a superseding decision proposed, never
   overridden in the plan.
2. Sketch the seams the tests will use: existing seams before new ones, the
   highest seam possible, as few as possible. Where the seam's shape is
   itself open, use the `codebase-design` skill's vocabulary. Check the
   seams with the user.
3. Write the record in the shape below, with the frontmatter and the id
   `plans/README.md` prescribes, and add it to the order of work there. If
   the work fits one PR, write a slice from `.kipu/templates/plan.md`
   instead, with the same frontmatter rules, and stop here.
4. Name every spec section the work changes; the shipping PRs update them.

## The record

All sections H2, in this order. The title is frontmatter, so no H1.

- **Goal**: one paragraph, what the work yields, end to end.
- **Scope**: the deliverables, in the vocabulary of the glossary and of
  `plans/README.md`. Decisions are cited by prefix, never restated. Spec
  sections that change are named.
- **Seams under test**: the seams from step 2, and the fixtures.
- **Exit demo**: one observable check an agent runs when the record closes.
- **Out of scope**: what is deferred, and to which record if known.
- **Outcome**: empty; written when the record is finished.
- **Log**: empty; `kipu note` appends here.

A record carries no done-when list: its slices do, and the record closes
when they have shipped and the exit demo holds. A gate between records (an
acceptance period, a review) is itself a plan the next record is
`blocked_by` and tagged `gate`, so the fold holds it and agents stop at
it.

References age well: a crate, a module, a spec section, a decision prefix.
A `file:line` is evidence of the current state, never an instruction. Code
appears only where a probe produced a snippet that encodes a decision,
trimmed to that decision.
