---
name: to-plan-slices
description: "Split a plan record into one-PR tracer-bullet slices with blocked_by edges, hardened by adversarial review rounds and placed in the order of work."
---

# To Plan Slices

Split a record into slices: tracer bullets, each one focused PR, each
declaring what gates it. Nothing here restates the store; read it:

- `plans/README.md`, "Writing a plan": the two shapes, the frontmatter a
  plan carries, prefix versus full id, how to mint an id by hand.
- `.kipu/collections/plan.toml`: the states, the fields, the headings.
- `.kipu/README.md` for the frontmatter and edge rules.
- `.kipu/templates/plan.md`: the slice body. It is the body only; the
  frontmatter is yours to write.

## Process

1. Read the record in full, and every spec section it names.
2. Draft the slices. Each cuts a narrow but complete path through every
   layer it touches (lexer, syntax, parser, hir, tests) and is demoable alone; a slice
   that ships one layer works only when its siblings land, so it is not a
   slice. Size is one PR in one fresh context window. Split only where a
   reviewer could reject one slice while approving its neighbour. Any
   prefactoring is its own slice, first. A wide refactor (one mechanical
   change whose blast radius spans the codebase) is sequenced expand,
   migrate in batches per crate, contract, each batch a slice blocked by
   the expand, the contract blocked by every batch.
3. Give each slice its frontmatter: `title`; `status: todo`; `parent` and
   `derived_from` naming the record in string form, as `kipu split` will
   write them; `blocked_by` for every deliverable it consumes from another
   slice, never only a mention in prose, hard by default, `strength: soft`
   with a `note` for an ordering preference that does not gate; `priority`
   as the weight `plans/README.md` defines, `medium` unless the slice is
   pulled ahead or pushed back. Sequence lives in the edges only. Edge
   targets are full ids; prose cites by prefix.
4. Present the numbered breakdown to the operator: title, blocked by, what
   it delivers. The operator sets the granularity; iterate on their call.
5. Run adversarial review rounds: fresh Opus sub-agents each round, given
   the record, the slices and the brief below. Fix between rounds. Stop
   when two consecutive rounds return nothing actionable, or when the
   operator calls it.
6. Write one file per slice from the template, blockers first so edges cite
   real ids, minted as `plans/README.md` says (once `kipu split` exists it
   mints the ids and the `parent` and `derived_from` edges; the
   `blocked_by` edges, the priorities and the bodies are still yours).
   Add them to the order of work there, and move the record to `doing` in
   the same commit, so only slices are ever ready. The record closes when
   its slices have shipped and its exit demo holds.

Shared context lives in the record and a slice cites it by prefix, never
copies it. What one slice learns that a later one needs goes into the
record's Log or a note item, not into the next slice's body.

## Review brief

- **Contradictions** with the specs and the recorded decisions.
- **Edges that lie**: a slice consumes a deliverable with no `blocked_by`
  edge, direct or transitive, to the slice that produces it.
- **Coverage**: inventory every verb, flag, diagnostic code and spec rule in
  the record's scope and diff it against the union of the slices. Nothing
  uncovered, nothing assigned twice.
- **Done-when**: every line observable, at least one runnable; a line that
  reads the store's own state says when it is checked.
- **Normative claims**: a slice asserts no behaviour the specs do not; where
  a rule is missing, the spec edit is part of that slice's Work.
- **Format**: the frontmatter parses under `.kipu/README.md`'s rules; the template's
  sections are present and nothing is padded.
