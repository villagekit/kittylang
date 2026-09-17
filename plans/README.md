# plans: the run sheet

The `plan` collection, declared in [.kipu/collections/plan.toml](../.kipu/collections/plan.toml).
The frontmatter is the machine-readable truth (`status`, `priority`,
`parent`, `blocked_by`, `tags`); this README is commentary, and `kipu
ready` is the order of work. Ids are twelve hex characters; prose cites a
plan by any unique prefix. Nothing is renumbered because nothing is
numbered.

## Order of work

The epic [Kitty Lang to M4](436aea0e22af-kitty-lang-to-m4.md)
(`436aea0e`) parents the milestone records. The grilling of 2026-09-12
set the order: specs and fixtures before grammar, tooling before both,
and the questions it could not settle held as design plans until the
tooling makes them visible.

1. [Tooling for language design](5c5f4b9256b3-tooling-for-language-design.md)
   (`5c5f4b92`), `doing`, its slices: the `kitty` command (`f280931f`),
   the examples test (`942ff7a4`), then the playground (`45a1b9e9`).
2. [M2: the parser](1cee599ce218-m2-the-parser.md) (`1cee599c`),
   `doing`, its slices: the lexing and grammar specs (`0248a546`, done) first,
   then, each waiting on the specs and the examples test, the lexer
   (`9d84d1f9`), the parser never panics (`277624e4`), function return
   types (`466ffceb`), keyword arguments with `=` and spread
   (`4423cd0a`), value segments after a type path (`1493e2aa`); then attributes (`328c0a99`), `let with` (`94bcf824`)
   and the fuzz target (`378fe0c3`); last, the examples rewritten
   (`75d0d7ee`), the record's exit demo.
3. The design plans, tagged `design`, blocked by the tooling record:
   [newlines inside brackets](d0658cb19697-design-newlines-inside-brackets.md),
   [supertraits and blanket impls](3738718cde03-design-supertraits-and-blanket-impls.md),
   [positional arguments in indented blocks](e6a33eab19d4-design-positional-arguments-in-indented-blocks.md),
   [tuple structs and Parts](dd325e81ad2c-design-tuple-structs-and-parts.md);
   and [modules and imports](a89ddd383a16-design-modules-and-imports.md),
   blocked by M2 because it belongs to the M3 conversation.
4. [Gate: M2 accepted](0ccbdb209358-gate-m2-accepted.md) (`0ccbdb20`),
   the user's, then the [M3 grilling](5890c35571be-m3-grilling.md)
   (`5890c355`), which mints the M3 record.

The first slice,
[Make just check green](8f370f53521b-make-just-check-green.md)
(`8f370f53`), is done: the gate is green and every later slice ends on it.

## Writing a plan

Two shapes, one collection:

- A **record** is work too big for one PR: a milestone, or a feature
  `/to-plan-slices` will split. Its body is Goal, Scope, Seams under test,
  Exit demo, Out of scope, Outcome, Log: all H2, no H1, the title in
  frontmatter. `/to-plan` writes one.
- A **slice** is one PR. Its body is [.kipu/templates/plan.md](../.kipu/templates/plan.md):
  a paragraph, Work, Seams under test, Done when, Outcome, Log. `kipu new
  plan` copies that body; a record's body is written by hand.

Frontmatter, per [.kipu/README.md](../.kipu/README.md): `title`
(required); `status: todo`; `parent` naming the record, if it has one (a
slice also carries `derived_from` to it); `blocked_by` for every plan whose
deliverable this one consumes, hard unless `strength: soft` with a `note`
for an ordering preference; `priority` only on a slice, `medium` unless
it is pulled ahead (`urgent`, `high`) or pushed back (`low`); `tags` for
the three process tags this store uses, `gate`, `epic` and `design`. Sequence lives in
edges and nowhere else: priority is the weight that breaks ties among
ready items, and a record carries none. An edge target is the full
twelve-hex id, never a prefix. Citations in a body are `[[<full id>]]`;
prose and commit subjects cite by prefix.

Minting is `kipu new plan --title <text>`. By hand, where the binary is
not at hand: `openssl rand -hex 6` is the id, redrawn if YAML would read
it as a number (all digits, or a digit run with one `e` and more digits);
`find plans notes decisions -name '<id>*'` must print nothing; the file is
`plans/<id>-<slug>.md`, the slug the title lowercased with runs of
non-alphanumerics as one hyphen.

A record moves to `doing` in the commit that mints its slices, so a
sliced record never competes with them for ready. A slice moves to `done`
with its Outcome in the PR that ships it. A record stays open until its
slices have shipped and its exit demo holds; then the user runs the demo
and finishes it with an Outcome. A gate between records, such as an
acceptance period, is a plan the next record is `blocked_by` and tagged
`gate`: it is the user's, and agents stop at it.

## Vocabulary

Process terms, defined here because no spec owns them:

- **Record**: a plan too big for one PR, holding the goal, scope, seams and
  exit demo its slices are cut from.
- **Slice**: a plan that is one PR. Cut from a record it carries `parent`
  and `derived_from` to it; written directly it carries neither.
- **Seam**: the interface a test exercises. The fewer and the higher, the
  better.
- **Gate**: a plan tagged `gate` that holds the next milestone until
  something outside the code has happened, such as an acceptance period.
  It takes the slice body though it is no PR, and it is the user's to
  move.
- **Epic**: the record tagged `epic` that parents the milestones.
- **Site**: a place in an example that an open design plan owns, so its
  parse errors stand until the plan decides. The examples test names
  each site by its lines and the plan's prefix.
- **Design plan**: a plan tagged `design` that holds a language-design
  question too open to settle in one grilling: the question, the evidence
  so far and the options. Its Work is fixed, in this order: Opus
  sub-agents research the question against primary sources; the main
  session proposes designs; Opus sub-agents review the proposals
  adversarially; then a grilling, where the user decides. Its Outcome is
  the decision item it produced. It takes the slice body though it is no
  PR.
- **Order of work**: the sequence `blocked_by` and `priority` give, which
  `kipu ready` prints. The **run sheet** is this file, its commentary.
