# plans: the run sheet

The `plan` collection, declared in [.kipu/collections/plan.toml](../.kipu/collections/plan.toml).
The frontmatter is the machine-readable truth (`status`, `priority`,
`parent`, `blocked_by`, `tags`); this README is commentary, and `kipu
ready` is the order of work. Ids are twelve hex characters; prose cites a
plan by any unique prefix. Nothing is renumbered because nothing is
numbered.

## Order of work

No epic and no milestone record yet. The milestones in
[DESIGN.md](../DESIGN.md#milestones) become records here through a
grilling and `/to-plan`, then slices through `/to-plan-slices`.

1. [Make just check green](8f370f53521b-make-just-check-green.md)
   (`8f370f53`, todo). The first slice, so the working loop has a green
   gate to stand on.

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
the two process tags this store uses, `gate` and `epic`. Sequence lives in
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
- **Order of work**: the sequence `blocked_by` and `priority` give, which
  `kipu ready` prints. The **run sheet** is this file, its commentary.
