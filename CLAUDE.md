# CLAUDE.md - Working methodology

This repo builds **Kitty Lang**, a friendly scripting language with Rust's
type system, to be the foundation of [Village Kit](https://github.com/villagekit/villagekit),
a code-as-CAD system for open source makers. This repo is a kipu store:
`plans/`, `notes/` and `decisions/` are collections declared in `.kipu/`.

The written knowledge lives in six places, each with its own contract:

- **[DESIGN.md](./DESIGN.md)** is the root: what Kitty Lang is and is not,
  the principles, the structure, the milestones.
- **[specs/](./specs/)** holds the normative design: current-state
  descriptions of what each part of the language and the compiler must
  hold, kept true in the same PR as any change to designed behaviour.
  `specs/README.md` is the reading order.
- **[docs/](./docs/)** is the narrative documentation for an outside reader,
  never referencing plans or decisions. Its first page is
  [docs/context.md](./docs/context.md), the glossary: the canonical terms for
  code, specs, items and commit messages. Process terms live in
  `plans/README.md`.
- **[decisions/](./decisions/)** is the `decision` collection: one item per
  decision, hash-id filename, `status` in frontmatter. Append-only; a
  decision is superseded by a new item carrying a `supersedes` edge, never
  rewritten. Cite one by its prefix or a link to its file.
- **[plans/](./plans/)** is the `plan` collection: the work, as items with
  `status`, `priority`, `parent`, `blocked_by` and `tags` in frontmatter.
  Two shapes, the record and the one-PR slice, both described in
  `plans/README.md` ("Writing a plan"). That README is the run sheet's
  commentary; `kipu ready` is the order of work. A slice moves to `done`
  with its `## Outcome` in the PR that ships it; a record moves to `doing`
  when its slices are minted and closes when they have shipped and its
  exit demo holds. A gate between milestones is itself a plan the next one
  is `blocked_by`, tagged `gate`, the user's to move. Nothing is numbered,
  so nothing is ever renumbered.
- **[research/](./research/)** holds the dated syntheses of research sweeps,
  cited to primary sources. `research/README.md` is the session index.
- The **`transcripts` repo**, shared by every villagekit project and checked
  out as a sibling (`../transcripts`), holds the records of agent sessions
  worth keeping under `kittylang/`. After a research sweep or a grilling,
  archive the session per the `/transcripts` skill; raw `.jsonl`
  transcripts stay outside both repos.

`sketches/` is the language's design history: fifteen dated syntax
sketches, read-only. The decisions they reached are carried into
`decisions/`; the sketches themselves are evidence, not design.
`examples/` holds the `.kitty` programs the language is being built to run.

## Skills

The process lives in skills under `.claude/skills/`. The skills are generic
discipline; this file is where they bind to this repo.

**The main flow, idea to ship.**

1. `grilling` (from the `mattpocock-skills` plugin) sharpens the idea by
   interview, recording decisions in `decisions/` and the specs as they
   crystallise. Facts are the agent's to find; decisions are the user's.
   `/research` runs the sweeps that feed it.
2. `/to-plan` turns the settled conversation into a plan item: the
   record, with its seams under test, decisions cited.
3. `/to-plan-slices` splits that plan into one-PR plans carrying `parent`
   and `blocked_by` edges; the parent moves to `doing` and stays open until
   its slices are done and its exit demo holds.
4. `/implement` builds one plan: orient, scope, test-first, verify, review,
   record, commit. It drives `/tdd` at the plan's seams and `/code-review`
   before committing. `/orchestrate` runs `/implement` over a series of plans.

Keep steps 1-3 in one context window; each `/implement` starts fresh from
its plan.

**References underneath.** `codebase-design` and `domain-modeling`, both
from the `mattpocock-skills` plugin.

**Standalone.** `/documentation`, `/transcripts`, `/handoff`,
`/resolving-merge-conflicts`.

## Principles

- Premature optimization is the root of all evil.
- Do not second-guess or make assumptions. When in doubt, verify or ask.
- Prefer robustness over performance.
- Achieve performance with simple fit-for-purpose abstractions, not clever
  hacks.

### Complexity check

Before adding non-trivial code, verify:

1. The approach is solid, not just the first thing that came to mind.
2. No simpler alternative achieves the same goal.
3. Compare to the normative references where relevant: the specs, the
   sketches' decisions, and the compilers this one learns from
   (rust-analyzer, eldiro and its successors, Gleam, Flix, Roc; see the
   reading list in `notes/`).
4. Check if a good crate already handles the task (mind the license).

## Working style

- Plain, direct, human-friendly language.
- Value the reader's attention: be clear and concise, do not over-explain.
- Knowledge lives in the repo, never in agent session memory. This project
  is worked on from multiple computers with multiple agents.
- No em dashes in new prose. Use hyphens, commas, or shorter sentences.
- Don't co-author as Claude. No `Co-Authored-By` in commit messages. No
  `🤖 Generated with Claude Code` in pull request descriptions. No
  session-link trailers in either.
- Comments should age well. Describe intent or a non-obvious constraint, not
  the change you just made.
- Commit subjects are imperative and scoped by crate, and cite the plan's
  prefix where the commit ships one: `kitty-parser: parse match arms (plan
  8f370f53)`.

## Domain model

The domain model is actively maintained, not passively read:

- **Use the glossary's vocabulary.** When naming a concept in code, docs, a
  plan, or a commit message, use the term as `docs/context.md` defines it.
  Process terms (record, slice, seam, gate, order of work) live in
  `plans/README.md`, never in the glossary. A missing term is a signal:
  reconsider, or add it to the right home as part of the work.
- **Sharpen terms as they crystallise.** If the user's words and the
  glossary disagree, ask which is right, then update both in the same PR.
- **Record decisions sparingly.** A decision earns an item when it is hard
  to reverse, surprising without context, and the result of real
  trade-offs. The specs restate the answer; the item holds the rationale.
- **Flag conflicts.** If work contradicts a recorded decision, surface it
  and propose superseding it. Never silently override.

## Plans vs. reality

- The user steers by editing `plans/`, `specs/`, and `decisions/` directly
  or by instruction. Treat plan edits you did not make as new requirements.
- If a plan and the code (or a plan and good sense) conflict, flag it, never
  silently override.
- Don't guess on load-bearing choices. Always confirm before anything
  irreversible or that affects shared systems.
- Never clean probe edits with a bare `git checkout`/`git restore`; stash or
  revert by path.
- Agents may edit any file in this repo, `.kipu/`, `.claude/`, `CLAUDE.md`
  and the build files included. Say what changed and why in the commit.

## Sub-agents

Sub-agents run on Opus by default (reviews, research, design alternatives),
never the main session's model unless the user explicitly asks. Menial work
(extraction, bulk edits, renames) runs on Sonnet. Sub-agents start cold: the
prompt carries the context they need. No tool restrictions.

## Commands

| Command | Purpose |
|---|---|
| `just check` | Full quality gate (fmt check, clippy `-D warnings`, tests); bound it: `timeout 600 just check` |
| `just test` | Tests only |
| `just build` | Build the full workspace |
| `just clippy` | Clippy with warnings as errors |
| `just fmt` | Format all crates |
| `just playground` | Build the playground's wasm into `playground/web/pkg/` (adds the wasm target and `wasm-bindgen-cli` on first run) and say how to serve the page |
| `just fuzz` | Fuzz `kitty_parser::parse` for 60 seconds, seeded from `examples/`; needs nightly, installs `cargo-fuzz` on first run; not part of `just check`; bound it once installed: `timeout 120 just fuzz` |

No CI host yet: the gate is run locally.

The `kipu` binary is built from the sibling checkout
(`cargo install --path ../../ahdinosaur/kipu/crates/kipu-cli`). Where it is
not at hand, items are minted by hand as `plans/README.md` says.

## Upstream sources

**Read them, don't recall them.** When a claim about an upstream crate
(`logos`, `eventree`, `text-size`, `fastnum`, `ariadne`) is load-bearing,
open its source in the cargo registry
(`~/.cargo/registry/src/*/<crate>-<version>/`) and cite the crate, version
and path. When a claim is about another language's semantics (Rust, Gleam,
Flix, Julia), read its reference, not a summary of it.

## Conventions

- Rust edition 2021, `rust-version` 1.83 (`Cargo.toml`). `Cargo.lock` is
  committed: it holds the dependency versions the gate tested. A `cargo
  update` keeps every dependency's own `rust-version` at or under ours;
  cargo prints `requires Rust x.y` beside a version that is not.
- **Licensing.** No strong copyleft (GPL) libraries, and no GPL code as a
  reference. Weak copyleft (LGPL) with care: link, never transcribe.
  Prefer permissive.
- **One grammar, one home.** Token kinds live in `kitty-lexer`, node kinds
  in `kitty-syntax`, and the grammar in `kitty-parser`; nothing else
  lexes or parses source text.
- **Resilience.** The lexer and parser never panic and always produce a
  tree, whatever the input. Errors are collected with spans, never thrown.
- **Errors.** Compiler errors (lexing, parsing, analysis) are data: one
  enum per stage, every variant carrying its span, collected into a list
  and never thrown, with a hand-written `Display` as `ParseError` has.
  They are rendered for a user through `ariadne` from `kitty-meta` spans.
  Infrastructure errors (an embedding, a CLI, a file read) use `thiserror`,
  one enum per layer, a blank line between variants, every variant with a
  doc comment (when, and what to do) and a terse lowercase `#[error]`
  message with no trailing period. No `anyhow`-style catchalls in library
  code, and no `#[error(transparent)]`: a wrapper names the operation and
  interpolates the wrapped error.
- **Imports** grouped, in order: std, external crates, internal crates
  (`kitty_*`), within crate. Blank line between groups.
- **Module exports**: public entryway at the top, private helpers below.
- Public items have rustdoc. After code changes, keep the crate README
  and `specs/` true.
- For review observations that don't lead to a change now: `// Note(cc): xxx`
  or `// TODO(cc): xxx`.

## Testing

The discipline is `/tdd`; the repo's bindings:

- TDD applies in full: the whole pipeline is pure, deterministic code
  (source text in, tokens, tree, HIR, value out). The I/O boundaries, the
  `kitty` command and the playground page, are exempt but kept thin: they
  move text between the outside and the library crates and decide
  nothing themselves.
- Snapshot tests through `expect-test` are the house style: a source
  string in, the rendered tokens or tree out, the expectation updated with
  `UPDATE_EXPECT=1` only when the change is intended and reviewed.
- Highest-value targets: the indenter (indentation to block tokens), each
  grammar rule's happy path and its recovery on malformed input, spans on
  every node and value, the `.kitty` programs in `examples/` parsed
  end to end.
- Add tests for specific edge cases, not for count. Remove redundant tests.

## Tracing

No `println!`/`dbg!` in library code. `tracing` with structured fields
where a span adds context, once a consumer exists. A binary's stdout is
its data channel: it writes through one locked `Write` handle, so the
output can be captured, and reports failures on stderr.

## Structure

One cargo workspace, the crates named `kitty-<dir>`:

- `cli/`: the `kitty` command, `lex` and `parse`: prints what the
  compiler sees.
- `lexer/`: `logos` token kinds and the indenter that turns indentation
  into block tokens.
- `syntax/`: the `eventree` tree config: node kinds, the syntax tree types.
- `cst/`: typed views over the syntax tree (the concrete syntax tree).
- `parser/`: the resilient grammar, events, the sink that builds the tree,
  the parse errors.
- `hir/`: the high-level intermediate representation the analysis will
  produce. Types only so far.
- `meta/`: source ids, spans, diagnostic rendering.
- `number/`: the `Number` type over `fastnum` decimals.
- `playground/`: the compiler as a library for a web page: `inspect_json`
  behind a `wasm-bindgen` export, and `web/index.html`, the page, built
  by `just playground`.
- `examples/`: `.kitty` programs, the end-to-end fixtures.
- `fuzz/`: the `cargo-fuzz` target over `kitty_parser::parse`, its own
  cargo root outside the workspace, run by `just fuzz`.
- `sketches/`: the syntax design history, read-only.
