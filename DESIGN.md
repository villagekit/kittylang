# Kitty Lang

Kitty Lang is **a friendly scripting language with Rust's type system**:
structs, traits and generics, made accessible to beginners and embeddable
in the apps that use it. It exists to be the foundation of
[Village Kit](https://github.com/villagekit/villagekit), a code-as-CAD
system for open source makers, where a program is a physical design and
every value on screen can be traced back to the code that made it.

The syntax was worked out through the fifteen sketches in
[sketches/](sketches/), read-only history; the decisions they reached are
carried in [decisions/](decisions/README.md). The programs in
[examples/](examples/) are what the language is being built to run.

**Values**, ranked, the tiebreak for every scoping fight: a language that
runs the Village Kit examples end to end; then the tooling that makes it
pleasant in an editor; then ambition where it makes the best possible
language.

## Principles

1. **Accessible to beginners**, like Logo. **Powerful for experts**, like
   Haskell. The strangeness budget is spent on the type system, not the
   syntax.
2. **Structs and traits**, like Rust. A Rust-like type system: primitives,
   tuples and lists, enums (`Boolean`, `Option`, `Result`), generics with
   trait bounds in `where` clauses.
3. **Embeddable.** The compiler is a library first, usable from web and
   native apps. No daemon, no network.
4. **Language-aware.** The compiler is the linter, formatter and language
   server. The lexer and parser are resilient: wrong syntax anywhere still
   yields a tree and useful feedback everywhere else.
5. **Debug-friendly and evaluation-aware.** Every token, node and value
   keeps the span it came from, so a user knows exactly how and why
   something went wrong, and an editor can turn an interaction with a 3d
   object back into an edit of the code.
6. **Secure.** Safe with untrusted input: no ambient authority, no escape
   from the embedding.

Inspired by future-thinking languages: Julia, Flix, Gleam.

## Structure

One cargo workspace, the crates named `kitty-<dir>`, the pipeline in
order:

- `lexer/`: source text to tokens, `logos` kinds and the indenter that
  turns indentation into block tokens.
- `syntax/`: the `eventree` tree config, node kinds, the syntax tree types.
- `cst/`: typed views over the syntax tree.
- `parser/`: tokens to a concrete syntax tree, resilient, errors collected
  with spans.
- `hir/`: the high-level intermediate representation the analysis produces.
- `meta/`: source ids, spans, diagnostic rendering through `ariadne`.
- `number/`: `Number`, a `fastnum` decimal.
- `cli/`: the `kitty` command, which prints what the compiler sees: the
  tokens, the tree, the errors.
- `playground/`: the compiler as a library, through `wasm-bindgen`, and
  the web page that shows the tokens, the tree and the errors as the
  source is typed.

Around the code: `specs/`, the normative design; `docs/`, narrative for an
outside reader, opened by the glossary; `decisions/`, the decision
collection; `plans/`, the plan collection and the run sheet; `research/`,
dated syntheses of research sweeps; `examples/`, the end-to-end fixtures;
`fuzz/`, the fuzz target over the parser, outside the workspace, run on
demand; `sketches/`, the syntax history.

## Milestones

Cut so each bar is measurable with the tool that exists when it is checked.
The plans: [plans/README.md](plans/README.md).

- **M1: the lexer.** Source text to tokens, indentation to blocks. Done.
- **M2: the parser.** Tokens to a concrete syntax tree for the whole
  surface syntax the grilling of 2026-09-12 settled, resilient to
  malformed input, never panicking, the lexing and grammar specs written
  first and kept true. The exit bar: `just check` green, and the examples
  test showing zero parse errors for `units.kitty`, `3d-math.kitty` and
  `sample.kitty`, and for the other three only errors at the sites the
  open design plans own, each listed with its plan beside it.
- **M3: analysis.** Concrete syntax tree to HIR: name resolution, the
  item tree, types checked bidirectionally, with diagnostics rendered
  through `ariadne`. To be designed by a grilling and sliced before it
  starts.
- **M4: evaluation.** HIR to values, each value carrying the span that
  made it, embeddable from a host program. To be designed after M3.

## Engineering substrate

- Rust edition 2021, `rust-version` 1.83; `logos` for the lexer,
  `eventree` for the tree, `text-size` for ranges, `fastnum` for numbers,
  `ariadne` for diagnostics.
- The local quality gate is `just check` (fmt check, clippy `-D warnings`,
  tests). No CI host yet.
- Tests: `expect-test` snapshots of tokens and trees per grammar rule, the
  indenter's cases, recovery on malformed input, and the `examples/`
  programs end to end.
