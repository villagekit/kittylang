---
title: M3 grilling
status: todo
parent: 436aea0e22af
blocked_by:
  - 0ccbdb209358
  - d0658cb19697
  - 3738718cde03
  - e6a33eab19d4
  - dd325e81ad2c
  - a89ddd383a16
priority: medium
---

The grilling that designs M3, analysis: concrete syntax tree to HIR, name
resolution, the item index, bidirectional type checking, trait
resolution, diagnostics through `ariadne`. Held by [[0ccbdb209358]] and
by the design plans whose answers it consumes. Its inputs: the research
synthesis `research/20260909-rust-like-compiler-architecture-synthesis.md`,
whose section 9 lists twenty open questions; the outcome of
[[a89ddd383a16]] on modules and [[3738718cde03]] on supertraits; and one
answer the grilling of 2026-09-12 gave without a decision item: trait
objects, `impl Trait` as a type with dynamic dispatch, are wanted, and
the user is open to an interpreted design with runtime reflection if
that is the best language. One syntax question waits here too, since
the analysis is what gives it weight: the class of a labelled generic
argument's label. [[95cd2585f916]] writes the argument with a value
identifier, `Map[key = String]`, and the parameter with a type
identifier, `Vector3[N = Number]`; the parser takes a type identifier
for both (`specs/grammar.md`, Types), carried from [[6419ca56e149]].

## Questions carried from M2

Seven surface-syntax questions the M2 slices left open, each a language
decision and so the user's. Research of 2026-09-19 (Opus, read-only) found
evidence and a lean for each; none is decided. `specs/grammar.md` lists all
but the fifth as a Non-guarantee, so the spec already says they are open.

1. **Nesting of `#= ... =#`.** Today the first `=#` ends the comment
   (`specs/lexing.md`), so commenting out a block that holds a comment
   gives four confusing errors. Julia, Rust, OCaml and Swift all nest; C
   is the outlier. Lean: nest, about ten lines in `comment()`
   (`lexer/src/token.rs`).
2. **An attribute before `export`.** The spec puts it after
   (`export @label("x") fn f()`); [[03212e993893]] only says "the line
   before the declaration". Rust and Swift put attributes outermost;
   TC39 allows either but not mixed. No example exercises it. Lean:
   accept the attribute before `export`.
3. **The class of a generic argument's label.** Beyond the note above:
   the only true labelled-argument example in the repo is
   `Map[key = String, value = Mesh]`; `Vector3[N = Number]` is a
   parameter default. A parameter is a type identifier, so `key` could
   never name one. Rust and Scala label with the parameter's own type
   name. Lean: type identifier, and supersede the `Map[key = ...]` line
   of [[95cd2585f916]] to `Map[Key = String, Value = Mesh]`.
4. **A nested pattern in a constructor field.** `Some(None)` has no
   spelling; `Self { x = Some(a) }` cascades into five errors. Rust,
   OCaml and Gleam all nest, and the pattern rule is already recursive
   one level up. Lean: a field holds a full pattern.
5. **Or-pattern spelling.** Built as the `or` keyword; no decision
   covers it, and `|` does not lex. Rust, OCaml, Gleam and Python use
   `|`; Flix reserves the words `and`/`or`/`not`. Lean: keep `or`. Two
   defects to settle with it: `2 or 3 or 4` builds a right-nested
   `PatternOr` rather than one flat node (recorded in the spec as built),
   and `pattern()` in `parser/src/grammar/pattern.rs` discards its
   completed marker when no `or` follows.
6. **Where `...spread` may stand in `( )`.** The parser takes any number
   anywhere; all six uses in the repo lead an indented block. Gleam and
   Roc put the update first; Rust puts it last; JavaScript and Python
   allow any and document which value wins. Lean: first only, once, with
   a clear error elsewhere.
7. **`a.B`, a type identifier after a value.** Unparsed; nothing in the
   repo, sketches or docs uses it. The one mainstream use is a
   module-qualified type (Gleam), which [[a89ddd383a16]] on modules
   decides. Lean: leave it unparsed, do not close the door.

Not researched, found in the M2 review notes, for the analysis design
rather than for a decision: the CST's positional accessors and its
`Expression` compound without type paths (`cst/src/lib.rs`);
`Literal::Boolean` unreachable in `hir/src/lib.rs`;
`DeclarationFunction::body` returning `None` because `FunctionBody` has
no CST view; the CST views having no tests of their own.

## Work

Fresh conversation, `grilling` over the inputs above, then `/to-plan`
for the M3 record and `/to-plan-slices` for its slices; `specs/types.md`
and `specs/analysis.md` cut as the answers crystallise.

## Seams under test

None: the outcome is a record.

## Done when

- The M3 record is minted with `parent` [[436aea0e22af]] and its slices
  are in the order of work
- `specs/types.md` and `specs/analysis.md` exist

## Outcome

## Log
