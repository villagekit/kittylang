---
title: Write the lexing and grammar specs
status: todo
parent: 1cee599ce218
priority: high
derived_from: 1cee599ce218
---

`specs/lexing.md` and `specs/grammar.md`, the first two specs, written
before any lexer or grammar code moves ([[1cee599ce218]]). They state the
lexer and the grammar as they are, plus the surface the grilling of
2026-09-12 settled, stated as normative current-state text. Where the code
disagrees, the code is the defect, the slices that follow fix it, and
the record's Log records that call; no spec names a slice.

## Work

- `specs/lexing.md`: token kinds and their spellings; value and type
  identifiers ([[f2708b12e004]]); numbers, double-quoted strings, `True`
  and `False` as plain type identifiers and no keyword (the code has a
  `Boolean` token today); comments `#` and
  `#= =#` (the code lacks the second today) and no metadata comment ([[03212e993893]]); the package token
  and the `:1` version suffix; the `@` token; the indenter's rule for
  indent, dedent and blank lines, with newlines inside brackets stated
  as undecided and cited to [[d0658cb19697]].
- `specs/grammar.md`: the module and its imports and exports;
  declarations (struct, prop, enum and case, trait, impl, fn with
  parameters, return type and mandatory `=>` then an inline expression
  or an indented block); attributes before a declaration
  ([[03212e993893]]); expressions, including calls with positional `( )`
  and keyword `{ }` or indented arguments using `=` ([[95cd2585f916]],
  [[881178303bc8]]), `...spread`, `let` with the newline as implicit `in`
  and `in` allowed on one line, `let with`, `if` with and without `else`,
  `match`, lambdas; patterns; types, generics with `[ ]`
  ([[ec7345d92813]]), `where` clauses, associated types
  ([[fb9b8ee85b39]]); recovery: every rule produces a node, errors
  collected with spans, no panic. Positional lines in an indented
  argument block, tuple structs and trait-for-trait impls are stated as
  undecided and cited to their design plans.
- `specs/README.md`: the table's first two rows point at the files,
  "comments and metadata" becomes "comments" ([[03212e993893]]), and
  the "No spec is written yet" paragraph goes.
- `DESIGN.md`: the M2 milestone's exit bar restated as the record's exit
  demo says it, since the design plans own some example sites.
- `docs/context.md`: attribute, spread and `let with` added; the
  **Metadata comment** term deleted, since [[03212e993893]] removed the
  form. The command
  and the playground are [[f280931fdfe2]]'s and [[45a1b9e958a2]]'s.

Verify first: read `parser/src/grammar/*.rs` and `lexer/src/token.rs`
against every rule written, and list in this slice's Outcome every
rule the code does not hold and the slice that will hold it, so the
gap is tracked in plans, never in the spec.
Specs: creates both.
Not this slice: any code.

## Seams under test

None: prose. The rules are made true by [[9d84d1f97437]] to [[75d0d7eea26c]].

## Done when

- `specs/lexing.md` and `specs/grammar.md` exist, every requirement in RFC 2119 words, no slice or plan named in either
- The Outcome lists every rule the code does not yet hold and the slice that holds it
- `specs/README.md`'s table links them
- `timeout 600 just check` is green

## Outcome

## Log
