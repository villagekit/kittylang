---
title: Write the lexing and grammar specs
status: done
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

Shipped: `specs/lexing.md` and `specs/grammar.md`, the settled surface as
normative text with every requirement citing the test that holds it or
`review only`, and a **Gap:** line per section where the code falls short;
`specs/README.md` links them; `DESIGN.md`'s M2 bar restated as the
record's exit demo; `docs/context.md` gains attribute, spread and `let
with`, loses the metadata comment, and points the lexing and parsing
terms at the specs.

Reading of the plan: the Done-when line "no slice or plan named" was read
as no implementation slice. The Work section asks twice that undecided
points be cited to their design plans, which the specs do by link
(newlines inside brackets, positional lines in a block, tuple structs,
trait-for-trait impls, modules and imports).

Rules the code does not hold, and the slice that holds each:

- Lexing: the package token lexes whole; `@` is an `At` token; `True` and
  `False` are type identifiers, so the `Boolean` kind leaves the lexer and
  the literal sets of the expression and pattern rules, and the unused
  `LiteralBoolean` node kind goes; `#= =#` comments: [[9d84d1f97437]].
  The `with` keyword: [[94bcf8245325]].
- Grammar: the import version suffix kept in the tree and `from` as a
  function name: [[9d84d1f97437]]. No panic on any input (the rule
  asserts, the unwraps on empty input, the `Self` pattern):
  [[277624e4f5fe]]. Return types, and a trait function ending at one:
  [[466ffcebbfc1]]. Keyword arguments with `=`, the `{ }` and block call
  forms, spread, `{ }` constructor patterns and `=` in `( )` fields
  (that rule also accepts `self` as a keyword-argument label today, which
  the spec does not): [[4423cd0aa2bb]]. A value segment after a type path: [[1493e2aa1777]].
  Attributes: [[328c0a9906fb]]. `let` without `in` and `let with`:
  [[94bcf8245325]].

Rules the code does not hold with no slice yet, for the operator:

- The lexer aborts on a block nested inside a tab-indented block:
  `lexer/src/indenter.rs` splits the whitespace run by an indentation
  width, not a byte offset. The same file queues no `Dedent` when a
  newline is followed by an `Error` token.
  The parser hangs on `fn f() => match x` and on `fn f() where`: the arm
  and bound loops consume nothing at end of input. [[277624e4f5fe]]'s
  inventory is asserts and unwraps in the parser; neither a lexer panic
  nor a hang is in it, nor in [[378fe0c39238]]'s bar.
- Attributes on `const` and `type` declarations: the spec follows
  [[03212e993893]] (any declaration); [[328c0a9906fb]]'s Work lists
  struct, enum, trait, impl, fn, prop and case. Whether an attribute
  precedes or follows `export` is unsettled too.

- A labelled generic argument is `Name = Type` ([[95cd2585f916]] lists
  `Map[key = String]`); `parser/src/grammar/type.rs` takes `Name: Type`.
  [[4423cd0aa2bb]] is the natural home. The decision's examples spell the
  label both as a type identifier (`Vector3[N = Number]`) and a value
  identifier (`Map[key = String]`); the spec states the `=` and leaves the
  label's class unsettled. A decision is needed.
- A constructor pattern's field is a name, as the code and
  [[95cd2585f916]]'s examples have it; [[4423cd0aa2bb]] says `name =
  pattern`. Whether a field may nest a pattern (`Some(None)`) is
  undecided; the spec lists it as a non-guarantee.

Findings, no change made:

- `pattern` in `parser/src/grammar/pattern.rs` parses an or-pattern on
  the `or` keyword token, untested: its tests write `|`, which does not
  lex, and the rule returns `None` on the plain path. No decision covers
  or-patterns; the spec states the `or` spelling as built and notes that.
- Review findings rejected: `DESIGN.md`'s M2 bar duplicates the record's
  exit demo (the plan's Work asks for that restatement); the token table's
  Enforced-by column and the member table are kept, since the
  specification skill wants a behaviour table to carry its enforcement,
  and the table is the finite set.
- The indenter counts a comment-only line's indentation, and accepts a
  dedent to a level not on the stack without an error. Both are in the
  spec's non-guarantees.
- The token-kind table is written by hand, since this slice ships no
  code; a test that renders it from `TokenKind` would keep it honest.
- The `import a: b` rename alias sits oddly beside [[95cd2585f916]]; left
  to [[a89ddd383a16]].
- Where a spread stands among the arguments of `( )` is settled by
  nothing; the spec lists it as a non-guarantee.

## Log

- 2026-09-17: shipped after five review rounds (Standards clean in round
  four; round five's one Spec finding was a wording fix verified against
  the indenter and applied without a further round). The record's Log
  records the call that the code is the defect where the specs and the
  code disagree.
