---
title: "Lexer: packages, attributes, no metadata comments, import versions"
status: todo
parent: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
  - f280931fdfe2
priority: medium
derived_from: 1cee599ce218
---

The lexer catches up with the settled surface ([[1cee599ce218]]): a package
name lexes whole, `@` is a token, `True` and `False` are type
identifiers and not a keyword, the multi-line comment exists, `from` is a contextual keyword, and an
import may carry a version suffix.

## Work

- `lexer/src/token.rs`: the `Package` regex, whose trailing class lacks
  a `+` so `@std/math` lexes as `@std/m` and `ath`; an `At` token for
  `@` not followed by a package path; the `Boolean` kind, a dedicated
  `(True|False)` regex that shadows `IdentifierType`, removed so the two
  lex as type identifiers per [[f2708b12e004]] and the parser treats
  them as any variant: the kind leaves `LITERAL_FIRST` in
  `parser/src/grammar/expression.rs` and `PATTERN_LITERAL_FIRST` in
  `pattern.rs`, `NodeKind::LiteralBoolean` leaves `kitty-syntax`, the
  `Boolean` token and `Literal` compound views leave `kitty-cst`, and
  the affected declaration snapshots are updated: a `True` is a type
  path, nothing more; the `#= ... =#` multi-line comment added, since
  today `#.*` is the only comment. There is no metadata comment kind to
  remove: [[cb55e71a221a]] was never lexed.
- `fn from(value)` parses: `from` is a token kind today, so every
  `from` method in `units.kitty` is an error. The parser holds tokens
  and ranges but no source text, so a contextual keyword matched by
  text would mean threading the source through `Parser` and `Source`;
  instead the function-name rule and the value path segment accept the
  `From` token as a name, the narrowest change that makes `impl From`
  work. `import` and `export` stay keywords; a `from` variable or field
  waits for a need.
- `kitty-syntax` and `kitty-parser`: the import rule accepts an optional
  `:` and number after the package, kept in the tree as a version node,
  its meaning deferred to [[a89ddd383a16]].
- The lexer snapshots for each change; the examples test's expectation
  shrinks by every line-1 import error.

Interfaces: produces the `At` token for [[328c0a9906fb]].
Verify first: `cargo run -p kitty-cli -- lex examples/3d-math.kitty` shows
the split package on line 1; `grep -n "True|False" lexer/src/token.rs`
shows the `Boolean` kind; `parser/src/parser.rs` and `source.rs` hold
no `&str`.
Specs: `lexing.md`, packages, identifiers and comments; `grammar.md`,
imports.
Not this slice: parsing attributes ([[328c0a9906fb]]).

## Seams under test

`kitty_lexer::lex` snapshots; the import rule's parser snapshot; the
examples test.

## Done when

- `cargo run -p kitty-cli -- lex examples/chair.kitty` shows `@std/assembly` as one package token, `@label` as an `At` token then an identifier, and `False` as a type identifier
- A parser snapshot shows `import Assembly from @std/assembly:1` with a version node holding `1`, and another shows `fn from(value) => value` parsing as a function named `from`
- The examples test's expectations for `3d-math.kitty` and `3d-object.kitty` no longer list an error on line 1
- `timeout 600 just check` is green

## Outcome

## Log
