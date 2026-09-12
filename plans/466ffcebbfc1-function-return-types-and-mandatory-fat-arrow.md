---
title: Function return types
status: todo
parent: 1cee599ce218
blocked_by:
  - 0248a546fe20
  - 942ff7a43d93
priority: medium
derived_from: 1cee599ce218
---

Functions declare their return type ([[1cee599ce218]]): `fn length(self): N =>`
and `fn default(): Self` in a trait parse. The mandatory `=>` before a
body is already held by the grammar and only needs its spec sentence.

## Work

- `kitty-syntax` gains a return type node; `parser/src/grammar/function.rs`
  parses an optional `: Type` after the parameter list, then requires
  `=>` when a body follows. In a trait, a declaration with no body ends
  at the return type. The body after `=>` is an inline expression or an
  indented block, as today.
- `kitty-cst` views for the return type.
- Snapshots: happy path with and without a return type, a trait
  declaration ending at the return type, a return type with no `=>` and a
  body recovering with one error.

Interfaces: none beyond the node; [[75d0d7eea26c]] rewrites `fn default()`.
Verify first: `function_declaration_option_name_body` in
`parser/src/grammar/function.rs` goes from the parameter list straight
to `where` or `=>`, with no `:` branch.
Specs: `grammar.md`, functions.
Not this slice: lambdas' return types, which stay unannotated.

## Seams under test

`kitty_parser::parse` snapshots per case; the examples test.

## Done when

- The examples test's expectations list no error at a `): Type =>` site in `units.kitty` or `3d-math.kitty`
- `timeout 600 just check` is green

## Outcome

## Log
