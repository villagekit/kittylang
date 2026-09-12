---
title: Attributes
status: todo
parent: 1cee599ce218
blocked_by:
  - 9d84d1f97437
  - 4423cd0aa2bb
  - 277624e4f5fe
priority: medium
derived_from: 1cee599ce218
---

Attributes ([[03212e993893]]): `@name(args)` on the lines before a
declaration attaches to it, the arguments an ordinary argument list, so
`@range(min = 5, max = 10)` and `@requires(fn (self) => ...)` parse
([[1cee599ce218]]).

## Work

- `kitty-syntax`: an attribute node holding the name and the argument
  list; `parser/src/grammar/declaration.rs`: zero or more attributes
  before a struct, enum, trait, impl, fn, prop or case, each `At` then a
  value identifier then the argument list from [[4423cd0aa2bb]] (parentheses
  optional when there are no arguments). An attribute with nothing after
  it is an error with a `Missing` declaration.
- `kitty-cst` views: a declaration's attributes, an attribute's name and
  arguments.
- Snapshots: one attribute, several, one with a lambda argument, one
  dangling.

Interfaces: consumes the `At` token from [[9d84d1f97437]] and the argument list
from [[4423cd0aa2bb]].
Verify first: `grep -rn "At\b" parser/src` prints nothing before
[[9d84d1f97437]] lands.
Specs: `grammar.md`, attributes.
Not this slice: what the host does with an attribute.

## Seams under test

`kitty_parser::parse` snapshots; the examples test on `chair.kitty`.

## Done when

- Snapshots show one attribute, several, one with a lambda argument and one dangling, each attached to the declaration below or to a `Missing` node
- The examples test's expectation for `chair.kitty` lists errors on `@` lines only at the `:` arguments [[75d0d7eea26c]] rewrites
- `timeout 600 just check` is green

## Outcome

## Log
