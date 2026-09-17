---
title: Attributes
status: done
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

Shipped. `@name(args)` on the lines before a declaration parses at
module level and in struct, enum, trait and impl bodies. The tree has a
new `Attribute` node (name, then an optional `FunctionArgList` in any of
the three call forms), and attribute nodes are the first children of
the declaration node, rust-analyzer's shape: the declaration rules now
take the caller's marker, started before the attributes. A dangling
attribute abandons that marker and sits beside a `Missing` node
(`Marker::abandon` gains its first caller). `At` joined
`DECLARATION_FIRST` and the four item-first sets, so it also serves as
a recovery token. `kitty-cst` gains `Attribute::name`,
`Attribute::arguments`, `attributes()` on every declaration view (the
leading attribute children only, so a dangling attribute in a body is
not among a struct's) and `ModuleLocal::declaration`.

Two things beyond the Work list, both needed for the Done-when on
`chair.kitty`, since a stray `:` in `@range(min: 5, ...)` cascaded
through the whole struct without them: in `( )` the `label :` spelling
latches into keyword arguments as `label =` does, so the error is
`expected ‘=’, but found ‘:’` at the colon, as it already was in `{ }`
and blocks; and in `( )` and `{ }` any other stray token between
arguments is one `Error` node and the list goes on. Chair's errors on
`@` lines are exactly the nine at its colons; the other examples change
only by `‘@’` joining the expected-item lists.

Flags for the operator, not decided here: an attribute before `export`
does not attach (`@a` then `export fn`), the spec's non-guarantees say
the position is unsettled and the productions put it after `export`;
`chair.kitty` puts its attributes inside the exported struct, so it is
unaffected.

Review findings not applied, with the reason: the two adjacent booleans
on the `*_optional_type_value` rules and the five parallel item
dispatchers with the same prologue and epilogue are the file's existing
shape, one helper over a closure would trade five plain if-chains for
indirection; the generic stray-token error lists every kind the
expression loop tested, which is the expected-list contract in the
grammar spec; a bare dangling `@name` at the end of input lists the
argument-list openers among the expected kinds, which is honest since
one could follow; `attributes()` on the compound views (`Declaration`,
`StructItem`, `TraitItem`, `ImplTraitItem`) has no caller yet, but the
compounds are what a consumer iterates, so that is where the view
belongs.

No visual gate: the change touches no route.

## Log
