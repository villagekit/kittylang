---
title: Function return types
status: done
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

Shipped: `FunctionReturnType` in `kitty-syntax`; `function_declaration`
in `parser/src/grammar/function.rs` takes a `FunctionForm` (declaration,
trait declaration, lambda) in place of the two bools, parses `: Type`
after the parameter list for the two declaration forms, and still
requires `=>` before a body outside a trait. A type left out after the
colon recovers at `where` or `=>`. `kitty-cst` gains
`DeclarationFunction::return_type` and `FunctionReturnType::annotation`.

Snapshots: `function_with_a_return_type`,
`trait_function_ends_at_its_return_type`,
`trait_function_ends_at_its_where_clause`,
`function_with_a_return_type_left_out_recovers`,
`function_with_a_return_type_and_no_fat_arrow_recovers`, all in
`parser/src/grammar/declaration.rs`. Every `): Type =>` site in the
examples now parses clean. The examples test's expectations changed
shape as well as size: the bodies after those sites are now parsed as
function bodies instead of falling out to module level, so the
constructs inside them that later plans own (`Self { x: ... }`
construction, the `let Self { x, y, z }` pattern, `let with`, spread,
`from`) now report from inside a body, as `missing dedent` and
`expected ‘in’` where they used to be a run of module-level errors.
The same holds for `lex_example_3d_math` in `parser/src/lib.rs`.

In flight: the existing `trait_function_without_a_parameter_list_recovers`
snapshot changed for the better, `fn parts: Parts` now keeps `Parts` as
the return type and reports one error instead of two. The spec's
"review only" marks on the return-type sentences now cite tests, and its
gap line keeps only `from`.

Not touched: the `where` handling and the match-arm/where-bound
recovery loop that hangs on `fn f() where T: T => 1` stay as they were;
the return-type branch sits before the `where` branch and does not
enter it. `DeclarationFunction::body` in `kitty-cst` looks for an
`Expression` among the function's direct children, but the body is
wrapped in a `FunctionBody` node, which has no CST view at all, so
`body` returns `None`; a pre-existing gap, left for the plan that first
consumes the CST views.

Review: the Standards round asked for the production to name the node
(added `ReturnType` to the grammar block), rustdoc on the new CST
views, a snapshot for a type left out after the colon, and predicates
on `FunctionForm` in place of three `!=` comparisons; all applied. The
Spec round found the `=>` recovery sentence in the spec overstated
(the block's later lines fall out of the body; reworded to what the
rule does) and the trait `where` ending untested (snapshot added).
The second Standards round caught the `trait_function_ends_at_its_return_type`
snapshot lost to an editing slip (restored) and the `=>` recovery
sentence claiming more than its snapshot shows (narrowed).
Deferred: a test driving the new CST views, since `check_grammar` only
asserts the root casts and no view in `kitty-cst` is tested yet; the
plan that first consumes the views owns that. Also deferred: a
snapshot pinning that a lambda refuses `: Type`, since
lambda return types are out of this slice and the cascade it gives
today (`expected ‘=>’, but found ‘:’`) is the plain `expect` behaviour.

## Log
