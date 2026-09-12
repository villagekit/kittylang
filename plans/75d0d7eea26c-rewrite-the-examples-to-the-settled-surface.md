---
title: Rewrite the examples to the settled surface
status: todo
parent: 1cee599ce218
blocked_by:
  - 9d84d1f97437
  - 277624e4f5fe
  - 466ffcebbfc1
  - 4423cd0aa2bb
  - 328c0a9906fb
  - 94bcf8245325
  - target: 378fe0c39238
    strength: soft
    note: the exit demo runs last, after the fuzz target has seeded from the examples
  - 1493e2aa1777
priority: medium
derived_from: 1cee599ce218
---

The examples become the settled surface ([[1cee599ce218]]): every edit the
grilling of 2026-09-12 listed lands, and the examples test's
expectation shows only the sites the open design plans own, each
annotated with the plan's prefix. This is the M2 exit demo.

## Work

- `examples/`: `seatHeight` to `seat_height`; `GridPanel` imported
  beside `GridBeam`; `fn parts: Parts` to `fn parts(self): Parts`;
  `regular(self)` to `regular()`, `Self.regular()` unchanged;
  `fn plugins()` a method of `impl Assembly for Chair` returning
  `List(SmartFasteners())`; `false` and `true` to `False` and `True`;
  `'top'` to `"top"`; `[0, seat_width]` and its siblings to
  `(0, seat_width)`; `fn default()` in `3d-object.kitty` given `=>`;
  every `x: 0` argument and `Self { x: ... }` to `=`; the bare `in`
  line after the `let with` block in `chair.kitty` removed.
- Left as they are, each error in the expectation followed by the
  plan's prefix in a comment: `impl Object3d for Assembly`
  ([[3738718cde03]]); `struct Parts(...)` and `PartsItem.None`
  ([[dd325e81ad2c]]); the positional lines under `Vector3` and `Parts` and the `if` with no
  `then` among them ([[e6a33eab19d4]]); `case Name(Type)` variant
  payloads in `assembly.kitty` ([[dd325e81ad2c]]).
- The record's exit demo run and its result written to its Log.

Interfaces: consumes every grammar slice of the record.
Verify first: `cargo test -p kitty-parser examples` before editing, to
see which errors are grammar and which are the examples.
Specs: none; `examples/` is the fixture set.
Not this slice: anything the design plans own.

## Seams under test

The examples test.

## Done when

- The expectations for `units.kitty`, `3d-math.kitty` and `sample.kitty` are empty
- Every remaining line in the other three expectations carries a design plan prefix
- `timeout 600 just check` is green

## Outcome

## Log
