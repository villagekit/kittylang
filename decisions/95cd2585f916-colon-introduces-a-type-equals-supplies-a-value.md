---
title: Colon introduces a type, equals supplies a value
status: accepted
tags:
  - syntax
date: 2026-09-12
---

## Context

The sketches used `=` to set a struct field or a metadata key (sketch 004
onwards) and `:` for keyword arguments in a call (sketch 008 onwards),
and [[881178303bc8]] then made a struct call and a function call the same
form, so the two spellings met at the same site. Meanwhile `:` already
introduces a type everywhere a thing is declared: `prop x: Number`,
`fn f(x: Length): Self`. Letting `:` also supply a value, as Rust struct
literals and Gleam labelled arguments do, overloads one character with
two meanings that look identical in an indented block.

## Decision

`:` always introduces a type. `=` always supplies a value by name. Every
site:

```
prop back_height: Number              # declare: colon, type
fn translate(self, x: Length): Self   # declare: colon, type
GridBeam.Z(x = 0, y = 0)              # call: equals, value
Self { x = self.x + other.x }         # construct: equals, value
Self                                  # construct, indented block
  back_height = 10
let Self { x, y, z } = self           # pattern shorthand
let Self { x = a } = self             # pattern rename
@range(min = 5, max = 10)             # attribute arguments
Vector3[N = Number]                   # generic default
Map[key = String, value = Mesh]       # labelled generic arguments
```

## Consequences

A beginner learns one sentence: colon is a type, equals is a value. `=`
also binds in `let`, as it does in Python, and record patterns use `=`
as OCaml's do, so no pattern spelling is invented. [[881178303bc8]] stands
as written; it is about the brackets, not the separator. The option
rejected was `:` everywhere a value is supplied by name, which would have
given `:` a third meaning in labelled generic arguments.

Decided in the grilling of 2026-09-12.
