---
title: "Call syntax: positional, keyword and indented arguments"
status: accepted
date: "2025-02-10"
tags: [syntax]
---

## Context

Structs and functions need a call form that reads well both inline and as
an indented block, and Village Kit programs pass many named parameters.

## Decision

A struct or function is called with:

- `()` for positional arguments;
- `{}` for keyword arguments;
- an indented block for keyword arguments, the same as `{}`.

Arguments are converted automatically where `Into<T>` is implemented.

## Consequences

Two call surfaces for keyword arguments, one grammar. Automatic `Into`
conversion means a `Meter` can be passed where a `Length` is expected
(sketch 012's units example).

Carried from [sketches/012.md](../sketches/012.md).
