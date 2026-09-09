---
title: "Traits have associated types"
status: accepted
date: "2025-02-10"
tags: [types]
---

## Context

Operator traits such as `Add` and `Mul` over unit types (sketch 011's
dimensions) need an output type that depends on the implementing type.

## Decision

Traits may declare associated types, as Rust's do.

## Consequences

`impl Add for Length` can name `type Output = Length`; the type checker
must resolve associated types through impls.

Carried from [sketches/014.md](../sketches/014.md).
