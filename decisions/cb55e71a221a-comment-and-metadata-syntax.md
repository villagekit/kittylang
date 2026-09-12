---
title: "Comment and metadata syntax"
status: superseded
date: "2025-02-10"
tags: [syntax, lexer]
---

## Context

Struct props carry metadata such as defaults, and earlier sketches spent a
nested block on each (`prop x: Number` then `default = 0` below it). A
comment-shaped form keeps metadata beside the declaration.

## Decision

- `#` opens a single-line comment.
- `#= ... =#` is a multi-line comment.
- `#{ ... }` is a single-line metadata comment.
- `#={ ... }=#` is a multi-line metadata comment.

## Consequences

`#{ default = 0 }` above a `prop` attaches metadata to it. Metadata is
lexed, so the compiler sees it; plain comments are not.

Carried from [sketches/013.md](../sketches/013.md).
