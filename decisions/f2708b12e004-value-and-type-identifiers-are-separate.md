---
title: "Value and type identifiers are separate"
status: accepted
date: "2025-09-16"
tags: [syntax, lexer]
---

## Context

In a pattern such as `Some(thing)` or a bare `None`, the parser must know
whether a name is an existing type or a new variable binding. With one
identifier class there is no way to tell.

## Decision

Value identifiers and type identifiers are lexed as different token kinds,
distinguished by their spelling (lowercase values, capitalised types), so
a pattern can tell a constructor from a binding without lookup.

## Consequences

A pattern `Some(thing)` binds `thing` and matches `Some`; `None` alone
matches, never binds. The rule also unifies `.` across values and types,
since a path segment's kind is known from its spelling.

Carried from the old `NOTES.md`, "Decisions".
