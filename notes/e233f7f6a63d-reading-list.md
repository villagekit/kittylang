---
title: "Reading list"
tags: [research]
---

Sources the language and compiler draw on. The first group was used
directly in the code that exists; the rest are references to read, not
recall, when the work reaches them.

## Used

- https://lunacookies.github.io/lang/ (the eldiro series: the lexer,
  event-based parser and sink are shaped on it)
- https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
- https://thunderseethe.dev/series/making-a-language/
- https://astexplorer.net/

## Compilers and languages

- [tuqqu/oxide-lang](https://github.com/tuqqu/oxide-lang)
- [rmehri01/bidirectional](https://github.com/rmehri01/bidirectional)
  (404 as of 2026-09-09; JDemler's implementation below covers the same
  2013 algorithm, but has no license file: read, never transcribe)
- [JDemler/BidirectionalTypechecking](https://github.com/JDemler/BidirectionalTypechecking)
- [minirust/minirust](https://github.com/minirust/minirust)
- [zesterer/tao](https://github.com/zesterer/tao/)
- [roc-lang/roc](https://github.com/roc-lang/roc)
- [gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- [flix/flix](https://github.com/flix/flix)
- [sway](https://docs.fuel.network/docs/sway/)
- [naalit/pika](https://github.com/naalit/pika/)
- [rhombus](https://docs.racket-lang.org/rhombus) and
  [shrubbery](https://docs.racket-lang.org/shrubbery/), for
  indentation-sensitive syntax
- The eldiro lineage: [eldiro](https://github.com/lunacookies/eldiro),
  [fictional-dollop](https://github.com/lunacookies/fictional-dollop),
  [haze](https://github.com/lunacookies/haze),
  [gingerbread](https://github.com/gingerbread-lang/gingerbread),
  [capy](https://github.com/capy-language/capy)
- rust-analyzer's `hir-def` `item_tree.rs`, the model for the item tree

## Type checking

- [gadt.pdf](https://www.cl.cam.ac.uk/~nk480/gadt.pdf)
- [bidirectional.pdf](https://davidchristiansen.dk/tutorials/bidirectional.pdf)
- [nbe](https://davidchristiansen.dk/tutorials/nbe/), via a
  [r/ProgrammingLanguages thread](https://www.reddit.com/r/ProgrammingLanguages/comments/14czkbu/comment/joppssr/)
- [Class notes on type inference](https://cs.hofstra.edu/~cscccl/csc123/typing.pdf)

## Essays

- https://mckayla.blog/posts/all-you-need-is-data-and-functions.html
- https://steveklabnik.com/writing/the-language-strangeness-budget

## Log

- 2026-09-09: the eldiro lineage, thunderseethe, the type-checking papers,
  oxide-lang, Gleam and rust-analyzer were read by the research sweep in
  `research/20260909-rust-like-compiler-architecture-synthesis.md`.
