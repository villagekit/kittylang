## Resources

- \*[tuqqu/oxide-lang](https://github.com/tuqqu/oxide-lang)
- \*[gadt.pdf](https://www.cl.cam.ac.uk/~nk480/gadt.pdf)
- \*[bidirectional.pdf](https://davidchristiansen.dk/tutorials/bidirectional.pdf)
- \*[rmehri01/bidirectional](https://github.com/rmehri01/bidirectional)
- https://www.reddit.com/r/ProgrammingLanguages/comments/14czkbu/comment/joppssr/
    - [nbe](https://davidchristiansen.dk/tutorials/nbe/)
- [JDemler/BidirectionalTypechecking](https://github.com/JDemler/BidirectionalTypechecking)
- [minirust/minirust](https://github.com/minirust/minirust)
- [Class notes on Type Inference](https://cs.hofstra.edu/~cscccl/csc123/typing.pdf)
- [zesterer/tao](https://github.com/zesterer/tao/)
- [roc-lang/roc](https://github.com/roc-lang/roc)
- [gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- [flix/flix](https://github.com/flix/flix)
- [sway/sway](https://docs.fuel.network/docs/sway/)
- https://mckayla.blog/posts/all-you-need-is-data-and-functions.html
- https://lunacookies.github.io/lang/
  - https://github.com/lunacookies/eldiro
  - https://github.com/lunacookies/fictional-dollop
  - https://github.com/lunacookies/haze
  - https://github.com/gingerbread-lang/gingerbread
  - https://github.com/capy-language/capy
- https://github.com/naalit/pika/

## TODO

- rename to Ako ("to learn" in Maori)?
  - https://maoridictionary.co.nz/word/158

- use `cstree` or `rowan`?
  - https://github.com/zesterer/chumsky/pull/681
  - https://github.com/spreadsheet-lang/spreadsheet/blob/main/lang/src/parser.rs
- use `ungrammar`?
  - https://rust-analyzer.github.io/blog/2020/10/24/introducing-ungrammar.html
- use `chalk`?
  - https://rust-lang.github.io/chalk/book/
  - or use concepts: https://rustc-dev-guide.rust-lang.org/traits/goals-and-clauses.html
  - or use next-gen solver concepts: https://rustc-dev-guide.rust-lang.org/solve/trait-solving.html
- interesting to see how `fuel` handles Spans in the AST
  - Spanned is a trait
  - only tokens actually store spans, then tree enums implement functions to return the node span using the leaf spans
  - also means all tokens are stored in the AST

## Language

- Rust-like type system
- Functions can be called with either positional args or keyword args
  - Type generics are the same
- Functions can describe multi-arity multi-type overloads
- Use only `where` clause to describe type trait bounds, not in generic params
- Use `.` instead of `::` for type paths
- Can we remove the turbofish? (`::<T>`)
  - https://github.com/rust-lang/rust/blob/e98309298d927307c5184f4869604bd068d26183/src/test/ui/parser/bastion-of-the-turbofish.rs
  - https://github.com/rust-lang/rfcs/pull/2527#issuecomment-414635205
  - Is there a better alternative?
    - Use `[]` for generics, use `()` for tuples, use `List()` for lists, use `list(x)` for indexing
      - Am interested in this...
      - Scala implements `.apply` on the List class to do this: https://www.scala-lang.org/api/current/scala/collection/immutable/List.html
    - Always parse `<>` in favor of generics
- Use labelled arguments like Gleam: https://tour.gleam.run/everything/#functions-labelled-arguments
- Match like Gleam: https://tour.gleam.run/everything/#data-types-record-pattern-matching
- `True` and `False` are part of a `Boolean` enum
  - but they are aliased so you can use as `True` and `False`.
