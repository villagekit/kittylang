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

## TODO

- rename to Ako ("to learn" in Maori)?
  - https://maoridictionary.co.nz/word/158

- use `cstree` or `rowan`?
  - https://github.com/zesterer/chumsky/pull/681
  - https://github.com/spreadsheet-lang/spreadsheet/blob/main/lang/src/parser.rs
- use `ungrammar`?
  - https://rust-analyzer.github.io/blog/2020/10/24/introducing-ungrammar.html

## Language

- Rust-like type system
- Functions can be called with either positional args or keyword args
  - Type generics are the same
- Functions can describe multi-arity multi-type overloads
- Use `.` instead of `::` for type paths
