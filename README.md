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

- Used
  - https://lunacookies.github.io/lang/
  - https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
  - https://thunderseethe.dev/series/making-a-language/
  - https://astexplorer.net/

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
- Only top-level declarations and expressions
  - In `let`, a newline is an implicit `in`.
- Use only `where` clause to describe type trait bounds, not in generic params
- Use `.` instead of `::` for type paths
- Use `[]` for generics, use `()` for tuples, use `List()` for lists, use `list.get` and `list.set` for get and set.
  - Although Flix seems to be able to use `[]` for generics _AND_ lists.
- Use labelled arguments like Gleam: https://tour.gleam.run/everything/#functions-labelled-arguments
- Match like Gleam: https://tour.gleam.run/everything/#data-types-record-pattern-matching
- `True` and `False` are part of a `Boolean` enum
  - but they are aliased so you can use as `True` and `False`.
- Every value has a reference to the code span which created it.
  - This is important for code-as-CAD editors where you can interact with an object in 3d space and it performs a macro on the code.

## Requirements

- Compiler can be used as linter, formatter, language server, etc
  - Even if some syntax is wrong, the compiler is able to parse the remaining correct syntax and still give useful feedback

## Open questions

- Should I rename the AST to:
    - "Item X"
      - And say within a struct, "Struct X"
    - "Expression X"
    - "Pattern X"
    - "Type X"
    - "Module X"
- Should I change "Type Path" to be like how Rust does it?
    - With "Type Path Segment", which has `identifier` and `arguments`
      - TODO: how do they do the type projection `<T as Trait>::Assoc`?
        - QSelf... hmm.. this is complicated
    - This is actually used in "Expr Path"
- Should values be lowercase and types be uppercase?
  - Like we encode this in the tokens: value identifier vs type identifier
