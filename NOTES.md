# Notes

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
- https://steveklabnik.com/writing/the-language-strangeness-budget
- [rhombus](https://docs.racket-lang.org/rhombus)
  - [shrubbery](https://docs.racket-lang.org/shrubbery/)

- Used
  - https://lunacookies.github.io/lang/
  - https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
  - https://thunderseethe.dev/series/making-a-language/
  - https://astexplorer.net/

## TODO

- write ItemTree like https://github.com/rust-lang/rust-analyzer/blob/master/crates/hir-def/src/item_tree.rs
- Add `++` operator for append / concatenate
- Add `value.[Trait]` to cast value as trait object, to call trait method (with no ambiguity).


## Open questions

- Use `choice` instead of `enum`?
- Use `{}` instead of `[]` for type arguments?
    - This is what Julia does.
    - Because we don't need `{}` for blocks or objects.
    - But we could use `[]` for field get.

## Decisions

- Separated value and type identifiers, to be able to distinguish them in a pattern:
  - Otherwise no way to know if `None` is an existing type or a new variable name
  - Also helps for unifying `.` across both values and types.

```
match maybe_thing
  Some(thing) => ()
  None => ()
```
