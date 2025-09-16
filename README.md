# Kitty Lang

Kitty Lang is a _work_in_progress_ programming language, basically "what if Rust was a friendly scripting language".

Kitty Lang is intended to be used as the foundation for [Village Kit](https://github.com/villagekit/villagekit), a future-thinking code-as-CAD system for open source makers.

## Principles

- Accessible to beginners, like [Logo](https://en.wikipedia.org/wiki/Logo_(programming_language))
- Powerful for experts, like [Haskell](https://www.haskell.org/)
- Structs and traits, like [Rust](https://rust-lang.org)
- Embeddable, so can be used in web or native apps
- Language-aware, so compiler can be used as linter, formatter, language server, etc
  - Even if some syntax is wrong, the compiler is able to parse the remaining correct syntax and still give useful feedback
- Debug-friendly, so you know exactly how and why something went wrong
- Evaluation-aware, so you even know where values come from
  - Every value has a reference to the code span which created it
  - This is important for code-as-CAD editors where you can interact with an object in 3d space and it performs a macro on the code
- Secure, so you are safe with untrusted input
- Inspired by future-thinking languages: [Julia](https://julialang.org/), [Flix](https://flix.dev/), [Gleam](https://gleam.run/)

## Progress

At the moment

- [x] Lexer (String -> Tokens)
- [x] Parser (Tokens -> Concrete Syntax Tree)
- [ ] Analysis (Concrete Syntax Tree -> High-level Intermediate Representation)
- [ ] Evaluator (High-level Intermediate Representation -> Value)
- ???

## Language

(_Incomplete_)

- Rust-like type system
  - Primitives
    - `()`: Unit type
    - `Number`
    - `String`
  - Compound types:
    - `(T, U...)`: Tuple
    - `List`
  - Enum types:
    - `Boolean`
    - `Option`
    - `Result`
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
