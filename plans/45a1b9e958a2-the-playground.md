---
title: The playground
status: todo
parent: 5c5f4b9256b3
blocked_by: f280931fdfe2
priority: medium
derived_from: 5c5f4b9256b3
---

A web page where the source is typed on the left and the tokens, the
syntax tree and the errors appear on the right, updated on every
keystroke, the source coloured by the lexer's own token kinds. The
instrument the design plans wait for ([[5c5f4b9256b3]]), and the first
embedding of the compiler as a library.

## Work

- A new crate `playground/` (`kitty-playground`), a workspace member
  with `crate-type = ["cdylib", "rlib"]`, exporting through
  `wasm-bindgen` one function that takes the source text and returns
  the tokens (kind, range), the tree as the parser prints it, and the
  errors (range, message), as JSON. The function is pure; the
  `wasm-bindgen` wrapper is one line over it.
- `playground/web/index.html`, plain HTML, CSS and JavaScript, no
  framework, no bundler: a textarea, a coloured overlay driven by the
  token ranges (one CSS class per token kind), the tree pane, the errors
  underlined in the source and listed beneath. Highlighting comes only
  from `kitty-lexer`, per CLAUDE.md's "one grammar, one home".
- A `just playground` recipe that builds with `wasm-pack` (or
  `wasm-bindgen-cli` if `wasm-pack` proves heavier than needed) into
  `playground/web/pkg/`, ignored by git, and prints how to open the
  page. The wasm target is installed as part of the recipe's
  instructions, not assumed.
- `CLAUDE.md`'s and `DESIGN.md`'s structure lists gain `playground/`;
  the Commands table gains `just playground`.
- Licences of `wasm-bindgen`, `serde-wasm-bindgen` or `serde_json`, and
  `wasm-pack` checked and noted in the crate README.

Interfaces: consumes the public token `Display` and `Parse` printing
from [[f280931fdfe2]].
Verify first: `rustup target list --installed` for `wasm32-unknown-unknown`.
Specs: none; `docs/context.md` gains "the playground".
Not this slice: analysis results, a language server, persistence of the
text, sharing links.

## Seams under test

The pure function from source to the JSON value, one `expect-test`
snapshot over a short program with one error. The page is I/O, untested.

## Done when

- `just playground` builds the wasm and reports where the page is
- Opening the page in a browser and pasting `examples/units.kitty` shows the coloured source, the tokens, the tree and the errors, and editing the text updates all four
- `cargo test -p kitty-playground` passes the snapshot
- `timeout 600 just check` is green

## Outcome

## Log
