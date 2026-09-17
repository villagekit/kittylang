# kitty-playground

The playground is a web page where the source is typed on the left, the
tokens and the syntax tree appear on the right and the errors are listed
beneath the source, all updated on every keystroke. It is the second
instrument for language design, after the `kitty` command, and the first
embedding of the compiler as a library.

```sh
just playground
python3 -m http.server -d playground/web 8000
# then open http://localhost:8000/
```

## The crate

Three names, one job:

- `kitty_playground::inspect` takes source text and returns an
  `Inspection`: `tokens`, each `{kind, start, end}` with the kind's name
  as the lexer's tests print it and the byte range in the source;
  `tokens_text`, the tokens printed the way `kitty lex` prints them;
  `tree`, the syntax tree printed the way `kitty parse` prints it; and
  `errors`, each `{start, end, message}` with a byte range (empty for a
  missing token) and the parse error's message.
- `kitty_playground::inspect_json` is the same as a JSON string. It is
  pure and tested by a snapshot.
- The `wasm-bindgen` export, `inspect` in JavaScript, is one line over
  `inspect_json`.

## The page

`web/index.html` is plain HTML, CSS and JavaScript: no framework, no
bundler. A `textarea` sits over a `pre` that holds the same text cut into
one `span` per token, with the class `tok-<Kind>` for the lexer's token
kind, so the colouring comes from `kitty-lexer` alone. Error ranges get
the class `err` and a wavy underline; a missing token gets a red bar at
its offset. The errors are listed beneath the source with their line and
column; the tokens and the tree fill the right-hand column.

`just playground` builds `web/pkg/`, which git ignores. It adds the
`wasm32-unknown-unknown` target if it is missing and installs
`wasm-bindgen-cli` at the version `Cargo.lock` pins for the crate, since
the two must match. The tool builds on the active toolchain and wants a
newer Rust than the workspace's `rust-version` (1.86 for 0.2.126). The
page imports `pkg/kitty_playground.js` as a module, and that fetches the
wasm, which browsers refuse from `file://`: serve the directory over
HTTP.

## Licences

Every crate this one adds to `Cargo.lock`, read from its manifest:

- `wasm-bindgen` 0.2 and its `wasm-bindgen-macro`,
  `wasm-bindgen-macro-support` and `wasm-bindgen-shared`, and the
  `wasm-bindgen-cli` tool: MIT OR Apache-2.0
- `serde`, `serde_core`, `serde_derive` and `serde_json` 1.0: MIT OR
  Apache-2.0; their dependencies `itoa` (MIT OR Apache-2.0), `memchr`
  (Unlicense OR MIT), `zmij` (MIT)
- `bumpalo` and `cfg-if`, pulled in by `wasm-bindgen`: MIT OR Apache-2.0
- `wasm-pack` is not used: `wasm-bindgen-cli` alone does the job.
