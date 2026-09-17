---
title: The playground
status: done
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

Shipped. `playground/` (`kitty-playground`, `cdylib` and `rlib`) has one
pure function, `inspect_json`, over `inspect`, which lexes and parses the
source and returns `tokens` (kind name, byte range), `tokens_text` (the
lexer's `Tokens` printing, as `kitty lex`), `tree` (the parser's
printing, as `kitty parse`) and `errors` (byte range, message) as JSON;
the `wasm-bindgen` export `inspect` is one line over it. Snapshot:
`a_program_with_one_error_gives_its_tokens_tree_and_error`,
`playground/src/lib.rs`. `playground/web/index.html` is one file, no
framework, no bundler: a `textarea` over a `pre` cut into one span per
token with the class `tok-<Kind>`, the error ranges wavy-underlined, a
missing token a red bar at its offset (one bar per offset, its title
listing the messages there), the errors listed beneath the source with
line and column, the tokens and the tree on the right. `just playground`
adds the wasm target if missing, installs `wasm-bindgen-cli` at the
version `Cargo.lock` pins for the crate (the two must match), builds
into `playground/web/pkg/` (ignored) and prints how to serve the page.
`CLAUDE.md`, `DESIGN.md` and the glossary gained the crate, the recipe
and the term; the crate README notes the licences of every crate added
to the lockfile, all permissive.

Verified in headless Chrome, not by reading: a harness served beside the
page pasted `examples/units.kitty` into the textarea, dispatched `input`,
and read the panes: 248 token lines, identical to `kitty lex`; the tree
identical to `kitty parse`; the one error listed and underlined; the
coloured overlay's text equal to the source. An appended broken line
re-rendered all four with the second error marked and listed at the
position `kitty parse` gives (33:24). The same for two missing tokens at
one offset, a lone surrogate, non-ASCII text, and a U+2028 separator.

Deviations: `wasm-pack` was not tried, `wasm-bindgen-cli` alone does the
job, as the plan allowed. `wasm-bindgen` is pinned to 0.2.126 in the
lockfile to match the installed tool; the recipe installs the tool to
match the lockfile wherever they differ. `wasm-bindgen-cli` 0.2.126
wants Rust 1.86, above the workspace's `rust-version` 1.83, the shape
[[f280931fdfe2]] met with `clap`; it builds on the active toolchain, so
the pin is not a bound on it, and the recipe comment and README say so.
The JSON carries `tokens_text` beside the structured `tokens`, so the
tokens pane is the lexer's own printing (the plan's Interfaces line)
while the overlay has ranges to cut by. The page computes line and
column itself, from the byte range, by ariadne's rule (0.5.1,
`src/source.rs` separators and CRLF fold, `src/write.rs` byte-to-char
column, an end-of-text offset on the last line), checked equal to
`kitty parse` on every case tried. `CLAUDE.md`'s Testing said no I/O
boundary existed yet; it now names the command and the page as the
exempt, thin boundaries.

Review: four rounds on fresh sub-agents. Applied: the lexer's printing
for the tokens pane; a lone surrogate counted as the three bytes of the
U+FFFD the bindings' `TextEncoder` sends; missing-token markers merged
per offset; the marker's width netted to zero so the overlay stays
aligned; columns in code points and lines by ariadne's separator set,
CRLF folded, an offset on the LF of a pair kept on its line; the
README's licence list completed; wording in the README and glossary
(errors beneath, tokens and tree beside); a test hook removed; the
toolchain caveat. Rejected or deferred: a `debug_assert!` that the
tokens tile the source (the overlay is cut at every boundary over the
whole text, so a gap would show as uncoloured text, not garbled text);
emitting token text or UTF-16 offsets instead of byte offsets (byte
offsets keep the tokens pane identical to `kitty lex`); line and column
on `ErrorView` rather than in the page (the plan's errors are range and
message, and `kitty-meta` has no line function to share yet); the
`.tok-*` class list mirroring the lexer's enum (a new kind renders
uncoloured until a CSS line is added; the comment says so); the sample
program in the page (a demonstration, not a fixture); `just check` not
building the wasm target (`just playground` is the check, on demand).

Visual gate: none in this repo; the screenshot and the headless checks
above stand in.

This was the last open slice of [[5c5f4b9256b3]]; its exit demo is the
operator's to run.

## Log
