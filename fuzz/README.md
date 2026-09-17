# kitty-fuzz

The fuzz target that keeps the never-panic rule held as the grammar grows:
`parse` feeds arbitrary bytes that are valid UTF-8 to `kitty_parser::parse`
(the lexer, the indenter and the parser together). A panic, an abort or a
hang is a failure; a parse error is not.

It is not part of `just check`. Run it on demand:

```sh
timeout 120 just fuzz        # 60 seconds of fuzzing
just fuzz 600                # longer
```

The recipe installs `cargo-fuzz` if it is absent and needs the nightly
toolchain (`rustup toolchain install nightly`); the first run pays for the
install and the build, so bound it only once they are in place. It seeds
the corpus from `examples/` on every run; the corpus and the findings
under `fuzz/corpus/` and `fuzz/artifacts/` are not committed.

When a run finds something, it writes the input to
`fuzz/artifacts/parse/` and prints how to replay and shrink it:

```sh
cargo +nightly fuzz run parse fuzz/artifacts/parse/crash-<hash>
cargo +nightly fuzz tmin parse fuzz/artifacts/parse/crash-<hash>
```

Turn the shrunk input into a snapshot test at the rule it broke, fix the
rule, and replay the artifact before deleting it. A `timeout-` or `oom-`
artifact is a loop that consumes nothing; `specs/grammar.md` says how the
loops must end.

This crate is excluded from the workspace, because `cargo fuzz` builds it
with its own profile and sanitizer flags. So `just check` never formats,
lints, builds or tests it: a change to `kitty_parser::parse` shows up here
only on the next `just fuzz`, and `cargo fmt` is run in this directory by
hand.
