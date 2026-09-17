# kitty-cli

The `kitty` command prints what the compiler sees. It is the first
instrument for language design: a syntax question is answered by running
a file through it, not by reading the grammar.

```sh
cargo run -p kitty-cli -- lex examples/units.kitty
cargo run -p kitty-cli -- parse examples/units.kitty
```

## Subcommands

- `kitty lex <file>` prints the token stream, one token per line, as
  `Kind@start..end`, the way the lexer's snapshot tests print it. The
  ranges are byte offsets into the file. The exit code is 1 when the
  stream holds an `Error` token, 0 otherwise.
- `kitty parse <file>` prints the syntax tree, every token with its text,
  the way the parser's snapshot tests print it. Then it prints each parse
  error as a report: the headline `syntax error`, the file, line and
  column, the source line, and a label with what was expected and what
  was found. The exit code is 1 when there are parse errors, 0 otherwise.

Both exit 2 when the file cannot be read, is not UTF-8, or the output
cannot be written; the reason goes to stderr, except for a closed pipe
(`kitty parse file | head`), which is silent.

The output has no colour, so it can be piped to a file or a diff.
