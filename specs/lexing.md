# Lexing

Source text to tokens: the token kinds and their spellings, and the
indenter that turns indentation into block tokens. The subsystem is the
`kitty-lexer` crate; its entry is `kitty_lexer::lex`, which takes a `&str`
and yields `Token { kind, range }` values in source order.

The contract, the citation form and the **Gap:** marker are in
[README.md](README.md).

## Terms

Token, token kind and indenter are defined in the glossary
([docs/context.md](../docs/context.md#lexing)). This spec adds:

- **Trivia**: the token kinds the parser skips: whitespace, newline and
  comment. `Indent` and `Dedent` are not trivia.
- **Indentation level**: the width of the whitespace that opens a line, a
  space counting one and a tab counting four.

## Contract

- The lexer must yield tokens whose ranges tile the source in order, with
  no gap and no overlap (`lex_example_basic`, `lexer/src/lib.rs`).
- The lexer must yield an `Error` token for each match attempt that
  fails, and continue (review only).
- The lexer must not panic on any input (review only).
- The lexer must yield block tokens (`Indent`, `Dedent`) in place of
  indentation, as the [indenter](#the-indenter) section says, so the parser
  sees blocks, not whitespace (`lex_indent`, `lexer/src/lib.rs`).
- Every other token must correspond to one match of one spelling in the
  [tokens](#tokens) table (review only). `@` and `#` are matched by a
  callback that reads on by hand: `logos` never backtracks, so a package
  pattern that fails after `@label` could not fall back to `@`; and it
  takes the longest match, so beside a line pattern `#= x =# y` on one
  line would be one comment to the end of the line, not one that ends at
  its `=#` (`at_or_package`, `comment`, `lexer/src/token.rs`;
  `lex_at_before_attribute_name`, `lex_comment_multi_line`,
  `lexer/src/lib.rs`; the rules are `logos`'s, see
  [references](#references)).
- `kitty_lexer::Tokens` must print the stream one token per line as
  `Kind@start..end`, the form the snapshot tests and the kitty command
  share (`lex_example_basic`, `lexer/src/lib.rs`).
- Where two spellings match the same text, the spelling with the higher
  priority must win. `logos` 0.15.0 gives each spelling a static priority
  from its shortest possible match, a literal counting more than a class,
  so a keyword beats an identifier; two spellings of equal priority are a
  compile error, resolved with an explicit priority as `Fn` and `_` do
  (`lex_fn_keyword`, `lexer/src/token.rs`; the rule is `logos`'s, see
  [references](#references)).

## Tokens

| Kind | Spelling | Enforced by |
| --- | --- | --- |
| `Whitespace` | one or more of space, tab, form feed | (`lex_whitespace`, `lexer/src/token.rs`) |
| `Newline` | `\n` or `\r\n` | (`lex_newline_crlf`, `lexer/src/token.rs`) |
| `Comment` | `#` to the end of the line; or `#=` to the first following `=#`, newlines included | (`lex_comment`, `lexer/src/token.rs`; `lex_comment_multi_line`, `lexer/src/lib.rs`) |
| `Indent`, `Dedent` | emitted by the indenter, never matched | (`lex_indent`, `lexer/src/lib.rs`) |
| `String` | `"` to `"`, with the escapes `\"`, `\n`, `\t`, `\u` | (`lex_string`, `lexer/src/token.rs`) |
| `Number` | `0` or an integer with no leading zero, then an optional `.` fraction, then an optional `e` or `E` exponent with an optional sign | (`lex_number`, `lexer/src/token.rs`) |
| `IdentifierValue` | leading underscores, then lowercase letters, then `_`-separated runs of lowercase letters and digits: `seat_width` | (`lex_identifier_value`, `lexer/src/token.rs`) |
| `IdentifierType` | leading underscores, then an uppercase letter and lowercase letters or digits, repeated: `GridBeam`, `Vector3` | (`lex_identifier_type`, `lexer/src/token.rs`) |
| `Package` | `@`, a name, `/`, a name, each name one or more of letters, digits and `-`: `@std/math`, `@villagekit/smart-fasteners` | (`lex_package`, `lexer/src/lib.rs`) |
| `At` | `@` not opening a package | (`lex_at_before_attribute_name`, `lexer/src/lib.rs`) |
| `ParenOpen`, `ParenClose` | `(`, `)` | (`lex_left_parenthesis`, `lexer/src/token.rs`) |
| `BraceOpen`, `BraceClose` | `{`, `}` | (`lex_left_brace`, `lexer/src/token.rs`) |
| `BracketOpen`, `BracketClose` | `[`, `]` | (`lex_left_bracket`, `lexer/src/token.rs`) |
| `Comma` | `,` | (`lex_comma`, `lexer/src/token.rs`) |
| `Colon` | `:` | (`lex_colon`, `lexer/src/token.rs`) |
| `Ellipses` | `...` | (`lex_ellipses`, `lexer/src/token.rs`) |
| `DotBracketOpen` | `.[` | (`type_projection_happy`, `parser/src/grammar/type.rs`) |
| `Dot` | `.` | (`lex_dot`, `lexer/src/token.rs`) |
| `Arrow` | `->` | (`type_function_happy`, `parser/src/grammar/type.rs`) |
| `FatArrow` | `=>` | (`lex_fat_arrow`, `lexer/src/token.rs`) |
| `Underscore` | `_` alone | (`pattern_wildcard`, `parser/src/grammar/pattern.rs`) |
| `Plus`, `Minus`, `Multiply`, `Divide` | `+`, `-`, `*`, `/` | (`lex_plus`, `lexer/src/token.rs`) |
| `GreaterEqual`, `Greater`, `LessEqual`, `Less` | `>=`, `>`, `<=`, `<` | (`lex_greater_equal`, `lexer/src/token.rs`) |
| `EqualEqual`, `NotEqual`, `Equal` | `==`, `!=`, `=` | (`lex_equal_equal`, `lexer/src/token.rs`) |
| `Error` | any byte no other spelling matches; or `#=` with no `=#` after it, to the end of the input | review only for the first; (`lex_comment_multi_line_unterminated`, `lexer/src/lib.rs`) for the second |

Keywords, one kind each, spelled as written: `fn`, `Fn`, `let`, `in`,
`with`, `if`, `then`, `else`, `match`, `case`, `self`, `Self`, `type`,
`const`, `enum`, `struct`, `prop`, `impl`, `trait`, `where`, `for`,
`import`, `export`, `from`, `and`, `or`, `xor`, `not`, `rem`
(`lex_let_keyword`, `lex_with_keyword`, `lexer/src/token.rs`).

The kinds are the `TokenKind` enum in `lexer/src/token.rs`; a kind absent
from this table is a defect in one of the two.

## Identifiers

- A value identifier and a type identifier must be distinct kinds, told
  apart by their first letter
  ([f2708b12](../decisions/f2708b12e004-value-and-type-identifiers-are-separate.md);
  `lex_identifier_value`, `lex_identifier_type`, `lexer/src/token.rs`).
- `True` and `False` must lex as type identifiers: they are variants of
  the `Boolean` enum and no keyword (`lex_true_as_type_identifier`,
  `lex_false_as_type_identifier`, `lexer/src/token.rs`).
- `Self` and `self` must lex as keywords, not identifiers
  (`lex_example_3d_math`, `lexer/src/lib.rs`).
- `from` must lex as a keyword; the grammar accepts it where a function
  name stands ([grammar](grammar.md#functions)) (`lex_from_keyword`,
  `lexer/src/token.rs`).
- Note: a camel-case name such as `seatHeight` is not one identifier. It
  lexes as `seat` then `Height`.

## Literals

- A string literal must be double-quoted. There is no single-quoted form
  (`lex_string`, `lexer/src/token.rs`).
- A number literal must be decimal, as the [tokens](#tokens) table spells
  it; there is no sign in the literal, a leading `-` being the unary
  operator (`negation`, `parser/src/grammar/expression.rs`).

## Packages and versions

- A package name must lex as one `Package` token, `@` included
  (`lex_package`, `lexer/src/lib.rs`).
- A version suffix on an import, `@std/assembly:1`, must lex as `Colon`
  then `Number` after the package; the import rule owns it
  ([grammar](grammar.md#module)) (`lex_package`, `lexer/src/lib.rs`).
- An `@` that does not open a package must lex as `At`, the attribute
  marker ([03212e99](../decisions/03212e993893-attributes-replace-metadata-comments.md))
  (`lex_at_before_attribute_name`, `lexer/src/lib.rs`).

## Comments

- `#` must open a comment that ends at the end of the line
  ([cb55e71a](../decisions/cb55e71a221a-comment-and-metadata-syntax.md),
  comment syntax; `lex_comment`, `lexer/src/token.rs`).
- `#=` must open a comment that ends at the first following `=#`, lines
  between included (`lex_comment_multi_line`, `lexer/src/lib.rs`).
- A comment is trivia: the lexer must yield it as a token
  (`lex_comment`, `lexer/src/token.rs`).
- The parser must skip a comment
  (`infix_expression_interspersed_with_blocks_and_comments`,
  `parser/src/grammar/expression.rs`).
- The compiler reads no comment: `#{` and `#={` open ordinary comments
  ([03212e99](../decisions/03212e993893-attributes-replace-metadata-comments.md))
  (`lex_metadata_shapes_as_plain_comments`, `lexer/src/lib.rs`).
- A newline inside a multi-line comment belongs to the comment token, so
  the indenter does not see it (`lex_comment_multi_line`,
  `lexer/src/lib.rs`).

## The indenter

The indenter runs over the matched tokens and keeps a stack of open
indentation levels, starting at `0`. It acts on each `Newline` by looking
at what follows it.

- When a `Newline` is followed by a `Newline`, or by `Whitespace` then a
  `Newline`, the line is blank: the indenter must change no level, and
  yields the whitespace as `Whitespace` (`lex_indent_empty_lines`,
  `lexer/src/lib.rs`).
- When a `Newline` is followed by `Whitespace` then any other token, the
  indenter must measure the indentation level of that whitespace and
  compare it with the top of the stack (`lex_indent`, `lexer/src/lib.rs`):
  - If it is greater, the indenter must push it, yield `Whitespace` for
    the part of the run up to the previous level (when that part is not
    empty), then yield one `Indent` whose range is the rest of the run.
  - If it is less, the indenter must yield the whole run as `Whitespace`,
    then pop each level greater than it and yield one `Dedent` per level
    popped, each with an empty range at the end of the run
    (`lex_indent_2`, `lexer/src/lib.rs`).
  - If it is equal, the indenter must yield the run as `Whitespace` and
    nothing else.
- When a `Newline` is followed by a token that is not `Whitespace` and
  not `Newline`, the indenter must pop every level above `0` and yield one
  `Dedent` per level, each with an empty range at the end of the newline
  (`lex_indent_2`, `lexer/src/lib.rs`).
- At the end of the source, the indenter must pop every level above `0`
  and yield one `Dedent` per level, each with an empty range at the end of
  the source (`lex_indent`, `lexer/src/lib.rs`).
- The indenter must yield the `Newline` first and the tokens it queued
  after it, so a block's `Indent` follows the newline that ends the line
  above it (`lex_example_basic`, `lexer/src/lib.rs`).
- Indentation on the first line of the source must yield `Whitespace`
  and no `Indent` (review only).
- Whether a newline inside `( )`, `[ ]` or `{ }` is subject to these
  rules is undecided
  ([design plan d0658cb1](../plans/d0658cb19697-design-newlines-inside-brackets.md)).
  Today the indenter applies them to every newline, brackets included.

Gap: the indenter measures a tab as four but splits the whitespace run by
an indentation width, so a block nested inside a tab-indented block
yields a `Whitespace` token that overruns the text and aborts in
`TextRange::new`. And a `Newline` followed by an `Error` token queues no
`Dedent`; the dedents arrive at the next newline that dedents, or at the
end of the source.

Example (non-normative). For

```
fn foo()
  fn bar()
    baz
```

the tokens after `fn foo()` are `Newline`, `Indent` (the two spaces),
`fn`, ..., `Newline`, `Whitespace` (two spaces), `Indent` (two more),
`baz`, `Newline`, `Dedent`, `Dedent` (`lex_indent`, `lexer/src/lib.rs`).

## Failure

- If a match attempt fails, then the lexer must yield one `Error` token
  for the bytes that attempt consumed and resume after them, so a run of
  unmatched bytes yields one `Error` token per attempt (review only).
- If a `#=` has no `=#` after it, then the lexer must yield one `Error`
  token from the `#` to the end of the input
  (`lex_comment_multi_line_unterminated`, `lexer/src/lib.rs`).
- The lexer reports nothing else. Unbalanced indentation, an unterminated
  string and an unterminated multi-line comment are the parser's to report
  from the tokens it receives (review only).

## Non-guarantees

- The indenter does not check that a dedent returns to a level on the
  stack: a line indented less than the top and more than the level below it
  pops the top and pushes nothing.
- The indenter does not exempt a line holding only a comment: its
  indentation counts like a line of code.
- A tab's width of four is the current measure, not a promise.

## References

- [grammar.md](grammar.md): what the parser does with these tokens.
- Decisions [f2708b12](../decisions/f2708b12e004-value-and-type-identifiers-are-separate.md),
  [cb55e71a](../decisions/cb55e71a221a-comment-and-metadata-syntax.md) (comment syntax; superseded for metadata),
  [03212e99](../decisions/03212e993893-attributes-replace-metadata-comments.md).
- `logos` 0.15.0 matches the spellings. Its disambiguation rule is the
  crate's `book/src/token-disambiguation.md`; the arithmetic that holds
  is `Mir::priority` in `logos-codegen-0.15.0/src/mir.rs`, which counts
  a class as the book's prose does not. That it never backtracks is
  `book/src/common-regex.md`; a callback may return the token kind
  itself, `logos-0.15.0/src/internal.rs` (`CallbackResult for T`), and
  extend the token with `Lexer::bump`, `src/lexer.rs`. All are read in
  the cargo registry.
