# Context

The domain glossary. When code, specs, plans, decisions, or commit messages
name one of these concepts, they use the term as defined here. Definitions
only; each entry links to the crate or spec that owns the mechanism. Where
a spec exists it is the owner; until then the crate is.

## The shape of the project

- **Kitty Lang**: the language. `kitty` in crate names and `.kitty` in
  file names. ([DESIGN.md](../DESIGN.md))
- **Village Kit**: the code-as-CAD system Kitty Lang is built for; its
  designs are the programs in `examples/`.
- **Pipeline**: source text to tokens to tree to HIR to values, one crate
  per stage. ([DESIGN.md](../DESIGN.md#structure))
- **Milestone**: a band of work with a measurable exit bar (M1 to M4).
  ([DESIGN.md](../DESIGN.md#milestones))
- **Kitty command**: the `kitty` binary that prints what the compiler
  sees: `kitty lex` the tokens, `kitty parse` the tree and the parse
  errors. ([cli/README.md](../cli/README.md))

## Lexing

- **Token**: a kind and a range of the source text.
  ([specs/lexing.md](../specs/lexing.md))
- **Token kind**: the closed enum of token classes, `logos`-derived.
  Value and type identifiers are distinct kinds.
  ([specs/lexing.md](../specs/lexing.md#tokens))
- **Indenter**: the pass that turns changes of indentation into block
  open and close tokens, so the parser sees blocks, not whitespace.
  ([specs/lexing.md](../specs/lexing.md#the-indenter))

## Parsing

- **Syntax tree**: the lossless tree of every token in the source, built
  by `eventree`, nodes and tokens both carrying ranges. (`kitty-syntax`)
- **Node kind**: the closed enum of syntax tree node classes.
  (`kitty-syntax`)
- **Concrete syntax tree (CST)**: typed views over syntax tree nodes, one
  type per node kind, cast on demand. (`kitty-cst`)
- **Parser**: the resilient recursive-descent grammar over tokens. It
  emits events, never builds the tree itself.
  ([specs/grammar.md](../specs/grammar.md))
- **Event**: what the parser emits: start node, token, or finish node.
  Errors are collected beside the events, never among them.
  (`kitty-parser`)
- **Marker**: the parser's handle on an open node, completed with a kind
  or abandoned. (`kitty-parser`)
- **Sink**: the pass that turns events into a syntax tree. (`kitty-parser`)
- **Parse error**: a missing or unexpected token with the kinds expected
  and where; collected, never thrown.
  ([specs/grammar.md](../specs/grammar.md#contract))
- **Recovery**: how the parser continues after an error so the rest of
  the source still parses.
  ([specs/grammar.md](../specs/grammar.md#recovery))
- **Attribute**: `@name(args)` on the line before a declaration: data
  the host reads from the item, its arguments an ordinary call argument
  list. The names are the host's, not the compiler's.
  ([specs/grammar.md](../specs/grammar.md#attributes))
- **Spread**: `...value` among a call's arguments: the fields of `value`
  supplied as keyword arguments.
  ([specs/grammar.md](../specs/grammar.md#calls-and-arguments))
- **let with**: `let with value` followed by an indented list of names,
  each bound to the field of that name on `value`: destructuring over
  several lines. ([specs/grammar.md](../specs/grammar.md#let))

## Analysis

- **HIR**: the high-level intermediate representation, the tree analysis
  produces from the CST, with names resolved. (`kitty-hir`)
- **Span**: a source id and a text range: where something came from.
  Every node and value keeps one. (`kitty-meta`)
- **Diagnostic**: a message with a span that the compiler gives a
  reader: a parse error today, analysis errors later. It is rendered as
  a **report** through `ariadne`: a headline, the source line, a label
  per span. (`kitty-meta`)
- **Source**: one unit of source text with an id. (`kitty-meta`)
- **Number**: the language's one numeric type, a decimal. (`kitty-number`)
