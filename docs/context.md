# Context

The domain glossary. When code, specs, plans, decisions, or commit messages
name one of these concepts, they use the term as defined here. Definitions
only; each entry links to the crate or spec that owns the mechanism. Until
the specs exist the crate is the owner.

## The shape of the project

- **Kitty Lang**: the language. `kitty` in crate names and `.kitty` in
  file names. ([DESIGN.md](../DESIGN.md))
- **Village Kit**: the code-as-CAD system Kitty Lang is built for; its
  designs are the programs in `examples/`.
- **Pipeline**: source text to tokens to tree to HIR to values, one crate
  per stage. ([DESIGN.md](../DESIGN.md#structure))
- **Milestone**: a band of work with a measurable exit bar (M1 to M4).
  ([DESIGN.md](../DESIGN.md#milestones))

## Lexing

- **Token**: a kind and a range of the source text. (`kitty-lexer`)
- **Token kind**: the closed enum of token classes, `logos`-derived.
  Value and type identifiers are distinct kinds. (`kitty-lexer`)
- **Indenter**: the pass that turns changes of indentation into block
  open and close tokens, so the parser sees blocks, not whitespace.
  (`kitty-lexer`)
- **Metadata comment**: a `#{ ... }` comment the compiler reads, attached
  to the declaration below it. (`kitty-lexer`)

## Parsing

- **Syntax tree**: the lossless tree of every token in the source, built
  by `eventree`, nodes and tokens both carrying ranges. (`kitty-syntax`)
- **Node kind**: the closed enum of syntax tree node classes.
  (`kitty-syntax`)
- **Concrete syntax tree (CST)**: typed views over syntax tree nodes, one
  type per node kind, cast on demand. (`kitty-cst`)
- **Parser**: the resilient recursive-descent grammar over tokens. It
  emits events, never builds the tree itself. (`kitty-parser`)
- **Event**: what the parser emits: start node, token, finish node, or an
  error. (`kitty-parser`)
- **Marker**: the parser's handle on an open node, completed with a kind
  or abandoned. (`kitty-parser`)
- **Sink**: the pass that turns events into a syntax tree. (`kitty-parser`)
- **Parse error**: a missing or unexpected token with the kinds expected
  and where; collected, never thrown. (`kitty-parser`)
- **Recovery**: how the parser continues after an error so the rest of
  the source still parses. (`kitty-parser`)

## Analysis

- **HIR**: the high-level intermediate representation, the tree analysis
  produces from the CST, with names resolved. (`kitty-hir`)
- **Span**: a source id and a text range: where something came from.
  Every node and value keeps one. (`kitty-meta`)
- **Source**: one unit of source text with an id. (`kitty-meta`)
- **Number**: the language's one numeric type, a decimal. (`kitty-number`)
