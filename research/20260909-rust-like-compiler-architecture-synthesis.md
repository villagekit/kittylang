# The simplest good architecture for a Rust-like compiler

A synthesis of the research sweep run on 2026-09-09, before the design
interview for M3 (analysis) and M4 (evaluation).

**What was asked.** Given eleven sources named by the user (the eldiro
lineage, the thunderseethe series, two bidirectional typing papers and a
Rust implementation of one, and the oxide-lang interpreter), what is the
simplest good architecture for the analysis and evaluation stages of a
Rust-like scripting language with structs, enums, traits with associated
types and where-bounded generics? What should Kitty copy, what can it skip
because it does not need everything, and what did the sources get wrong?

**What ran.** Seven Opus sub-agents, one per source family, each reading
primary sources at a pinned commit and citing file and function: (1) the
eldiro series and repo; (2) fictional-dollop and haze; (3) gingerbread and
capy; (4) the thunderseethe series and its repos; (5) the Dunfield and
Krishnaswami paper, Christiansen's tutorial and the Rust implementation;
(6) oxide-lang; (7) a comparator on Gleam and rust-analyzer, the two
compilers CLAUDE.md names as normative references, which the user's list
did not include. Their full reports are in the shared transcripts repo,
`../transcripts/kittylang/2026-09-09-rust-like-compiler-architecture/`.
Load-bearing claims were spot-checked against the clones by the main
session.

**What it feeds.** The grilling that settles M3's design and M2's exit bar,
and through it `specs/analysis.md`, `specs/types.md`, the decisions the
grilling records, and the M3 plan record. The open questions at the end
are the grilling's agenda.

**Caveats.** `github.com/rmehri01/bidirectional` returns 404 and is absent
from that account's public repositories; the sweep substituted
`JDemler/BidirectionalTypechecking`, which implements the same 2013
algorithm, and read that paper too. That substitute has no license file,
so it is a reference to read, never to transcribe. Everything here is
true of the pinned commits listed at the end; the eldiro-lineage repos
are dormant, rust-analyzer and Gleam move.

## The answer

Every source that finished an analysis stage converged on the same
skeleton, and the ones that skipped part of it paid for it later:

1. **An item index before any body work.** Collect every top-level
   signature (struct, enum, trait, impl, fn, with their generics and
   bounds) into a map first; lower and check bodies only after every name
   is known. This is what makes forward references and mutual recursion
   free, and it is the only way to find impls, which are anonymous.
2. **An arena HIR with spans in a side table**, a `Missing` hole for
   anything the resilient parser left out, and types in a second side
   table rather than a typed copy of the tree.
3. **Bidirectional checking with unification variables inside bodies
   only**, annotations mandatory on every item, no let-generalisation,
   rigid type parameters from day one. The union-find is `ena`.
4. **Trait resolution as an impl index plus a bounded direct lookup**, not
   a solver. Associated types resolve by name lookup once the impl is
   picked.
5. **Errors collected, never thrown, with a poison value** that stops one
   error from cascading: a fresh type variable, not an error type.

The rest of this document says where each piece comes from and what it
replaces.

## 1. The item index comes first

The eldiro series never gets here: it stops immediately after lowering,
its HIR has no spans, no name resolution and no types (eldiro
`f3d588f`, `crates/hir/src/database.rs`; Part Twenty ends "In the next
part, we'll implement string literals", never published). Everything
after lowering is what its author and her successors had to invent.

What they invented, three times over, is the index. fictional-dollop has
`raw_index::Stub` (signatures only, no bodies) resolved into
`resolved_index::Stub` before any body is lowered (`e7fbf7d`,
`crates/raw_index/src/lib.rs:11-18`, `crates/resolved_index/src/lib.rs`).
gingerbread has `hir::index` producing an `Index` per file and a
`WorldIndex` across files, walked before `hir::lower` touches a body
(`9af94d6`, `crates/hir/src/index.rs`, `world_index.rs`, `body.rs`). capy
kept the same split (`b381c40`, `crates/hir/src/index.rs`). haze, the
author's last compiler, was wiped and rebuilt without the lossless tree,
without markers, without collected errors, but the `indexer` then
`resolver` then `sema` order survived the wipe (`dd18376`,
`src/indexer.rs`, `src/resolver.rs`; commit `ed585a5` "Wipe"). Gleam does
the same thing under different names: register custom types, sort type
aliases topologically, register every function signature, then infer
bodies in strongly-connected groups from a call graph (`19bf207`,
`compiler-core/src/analyse.rs:239-320`, `call_graph.rs`).

rust-analyzer's item tree exists for two reasons, and only one applies to
Kitty. The incrementality reason (an "invalidation barrier" so editing a
body does not recompute name resolution, `f312032`,
`crates/hir-def/src/item_tree.rs:11-14`) does not. The other does:
`impl Add for Length` is anonymous. No scope points at it, so before any
body is typed the module must have been swept and an index from
`(trait, head constructor of the self type)` to impl ids built
(`crates/hir-ty/src/method_resolution.rs:659-666, 814-825`). Gleam never
needs this index because Gleam has no impls.

**For Kitty.** The index is a crate boundary or a module boundary,
signatures only, keyed by interned names, holding for each item its
generics and where bounds as types. Kitty's `fn f(x: T): U` and
`prop x: Number` are annotated, so signatures can be collected in this
pass without inferring any body (gingerbread's choice; capy's index
degenerated to a name set because its definitions had no annotations and
it then needed a restart-on-missing-dependency state machine to order
inference, `crates/hir_ty/src/lib.rs:47, 576-700`). Cycles among type
aliases and constants get one "circular definition" diagnostic from a
topological sort, as both capy and Gleam do.

## 2. Lowering: total, spanned, fused with resolution

**Total.** The one trick eldiro teaches is that lowering takes an
`Option` and returns a hole: `lower_expr(&mut self, ast: Option<ast::Expr>)
-> Expr` yields `Expr::Missing` when the accessor gave `None`
(`crates/hir/src/database.rs:23-35`). Every descendant kept it
(fictional-dollop `crates/hir/src/lib.rs:135-155`; gingerbread and capy
`lower_expr`). Kitty's parser already emits an empty `NodeKind::Missing`
node with a position where eldiro emits nothing, so Kitty's holes can
carry a span, which eldiro's cannot. The hole needs to exist for
expressions, patterns and type annotations alike.

**Spanned, out of band.** Every source that dropped spans at the HIR
boundary became useless for an editor: eldiro, fictional-dollop and haze
have no map from a semantic node back to source. Every source that kept
them did it the same way: bulk expression spans in an `ArenaMap<Idx<Expr>,
TextRange>` filled right after allocation, inline ranges on the few
non-expression nodes that need them (gingerbread
`crates/hir/src/body.rs:12-20`; capy `body.rs:159-174, 255-320`);
rust-analyzer's `BodySourceMap` is the same table with the stated reason
that inference should be "agnostic to the actual positions of expressions
in the file" (`crates/hir-def/src/expr_store/body.rs:60-70`);
thunderseethe keys diagnostics by `NodeId` and resolves the span at report
time through `ast_to_cst` (`af30598`, `desugar/base/src/lib.rs:27-56`),
and panoply stores `spans: FxHashMap<Idx<Term>, Span>` beside its arena
"because we won't need them for most operations" (`32054d7`,
`crates/ast/src/lib.rs:296-302`). Gleam is the one counterexample, with
`location: SrcSpan` on every typed node, and pays 1940 lines of
`TypedExpr` mirroring 341 lines of `UntypedExpr` (`compiler-core/src/ast/
typed.rs`, `untyped.rs`).

**Fused.** fictional-dollop and haze both resolve names and check types
in the same pass that lowers: `fn expr(cst) -> (Id<Expr>, Id<Ty>)`
(`crates/hir/src/lib.rs:135-155`), `analyze_expression(ast, requested_ty)
-> Idx<Expression>` with `expression_tys` as a side table
(`src/sema.rs:39-47`). thunderseethe splits them (desugar, then name
resolution, then types), but only ever resolved local variables with a
persistent map (`name_resolution/base/src/lib.rs`); the real item-level
resolution lives in panoply's 1859-line `crates/nameres`. For Kitty, with
the index already in hand, resolving a name during lowering is a lookup,
and a miss can produce a hole with a diagnostic immediately. The one
reverse map worth building during lowering is gingerbread's `symbol_map:
FxHashMap<ast::Ident, Symbol>` from a CST identifier to what it resolved
to (`crates/hir/src/body.rs:76-83`): a dozen lines, and it is the whole of
go-to-definition and hover (`crates/ide/src/lib.rs:244-284`). capy dropped
it and has no IDE.

**Arenas, not boxes.** Part Nineteen of the series moves from `Box<Self>`
to a typed `Idx<T>` arena and then deletes its hand-rolled arena for
`la-arena`; fictional-dollop rolled its own again and haze went back to
`la-arena`; gingerbread rolled one and capy replaced it with `la-arena
0.3`. The lesson has now been learned four times. Kitty's `hir/src/lib.rs`
has `Node<T>` with two TODOs, "Use arena id to refer to nodes" and "Add
optional type?"; the sources answer both: `Idx` into per-body arenas for
expressions, patterns and type annotations, with the type in a side table.
One latent bug to avoid: fictional-dollop compares `Ty::Pointer(Id<Ty>)`
by arena index, so two structurally equal types allocated separately are
unequal (`crates/arena/src/lib.rs:97-101`). Types must be interned or
compared structurally, never by allocation.

## 3. Types: bidirectional, rank-1, annotated items, `ena`

**The papers say the simple shape suffices.** Dunfield and Krishnaswami
2013 (§8, "Eliminating type inference") describe exactly the design
point Kitty wants: drop the synthesis rules for unannotated lambdas and
unit, and turn them into checking rules against an unsolved existential
so that `f ()` with `f : ∀α. α → α` still checks. Those two rules survive
verbatim into the 2019 paper (Fig. 14, p. 9:21). The 2019 paper adopts
"the simpler rule that all polymorphic definitions are annotated" (§3,
p. 9:7) and does "no generalization at all: every polymorphic function
takes an annotation" (§8, p. 9:27). Its heavy machinery, polarised
subtyping and principality tracking, exists only to serve first-class
existentials and GADT equations (§2, pp. 9:4-9:6), neither of which Kitty
has. The ordered context's one distinctive payoff is "easily expressing
polymorphic generalization" (§8, p. 9:27), a problem Kitty declines to
have. The paper also admits it "relies upon monotypes" and lacks type
constructors with arguments and recursive types (§8, p. 9:26), which is
precisely Kitty's `List[Number]` and every user struct. So the papers are
the on-ramp and the ceiling, and Kitty sits at the on-ramp: Christiansen's
check/synthesise pair with annotation, application and checking-mode
lambdas (§1.2, pp. 3-6), plus the two DK rules above.

**The implementations agree.** thunderseethe's base checker is 691 lines
including tests: bidirectional `infer`/`check` that emit constraints,
a solver over the constraint list, then a substitution walk
(`types/base/src/lib.rs:527-550`). The substitution is `ena`'s
`InPlaceUnificationTable` with `TypeVar: UnifyKey, Value = Option<Type>`
(`types/base/src/lib.rs:107-156`), rustc's own union-find, permissively
licensed. The author's stated reason for the split: "constraint solving
only has to know about constraints and nothing about the AST", so it
survives the AST growing to 100+ nodes. Items are checked against their
annotation, never inferred, because inference of top-level definitions
"causes action at a distance", and only items carry a type scheme
("check-top-level-items"). Annotations force the rigid/flexible split: a
rigid `TypeVar` equal only to itself versus a solvable unifier, with
leftover unifiers converted to rigid variables so that "unification
variables don't escape the type checker" (`types/items/src/ty.rs:105-120`).
The author's account of adding that split late is a multi-night refactor;
Kitty has annotated items from the start and should have both kinds from
the first commit. Gleam's `Hydrator` does the same job under a different
name: named type variables in an annotation become rigid generics so the
signature is checked against the body, not inferred from it
(`compiler-core/src/type_/hydrator.rs:30-93`). rust-analyzer's expected
type is `Expectation::{None, HasType, Castable, RValueLikeUnsized}`
(`crates/hir-ty/src/infer.rs:2602-2607`); Kitty needs the first two.

**What happens without variables.** gingerbread and capy have no
unification at all. gingerbread is pure bottom-up synthesis and cannot
infer anything (`crates/hir_ty/src/lib.rs`, `expect_match`). capy grew
"weak types" (`IInt(0)` meaning any integer), a `Ty::max` join, and two
mutation passes, `replace_weak_tys` and `reinfer_usages`, that rewrite
already-typed expressions when a stronger type shows up
(`crates/hir_ty/src/globals.rs:254-430`). That is ad hoc unification for
twelve numeric types. Kitty has one `Number`, a real checker, and
generics with bounds, so `Ty::Var` plus `ena` is strictly simpler than
the alternative.

**The hole is a fresh variable.** Three sources independently chose the
same poison: thunderseethe's `Hole` gets a fresh unification variable
("a fresh type variable will always unify with a type... after that first
unification our type variable is solved"), Gleam's `ExprTyper::infer`
catches any error and returns `TypedExpr::Invalid` carrying a fresh
unbound variable (`type_/expression.rs:732-750, 1780-1794`), and
rust-analyzer's `infer_expr_coerce` reports a mismatch and returns the
target type anyway (`crates/hir-ty/src/infer/expr.rs:117-121`).
gingerbread and capy use a bottom `Unknown` type with an early return when
either side is unknown (`globals.rs:4262-4269`). The variable is the
better choice: it keeps inferring. thunderseethe's own caveat applies to
Kitty, which has `match` and `if`: a variable solved by its first
unification can still produce a second spurious error at a later branch.

**Errors carry provenance.** thunderseethe's constraints carry a
`Provenance` (a node id plus the reason the check was made) so the
generic solver can produce "expected this to be a function" rather than
"types differ" (`types/base/src/lib.rs:158-212`); panoply put a bare span
on each constraint and lost the reason. One error per node, the tree's
hierarchy giving the error hierarchy, and the result returned as
`(typed, errors)` rather than `Result`, is the shape Kitty's "collect,
never throw" rule wants. thunderseethe only retrofitted this onto its
base checker; the rows and items checkers still bail at the first error
(`types/items/src/unification.rs:31-42`). Kitty must design resilience
together with trait obligations, not after.

## 4. Traits without a solver

No source in the user's list implements traits. The eldiro lineage never
got past records; the papers' type languages have no class constraints,
methods or projections; thunderseethe's nearest analogue is its row
combination predicate, deferred while under-determined and woken when a
variable is solved (`types/items/src/lib.rs:60-118`), which is the right
lifecycle for a where bound but has no impl index and no coherence rule.
Gleam has no traits and splits `+` into `AddInt` and `AddFloat` at parse
time (`type_/expression.rs:1808-1855`), so its operator machinery cannot
host `impl Add for Length`. rust-analyzer has abandoned its own solver
twice: chalk is gone, replaced by rustc's next-generation solver vendored
as `ra-ap-rustc_next_trait_solver` (`crates/hir-ty/Cargo.toml:41`). The
lesson is not "use a solver" but "a general trait solver is not something
a small team maintains".

What rust-analyzer does before reaching the solver is most of the work,
and none of it is solving. For `a + b`: infer the left operand, mint a
variable for the right, map `+` to `Add::add` ("we always treat operators
as if they are overloaded", `crates/hir-ty/src/infer/op.rs:52-113,
158-205`); build the goal `Length: Add[?R]`; look up candidates by
`(TraitId, head constructor)` in `TraitImpls`, a hash map with a separate
blanket list (`method_resolution.rs:659-666, 814-825`); per candidate,
mint fresh generic arguments, unify the impl's self type with the
receiver, then turn the impl's where clauses into obligations
(`method_resolution/probe.rs:1595-1630`). Method calls add one step
before all that: inherent candidates are searched before trait
candidates (`probe.rs:1466-1475`), which Kitty needs because
`examples/chair.kitty` has `fn regular(self): Self` inside `struct Chair`.
Once the impl is picked, `Self.Output` is a lookup by name in the impl's
items plus substitution (`method_resolution.rs:460-486`).

**The bounded lookup.** Replace the solver with: candidates from the
index, unify each impl's self type and trait arguments with the goal,
require exactly one survivor (else "no impl" or "ambiguous", citing both
impls' spans), then recurse on each where clause with a depth counter
that reports exhaustion as a diagnostic. For a language with no
overlapping impls, no specialisation, no negative reasoning, no
lifetimes and no higher-ranked bounds, that is the whole solver, on the
order of 150 lines. Two cases need care. A projection on a generic
parameter (`T.Output` where `T: Add`) has no impl to pick and must stay
rigid, unifying only with an identical projection. And
`Length.from(m)` has two candidates, `From[Meter]` and `From[Feet]`,
separated only by the argument's type; rust-analyzer keeps the obligation
pending until the argument is known, and an eager design that infers the
argument first is a few hundred lines simpler and rejects some programs
Rust accepts. panoply's answer to ordering is to bucket constraints by
kind and solve equalities before obligations, so an unsolved obligation
is genuinely unsolvable rather than merely early (`crates/tc/src/infer.rs:
49-63, 1041-1090`).

Gleam's `FieldMap { arity, fields: label -> index }` with `reorder`
(`compiler-core/src/type_/fields.rs:11-90`) is the mechanism for Kitty's
keyword arguments: reorder at check time so the evaluator sees positional
arguments.

## 5. Evaluation: what the checker removes from the evaluator

oxide-lang (`5088a2d`) is the negative result. It has no stage between
parse and eval, so every conformance question is answered at run time by
one predicate, `vtype_conforms_val` (`oxide-interpreter/src/val.rs:
1050-1088`), called at every let, assignment, parameter bind, return,
field set and construction; trait conformance is `impls.contains(name)`
on a `Vec<String>`; match exhaustiveness is a runtime error. Its
environment is a chain of string-keyed hash maps with mangled keys
(`"Struct::name"`) for statics and enum variants (`env.rs:16-23`,
`env_val.rs:371-373`), each call allocating three of them. Every struct
instance receives a copy of its method table at construction, each entry
holding an `Rc` back to the instance, so every instance with methods
leaks (`val.rs:713-758`). It panics on `1 / 0`, an out-of-range index, an
integer overflow, and 20,000 nested parentheses (verified by building and
running it), and its embedding API takes a diverging error handler
(`engine.rs:10, 42-56`). Its golden-stdout tests faithfully record an
inverted boolean cast (`val.rs:466-467`, `tests/output/type_casting.output`).

The positive lessons are small: non-local control flow as a returned
`StmtVal { None, Break, Continue, Return(Val) }`, never an unwind
(`val.rs:594-600`); every effect behind a host-supplied trait object,
which is what makes the tests possible (`io.rs:8-12`); value identity for
mutable aggregates via a counter. The larger lesson is that the checker
does not only improve messages: it removes the dispatch-time machinery,
the per-value type tag, the string environment and the method-table copy.
With an index and a resolver, locals are slots and items are ids, and a
method call is resolved to an item id at check time so the value carries
nothing but its data and its span. No source in the sweep has a
tree-walking evaluator over a checked HIR to copy from; gingerbread emits
wasm and capy emits Cranelift.

Two resilience facts from the sweep bear on evaluation as much as
parsing. eldiro's fuzzer found a real panic (an integer overflow in
`Literal::parse`) in about one second, seeded with one corpus file
(`fuzz/fuzz_targets/main.rs`); capy's `infer_expr` walks a flattened
post-order list instead of recursing so deep nesting cannot overflow the
stack (`crates/hir_ty/src/globals.rs:1308-1345`). Kitty's "never panic on
untrusted input" needs an explicit depth counter in the parser, a
fuel or step budget and call-depth cap in the evaluator, and a fuzz
target over the whole pipeline seeded with `examples/`.

## 6. Diagnostics and tests

Everyone hand-rolls diagnostics as one enum per stage, each variant
carrying a range, unified behind `range()`, `severity()`, `message()`
(gingerbread `crates/diagnostics/src/lib.rs:1-155`; Gleam's roughly 140
`Error` variants each with a `SrcSpan`, `type_/error.rs:147`). Kitty's
convention already matches; the difference is rendering through
`ariadne` rather than caret art. panoply's `Diagnostic` shape, a
principal `Citation { span, message }` plus secondary citations
(`crates/base/src/diagnostic.rs:22-40`), maps directly onto `ariadne`
labels.

Tests are `expect-test` everywhere. The shapes worth lifting: one
directory of fixture files per stage, each holding input, a divider and
the stage's pretty-printed output, updated in place (fictional-dollop
`crates/test_utils/src/lib.rs:5-55`, haze `src/testing.rs`); multi-file
fixtures split on a `#- name` marker; the type checker's snapshot
printing every expression id with its type beside the signature (capy
`crates/hir_ty/src/tests/structs.rs`); diagnostics asserted as
`(kind, range)` tuples separately from the tree snapshot; and
`IndexMap` for anything that reaches a snapshot, after haze's
`HashMap`-order flakiness (`ea97e33`).

## 7. Skip

- **A general trait solver** (chalk, next-solver, canonicalisation,
  fulfilment contexts) and the ordered-context algorithm of the
  Dunfield and Krishnaswami papers. Kitty is rank-1 with annotated items.
- **Type variables inside the item index**, and capy's restart-on-missing-
  dependency inference. Kitty's items are annotated.
- **Weak numeric types and mutation passes** (capy). One `Number`, one
  expected type threaded downward.
- **Rows and effects** (thunderseethe, panoply); nominal types unify by
  name and arguments.
- **A typed copy of the tree** (Gleam's `TypedExpr`). Side tables.
- **Salsa, `AstId` interning, per-file invalidation barriers, a
  hand-rolled query engine** (rust-analyzer, thunderseethe's 1187-line
  `queries.rs`). Keep each pass a pure function from input to `(output,
  errors)` so a query wrapper is possible later.
- **Autoderef, coercions, adjustments, lifetimes, variance, const
  generics, layout, MIR** (rust-analyzer), and everything from
  thunderseethe's `lowering-base-ir` onward (System F, evidence passing,
  monomorphisation, closure conversion, wasm). The checker need not record
  instantiations for a tree-walker.
- **Pointers, sizes, ABI, comptime, types as expressions** (capy,
  fictional-dollop, haze); the C and aarch64 backends.
- **A type checker that calls the backend** (capy's comptime, a crate
  cycle its author calls "unbelievably jank").
- **Runtime type tags, string environments, `any`, unions, casts**
  (oxide). All decided statically.
- **`SmolStr` names in the HIR** (eldiro) and `String`-keyed resolved
  paths (fictional-dollop, haze). Interned ids into the index, the string
  kept for diagnostics.
- **Hand-rolled arenas.** `la-arena`.

## 8. Found in Kitty's own code

The eldiro agent compared the series' parser against `kitty-parser` and
found Kitty's divergences to be improvements (threaded recovery sets,
a positioned `Missing` node, trivia not attached to a closing node,
`events.insert` instead of forward parents). Two things to check,
neither present in eldiro:

- `Parser::parse` calls `expect("Expected no empty events")`
  (`parser/src/parser.rs:36`), and `Marker::abandon` only pops when the
  marker is the last event, leaving a `None` mid-stream otherwise
  (`parser/src/marker.rs`). A latent panic against the no-panic rule,
  unreachable only while `abandon` is unused. Fix it when the first
  recovery rule abandons a node.
- `Parser::expect` clears `expected_kinds` before calling `at`
  (`parser/src/parser.rs:50`), discarding alternatives accumulated by
  earlier `at` calls; eldiro clears only on `bump` and on error. This
  narrows "expected X" messages. Confirm it is intended.

The `events.insert` in `precede` is O(n) per call, quadratic on
pathologically left-nested input; fine for CAD-scale sources, worth a
note.

## 9. Open questions for the grilling

Consolidated from the seven reports, grouped by the decision they force.

**Annotations and inference.**

1. Are parameter and return annotations mandatory on every `fn`, including
   in `impl` blocks? `examples/units.kitty` has `fn add(self, other) =>`
   with neither, which only works if impl signatures are inherited from
   the trait by checking. Rule, or an example that predates the rule?
2. Do closures ever need annotations? `@requires(fn (self) => ...)` in
   `chair.kitty` checks only if the attribute's parameter type is pushed
   in; otherwise it is the redex Christiansen says cannot be typed.
3. Is a `let` ever polymorphic? If not (Rust's answer), generalisation
   never enters the compiler and a generic `fn`'s scheme is read straight
   off its annotation, with an error if the body introduced variables.
4. Must recursive functions be annotated, or does Kitty want Gleam-style
   monomorphic recursion within a call-graph group?

**Traits.**

5. Are blanket impls and overlapping impls banned? If both are, impl
   selection is a hash lookup plus one unification and coherence is a
   duplicate-key check at index time. This one answer decides whether the
   bounded lookup is the design or a first approximation.
6. Eager or deferred obligations: must the left operand's type be known
   when `+` is typed, or may the goal stay pending until the body's end?
7. May a where clause mention an associated type of a bounded parameter
   (`where T: Iter, T.Item: Add`)? What depth, and what does exhaustion
   report?
8. May an inherent method shadow a trait method of the same name?
9. Does every `+` go through `Add`, or is there a builtin fast path for
   `Number`?
10. Is the `Into` conversion at call sites (decision `88117830`) a
    coercion inserted at a known checking goal, resolved as a trait
    obligation? Which impl, and is uniqueness required?

**HIR shape.**

11. Spans inline on `Node<T>` or in a side table keyed by `Idx`? The
    sources say side table; Kitty's draft says inline.
12. Does the HIR desugar anything (`let ... in` versus the implicit-`in`
    newline, `if` versus `match`, the two keyword-argument spellings)? If
    not, what earns the HIR its place beyond arena allocation?
13. One `Bodies` per file or per item? Both eldiro descendants chose per
    file; an editor re-checking on keystroke wants per item. Does Village
    Kit ever have more than one file?
14. What is a resolved name: an interned id into the index, a `DefId`, or
    the `&'src str` the draft uses, which ties the HIR to the source
    buffer's lifetime?
15. Does lowering emit diagnostics (unbound name, literal out of range),
    or only shapes?

**Evaluation.**

16. Do values carry a `Span` or an `ExprId` back into the HIR? Which span
    does `2 + 2` produce?
17. Is a struct value copied (each copy with its own span) or shared (one
    span for many uses)?
18. What are the resource limits: parser depth, evaluator fuel and call
    depth, as first-class diagnostics?
19. Division by zero and `fastnum` overflow: error variants with spans, or
    values?
20. Should the checker record which impl was selected at each call, since
    the evaluator will need method dispatch?

## Sources

Pinned commits, cloned under the session's scratchpad and read by the
sub-agents; the full reports with line-level citations are in the
transcripts directory named above.

- lunacookies, "Make A Language", https://lunacookies.github.io/lang/
  (Parts 17 to 20 read in full); `github.com/lunacookies/eldiro` @
  `f3d588f8a76e2e4317c1d77ae2758b0781bb5af3` (tag `part20`, the same as
  `master`).
- `github.com/lunacookies/fictional-dollop` @
  `e7fbf7da6b8f6f65edfc399db4f80ec130a8c744`;
  `github.com/lunacookies/haze` @ `dd18376e01a1baa08345446a57a08387f5735fe3`
  (and the pre-wipe tree at `39f9d99`, wipe commit `ed585a5`).
- `github.com/gingerbread-lang/gingerbread` @
  `9af94d609fdffa1f25a8f4792abdd002a3270261`;
  `github.com/capy-language/capy` @
  `b381c400d4db4d6058d7579addf04a0b9825e941`.
- thunderseethe, "Making a Language",
  https://thunderseethe.dev/series/making-a-language/ (eight posts in
  full, four in part); `github.com/thunderseethe/making-a-language` @
  `af30598437923b8df8668a730c6788f557661631`;
  `github.com/thunderseethe/panoply` @
  `32054d7be7b82ae36b20c300256d2cb0590f893c`.
- Jana Dunfield and Neelakantan R. Krishnaswami, "Sound and Complete
  Bidirectional Typechecking for Higher-Rank Polymorphism with
  Existentials and Indexed Types", PACMPL 3(POPL) Article 9, 2019,
  https://www.cl.cam.ac.uk/~nk480/gadt.pdf; and their "Complete and Easy
  Bidirectional Typechecking for Higher-Rank Polymorphism", ICFP 2013,
  arXiv:1306.6032v2 (added: it is the paper the implementation follows).
- David Raymond Christiansen, "Bidirectional Typing Rules: A Tutorial",
  17 October 2013, https://davidchristiansen.dk/tutorials/bidirectional.pdf.
- `github.com/rmehri01/bidirectional`: 404 on 2026-09-09. Substitute:
  `github.com/JDemler/BidirectionalTypechecking` @
  `1ca2a40121addb09df18c5b0191b23c47abc9456`, no license file, reference
  only.
- `github.com/tuqqu/oxide-lang` @ `5088a2d7278608b67114263c42f13e7206e2e338`
  (MIT), built and run to verify the panics.
- `github.com/gleam-lang/gleam` @ `19bf207ebb7d953ea1391f041da48c214ee1440a`;
  `github.com/rust-lang/rust-analyzer` @
  `f3120321073d8046795c6824976be8b0ae92c999`.
