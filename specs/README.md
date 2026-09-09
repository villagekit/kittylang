# Specs

Internal design specifications: normative, current-state, one subsystem per
file. A spec states what must hold, for the maintainer (human or agent) who
needs the intended design without re-deriving it from code.

The contract:

- **Normative.** Requirements use lowercase must / must not / should / may
  in RFC 2119's senses. Notes and examples are non-normative.
- **Current-state.** No "we will", no "used to"; rationale lives in
  [decisions/](../decisions/), pending work in [plans/](../plans/).
- **Kept true in the same PR** as any change to designed behaviour. A spec
  that disagrees with the code is a defect in one of them; decide which
  knowingly and record the call.
- **One fact, one home.** A spec links to other specs, the glossary
  ([docs/context.md](../docs/context.md)) and decisions; it restates none
  of them.

No spec is written yet. The first ones are cut when the language surface
is settled by a grilling over the decisions in `decisions/`, the README's
"Language" section and the sketches; the expected set, one per stage of
the pipeline:

| Spec | Subject |
| --- | --- |
| `lexing.md` | Tokens, identifiers, literals, comments and metadata, the indenter |
| `grammar.md` | The surface syntax and the tree it parses to, recovery |
| `types.md` | The type system: primitives, structs, enums, traits, generics |
| `analysis.md` | Name resolution, the item tree, HIR |
| `evaluation.md` | Values, spans on values, the embedding surface |

Read in that order once they exist.
