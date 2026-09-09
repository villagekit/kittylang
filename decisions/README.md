# Decisions

The `decision` collection, declared in [.kipu/collections/decision.toml](../.kipu/collections/decision.toml):
one item per decision, hash-id filename, `status` in frontmatter
(`proposed`, `accepted`, `superseded`), `date` a declared field. A decision
earns an item when it is hard to reverse, surprising without context, and
the result of a real trade-off; the specs restate the answer and the item
holds the rationale. Items are append-only: a decision is superseded by a
new item carrying a `supersedes` edge, never rewritten. Cite a decision by
its prefix or by a link to its file.

The first items carry the decisions the syntax sketches reached
([sketches/](../sketches/), 012 to 015) and the one the old NOTES.md
recorded, dated by the commit that wrote them. Their rationale is what the
sketch or note said and no more; a later grilling may deepen or supersede
them.
