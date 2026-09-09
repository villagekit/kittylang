---
name: documentation
description: Write, review, and restructure documentation using the Diátaxis framework (tutorials, how-to guides, reference, explanation). Use when writing or reviewing reader-facing documentation - the narrative docs in `docs/`, rustdoc, READMEs - deciding where a piece of content belongs, or when a page seems to be doing several jobs at once. Not for specs, ADRs, plans, or commit messages.
---

# Documentation (Diátaxis)

Diátaxis (https://diataxis.fr/) divides documentation into four types, each
serving one user need. Every page should serve exactly one; a paragraph
drifting into another type is the signal to move it.

## The four types

|          | Tutorial                 | How-to guide       | Reference                  | Explanation                            |
|----------|--------------------------|--------------------|----------------------------|----------------------------------------|
| answers  | "Can you teach me to…?"  | "How do I…?"       | "What is…?"                | "Why…?"                                |
| serves   | learning (study)         | a goal (work)      | information (work)         | understanding (study)                  |
| form     | a lesson                 | steps to a result  | dry description            | discursive explanation                 |
| analogy  | teaching a child to cook | a recipe           | the back of a food packet  | an article on culinary social history  |

## The compass

To classify content, or decide what to write, ask two questions:

1. Does it inform **action** (practical steps, doing) or **cognition** (facts, thinking)?
2. Does it serve **acquisition** of skill (study) or **application** of skill (work)?

action + acquisition = tutorial. action + application = how-to guide.
cognition + application = reference. cognition + acquisition = explanation.

Apply the compass at any scale: a whole page, a section, a single sentence.

## Rules per type

**Tutorial** - a lesson in which the learner does something meaningful, and it
works every time.
- Open with where it goes: "In this tutorial we will build X." Never "you will learn".
- Every step produces a visible result; show the expected output.
- One path. No options, no alternatives, no abstraction.
- Minimal explanation: one clause ("we use HTTPS because it is safer"), then link out.
- Reliability is everything: a step that fails destroys the learner's confidence.

**How-to guide** - directions for an already-competent user's real-world goal.
- Title it "How to \<goal\>", named for a user need, never a tool operation.
- Assume competence: omit the unnecessary, no completeness for its own sake.
- Action only, including judgement calls: no teaching, no explaining, no
  reference dumps. Link instead.
- Branch where reality branches: "If X, do Y."

**Reference** - neutral description of the machinery, consulted during work.
- Describe, including how the machinery works. Do not instruct or opine; link
  out instead.
- Structure mirrors the product (module, class, method), which also exposes gaps.
- Austere, consistent, exact. State facts, list options, give warnings.
- Examples illustrate; they must not grow into lessons or rationale.

**Explanation** - discussion that deepens understanding, read away from the work.
- The title should survive the prefix "About": "About user authentication".
- Give context: why things are so, design decisions, constraints, history, alternatives.
- Opinion and perspective belong here, and only here.
- Do not absorb instructions or technical description; link to their proper home.

## The failure mode: mixing

Types blur into their neighbours, and the blur damages both sides: explanation
inside a tutorial breaks the learner's focus; teaching inside a how-to guide
slows the competent user; instructions inside explanation hide them from the
place users look for them. The fix is always the same: move the content to its
own home and link to it. A one-clause inline note plus a link beats an inline
digression.

## Workflow

Improve iteratively. No big-bang restructures, and never create four empty
sections to fill in later.

1. Take the page in front of you, existing or about to be written; don't go
   looking for others to fix.
2. Run the compass: what type is this? What user need does it serve?
3. Find content that belongs elsewhere; decide one small improvement.
4. Make it. Repeat.

Good structure emerges from these small moves. The four types are a complete
set of needs, not a mandatory four top-level sections: a docs site may split by
audience first and apply the four types within each. What matters is that no
page muddles two purposes.

Diátaxis decides a piece of content's type and where it lives; the repo's
CLAUDE.md (working style) governs house voice and page shape.

See [REFERENCE.md](REFERENCE.md) for language patterns and boundary tests.
