---
name: code-review
description: "Review the changes since a fixed point (commit, branch, tag, or merge-base) along two axes: Standards (does the code follow this repo's documented standards and hold its invariants?) and Spec (does the code match what the originating plan and specs asked for, and do its load-bearing claims verify?). Runs both reviews in parallel sub-agents and reports them side by side. Use when the user wants to review a branch, a PR, work-in-progress changes, or asks to \"review since X\"."
---

Two-axis review of the diff between `HEAD` and a fixed point:

- **Standards**: does the code conform to this repo's documented standards, and hold the invariants those standards protect?
- **Spec**: does the code faithfully implement what was asked, and do its load-bearing claims hold up against the references?

Both axes run as **parallel sub-agents** so they don't pollute each other's context, then this skill aggregates their findings. Each review starts cold: the prompt must carry everything the reviewer needs.

## Process

### 1. Pin the fixed point

Whatever the user said is the fixed point (a commit SHA, branch name, tag, `main`, `HEAD~5`, etc.). If they didn't specify one, and the work is uncommitted, the fixed point is `HEAD` and the diff is the working tree; otherwise ask.

Capture the diff command once: `git diff <fixed-point>...HEAD` (three-dot, so the comparison is against the merge-base), or `git diff HEAD` for the working tree. Also note the list of commits via `git log <fixed-point>..HEAD --oneline`.

Before going further, confirm the fixed point resolves (`git rev-parse <fixed-point>`) and the diff is non-empty. A bad ref or empty diff should fail here, not inside two parallel sub-agents.

### 2. Identify the spec sources

What the change was supposed to do, in this order:

1. The plan or issue the change implements: a path the user passed, a plan referenced in the commit messages or the branch name, or the plan the session has been working from.
2. The specs the change touches (the repo's CLAUDE.md says where they live): any statement in them the diff makes false is a finding, unless the diff updates it.
3. The recorded decisions in the area, and any survey or research note the change cites.
4. If no plan is found, ask the user. If there isn't one, the **Spec** sub-agent reviews against the specs and the recorded decisions alone and says so.

### 3. Identify the standards sources

Anything in the repo that documents how code should be written: the CLAUDE.md conventions, `CONTRIBUTING.md`, a style guide.

On top of whatever the repo documents, the Standards axis always carries the **smell baseline** below: a fixed set of Fowler code smells (_Refactoring_, ch.3) that applies even when a repo documents nothing. Two rules bind it:

- **The repo overrides.** A documented repo standard always wins; where it endorses something the baseline would flag, suppress the smell.
- **Always a judgement call.** Each smell is a labelled heuristic ("possible Feature Envy"), never a hard violation. Like any standard here, skip anything tooling already enforces.

Each smell reads *what it is* → *how to fix*; match it against the diff:

- **Mysterious Name**: a function, variable, or type whose name doesn't reveal what it does or holds. → rename it; if no honest name comes, the design's murky.
- **Duplicated Code**: the same logic shape appears in more than one hunk or file in the change. → extract the shared shape, call it from both.
- **Feature Envy**: a method that reaches into another object's data more than its own. → move the method onto the data it envies.
- **Data Clumps**: the same few fields or params keep travelling together (a type wanting to be born). → bundle them into one type, pass that.
- **Primitive Obsession**: a primitive or string standing in for a domain concept that deserves its own type. → give the concept its own small type.
- **Repeated Switches**: the same `match`/`if`-cascade on the same type recurs across the change. → replace with a trait, or one table both sites share.
- **Shotgun Surgery**: one logical change forces scattered edits across many files in the diff. → gather what changes together into one module.
- **Divergent Change**: one file or module is edited for several unrelated reasons. → split so each module changes for one reason.
- **Speculative Generality**: abstraction, parameters, or hooks added for needs the plan doesn't have. → delete it; inline back until a real need shows.
- **Message Chains**: long `a.b().c().d()` navigation the caller shouldn't depend on. → hide the walk behind one method on the first object.
- **Middle Man**: a type or function that mostly just delegates onward. → cut it, call the real target direct.
- **Refused Bequest**: an implementer that ignores or overrides most of what it inherits. → drop the inheritance, use composition.

### 4. Spawn both sub-agents in parallel

Spawn both on **Opus**, with no tool restriction: a reviewer may run the tests, read upstream crate sources, or build a fixture repo when a finding needs it.

**Standards sub-agent prompt** should include:

- The full diff command and commit list.
- The list of standards-source files you found in step 3, **plus the smell baseline from step 3** pasted in full (the sub-agent has no other access to it).
- The brief: "Report, per file/hunk where relevant, (a) every place the diff violates a documented standard: cite the standard (file + the rule); (b) every invariant the standards protect that the diff could break, checked by reading the code: the lexer and parser never panic and always produce a tree, whatever the input (errors are collected, not thrown); every token, node and value keeps the span it came from, so a diagnostic or an editor can point back at the source; the parser reads tokens only through the lexer and the syntax kinds only through `kitty-syntax`, so no crate re-derives the grammar; a plan item keeps its id for life (a slug rename is legal), edges written in the canonical direction only, a stored edge target a full id and never a prefix, priority a weight and never a sequence (order lives in edges); no copyleft code referenced or ported and no copyleft dependency added; tests written first where the methodology demands and not weakened to pass, no unbounded waits in tests, assumptions guarded with `debug_assert!` where cheap; (c) any baseline smell you spot: name it and quote the hunk. Distinguish hard violations from judgement calls: documented-standard breaches and invariant violations can be hard, but baseline smells are always judgement calls, and a documented repo standard overrides the baseline. Skip anything tooling enforces; run the project's quality gate if you doubt it was run. Under 400 words, critical first (correctness, invariant violations, things that would mislead future work), each with file:line, and say what you checked and found clean."

**Spec sub-agent prompt** should include:

- The diff command and commit list.
- The paths of the plan, the touched specs, and the recorded decisions in the area.
- The brief: "Report: (a) requirements the plan asked for that are missing or partial; (b) behaviour in the diff that wasn't asked for (scope creep); (c) requirements that look implemented but where the implementation looks wrong; (d) statements in the touched specs and decisions the diff makes false, and terms used in a sense the specs do not define; (e) load-bearing claims about an upstream API, a standard, or an algorithm, verified by reading the normative reference (the crate's source, the standard's text), never from memory, and any claim that does not hold; (f) a simpler shape or abstraction that would do the same job: imagine alternatives and compare them against the one chosen; and anything that will cost the maintainer in years to come; (g) where the change cites a survey or research note, whether the cited entry actually supports the claim. Quote the plan or spec line for each finding. Under 400 words, critical first (correctness, invariant violations, things that would mislead future work), each with file:line, and say what you checked and found clean."

### 5. Aggregate

Present the two reports under `## Standards` and `## Spec` headings, verbatim or lightly cleaned. Do **not** merge or rerank findings, because the two axes are deliberately separate (see _Why two axes_).

End with a one-line summary: total findings per axis, and the worst issue _within each axis_ (if any). Don't pick a single winner across axes: that's the reranking the separation exists to prevent.

Verify a finding against the actual files before acting on it; reviewers start cold and can misread. Findings that do not lead to a change now become `// Note(cc): ...` or `// TODO(cc): ...` comments for future readers.

## Why two axes

A change can pass one axis and fail the other:

- Code that follows every standard but implements the wrong thing → **Standards pass, Spec fail.**
- Code that does exactly what the plan asked but breaks the project's conventions or invariants → **Spec pass, Standards fail.**

Reporting them separately stops one axis from masking the other.
