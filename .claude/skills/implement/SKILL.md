---
name: implement
description: Implement one plan end to end - orient, scope, test-first, verify, review, record, commit. Use only when the user or an orchestrator hands you a plan (or names work to do); this is the repo's working loop, and it ends in a commit.
---

# Implement

Build the work described by a plan, through the repo's working loop. One pass of this skill is one focused commit (and one PR, when asked). The repo's CLAUDE.md holds the standing rules this loop runs under: conventions, the quality gate, where plans, specs, and decisions live.

## The loop

1. **Orient.** Read the plan the user or orchestrator named; if none was named, pick the next one from the repo's order of work. A plan tagged `gate` is the operator's: stop and report it, whoever handed it to you; the tag is the rule, not the body. Read the specs and the recorded decisions in its area. Treat plan edits you did not make as requirements.

2. **Scope one plan.** One plan is one focused PR. If it is bigger, split it first: when the user is present, ask them to run `/to-plan-slices`. Unattended, a record (a milestone, a multi-PR feature) is never split: stop and report, since the split needs the user's granularity call and its review rounds. A slice that turns out to be two or three PRs is split into that many, minted as `plans/README.md` ("Writing a plan") says from `.kipu/templates/plan.md`, each with `parent` and `derived_from` to the plan and `blocked_by` for what it consumes, the split plan moved to `doing` in the same commit; say so in your report and ship the first. A conflict between the plan and the code, or the plan and good sense, is flagged and resolved per the repo's CLAUDE.md, never silently overridden.

3. **Build test-first where pure.** Call the Skill tool for `tdd`, at the seams the plan names (agree them with the user if it names none). Red, green, one slice at a time; the git and filesystem boundary is exempt but kept thin, its decisions extracted into pure tested functions and its behavior exercised through fixture repos. Where a load-bearing claim about an upstream library is involved, read the crate's source and cite the crate, version, and path; never recall it. Run the relevant test target as you go; clippy regularly.

4. **Verify.** The project's full quality gate, green, bounded by a timeout. No exceptions, no "fix it next commit".

5. **Review.** Call the Skill tool for `code-review` against the fixed point this work started from. Verify each finding against the files, fix what is critical, and run the review again with fresh sub-agents until no critical feedback remains. Re-run the quality gate after fixes.

6. **Record.** In the same change as the code:
   - the crate README and rustdoc, for any public-API or behavior change;
   - the specs, if designed behavior changed, and the glossary (`docs/context.md`) if vocabulary changed (the `domain-modeling` skill for vocabulary);
   - a decision entry, if a decision was made that meets the `domain-modeling` skill's bar, recorded where the repo's CLAUDE.md says decisions live;
   - the plan this pass shipped moved to `done` with its `## Outcome` written (what shipped, what deviated from the plan and why, the decisions made in flight); when step 2 split a plan, that is the first slice, and the plan it was split from stays `doing` until every slice has shipped; the order of work updated; and when this was the last open child of a record, the report says so, since running the record's exit demo and finishing it is the operator's;
   - if the session ran a research sweep or a grilling, the session archived per the repo's convention.

7. **Commit.** Focused commits with imperative scoped subjects, citing the plan's prefix where the commit ships one (`kitty-parser: parse match arms (plan 8f370f53)`), on the current branch. Open the PR only when asked or when the branch is for one; the PR references the plan file and carries the docs, decisions, and plan updates with the code.

## Definition of done

- [ ] Tests exist for the pure logic, written first where the loop demands
- [ ] Quality gate green locally
- [ ] Fresh review(s) done, the last with no critical feedback
- [ ] Public items have rustdoc; a public-API or behavior change updates the affected rustdoc and crate README
- [ ] Specs updated if designed behavior changed; glossary updated if vocabulary changed
- [ ] Decision recorded if one was made; the plan this pass shipped (the slice, after a split) is `done` with its `## Outcome` written, and a plan this pass split is `doing`

Report completion only when all of it is done; if part of the scope is blocked, finish the rest and say exactly what was left out and why.
