---
name: resolving-merge-conflicts
description: "Use when you need to resolve an in-progress git merge/rebase conflict."
---

1. **See the current state** of the merge/rebase. Check git history, and the conflicting files.

2. **Find the primary sources** for each conflict. Understand deeply why each change was made, and what the original intent was. Read the commit messages, check the PRs, check original issues/tickets.

3. **Resolve each hunk.** Preserve both intents where possible. Where incompatible, pick the one matching the merge's stated goal and note the trade-off. Do **not** invent new behaviour. Always resolve; never `--abort`.

4. Run the project's **quality gate** (the repo's CLAUDE.md names it; otherwise discover it: typecheck, tests, format) and fix anything the merge broke. Nothing in `plans/` is ever renamed to settle a merge: a slug is decoration and an item keeps its id for life, with one exception. Two branches that minted the same id is a duplicate (`ID_DUPLICATE` once `kipu verify` runs here), and the later-minted item is re-minted, its edges repointed. Merge the run sheet `plans/README.md` and the `blocked_by` edges, which is where order lives.

5. **Finish the merge/rebase.** Stage everything and commit. If rebasing, continue the rebase process until all commits are rebased.
