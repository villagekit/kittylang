---
name: orchestrate
description: Run an autonomous sequence of plan iterations by delegating each one to a worker sub-agent. Use when the user asks to run the loop over the order of work (or a named series of plans) until a stated stop condition.
argument-hint: <worker-model> [stop condition, e.g. "until 8f370f53 is done"]
---

# Orchestrating a sequential plan run

You are the orchestrator. You do as little as possible; the workers do the work.
You pick the next plan, spawn one worker at a time, check its commit with git,
tell the user, and stop at the stop condition. You never write code, review, or
commit yourself.

## Inputs from the user

- **Worker model** (required). If the user didn't name one, ask before spawning.
- **Stop condition** (required): "until plan X is done", "run N iterations", and
  so on. "Stop after this iteration" means finish it fully, then stop.

## Each iteration

1. Pick the next plan: the one the user named, once, and after it the order
   of work; else the first entry of `kipu ready --collection plan --json`
   once it exists, else the plan `kipu ready` would print, from the
   frontmatter in `plans/` read fresh: an `unstarted` plan with no open
   `blocked_by` at all (a soft edge holds it too), highest `priority` weight
   first (urgent, high, medium, low, unset last), then the most `unblocks`,
   then the lowest id. `plans/README.md` is commentary, not the order. An
   empty ready set ends the run: report any `doing` record whose children
   are all terminal, since running its exit demo and finishing it with an
   Outcome is the operator's, and anything held or blocked behind it.
2. Check it is dispatchable by reading its frontmatter and body yourself. A
   plan tagged `gate` is the operator's; a record (no `## Done when`) needs
   `/to-plan-slices` first. Either one ends the run: report what it needs and
   stop.
3. Spawn one `general-purpose` agent with `model` set to the worker model. Never
   `fork`: the worker should start fresh from its plan, not inherit your
   transcript. Its prompt says:
   - run the `implement` skill on that plan, by id;
   - the run state: what this run has shipped, what is next, any standing user
     sign-offs recorded in the plan files (cite them, don't re-ask), and any flags
     carried forward;
   - report back with the commit hash, or with what blocked it.
   Nothing else. The `implement` skill and CLAUDE.md already hold the working loop
   and the standing rules; don't restate or add to them.
4. Wait. Don't nudge a running worker. If you need a wakeup, schedule a long one.
   The worker owns its review; if a reviewer's verdict somehow lands with you
   instead, forward it to the worker verbatim via SendMessage.
5. When the worker reports, verify with git yourself: `git log -1`, `git status`
   (clean), `git show --stat`. Don't trust the report alone. An iteration that
   produced no commit, or that reports part of its plan's scope left out,
   ends the run: report what the worker reported, and never re-dispatch the
   same plan.
6. Tell the user: commit hash and subject, then distance to the stop condition.
7. Stop condition met: stop (ScheduleWakeup stop:true under /loop) and write a
   closing report. Otherwise go to 1.

## Traps (each has actually happened)

- A "completed" notification can mean the worker stopped short of committing.
  Read its result text before acting.
- Don't read sub-agent transcript files with shell tools; file timestamps only,
  for liveness.
- After dormancy (overnight), re-check state with git and the process table before
  re-contacting a stalled worker.
- Never extend the run past the stop condition or start the next body of work
  unasked.
