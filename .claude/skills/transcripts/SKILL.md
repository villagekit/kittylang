---
name: transcripts
description: Archive agent working sessions - text-extracted transcripts of research sub-agents and grilling Q&A into the shared villagekit transcripts repo, and link them from the distilled records in research/. Use after a research session that ran sub-agents, after a grilling interview, or when the user asks to archive this session or save transcripts, session records, or raw agent output. Not for ordinary implementation sessions.
---

# Transcripts

After a session worth keeping (a sub-agent research sweep, a grilling
interview), archive two things:

1. **The transcripts**, text-extracted into the `transcripts` repo shared by
   every villagekit project, checked out as a sibling of this one
   (`../transcripts`), under this project's directory `kittylang/`. One
   directory per session, `kittylang/YYYY-MM-DD-<session-slug>/` (the slug
   names the session's focus; dates repeat): every sub-agent the session ran,
   descendants and failed runs included (a filtered record is not a
   record), and the user's side of the main session as `grill.md`. The
   main session is not archived wholesale: it is megabytes of
   orchestration whose products live in this repo already.
2. **The distilled record's pointer.** A session that leaves a distilled
   record in this repo (a `research/` synthesis, a spec it settled)
   should name the session directory it was built from; check that it
   does, and add the name if it does not. A grilling's synthesis is the
   design docs it settles, so a grilling produces only `grill.md`.

## Finding the raw transcripts

Claude Code writes raw `.jsonl` transcripts under
`~/.claude/projects/<munged-cwd>/`: the session's working directory with `/`
replaced by `-` (e.g. `-home-user-repos-project`), which is not always the
repo being archived.

- **The session**: `<session-id>.jsonl` at the top level. Identify the
  current session by grepping candidates for text you wrote this session;
  most-recently-modified is a hint, not proof, when sessions run
  concurrently.
- **Sub-agents**: `<session-id>/subagents/agent-<id>.jsonl`, one per agent,
  descendants included, all in the same flat directory. Each has a sidecar
  `agent-<id>.meta.json` whose `description`, `parentAgentId`, and
  `spawnDepth` say what the agent was for and who spawned it: build the
  name map from the sidecars, never by opening the extracts.

## Extracting

Never commit the `.jsonl` files: they are megabytes each, mostly tool
results (fetched pages, file reads). Confirm the sibling checkout exists
first (`../transcripts/.git`): the extractor creates missing output
directories, so extracting into an absent checkout leaves an untracked
orphan instead of failing. If it is missing, ask the user rather than
creating it. Then, from the root of this repo, extract the text (the
extractor is a standalone Rust tool; `cargo run` builds it on first use):

    cargo run --release --quiet \
        --manifest-path .claude/skills/transcripts/scripts/Cargo.toml -- \
        -o ../transcripts/kittylang/<session-dir> <agent.jsonl>...
    cargo run --release --quiet \
        --manifest-path .claude/skills/transcripts/scripts/Cargo.toml -- \
        --grill -o ../transcripts/kittylang/<session-dir> <session.jsonl>

The default mode keeps every user and assistant message verbatim and in
order, thinking included; keeps tool calls as one-line markers so the
trajectory and the sources consulted stay visible; drops tool results; and
reduces background-task notifications to one-line markers, because a
notification echoes the child agent's entire report, which the child's own
transcript already holds (left inline, the echoes double the archive).
`--grill` keeps only the user's messages and the AskUserQuestion exchanges
(each question, its options, and the user's answer). Extraction must stay
mechanical: never summarize a transcript by hand or with a sub-agent, or
the archive stops being verbatim.

Rename each output by role, per the archive repo's README:

- `research-<topic>.md`, `review-<what>.md`, `survey-<what>.md`, and the
  like, one per sub-agent, slugged from the sidecar's `description`
- `-sub-<id>` suffix (first 7 characters of the agent id) for descendants,
  prefixed with their top-level agent's slug
- `-failed` suffix for a run that died (its extract typically ends in an
  `API Error` line); keep it
- `grill.md` for the `--grill` extraction of the main session

## Committing

Commit the session directory in the `transcripts` repo as one focused
commit (`git -C ../transcripts ...`). Commit the distilled record in
this repo with the work the session produced, or as its own focused
commit; it names the session directory it was built from. Extracts are
verbatim, so skim for pasted secrets before committing.
