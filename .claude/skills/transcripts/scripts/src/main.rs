//! Extract conversation text from Claude Code `.jsonl` transcripts.
//!
//! Writes one `.md` per input (named after the input's stem, into `--outdir`
//! or next to the source). Keeps verbatim, in order: user text, assistant
//! thinking, assistant text, and AskUserQuestion exchanges (each question,
//! its options, and the user's answer). Keeps other tool calls as one-line
//! markers, and harness plumbing (local command output, injected skill
//! bodies, compaction summaries, messages from peer and coordinator agents)
//! under a System heading. Drops tool results and blocks that are entirely
//! a system reminder, and reduces background-task notifications to one-line
//! markers (one per notification; a block can batch several): a
//! notification echoes the child agent's entire report, which the child's
//! own transcript already holds.
//!
//! With `--grill`, keeps only the user's messages and the AskUserQuestion
//! exchanges: the record of a grilling session, a few KB instead of a
//! megabyte of orchestration.
//!
//! The per-file byte report on stdout is the tool's output, not a stray
//! diagnostic.

use std::collections::HashSet;
use std::env;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use serde_json::Value;

const USAGE: &str = "usage: extract-transcript [--grill] [-o OUTDIR] FILE.jsonl...";

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("extract-transcript: {e}");
            ExitCode::from(2)
        }
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut grill = false;
    let mut outdir: Option<PathBuf> = None;
    let mut files: Vec<PathBuf> = Vec::new();
    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--grill" => grill = true,
            "-o" | "--outdir" => {
                outdir = Some(
                    args.next()
                        .ok_or_else(|| format!("{arg} needs a value"))?
                        .into(),
                );
            }
            "-h" | "--help" => {
                println!("{USAGE}");
                return Ok(());
            }
            _ if arg.starts_with('-') => return Err(format!("unknown flag {arg}\n{USAGE}").into()),
            _ => files.push(arg.into()),
        }
    }
    if files.is_empty() {
        return Err(USAGE.into());
    }
    if let Some(dir) = &outdir {
        fs::create_dir_all(dir)?;
    }

    let kept = if grill {
        "Only the user's messages and the AskUserQuestion exchanges: each \
         question, its options, and the user's answer, verbatim."
    } else {
        "All user and assistant text is verbatim; tool calls are one-line \
         markers; tool results and background-task notification payloads \
         are omitted."
    };
    for src in &files {
        let stem = src
            .file_stem()
            .ok_or_else(|| format!("{}: no file name", src.display()))?;
        // Append rather than with_extension: an agent-x.meta.json sidecar
        // must become agent-x.meta.md, not clobber agent-x.md.
        let dst = outdir
            .clone()
            .unwrap_or_else(|| src.parent().unwrap_or(".".as_ref()).to_path_buf())
            .join(format!("{}.md", stem.to_string_lossy()));
        let name = src.file_name().unwrap_or_default().to_string_lossy();
        let header = format!(
            "<!-- Extracted mechanically from the raw session transcript \
             {name} (kept outside the repo: too large for git). {kept} -->\n\n"
        );
        let body = extract(&fs::read_to_string(src)?, grill);
        fs::write(&dst, header + &body)?;
        println!(
            "{}: {} bytes from {}",
            dst.display(),
            fs::metadata(&dst)?.len(),
            fs::metadata(src)?.len()
        );
    }
    Ok(())
}

/// How to render one user-side text block.
enum UserBlock {
    /// System reminders and empty blocks: pure noise, never kept.
    Drop,
    /// Harness plumbing shown for trajectory, dropped in grill mode.
    System,
    /// A background-task notification, reduced to a one-line marker.
    Notification,
    /// The user's own words, kept in every mode.
    User,
}

fn classify_user_text(text: &str, entry: &Value) -> UserBlock {
    let t = text.trim();
    if t.is_empty() || (t.starts_with("<system-reminder>") && t.ends_with("</system-reminder>")) {
        return UserBlock::Drop;
    }
    // The entry's origin is authoritative when present; the text-prefix
    // tests below it only cover entries that carry no origin. In
    // particular a mid-turn user message arrives wrapped in a harness
    // envelope with isMeta set but origin.kind "human": it is still the
    // user's words.
    match entry.pointer("/origin/kind").and_then(Value::as_str) {
        Some("task-notification") => return UserBlock::Notification,
        Some("human") => return UserBlock::User,
        Some(_) => return UserBlock::System,
        None => {}
    }
    if t.starts_with("<task-notification")
        || t.starts_with("[SYSTEM NOTIFICATION - NOT USER INPUT]")
    {
        return UserBlock::Notification;
    }
    // Anything the harness injected rather than the user typed: compaction
    // summaries, skill bodies and image placeholders (isMeta), slash
    // command envelopes and local-command output.
    if entry["isCompactSummary"].as_bool() == Some(true)
        || entry["isMeta"].as_bool() == Some(true)
        || t.starts_with("<local-command")
        || t.starts_with("<command-name>")
    {
        return UserBlock::System;
    }
    UserBlock::User
}

/// One marker per `<task-notification>` element in the block (the harness
/// batches several into one entry), or None if no `<task-id>` parses (the
/// caller then keeps the block whole). Tags are looked up within each
/// element's slice only, so one malformed element cannot borrow a tag from
/// the next.
fn notification_markers(text: &str) -> Option<String> {
    let starts: Vec<usize> = text
        .match_indices("<task-notification>")
        .map(|(i, _)| i)
        .collect();
    let markers: Vec<String> = starts
        .iter()
        .enumerate()
        .filter_map(|(n, &start)| {
            let end = starts.get(n + 1).copied().unwrap_or(text.len());
            let slice = &text[start..end];
            let id = tag(slice, "task-id")?;
            let status = tag(slice, "status").unwrap_or("?");
            let summary = tag(slice, "summary").unwrap_or("").trim();
            let summary = if summary.is_empty() {
                String::new()
            } else {
                format!("{summary} ")
            };
            Some(format!(
                "> task-notification: {summary}({id}, {status}); output omitted"
            ))
        })
        .collect();
    if markers.is_empty() {
        None
    } else {
        Some(markers.join("\n"))
    }
}

/// The content of the first `<name>...</name>` in `text`.
fn tag<'a>(text: &'a str, name: &str) -> Option<&'a str> {
    let open = format!("<{name}>");
    let start = text.find(&open)? + open.len();
    let len = text[start..].find(&format!("</{name}>"))?;
    Some(&text[start..start + len])
}

fn extract(jsonl: &str, grill: bool) -> String {
    let mut out: Vec<String> = Vec::new();
    let mut asks: HashSet<String> = HashSet::new();
    for line in jsonl.lines() {
        let Ok(entry) = serde_json::from_str::<Value>(line.trim()) else {
            continue;
        };
        let content = &entry["message"]["content"];
        match entry["type"].as_str() {
            Some("user") => {
                let blocks: Vec<Value> = match content {
                    Value::String(s) => {
                        vec![serde_json::json!({"type": "text", "text": s})]
                    }
                    Value::Array(a) => a.clone(),
                    _ => continue,
                };
                for block in &blocks {
                    let is_answer = block["type"] == "tool_result"
                        && block["tool_use_id"]
                            .as_str()
                            .is_some_and(|id| asks.contains(id));
                    if is_answer {
                        let t = result_text(block);
                        if !t.trim().is_empty() {
                            out.push(format!("## Answer\n\n{t}"));
                        }
                    } else if let Some(text) =
                        block["text"].as_str().filter(|_| block["type"] == "text")
                    {
                        match classify_user_text(text, &entry) {
                            UserBlock::User => out.push(format!("## User\n\n{text}")),
                            UserBlock::System if !grill => {
                                out.push(format!("## System\n\n{text}"));
                            }
                            UserBlock::Notification if !grill => {
                                out.push(
                                    notification_markers(text)
                                        .unwrap_or_else(|| format!("## System\n\n{text}")),
                                );
                            }
                            _ => {}
                        }
                    }
                }
            }
            Some("assistant") => {
                let Value::Array(blocks) = content else {
                    continue;
                };
                for block in blocks {
                    match block["type"].as_str() {
                        Some("tool_use") if block["name"] == "AskUserQuestion" => {
                            if let Some(id) = block["id"].as_str() {
                                asks.insert(id.to_string());
                            }
                            out.push(render_questions(&block["input"]));
                        }
                        Some("tool_use") if !grill => {
                            let name = block["name"].as_str().unwrap_or("?");
                            out.push(format!("> tool: {name}({})", compact_args(&block["input"])));
                        }
                        Some("thinking") if !grill => {
                            if let Some(t) = block["thinking"].as_str()
                                && !t.trim().is_empty()
                            {
                                out.push(format!("## Assistant (thinking)\n\n{t}"));
                            }
                        }
                        Some("text") if !grill => {
                            if let Some(t) = block["text"].as_str()
                                && !t.trim().is_empty()
                            {
                                out.push(format!("## Assistant\n\n{t}"));
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    out.join("\n\n") + "\n"
}

fn render_questions(input: &Value) -> String {
    let mut lines: Vec<String> = Vec::new();
    let questions = input["questions"].as_array().cloned().unwrap_or_default();
    for q in &questions {
        match q["header"].as_str().filter(|h| !h.is_empty()) {
            Some(h) => lines.push(format!("## Question: {h}")),
            None => lines.push("## Question".to_string()),
        }
        lines.push(String::new());
        lines.push(q["question"].as_str().unwrap_or("").to_string());
        if let Some(opts) = q["options"].as_array().filter(|o| !o.is_empty()) {
            lines.push(String::new());
            for o in opts {
                lines.push(format!(
                    "- **{}**: {}",
                    o["label"].as_str().unwrap_or(""),
                    o["description"].as_str().unwrap_or("")
                ));
            }
        }
    }
    lines.join("\n")
}

fn result_text(block: &Value) -> String {
    match &block["content"] {
        Value::String(s) => s.clone(),
        Value::Array(parts) => parts
            .iter()
            .filter(|p| p["type"] == "text")
            .filter_map(|p| p["text"].as_str())
            .collect::<Vec<_>>()
            .join("\n\n"),
        _ => String::new(),
    }
}

fn compact_args(input: &Value) -> String {
    let Some(map) = input.as_object() else {
        return String::new();
    };
    let parts: Vec<String> = map
        .iter()
        .map(|(k, v)| {
            let s = match v {
                Value::String(s) => s.clone(),
                other => other.to_string(),
            };
            let s = s.split_whitespace().collect::<Vec<_>>().join(" ");
            format!("{k}={}", truncate(&s, 120))
        })
        .collect();
    truncate(&parts.join(", "), 300)
}

/// At most `max` bytes, cut on a char boundary, with `...` marking a cut.
fn truncate(s: &str, max: usize) -> String {
    debug_assert!(max > 0);
    if s.len() <= max {
        return s.to_string();
    }
    let mut end = max;
    while !s.is_char_boundary(end) {
        end -= 1;
    }
    format!("{}...", &s[..end])
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    fn classify(text: &str, entry: Value) -> UserBlock {
        classify_user_text(text, &entry)
    }

    #[test]
    fn mid_turn_user_message_is_user_despite_is_meta() {
        // A message the user sends while a turn runs arrives wrapped in a
        // harness envelope, isMeta true, but origin.kind "human".
        let entry = json!({"isMeta": true, "origin": {"kind": "human"}});
        let text = "The user sent a new message while you were working:\nhi";
        assert!(matches!(classify(text, entry), UserBlock::User));
    }

    #[test]
    fn origin_beats_text_prefix() {
        // A human pasting notification-shaped text stays a user block.
        let entry = json!({"origin": {"kind": "human"}});
        let text = "[SYSTEM NOTIFICATION - NOT USER INPUT] just kidding";
        assert!(matches!(classify(text, entry), UserBlock::User));
    }

    #[test]
    fn harness_injections_are_system() {
        for (text, entry) in [
            ("skill body", json!({"isMeta": true})),
            ("summary", json!({"isCompactSummary": true})),
            ("<local-command-stdout>x</local-command-stdout>", json!({})),
            ("<command-name>/compact</command-name>", json!({})),
            ("peer message", json!({"origin": {"kind": "peer"}})),
        ] {
            assert!(matches!(classify(text, entry), UserBlock::System));
        }
    }

    #[test]
    fn notifications_and_reminders() {
        let entry = json!({"origin": {"kind": "task-notification"}, "isMeta": true});
        assert!(matches!(
            classify("anything", entry),
            UserBlock::Notification
        ));
        assert!(matches!(
            classify("<task-notification>...", json!({})),
            UserBlock::Notification
        ));
        assert!(matches!(
            classify("<system-reminder>x</system-reminder>", json!({})),
            UserBlock::Drop
        ));
        assert!(matches!(classify("  ", json!({})), UserBlock::Drop));
    }

    #[test]
    fn one_marker_per_batched_notification() {
        let text = "[SYSTEM NOTIFICATION - NOT USER INPUT]\npreamble\n\
            <task-notification>\n<task-id>a1</task-id>\n<status>completed</status>\n\
            <summary>Agent \"one\" finished</summary>\n<result>big report</result>\n\
            </task-notification>\n\
            <task-notification>\n<task-id>a2</task-id>\n<status>failed</status>\n\
            </task-notification>";
        let markers = notification_markers(text).unwrap();
        assert_eq!(
            markers,
            "> task-notification: Agent \"one\" finished (a1, completed); output omitted\n\
             > task-notification: (a2, failed); output omitted"
        );
        assert_eq!(notification_markers("no tags here"), None);
    }

    #[test]
    fn marker_tags_do_not_leak_across_elements() {
        // The first element has no status; it must not borrow the next
        // element's.
        let text = "<task-notification>\n<task-id>a1</task-id>\n</task-notification>\n\
            <task-notification>\n<task-id>a2</task-id>\n<status>completed</status>\n\
            </task-notification>";
        let markers = notification_markers(text).unwrap();
        assert!(markers.starts_with("> task-notification: (a1, ?);"));
    }

    #[test]
    fn truncate_cuts_on_char_boundaries() {
        assert_eq!(truncate("abc", 120), "abc");
        assert_eq!(truncate("abcd", 3), "abc...");
        // Multibyte char straddling the limit is dropped whole.
        assert_eq!(truncate("aøb", 2), "a...");
    }

    #[test]
    fn extract_default_and_grill_modes() {
        let jsonl = [
            json!({"type": "user", "message": {"content": "do the thing"}}),
            json!({"type": "assistant", "message": {"content": [
                {"type": "thinking", "thinking": "hmm"},
                {"type": "text", "text": "on it"},
                {"type": "tool_use", "name": "Read", "id": "t1",
                 "input": {"file_path": "/x"}},
            ]}}),
            json!({"type": "user", "message": {"content": [
                {"type": "tool_result", "tool_use_id": "t1", "content": "dropped"},
            ]}}),
            json!({"type": "assistant", "message": {"content": [
                {"type": "tool_use", "name": "AskUserQuestion", "id": "t2",
                 "input": {"questions": [{"header": "Scope", "question": "Which?",
                     "options": [{"label": "A", "description": "first"}]}]}},
            ]}}),
            json!({"type": "user", "message": {"content": [
                {"type": "tool_result", "tool_use_id": "t2",
                 "content": "\"Which?\"=\"A\""},
            ]}}),
        ]
        .map(|v| v.to_string())
        .join("\n");
        let full = extract(&jsonl, false);
        assert_eq!(
            full,
            "## User\n\ndo the thing\n\n\
             ## Assistant (thinking)\n\nhmm\n\n\
             ## Assistant\n\non it\n\n\
             > tool: Read(file_path=/x)\n\n\
             ## Question: Scope\n\nWhich?\n\n- **A**: first\n\n\
             ## Answer\n\n\"Which?\"=\"A\"\n"
        );
        let grill = extract(&jsonl, true);
        assert_eq!(
            grill,
            "## User\n\ndo the thing\n\n\
             ## Question: Scope\n\nWhich?\n\n- **A**: first\n\n\
             ## Answer\n\n\"Which?\"=\"A\"\n"
        );
    }

    #[test]
    fn extract_skips_malformed_lines() {
        let jsonl = "NOT JSON\n{\"type\":\"user\",\"message\":{\"content\":\"ok\"}}";
        assert_eq!(extract(jsonl, false), "## User\n\nok\n");
    }
}
