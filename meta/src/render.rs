use ariadne::{Config, IndexType, Label, Report, ReportKind, Source};
use text_size::TextRange;

use crate::{SourceId, Span};

/// Render one diagnostic as an `ariadne` report, returned as a string.
///
/// `source` names the source in the report's header; `text` is the whole
/// source; `message` is the report's headline; each label is a byte range
/// in `text` and the message to attach to it. With no labels the report
/// points at the start of the source. A label's range must lie inside
/// `text` on character boundaries: `ariadne` slices the source line by
/// byte column, and drops a label past the end without a trace. The
/// report carries no colour, so it can be captured or compared as plain
/// text.
pub fn render(
    source: SourceId,
    text: &str,
    message: &str,
    labels: &[(TextRange, String)],
) -> String {
    for (range, _) in labels {
        let (start, end) = (usize::from(range.start()), usize::from(range.end()));
        debug_assert!(
            text.is_char_boundary(start) && text.is_char_boundary(end),
            "label {start}..{end} is not on character boundaries inside the source"
        );
    }
    let span = |range: TextRange| Span::new(source, range);
    let primary = labels
        .first()
        .map(|(range, _)| span(*range))
        .unwrap_or_else(|| span(TextRange::empty(0.into())));

    let config = Config::default()
        .with_color(false)
        .with_index_type(IndexType::Byte);
    let report = Report::build(ReportKind::Error, primary)
        .with_config(config)
        .with_message(message)
        .with_labels(
            labels
                .iter()
                .map(|(range, message)| Label::new(span(*range)).with_message(message)),
        )
        .finish();

    let mut out = Vec::new();
    // The only writer is a `Vec<u8>`, which cannot fail to write.
    report
        .write((source, Source::from(text)), &mut out)
        .expect("writing a report into a Vec cannot fail");
    String::from_utf8(out).expect("ariadne writes UTF-8")
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use text_size::{TextRange, TextSize};

    use super::*;

    fn range(start: u32, end: u32) -> TextRange {
        TextRange::new(TextSize::new(start), TextSize::new(end))
    }

    #[test]
    fn a_report_names_the_source_and_points_two_labels_at_their_lines() {
        let text = "let x = 1\nlet y = x +\n";
        let actual = render(
            SourceId::from_path("examples/two.kitty"),
            text,
            "syntax error",
            &[
                (range(4, 5), "bound here".to_string()),
                (range(20, 21), "expected an operand after this".to_string()),
            ],
        );
        expect![[r#"
            Error: syntax error
               ╭─[ examples/two.kitty:1:5 ]
               │
             1 │ let x = 1
               │     ┬  
               │     ╰── bound here
             2 │ let y = x +
               │           ┬  
               │           ╰── expected an operand after this
            ───╯
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn an_empty_range_at_the_end_of_the_source_renders_as_a_label() {
        let text = "let y = x +";
        let actual = render(
            SourceId::from_path("examples/end.kitty"),
            text,
            "syntax error",
            &[(range(11, 11), "missing an expression".to_string())],
        );
        expect![[r#"
            Error: syntax error
               ╭─[ examples/end.kitty:1:12 ]
               │
             1 │ let y = x +
               │            │ 
               │            ╰─ missing an expression
            ───╯
        "#]]
        .assert_eq(&actual);
    }
}
