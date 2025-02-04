use ariadne::{Config, Label, Report, ReportKind, Source};

use crate::{SourceId, Span};

#[derive(Debug, Clone)]
pub struct ErrorReport {
    pub span: Span,
    pub message: String,
    pub labels: Vec<(Span, String)>,
    pub notes: Vec<String>,
}

impl ErrorReport {
    pub fn display(&self, source: &str, source_id: SourceId) {
        let mut report =
            Report::build(ReportKind::Error, self.span).with_message(self.message.clone());

        for (i, (span, msg)) in self.labels.clone().into_iter().enumerate() {
            report = report.with_label(Label::new(span).with_message(msg).with_order(i as i32));
        }

        for note in &self.notes {
            report = report.with_note(note);
        }

        report
            .with_config(Config::default().with_compact(false))
            .finish()
            .eprint((source_id, Source::from(source)))
            .unwrap();
    }
}

#[derive(Debug, Clone)]
pub struct ErrorReports {
    pub reports: Vec<ErrorReport>,
}

impl<E> From<Vec<E>> for ErrorReports
where
    E: Into<ErrorReport>,
{
    fn from(reports: Vec<E>) -> Self {
        let reports: Vec<ErrorReport> = reports.into_iter().map(Into::into).collect();
        ErrorReports { reports }
    }
}

impl ErrorReports {
    pub fn display(&self, source: &str, source_id: SourceId) {
        for report in &self.reports {
            report.display(source, source_id)
        }
    }
}
