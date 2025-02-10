use internment::Intern;
use std::{
    fmt,
    path::{Path, PathBuf},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(Intern<Vec<String>>);

impl fmt::Display for SourceId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.len() == 0 {
            write!(f, "?")
        } else {
            write!(f, "{}", self.0.clone().join("/"))
        }
    }
}

impl fmt::Debug for SourceId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl SourceId {
    pub fn empty() -> Self {
        SourceId(Intern::new(Vec::new()))
    }

    pub fn repl() -> Self {
        SourceId(Intern::new(vec!["repl".to_string()]))
    }

    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        SourceId(Intern::new(
            path.as_ref()
                .iter()
                .map(|c| c.to_string_lossy().into_owned())
                .collect(),
        ))
    }

    pub fn to_path(&self) -> PathBuf {
        self.0.iter().map(|e| e.to_string()).collect()
    }
}
