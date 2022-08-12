use std::path::{Path, PathBuf};

use super::source::Location;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticLevel {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    path: PathBuf,
    level: DiagnosticLevel,
    location: Location,
    message: String,
}

impl Diagnostic {
    pub fn new(
        path: PathBuf,
        level: DiagnosticLevel,
        location: Location,
        message: impl Into<String>,
    ) -> Self {
        Self {
            path,
            level,
            location,
            message: message.into(),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn level(&self) -> DiagnosticLevel {
        self.level
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn message(&self) -> &str {
        self.message.as_ref()
    }
}
