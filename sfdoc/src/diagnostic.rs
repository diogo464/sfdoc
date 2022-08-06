use std::path::PathBuf;

use crate::source::Location;

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
    body: String,
}

impl Diagnostic {
    pub fn new(
        path: PathBuf,
        level: DiagnosticLevel,
        location: Location,
        message: impl Into<String>,
        body: impl Into<String>,
    ) -> Self {
        Self {
            path,
            level,
            location,
            message: message.into(),
            body: body.into(),
        }
    }

    pub fn path(&self) -> &PathBuf {
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

    pub fn body(&self) -> &str {
        self.body.as_ref()
    }
}
