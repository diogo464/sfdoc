#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }
}

pub struct SourceReader<'s> {
    source: &'s str,
}

impl<'s> SourceReader<'s> {
    pub fn new(source: &'s str) -> Self {
        Self { source }
    }

    pub fn position(&self) -> Position {
        todo!()
    }

    /// Read the next available line.
    #[must_use = "use skip_line if the line is not used"]
    pub fn read_line(&mut self) -> Option<&'s str> {
        todo!()
    }

    /// Skip the next line.
    pub fn skip_line(&mut self) {
        let _ = self.read_line();
    }
}
