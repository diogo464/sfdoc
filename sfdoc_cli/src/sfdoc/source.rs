use std::str::{pattern::Pattern, Chars};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    offset: usize,
}

impl Position {
    pub fn new(offset: usize) -> Self {
        Self { offset }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: Position,
    length: usize,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self {
            start,
            length: end.offset - start.offset,
        }
    }

    pub fn new_with_len(start: Position, length: usize) -> Self {
        Self { start, length }
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        Position::new(self.start.offset() + self.length.saturating_sub(1))
    }

    pub fn len(&self) -> usize {
        self.length
    }
}

#[derive(Debug, Clone)]
pub struct SourceReader<'s> {
    source: Chars<'s>,
    position: Position,
}

impl<'s> SourceReader<'s> {
    pub fn new(source: &'s str) -> Self {
        Self::with_offset(source, Default::default())
    }

    pub fn with_offset(source: &'s str, offset: Position) -> Self {
        Self {
            source: source.chars(),
            position: offset,
        }
    }

    /// Returns the reader's current position.
    #[must_use]
    pub fn position(&self) -> Position {
        self.position
    }

    #[must_use]
    pub fn span(&self) -> Span {
        Span::new(
            self.position(),
            Position::new(self.position.offset() + self.as_str().len()),
        )
    }

    #[must_use]
    pub fn span_of_substr(&self, substr: &'s str) -> Span {
        let self_begin = self.as_str().as_ptr();
        let substr_begin = substr.as_ptr();
        let span_base = self.position.offset().min(substr_begin as usize);
        let span_start = span_base + unsafe { substr_begin.offset_from(self_begin).unsigned_abs() };
        Span::new(
            Position::new(span_start),
            Position::new(span_start + substr.len()),
        )
    }

    /// View the reader's remaining underlying source as a string.
    #[must_use]
    pub fn as_str(&self) -> &'s str {
        self.source.as_str()
    }

    #[must_use]
    pub fn into_str(self) -> &'s str {
        self.as_str()
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.as_str().is_empty()
    }

    /// Check if the reader's source starts with `pat`.
    #[must_use]
    pub fn starts_with<P: Pattern<'s>>(&self, pat: P) -> bool {
        self.as_str().starts_with(pat)
    }

    /// Read the next available line.
    #[must_use = "use skip_line if the line is not used"]
    pub fn read_line(&mut self) -> Option<&'s str> {
        todo!()
    }

    /// Obtain a [`SourceReader`] for the next line and advance the current reader past that line.
    #[must_use = "use skip_line if the line is not used"]
    pub fn line_reader(&mut self) -> Option<SourceReader<'s>> {
        todo!()
    }

    /// Skip the next line.
    pub fn skip_line(&mut self) {
        let _ = self.read_line();
    }

    pub fn skip_n(&mut self, n: usize) {
        todo!()
    }

    /// Skip characters while `predicate` returns `true` or until EOF.
    pub fn skip_while(&mut self, predicate: impl Fn(char) -> bool) {
        todo!()
    }

    /// Skip all whitespace until EOF or a non-whitespace character is found.
    pub fn skip_whitespace(&mut self) {
        self.skip_while(|c| c.is_whitespace());
    }

    pub fn read_while_spanned(&mut self, predicate: impl Fn(char) -> bool) -> (&'s str, Span) {
        todo!()
    }

    pub fn read_until_position_spanned(&mut self, end: Position) -> (&'s str, Span) {
        todo!()
    }

    pub fn read_until_spanned(&mut self, c: char) -> (&'s str, Span) {
        todo!()
    }

    pub fn read_to_end_spanned(&mut self) -> (&'s str, Span) {
        todo!()
    }
}
