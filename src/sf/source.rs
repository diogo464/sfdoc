pub type SourceStream<'r, 's> = &'r mut SourceReader<'s>;

pub trait Parse: Sized {
    type ParseError: std::fmt::Debug;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError>;
}

pub trait Spanned {
    fn span(&self) -> Span;
}

#[macro_export]
macro_rules! impl_spanned {
    ($t:ty) => {
        impl_spanned!($t, span);
    };
    ($t:ty, $s:ident) => {
        impl Spanned for $t {
            fn span(&self) -> Span {
                self.$s
            }
        }
    };
    ($t:ty, self => $s:ident) => {
        impl Spanned for $t {
            fn span(&self) -> Span {
                self.$s.span()
            }
        }
    };
    ($t:ty, self => $s:ident + $o:ident) => {
        impl Spanned for $t {
            fn span(&self) -> Span {
                self.$s.span().join(self.$o.span())
            }
        }
    };
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct Position(pub u32);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct Span {
    /// Beggining offset of the span in the source code. This is a byte offset.
    begin: Position,
    /// Length of the span in the source code. This is a byte length.
    length: u32,
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl From<Position> for Span {
    fn from(position: Position) -> Self {
        Span::new(position, position)
    }
}

impl Span {
    pub fn new(begin: Position, end: Position) -> Self {
        Self {
            begin,
            length: end.0 - begin.0,
        }
    }

    pub fn begin(&self) -> Position {
        self.begin
    }

    pub fn with_begin(&self, begin: Position) -> Self {
        let end = self.end();
        debug_assert!(begin.0 <= end.0);
        Self::new(begin, end)
    }

    pub fn length(&self) -> u32 {
        self.length
    }

    pub fn end(&self) -> Position {
        Position(self.begin.0 + self.length)
    }

    pub fn join(&self, other: Span) -> Span {
        Span::new(
            Position(self.begin.0.min(other.begin.0)),
            Position(self.end().0.max(other.end().0)),
        )
    }

    pub fn is_empty(&self) -> bool {
        self.length() == 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct Location {
    line: u32,
    column: u32,
}

impl Location {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Source<'s> {
    source: &'s str,
}

impl<'s> Source<'s> {
    pub fn new(source: &'s str) -> Self {
        Self { source }
    }

    pub fn lookup(&self, s: &impl Spanned) -> &'s str {
        let span = s.span();
        &self.source[span.begin.0 as usize..span.end().0 as usize]
    }
}

#[derive(Debug, Clone)]
pub struct SourceReader<'s> {
    source: Source<'s>,
    remaining: &'s str,
}

impl<'s> From<&'s str> for SourceReader<'s> {
    fn from(s: &'s str) -> Self {
        Self::new(Source::new(s))
    }
}

impl<'s> SourceReader<'s> {
    pub fn new(source: Source<'s>) -> Self {
        let remaining = source.source;
        Self { source, remaining }
    }

    /// Current position in the source code.
    pub fn position(&self) -> Position {
        Position((self.source.source.len() - self.remaining.len()) as u32)
    }

    pub fn source(&self) -> Source<'s> {
        self.source
    }

    /// Peek the remaining source as a string.
    pub fn peek_str(&self) -> &'s str {
        self.remaining
    }

    pub fn peek_char(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    pub fn is_eof(&self) -> bool {
        self.remaining.is_empty()
    }

    /// Attempt to parse item of type `T`. Returns `Ok(item)` if successful,
    /// `Err(error)` otherwise. In case of failure to parse an item, the reader is not advanced.
    pub fn parse<T: Parse<ParseError = E>, E>(&mut self) -> Result<T, E> {
        let marker = self.remaining;
        let result = T::parse(self);
        if result.is_err() {
            self.remaining = marker;
        }
        result
    }

    /// Read until the end of the line and skip the newline.
    pub fn read_line(&mut self) -> (&'s str, Span) {
        let (string, span) = self.read_while(|c| c != '\n');
        self.skip_char();
        (string, span)
    }

    /// Read until the end of the line but dont skip the newline.
    pub fn read_until_newline(&mut self) -> (&'s str, Span) {
        let (string, span) = self.read_while(|c| c != '\n');
        (string, span)
    }

    /// Read characters while `pred` returns true.
    pub fn read_while(&mut self, pred: impl Fn(char) -> bool) -> (&'s str, Span) {
        let start = self.position();
        let mut count = 0;
        for c in self.remaining.chars() {
            if !pred(c) {
                break;
            }
            count += c.len_utf8();
        }
        let string = &self.remaining[..count];
        self.remaining = &self.remaining[count..];
        (string, Span::new(start, self.position()))
    }

    /// Skip whitespace but not newline.
    pub fn skip_whitespace(&mut self) {
        let _ = self.read_while(|c| c.is_whitespace() && c != '\n');
    }

    /// Skip the current line.
    pub fn skip_line(&mut self) {
        let _ = self.read_line();
    }

    pub fn skip_while(&mut self, pred: impl Fn(char) -> bool) {
        let _ = self.read_while(pred);
    }

    pub fn skip_n(&mut self, n: usize) {
        let mut iter = self.remaining.chars();
        for _ in 0..n {
            iter.next();
        }
        self.remaining = iter.as_str();
    }

    pub fn skip_char(&mut self) {
        self.skip_n(1);
    }

    pub fn advance(&mut self, position: Position) {
        debug_assert!(position.0 >= self.position().0);
        let offset = position.0 - self.position().0;
        debug_assert!(offset <= self.remaining.len() as u32);
        self.remaining = &self.remaining[offset as usize..];
    }
}

#[derive(Debug, Clone)]
pub struct SourceIndexer {
    table: Vec<usize>,
}

impl SourceIndexer {
    pub fn new(source: &str) -> Self {
        let mut table = Vec::new();
        let mut index = 0;
        table.push(0);
        for c in source.chars() {
            if c == '\n' {
                table.push(index);
            }
            index += c.len_utf8();
        }
        table.push(index);
        Self { table }
    }

    pub fn locate(&self, position: Position) -> Location {
        let index = match self.table.binary_search(&(position.0 as usize)) {
            Ok(index) => index - 1,
            Err(index) => index - 1,
        };
        let line = index + 1;
        let column = position.0 - self.table[index] as u32;
        Location::new(line as u32, column as u32)
    }
}

#[cfg(test)]
mod tests {
    use super::{Parse, Source, SourceReader, Span, Spanned};

    #[derive(Debug)]
    struct Ident {
        span: Span,
    }

    impl Spanned for Ident {
        fn span(&self) -> Span {
            self.span
        }
    }

    impl Parse for Ident {
        type ParseError = ();

        fn parse(stream: &mut SourceReader) -> Result<Self, Self::ParseError> {
            let (_, span) = stream.read_while(|c| c.is_alphanumeric() || c == '_' || c == '.');
            Ok(Self { span })
        }
    }

    #[test]
    fn simple_parse() {
        let source = "foo bar baz";
        let source = Source::new(source);
        let mut reader = SourceReader::new(source);

        let foo = reader.parse::<Ident, _>().unwrap();
        reader.skip_whitespace();
        let bar = reader.parse::<Ident, _>().unwrap();
        reader.skip_whitespace();
        let baz = reader.parse::<Ident, _>().unwrap();

        assert_eq!(source.lookup(&foo), "foo");
        assert_eq!(source.lookup(&bar), "bar");
        assert_eq!(source.lookup(&baz), "baz");
    }
}
