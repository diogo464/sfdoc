use crate::source::{Parse, Source, SourceReader, SourceStream, Span, Spanned};

const PREFIX_COMMENT: &str = "--";

/// Ident is a string representing an identifier.
/// This is a string(alphanum + '_' + '.') without spaces. Example:
/// `Vector`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    span: Span,
}
impl_spanned!(Ident);

impl Ident {
    fn new(span: Span) -> Self {
        debug_assert!(!span.is_empty());
        Self { span }
    }

    fn valid_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '.'
    }
}

impl Parse for Ident {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let (_, span) = stream.read_while(Self::valid_char);
        if span.is_empty() {
            return Err(ItemParseError::new(span, "expected valid identifier"));
        }
        Ok(Self::new(span))
    }
}

/// Text is a, possibly empty, sequence of characaters.
/// Parsing Text will consume all characaters until the end of the line, not including the newline
/// itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Text {
    span: Span,
}
impl_spanned!(Text);

impl Text {
    fn new(span: Span) -> Self {
        Self { span }
    }
}

impl Parse for Text {
    type ParseError = !;
    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let (_, span) = stream.read_until_newline();
        Ok(Self::new(span))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum DescriptionInner {
    Single(Text),
    Many(Vec<Text>),
}

enum DescriptionInnerSpanIter<'a> {
    Single(Option<Span>),
    Many(std::slice::Iter<'a, Text>),
}

impl<'a> Iterator for DescriptionInnerSpanIter<'a> {
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            DescriptionInnerSpanIter::Single(span) => span.take(),
            DescriptionInnerSpanIter::Many(iter) => iter.next().map(|text| text.span()),
        }
    }
}

/// A description is a set of [`Text`] that could span multiple lines.
/// Parsing a Description will first consume all characaters until the end of the line, not including the newline itself.
/// Then it will try to consume all lines after, if they are a comment.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Description(DescriptionInner);

impl Spanned for Description {
    fn span(&self) -> Span {
        match self.0 {
            DescriptionInner::Single(ref t) => t.span(),
            DescriptionInner::Many(ref v) => {
                debug_assert!(!v.is_empty());
                let first = v[0].span();
                let last = v[v.len() - 1].span();
                first.join(last)
            }
        }
    }
}

impl Description {
    pub fn spans(&self) -> impl Iterator<Item = Span> + '_ {
        match self.0 {
            DescriptionInner::Single(text) => DescriptionInnerSpanIter::Single(Some(text.span())),
            DescriptionInner::Many(ref vec) => DescriptionInnerSpanIter::Many(vec.iter()),
        }
    }
}

impl Parse for Description {
    type ParseError = !;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let first_text = stream.parse::<Text, _>().unwrap();
        let mut description = Description(DescriptionInner::Single(first_text));

        loop {
            let mut lookahead_stream = stream.clone();
            lookahead_stream.skip_line(); // This consumes the newline not consumed by Text::parse
                                          // above and bellow.
            lookahead_stream.skip_whitespace();
            let line = lookahead_stream.peek_str();
            if line.starts_with("---") || (line.starts_with("--") && !line.starts_with("-- @")) {
                lookahead_stream.skip_while(|c| c == '-');
                lookahead_stream.skip_whitespace();
                let extra_text = lookahead_stream.parse::<Text, _>().unwrap();
                stream.advance(lookahead_stream.position());
                match description.0 {
                    DescriptionInner::Single(text) => {
                        description.0 = DescriptionInner::Many(vec![text, extra_text]);
                    }
                    DescriptionInner::Many(ref mut vec) => vec.push(extra_text),
                }
            } else {
                break;
            }
        }

        Ok(description)
    }
}

/// Path is an identifier that might be prefixed by a table identifier.
/// Example:
/// `myfunction` or `mytable.myfunction`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Path {
    table: Option<Ident>,
    name: Ident,
}

impl Spanned for Path {
    fn span(&self) -> Span {
        match self.table {
            Some(ref table) => table.span().join(self.name.span()),
            None => self.name.span(),
        }
    }
}

impl Path {
    fn new(table: Option<Ident>, name: Ident) -> Self {
        Self { table, name }
    }

    pub fn table(&self) -> Option<Ident> {
        self.table
    }

    pub fn name(&self) -> Ident {
        self.name
    }
}

impl Parse for Path {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let (_, first) = stream.read_while(|c| Ident::valid_char(c) && c != '.');
        if first.is_empty() {
            return Err(ItemParseError::new(
                first,
                "path cannot have empty table part",
            ));
        }
        let first = Ident::new(first);
        match stream.peek_char() {
            Some('.') => {
                stream.skip_char();
                let table = Some(first);
                let name = stream.parse::<Ident, _>()?;
                Ok(Self::new(table, name))
            }
            _ => Ok(Self::new(None, first)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeyKind {
    Name,
    Class,
    Libtbl,
    Server,
    Client,
    Shared,
    Field,
    Param,
    Return,
    Unknown,
}

/// Key for an attribute. Example:
/// `@key`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Key {
    kind: KeyKind,
    ident: Ident,
    span: Span,
}
impl_spanned!(Key);

impl Key {
    fn new(kind: KeyKind, ident: Ident, span: Span) -> Self {
        Self { kind, ident, span }
    }

    pub fn kind(&self) -> KeyKind {
        self.kind
    }

    /// Returns the key identifier.
    /// This does not contain the `@` symbol.
    pub fn ident(&self) -> Ident {
        self.ident.clone()
    }

    /// Returns the span of the key.
    /// Contains the `@` symbol and the ident.
    pub fn span(&self) -> Span {
        self.span
    }
}

impl Parse for Key {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let start = stream.position();
        match stream.peek_char() {
            Some('@') => stream.skip_char(),
            _ => {
                return Err(ItemParseError::new(
                    Span::new(start, start),
                    "expected `@` while parsing key",
                ))
            }
        }
        let ident = stream.parse::<Ident, _>()?;
        let kind = match stream.source().lookup(&ident) {
            "name" => KeyKind::Name,
            "class" => KeyKind::Class,
            "libtbl" => KeyKind::Libtbl,
            "server" => KeyKind::Server,
            "client" => KeyKind::Client,
            "shared" => KeyKind::Shared,
            "field" => KeyKind::Field,
            "param" => KeyKind::Param,
            "return" => KeyKind::Return,
            _ => KeyKind::Unknown,
        };
        let span = Span::new(start, ident.span().end());
        Ok(Self::new(kind, ident, span))
    }
}

/// Class types will decide how we parse the attributes
/// They come in a line attribute. Example:
/// -- @class table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClassKind {
    Hook,
    Type,
    Table,
    Field,
    Library,
    Function,
    Unknown,
}

impl<T: AsRef<str>> From<T> for ClassKind {
    fn from(s: T) -> Self {
        match s.as_ref() {
            "hook" => ClassKind::Hook,
            "type" => ClassKind::Type,
            "table" => ClassKind::Table,
            "field" => ClassKind::Field,
            "library" => ClassKind::Library,
            "function" => ClassKind::Function,
            _ => ClassKind::Unknown,
        }
    }
}

/// Class defines the type of section.
/// Example:
/// `hook` or `table`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Class {
    kind: ClassKind,
    ident: Ident,
}
impl_spanned!(Class, self => ident);

impl Class {
    fn new(kind: ClassKind, ident: Ident) -> Self {
        Self { kind, ident }
    }

    pub fn kind(&self) -> ClassKind {
        self.kind
    }

    pub fn ident(&self) -> Ident {
        self.ident.clone()
    }
}

impl Parse for Class {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let ident = stream.parse::<Ident, _>()?;
        let class_kind = ClassKind::from(stream.source().lookup(&ident));
        Ok(Self::new(class_kind, ident))
    }
}

#[derive(Debug, Clone)]
// Expected type after span.
pub struct TypeUnionParseError(Span);

/// A type union is used in parameters and return attributes to indicate the type of value being
/// used. Example:
/// `number` or `string|number` or `string|number|table?`
///
/// The span contains the `?` if it is optional.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeUnion {
    span: Span,
    types: Vec<Ident>,
    optional: bool,
}
impl_spanned!(TypeUnion);

impl TypeUnion {
    fn new(span: Span, types: Vec<Ident>, optional: bool) -> Self {
        debug_assert!(!types.is_empty());
        debug_assert!(types[0].span().begin() == span.begin());
        debug_assert!(
            types[0].span().join(types[types.len() - 1].span()).length() <= span.length()
        );
        Self {
            span,
            optional,
            types,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn types(&self) -> &[Ident] {
        self.types.as_ref()
    }

    pub fn optional(&self) -> bool {
        self.optional
    }
}

impl Parse for TypeUnion {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let mut optional = false;
        let mut types = Vec::new();
        let begin = stream.position();
        loop {
            let ident = stream.parse::<Ident, _>()?;
            types.push(ident);
            match stream.peek_char() {
                Some('|') => stream.skip_char(),
                Some('?') => {
                    optional = true;
                    stream.skip_char();
                    break;
                }
                _ => break,
            }
        }
        let end = stream.position();
        Ok(TypeUnion::new(Span::new(begin, end), types, optional))
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    name: Ident,
    description: Description,
}
impl_spanned!(Field, self => name + description);

impl Field {
    fn new(name: Ident, description: Description) -> Self {
        Self { name, description }
    }

    pub fn name(&self) -> Ident {
        self.name
    }

    pub fn description(&self) -> &Description {
        &self.description
    }
}

impl Parse for Field {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let name = stream.parse()?;
        let description = stream.parse().unwrap();
        Ok(Self::new(name, description))
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    name: Ident,
    description: Description,
    type_union: TypeUnion,
}
impl_spanned!(Parameter, self => type_union + description);

impl Parameter {
    fn new(name: Ident, description: Description, type_union: TypeUnion) -> Self {
        Self {
            name,
            description,
            type_union,
        }
    }

    pub fn name(&self) -> Ident {
        self.name
    }

    pub fn description(&self) -> &Description {
        &self.description
    }

    pub fn type_union(&self) -> &TypeUnion {
        &self.type_union
    }
}

impl Parse for Parameter {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let type_union = stream.parse()?;
        stream.skip_whitespace();
        let name = stream.parse()?;
        stream.skip_whitespace();
        let description = stream.parse().unwrap();
        Ok(Self::new(name, description, type_union))
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    type_union: TypeUnion,
    description: Description,
}
impl_spanned!(Return, self => type_union + description);

impl Return {
    fn new(type_union: TypeUnion, description: Description) -> Self {
        Self {
            type_union,
            description,
        }
    }

    pub fn type_union(&self) -> &TypeUnion {
        &self.type_union
    }

    pub fn description(&self) -> &Description {
        &self.description
    }
}

impl Parse for Return {
    type ParseError = ItemParseError;
    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let type_union = stream.parse()?;
        let description = stream.parse().unwrap();
        Ok(Self::new(type_union, description))
    }
}

#[derive(Debug, Clone)]
/// A key-value pair.
/// Example:
/// `@key value`
/// or
/// `@key`
/// or
/// `@param number|string? value this is a description`
pub struct KV<V> {
    key: Key,
    value: V,
}

impl<V: Spanned> Spanned for KV<V> {
    fn span(&self) -> Span {
        self.key.span().join(self.value.span())
    }
}

// TODO: delete later, not required
impl<V: Parse<ParseError = ItemParseError> + std::fmt::Debug> Parse for KV<V> {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let key = stream.parse()?;
        stream.skip_whitespace();
        let value = stream.parse()?;
        Ok(KV::new(key, value))
    }
}

impl<V> KV<V> {
    fn new(key: Key, value: V) -> Self {
        Self { key, value }
    }

    pub fn key(&self) -> &Key {
        &self.key
    }

    pub fn value(&self) -> &V {
        &self.value
    }

    pub fn split(self) -> (Key, V) {
        (self.key, self.value)
    }
}

#[derive(Debug)]
pub struct ItemParseError {
    span: Span,
    message: String,
}

impl ItemParseError {
    fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Header(Text),
    Description(Text),
    Name(KV<Path>),
    Class(KV<Class>),
    Libtbl(KV<Ident>),
    Server(KV<()>),
    Client(KV<()>),
    Shared(KV<()>),
    Field(KV<Field>),
    Parameter(KV<Parameter>),
    Return(KV<Return>),
    Unknown(KV<()>),
    SourceCode,
}

#[derive(Debug, Clone)]
pub struct Item {
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn split(self) -> (ItemKind, Span) {
        (self.kind, self.span)
    }
}
impl_spanned!(Item);

impl Parse for Item {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        let begin = stream.position();
        stream.skip_whitespace();

        if !stream.peek_str().starts_with(PREFIX_COMMENT) {
            let (_, span) = stream.read_line();
            let span = span.with_begin(begin);
            return Ok(Item {
                kind: ItemKind::SourceCode,
                span,
            });
        }
        stream.skip_n(PREFIX_COMMENT.len());

        if stream.peek_str().starts_with('-') {
            stream.skip_n(1);
            match stream.peek_char() {
                Some(' ') => {
                    stream.skip_whitespace();
                    let (_, span) = stream.read_line();
                    let span = span.with_begin(begin);
                    return Ok(Item {
                        kind: ItemKind::Header(Text::new(span)),
                        span,
                    });
                }
                _ => {
                    let (_, span) = stream.read_line();
                    let span = span.with_begin(begin);
                    return Ok(Item {
                        kind: ItemKind::Description(Text::new(span)),
                        span,
                    });
                }
            }
        }

        stream.skip_whitespace();
        match stream.peek_char() {
            Some('@') => {
                let key: Key = stream.parse()?;
                stream.skip_whitespace();
                let item_kind = match key.kind() {
                    KeyKind::Name => ItemKind::Name(KV::new(key, stream.parse()?)),
                    KeyKind::Class => ItemKind::Class(KV::new(key, stream.parse()?)),
                    KeyKind::Libtbl => ItemKind::Libtbl(KV::new(key, stream.parse()?)),
                    KeyKind::Server => ItemKind::Server(KV::new(key, ())),
                    KeyKind::Client => ItemKind::Client(KV::new(key, ())),
                    KeyKind::Shared => ItemKind::Shared(KV::new(key, ())),
                    KeyKind::Field => ItemKind::Field(KV::new(key, stream.parse()?)),
                    KeyKind::Param => ItemKind::Parameter(KV::new(key, stream.parse()?)),
                    KeyKind::Return => ItemKind::Return(KV::new(key, stream.parse()?)),
                    KeyKind::Unknown => ItemKind::Unknown(KV::new(key, ())),
                };
                let (_, span) = stream.read_line();
                let span = span.with_begin(begin);
                Ok(Item {
                    kind: item_kind,
                    span,
                })
            }
            Some(_) | None => {
                let (_, span) = stream.read_line();
                Ok(Self {
                    kind: ItemKind::Description(Text::new(span)),
                    span,
                })
            }
        }
    }
}

pub type NameAttr = Attr<Path>;
pub type ClassAttr = Attr<Class>;
pub type LibtblAttr = Attr<Ident>;
pub type ServerAttr = Attr<()>;
pub type ClientAttr = Attr<()>;
pub type SharedAttr = Attr<()>;
pub type FieldAttr = Attr<Field>;
pub type ParamAttr = Attr<Parameter>;
pub type ReturnAttr = Attr<Return>;
pub type UnknownAttr = Attr<()>;

#[derive(Debug, Clone)]
pub struct Attr<V> {
    key: Key,
    value: V,
    description: Description,
}

impl<V> Attr<V> {
    fn new(key: Key, value: V, description: Description) -> Self {
        Self {
            key,
            value,
            description,
        }
    }

    pub fn key(&self) -> &Key {
        &self.key
    }

    pub fn value(&self) -> &V {
        &self.value
    }

    pub fn description(&self) -> &Description {
        &self.description
    }

    pub fn split(self) -> (Key, V, Description) {
        (self.key, self.value, self.description)
    }
}

#[derive(Debug, Clone)]
pub enum Attribute {
    /// `@name`
    Name(NameAttr),
    /// `@class`
    Class(ClassAttr),
    /// `@libtbl`
    Libtbl(LibtblAttr),
    /// `@server`
    Server(ServerAttr),
    /// `@client`
    Client(ClientAttr),
    /// `@shared`
    Shared(SharedAttr),
    /// `@field`
    Field(FieldAttr),
    /// `@param`
    Parameter(ParamAttr),
    /// `@return`
    Return(ReturnAttr),
    /// `@unknown`
    Unknown(UnknownAttr),
}

impl Parse for Attribute {
    type ParseError = ItemParseError;

    fn parse(stream: SourceStream) -> Result<Self, Self::ParseError> {
        match stream.peek_char() {
            Some('@') => {}
            Some(_) | None => {
                return Err(ItemParseError::new(
                    Span::from(stream.position()),
                    "Expected '@' while parsing attribute",
                ))
            }
        };

        let key = stream.parse::<Key, _>()?;
        stream.skip_whitespace();
        match key.kind {
            KeyKind::Name => {
                let value = stream.parse::<Path, _>()?;
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Name(Attr::new(key, value, desciption)))
            }
            KeyKind::Class => {
                let value = stream.parse::<Class, _>()?;
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Class(Attr::new(key, value, desciption)))
            }
            KeyKind::Libtbl => {
                let value = stream.parse::<Ident, _>()?;
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Libtbl(Attr::new(key, value, desciption)))
            }
            KeyKind::Server => {
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Server(Attr::new(key, (), desciption)))
            }
            KeyKind::Client => {
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Client(Attr::new(key, (), desciption)))
            }
            KeyKind::Shared => {
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Shared(Attr::new(key, (), desciption)))
            }
            KeyKind::Field => {
                let value = stream.parse::<Field, _>()?;
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Field(Attr::new(key, value, desciption)))
            }
            KeyKind::Param => {
                let value = stream.parse::<Parameter, _>()?;
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Parameter(Attr::new(key, value, desciption)))
            }
            KeyKind::Return => {
                let value = stream.parse::<Return, _>()?;
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Return(Attr::new(key, value, desciption)))
            }
            KeyKind::Unknown => {
                let desciption = stream.parse::<Description, _>().unwrap();
                Ok(Attribute::Unknown(Attr::new(key, (), desciption)))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Section {
    // The span containing the entire section, but not source_line.
    span: Span,
    // The description after the `---` that begin a header
    header: Description,
    attributes: Vec<Attribute>,
    /// Span for the line of source code following this documentation section
    source_line: Span,
}
impl_spanned!(Section);

impl Section {
    pub fn header(&self) -> &Description {
        &self.header
    }

    pub fn attributes(&self) -> &[Attribute] {
        &self.attributes
    }

    pub fn source_line(&self) -> Span {
        self.source_line
    }
}

#[derive(Debug, Clone)]
pub enum Item2 {
    Section(Section),
    SourceLine(Span),
    UnmatchedAttribute(Attribute),
}

pub struct ItemParser<'a> {
    reader: SourceReader<'a>,
}

impl<'a> ItemParser<'a> {
    pub fn new(source: Source<'a>) -> Self {
        Self {
            reader: SourceReader::new(source),
        }
    }
}

impl<'a> Iterator for ItemParser<'a> {
    type Item = Result<Item2, ItemParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.reader.is_eof() {
            return None;
        }

        let begin = self.reader.position();
        self.reader.skip_whitespace();
        let line = self.reader.peek_str();
        if line.starts_with("-- @") {
            self.reader.skip_n(3);
            let attribute = match self.reader.parse::<Attribute, _>() {
                Ok(attribute) => attribute,
                Err(err) => return Some(Err(err)),
            };
            self.reader.skip_line();
            return Some(Ok(Item2::UnmatchedAttribute(attribute)));
        }

        if !line.starts_with("---") {
            let (_, span) = self.reader.read_line();
            return Some(Ok(Item2::SourceLine(span.with_begin(begin))));
        }
        self.reader.skip_n(3);
        self.reader.skip_whitespace();

        let description = self.reader.parse::<Description, _>().unwrap();
        self.reader.skip_line();

        let mut attributes = Vec::with_capacity(8);
        loop {
            let mut lookahead = self.reader.clone();
            lookahead.skip_whitespace();
            let line = lookahead.peek_str();
            if line.starts_with("-- @") {
                lookahead.skip_n(3);
                let attribute = match lookahead.parse::<Attribute, _>() {
                    Ok(attribute) => attribute,
                    Err(err) => {
                        self.reader.skip_line();
                        return Some(Err(err));
                    }
                };
                attributes.push(attribute);
                lookahead.skip_line();
                self.reader.advance(lookahead.position());
            } else {
                break;
            }
        }

        let span = Span::new(begin, self.reader.position());
        let (_, source_span) = self.reader.read_line();

        Some(Ok(Item2::Section(Section {
            span,
            header: description,
            attributes,
            source_line: source_span,
        })))
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::{
        item::{Class, Item2, ItemParser, Path},
        source::{Source, SourceReader, Spanned},
    };

    use super::{ClassKind, Ident};

    fn ident_parse_valid_helper(s: &str) {
        let source = Source::new(s);
        let mut stream = SourceReader::new(source);
        let ident = stream.parse::<Ident, _>().unwrap();
        let expected = s.split_once(' ').unwrap_or((s, "")).0;
        assert_eq!(ident.span().length(), expected.len() as u32);
        assert_eq!(source.lookup(&ident), expected)
    }

    #[test]
    fn ident_parse_valid() {
        ident_parse_valid_helper("Vector");
        ident_parse_valid_helper("abc_aa");
        ident_parse_valid_helper("_");
        ident_parse_valid_helper("_ dawdawf");
        ident_parse_valid_helper("1241_ dawdawf");
        ident_parse_valid_helper("1241_ dawd faw  f");
    }

    fn ident_parse_invalid_helper(s: &str) {
        let source = Source::new(s);
        let mut stream = SourceReader::new(source);
        stream.parse::<Ident, _>().unwrap();
    }

    #[test]
    #[should_panic]
    fn ident_parse_invalid_empty() {
        ident_parse_invalid_helper("");
    }

    #[test]
    #[should_panic]
    fn ident_parse_invalid_space() {
        ident_parse_invalid_helper(" ");
    }

    #[test]
    #[should_panic]
    fn ident_parse_invalid_space_word() {
        ident_parse_invalid_helper(" foo bar");
    }

    fn path_parse_valid_helper(s: &str, tbl: Option<&str>, name: &str) {
        let source = Source::new(s);
        let mut stream = SourceReader::new(source);
        let path = stream.parse::<Path, _>().unwrap();
        assert_eq!(path.table().map(|t| source.lookup(&t)), tbl);
        assert_eq!(source.lookup(&path.name()), name);
    }

    #[test]
    fn path_parse_valid() {
        path_parse_valid_helper(
            "builtins_library.select
",
            Some("builtins_library"),
            "select",
        );

        path_parse_valid_helper(
            "select
", None, "select",
        );
    }

    fn class_parse_valid_helper(s: &str, kind: ClassKind) {
        let source = Source::new(s);
        let mut stream = SourceReader::new(source);
        let expected = s.split_once(' ').unwrap_or((s, "")).0.trim();
        let class = stream.parse::<Class, _>().unwrap();
        assert_eq!(class.kind(), kind);
        assert_eq!(source.lookup(&class), expected)
    }

    #[test]
    fn class_parse_valid() {
        class_parse_valid_helper("hook", ClassKind::Hook);
        class_parse_valid_helper("hook  xx", ClassKind::Hook);
        class_parse_valid_helper("type", ClassKind::Type);
        class_parse_valid_helper("type  xx", ClassKind::Type);
        class_parse_valid_helper("table", ClassKind::Table);
        class_parse_valid_helper("table\n  xx", ClassKind::Table);
        class_parse_valid_helper("field", ClassKind::Field);
        class_parse_valid_helper("field \n  xx", ClassKind::Field);
        class_parse_valid_helper("library", ClassKind::Library);
        class_parse_valid_helper("library11 \n  xx", ClassKind::Unknown);
        class_parse_valid_helper("xlibrary", ClassKind::Unknown);
    }

    fn class_parse_invalid_helper(s: &str) {
        let source = Source::new(s);
        let mut stream = SourceReader::new(source);
        stream.parse::<Class, _>().unwrap();
    }

    #[test]
    #[should_panic]
    fn class_parse_invalid_empty() {
        class_parse_invalid_helper("");
    }

    #[test]
    #[should_panic]
    fn class_parse_invalid_space() {
        class_parse_invalid_helper(" ");
    }

    #[test]
    #[should_panic]
    fn class_parse_invalid_space_word() {
        class_parse_invalid_helper(" foo bar");
    }

    #[test]
    fn item_parser_valid() {
        const EXAMPLE_CODE: &str = r#"end

--- Sets a rendertarget texture to the specified texture key
-- @param string key The key name to set. $basetexture is the key name for most purposes.
-- @param string name The name of the rendertarget
function material_methods:setTextureRenderTarget(key, name)
	checkkey(key)
	checkluatype(name, TYPE_STRING)

	local rt = instance.data.render.rendertargets[name]
	if not rt then SF.Throw("Invalid rendertarget: "..name, 2) end

	local m = unwrap(self)
	m:SetTexture(key, rt)
end

--- Sets a keyvalue to be undefined
-- @param string key The key name to set
function material_methods:setUndefined(key)
	checkkey(key)
	unwrap(self):SetUndefined(key)
end

--- Sets a vector keyvalue
-- @param string key The key name to set
-- @param Vector v The value to set it to
function material_methods:setVector(key, v)
	checkkey(key)
	unwrap(self):SetVector(key, vunwrap(v))
end
"#;

        let source = Source::new(EXAMPLE_CODE);

        let mut iter = ItemParser::new(source);
        for _ in 0..2 {
            assert_matches!(iter.next(), Some(Ok(Item2::SourceLine(_))));
        }

        let section = match iter.next().unwrap() {
            Ok(Item2::Section(section)) => section,
            v @ _ => panic!(
                "Expected section, found: {:?}\n{}",
                v,
                iter.reader.peek_str()
            ),
        };

        assert_eq!(
            source.lookup(section.header()),
            "Sets a rendertarget texture to the specified texture key"
        );

        assert_eq!(section.attributes().len(), 2);

        let attr0 = match &section.attributes()[0] {
            crate::item::Attribute::Parameter(param) => param.value(),
            v @ _ => panic!("Expected parameter, found: {:?}", v,),
        };
        assert_eq!(source.lookup(attr0.type_union()), "string");
        assert_eq!(source.lookup(&attr0.name()), "key");
        assert_eq!(
            source.lookup(attr0.description()),
            "The key name to set. $basetexture is the key name for most purposes."
        );

        let attr1 = match &section.attributes()[1] {
            crate::item::Attribute::Parameter(param) => param.value(),
            v @ _ => panic!("Expected parameter, found: {:?}", v,),
        };
        assert_eq!(source.lookup(attr1.type_union()), "string");
        assert_eq!(source.lookup(&attr1.name()), "name");
        assert_eq!(
            source.lookup(attr1.description()),
            "The name of the rendertarget"
        );

        assert_eq!(
            source.lookup(&section.source_line()),
            "function material_methods:setTextureRenderTarget(key, name)"
        );
    }
}
