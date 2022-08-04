use std::str::{Lines, Split};

use super::source::{SourceReader, Span};

const DASH: char = '-';
const DOUBLE: &str = "--";
const TRIPLE: &str = "---";

#[derive(Debug, Clone, Copy)]
struct IdentParseError<'s>(&'s str);

impl<'s> std::fmt::Display for IdentParseError<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Invalid identifier: '{}', identifiers be empty or contain spaces",
            self.0
        )
    }
}

impl<'s> std::error::Error for IdentParseError<'s> {}

/// Ident is a string representing an identifier.
/// This is a string without spaces.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'s> {
    value: &'s str,
}

impl<'s> AsRef<str> for Ident<'s> {
    fn as_ref(&self) -> &str {
        self.value
    }
}

impl<'s> std::fmt::Display for Ident<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<'s> Ident<'s> {
    fn new(value: &'s str) -> Self {
        debug_assert!(!value.chars().any(char::is_whitespace));
        Ident { value }
    }

    //fn parse(reader: &mut SourceReader<'s>) -> Result<Self, IdentParseError<'s>> {
    //    let (value, span) = reader.read_until_spanned(' ');
    //    if value.is_empty() {
    //        return Err(IdentParseError(value));
    //    }
    //    Ok(Ident::new(value, span))
    //}

    pub fn as_str(&self) -> &'s str {
        self.value
    }
}

/// Class types that will decide how we parse the attributes
/// They come in a line attribute. Example:
/// -- @class table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClassKind {
    Hook,
    Type,
    Table,
    Library,
    Function,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Class<'s> {
    kind: ClassKind,
    ident: Ident<'s>,
}

impl<'s> Class<'s> {
    fn new(kind: ClassKind, ident: Ident<'s>) -> Self {
        Self { kind, ident }
    }

    pub fn kind(&self) -> ClassKind {
        self.kind
    }

    pub fn ident(&self) -> Ident {
        self.ident
    }
}

impl<'s> From<Ident<'s>> for Class<'s> {
    fn from(c: Ident<'s>) -> Self {
        let kind = match c.as_str() {
            "hook" => ClassKind::Hook,
            "type" => ClassKind::Type,
            "table" => ClassKind::Table,
            "library" => ClassKind::Library,
            "function" => ClassKind::Function,
            _ => ClassKind::Unknown,
        };
        Class::new(kind, c)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Description<'s> {
    value: &'s str,
}

impl<'s> From<&'s str> for Description<'s> {
    fn from(v: &'s str) -> Self {
        Self::new(v)
    }
}

impl AsRef<str> for Description<'_> {
    fn as_ref(&self) -> &str {
        self.value
    }
}

impl<'s> Description<'s> {
    fn new(value: &'s str) -> Self {
        Description { value }
    }

    pub fn as_str(&self) -> &'s str {
        self.value
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TypeUnionParseError<'s>(&'s str);

impl<'s> std::fmt::Display for TypeUnionParseError<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid type union: '{}'", self.0)
    }
}

impl<'s> std::error::Error for TypeUnionParseError<'s> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeUnion<'s> {
    value: &'s str,
    optional: bool,
}

impl<'s> TypeUnion<'s> {
    fn new(type_union: &'s str) -> Result<Self, TypeUnionParseError<'s>> {
        // Make sure that the type dont contain whitespace
        let (type_union, optional) = if type_union.ends_with('?') {
            (type_union.trim_end_matches('?'), true)
        } else {
            (type_union, false)
        };
        if type_union.contains(' ') {
            return Err(TypeUnionParseError(type_union));
        }
        Ok(TypeUnion {
            value: type_union,
            optional,
        })
    }

    /// Iterator over all the types in this union.
    pub fn types(&self) -> impl Iterator<Item = Ident<'s>> {
        self.value.split('|').map(Ident::new)
    }

    /// Is this type union optional.
    pub fn optional(&self) -> bool {
        self.optional
    }
}

#[derive(Debug, Clone)]
enum AttributeParseError<'s> {
    InvalidName(IdentParseError<'s>),
    InvalidClass(IdentParseError<'s>),
    InvalidLibtbl(IdentParseError<'s>),
    InvalidField(IdentParseError<'s>),
    InvalidParamTy(TypeUnionParseError<'s>),
    InvalidReturnTy(TypeUnionParseError<'s>),
    InvalidAttributeLine(&'s str),
}

impl<'s> std::fmt::Display for AttributeParseError<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeParseError::InvalidName(err) => write!(f, "Invalid name: {}", err),
            AttributeParseError::InvalidClass(err) => write!(f, "Invalid class: {}", err),
            AttributeParseError::InvalidLibtbl(err) => write!(f, "Invalid libtbl: {}", err),
            AttributeParseError::InvalidField(err) => write!(f, "Invalid field: {}", err),
            AttributeParseError::InvalidParamTy(err) => write!(f, "Invalid param ty: {}", err),
            AttributeParseError::InvalidReturnTy(err) => write!(f, "Invalid return ty: {}", err),
            AttributeParseError::InvalidAttributeLine(err) => {
                write!(f, "Invalid attribute line: {}", err)
            }
        }
    }
}

impl<'s> std::error::Error for AttributeParseError<'s> {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeKind<'s> {
    /// Name attribute. Example:
    /// -- @name MyName
    Name {
        name: Ident<'s>,
        description: Description<'s>,
    },
    /// Class attribute. Example:
    /// -- @class table
    Class(Class<'s>),
    /// Libtbl attribute. Example:
    /// -- @libtbl mytype_methods
    Libtbl(Ident<'s>),
    /// Server realm. Example:
    /// -- @server
    Server,
    /// Client realm. Example:
    /// -- @client
    Client,
    /// Shared realm. Example:
    /// -- @shared
    Shared,
    Field {
        name: Ident<'s>,
        description: Description<'s>,
    },
    /// Parameter attribute. Example:
    /// -- @param type name this is the description
    Parameter {
        ty: TypeUnion<'s>,
        name: Ident<'s>,
        description: Description<'s>,
    },
    /// Return attribute. Example:
    /// -- @return type this is the description
    Return {
        ty: TypeUnion<'s>,
        description: Description<'s>,
    },
    /// A description fragment. This is a line that did not have a key. Example:
    /// --- This is a description
    /// -- This is also a description
    Description(Description<'s>),
    /// Unknown attribute. Example:
    /// -- @xyz ajwld awdjlaw
    Unknown { key: Ident<'s>, value: &'s str },
}

impl<'s> AttributeKind<'s> {
    /// Parse an attribute kind. `line` should look like:
    /// -- @key value
    /// or
    /// -- @key
    /// or
    /// --- this is a description
    /// or
    /// -- this is a description
    fn new(line: &'s str) -> Result<Self, AttributeParseError<'s>> {
        let line = line.trim_start();
        if line.starts_with(TRIPLE) {
            return Ok(Self::Description(Description::from(line)));
        }

        if !line.starts_with(DOUBLE) {
            return Err(AttributeParseError::InvalidAttributeLine(line));
        }

        let line = line.trim_start_matches(DOUBLE).trim_start();
        if !line.starts_with('@') {
            return Ok(Self::Description(Description::from(line)));
        }

        let line = line.trim_start_matches('@');
        let (key, value) = line.split_once(' ').unwrap_or((line, ""));

        match key {
            "name" => {
                let (name, description) = value.split_once(' ').unwrap_or((value, ""));
                Ok(Self::Name {
                    name: Ident::new(name),
                    description: Description::new(description),
                })
            }
            "class" => {
                let class = value.split_once(' ').unwrap_or((value, "")).0;
                Ok(Self::Class(Class::from(Ident::new(class))))
            }
            "libtbl" => {
                let libtbl = value.split_once(' ').unwrap_or((value, "")).0;
                Ok(Self::Libtbl(Ident::new(libtbl)))
            }
            // TODO: Check that the value is empty
            "server" => Ok(Self::Server),
            "client" => Ok(Self::Client),
            "shared" => Ok(Self::Shared),
            "field" => {
                let mut iter = value.split_ascii_whitespace();
                let name = iter.next().unwrap_or_default();
                let description = iter.as_str();

                Ok(Self::Field {
                    name: Ident::new(name),
                    description: Description::from(description),
                })
            }
            "param" => {
                let mut iter = value.split_ascii_whitespace();
                let tunion = iter.next().unwrap_or_default();
                let name = iter.next().unwrap_or_default();
                let description = iter.as_str();

                Ok(Self::Parameter {
                    ty: TypeUnion::new(tunion).map_err(AttributeParseError::InvalidParamTy)?,
                    name: Ident::new(name),
                    description: Description::new(description),
                })
            }
            "return" => {
                let mut iter = value.split_ascii_whitespace();
                let tunion = iter.next().unwrap_or_default();
                let description = iter.as_str();

                Ok(Self::Return {
                    ty: TypeUnion::new(tunion).map_err(AttributeParseError::InvalidReturnTy)?,
                    description: Description::new(description),
                })
            }
            _ => Ok(Self::Unknown {
                key: Ident::new(key),
                value,
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute<'s> {
    kind: AttributeKind<'s>,
    value: &'s str,
}

impl<'s> Attribute<'s> {
    fn new(line: &'s str) -> Result<Self, AttributeParseError> {
        let kind = AttributeKind::new(line)?;
        Ok(Self { kind, value: line })
    }

    pub fn kind(&self) -> &AttributeKind<'s> {
        &self.kind
    }

    pub fn line(&self) -> &'s str {
        todo!()
    }

    pub fn line_number(&self) -> usize {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Section<'p, 's> {
    /// The reaming text after this section
    remaining: &'s str,
    /// The first class attribute in this section
    class: Option<Class<'s>>,
    /// Are there multiple class attributes in this section?
    multiple_classes: bool,
    /// The attributes in this section
    attributes: &'p [Attribute<'s>],
}

impl<'p, 's> Section<'p, 's> {
    fn new(
        remaining: &'s str,
        class: Option<Class<'s>>,
        multiple_classes: bool,
        attributes: &'p [Attribute<'s>],
    ) -> Self {
        Self {
            remaining,
            class,
            multiple_classes,
            attributes,
        }
    }

    pub fn line_number(&self) -> usize {
        panic!("remove")
    }

    pub fn remaining(&self) -> &'s str {
        self.remaining
    }

    pub fn class(&self) -> Option<&Class<'s>> {
        self.class.as_ref()
    }

    pub fn multiple_classes(&self) -> bool {
        self.multiple_classes
    }

    pub fn attributes(&self) -> &'p [Attribute<'s>] {
        self.attributes
    }

    pub fn following_line(&self) -> Option<&'s str> {
        self.remaining.lines().next()
    }
}

#[derive(Debug)]
pub struct ParseError;

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error")
    }
}

impl std::error::Error for ParseError {}

pub struct Parser<'s> {
    lines: Split<'s, char>,
    current_line: usize,

    // parse state
    attributes: Vec<Attribute<'s>>,
    spotted_class: Option<Class<'s>>,
    multiple_classes: bool,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            lines: source.split('\n'),
            current_line: 0,

            attributes: Vec::with_capacity(16),
            spotted_class: None,
            multiple_classes: false,
        }
    }

    pub fn next_section(&mut self) -> Option<Result<Section<'_, 's>, ParseError>> {
        while let Some(line) = self.peek_line() {
            let trimmed = line.trim_start();
            if !trimmed.starts_with(TRIPLE) {
                self.skip_line();
                continue;
            }
            return Some(self.parse_section());
        }
        None
    }

    fn parse_section(&mut self) -> Result<Section<'_, 's>, ParseError> {
        // clear the state from parsing the previous item
        self.attributes.clear();
        self.spotted_class = None;
        self.multiple_classes = false;

        loop {
            let line = match self.peek_line() {
                Some(line) => line.trim_start(),
                None => break,
            };

            if !line.starts_with(DOUBLE) || !line.starts_with(TRIPLE) {
                break;
            }

            let line = self
                .consume_line()
                .unwrap()
                .trim_start()
                .trim_start_matches(DASH);

            match Attribute::new(line) {
                Ok(attr) => {
                    match attr.kind() {
                        AttributeKind::Class(c) => match self.spotted_class {
                            Some(_) => self.multiple_classes = true,
                            None => self.spotted_class = Some(*c),
                        },
                        _ => {}
                    }
                    self.attributes.push(attr);
                }
                Err(e) => {
                    // TODO: return vec of errors
                }
            }
        }

        Ok(Section::new(
            self.remaining(),
            self.spotted_class,
            self.multiple_classes,
            &self.attributes,
        ))
    }

    fn remaining(&self) -> &'s str {
        self.lines.as_str()
    }

    fn peek_line(&self) -> Option<&'s str> {
        self.lines.clone().next()
    }

    fn consume_line(&mut self) -> Option<&'s str> {
        self.lines.next()
    }

    fn skip_line(&mut self) {
        self.consume_line();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_type_union_single_type() {
        let tunion = TypeUnion::new("foo").unwrap();
        assert_eq!(tunion.optional(), false);
        assert_eq!(tunion.types().count(), 1);
        assert_eq!(tunion.types().next(), Some(Ident::new("foo")));
    }

    #[test]
    fn parse_type_union_double_type() {
        let tunion = TypeUnion::new("foo|bar").unwrap();
        assert_eq!(tunion.optional(), false);
        assert_eq!(tunion.types().count(), 2);
        assert_eq!(tunion.types().next(), Some(Ident::new("foo")));
        assert_eq!(tunion.types().skip(1).next(), Some(Ident::new("bar")));
    }

    #[test]
    fn parse_type_union_double_type_optional() {
        let tunion = TypeUnion::new("foo|bar?").unwrap();
        assert_eq!(tunion.optional(), true);
        assert_eq!(tunion.types().count(), 2);
        assert_eq!(tunion.types().next(), Some(Ident::new("foo")));
        assert_eq!(tunion.types().skip(1).next(), Some(Ident::new("bar")));
    }

    #[test]
    #[should_panic]
    fn parse_type_union_double_with_remaining() {
        TypeUnion::new("foo|bar baz").unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_type_union_double_with_remaining_optional() {
        TypeUnion::new("foo|bar? baz").unwrap();
    }

    #[test]
    fn parse_attribute_name_valid() {
        let kind = AttributeKind::new("-- @name foo").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Name {
                name: Ident::new("foo"),
                description: Description::default()
            }
        );
    }

    #[test]
    #[should_panic]
    fn parse_attribute_name_invalid() {
        AttributeKind::new("-- @name foo bar").unwrap();
    }

    #[test]
    fn parse_attribute_class_valid() {
        let kind = AttributeKind::new("-- @class foo").unwrap();
        assert_eq!(kind, AttributeKind::Class(Class::from(Ident::new("foo"))));
    }

    #[test]
    #[should_panic]
    fn parse_attribute_class() {
        AttributeKind::new("-- @class foo bar").unwrap();
    }

    #[test]
    fn parse_attribute_libtbl_valid() {
        let kind = AttributeKind::new("-- @libtbl foo").unwrap();
        assert_eq!(kind, AttributeKind::Libtbl(Ident::new("foo")));
    }

    #[test]
    #[should_panic]
    fn parse_attribute_libtbl_invalid() {
        AttributeKind::new("-- @libtbl foo bar").unwrap();
    }

    #[test]
    fn parse_attribute_server_valid() {
        let kind = AttributeKind::new("-- @server").unwrap();
        assert_eq!(kind, AttributeKind::Server);
    }

    #[test]
    fn parse_attribute_client_valid() {
        let kind = AttributeKind::new("-- @client").unwrap();
        assert_eq!(kind, AttributeKind::Client);
    }

    #[test]
    fn parse_attribute_shared_valid() {
        let kind = AttributeKind::new("-- @shared").unwrap();
        assert_eq!(kind, AttributeKind::Shared);
    }

    #[test]
    fn parse_attribute_parameter_valid() {
        let kind = AttributeKind::new("-- @param foo bar desc desc").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Parameter {
                ty: TypeUnion::new("foo").unwrap(),
                name: Ident::new("bar"),
                description: Description::new("desc desc"),
            }
        )
    }

    #[test]
    fn parse_attribute_return_valid() {
        let kind = AttributeKind::new("-- @return foo bar desc desc").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Return {
                ty: TypeUnion::new("foo").unwrap(),
                description: Description::new("bar desc desc"),
            }
        )
    }

    #[test]
    fn parse_attribute_unknown_valid_1() {
        let kind = AttributeKind::new("-- @libtl foo").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Unknown {
                key: Ident::new("libtl"),
                value: "foo"
            }
        );
    }

    #[test]
    fn parse_attribute_unknown_valid_2() {
        let kind = AttributeKind::new("-- @libtl foo bar").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Unknown {
                key: Ident::new("libtl"),
                value: "foo bar"
            }
        );
    }
}
