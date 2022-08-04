use std::str::Split;

use super::{diagnostic::DiagnosticEmitter, Realm};

const TRIPLE: &str = "---";
const DOUBLE: &str = "--";

/// Class types that will decide how we parse the attributes
/// They come in a line attribute. Example:
/// -- @class table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Class<'s> {
    Hook,
    Type,
    Table,
    Library,
    Function,
    Unknown(Ident<'s>),
}

impl<'s> From<Ident<'s>> for Class<'s> {
    fn from(c: Ident<'s>) -> Self {
        match c.as_str() {
            "hook" => Class::Hook,
            "type" => Class::Type,
            "table" => Class::Table,
            "library" => Class::Library,
            "function" => Class::Function,
            _ => Class::Unknown(c),
        }
    }
}

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
pub struct Ident<'s>(&'s str);

impl<'s> AsRef<str> for Ident<'s> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'s> std::fmt::Display for Ident<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'s> Ident<'s> {
    fn new(s: &'s str) -> Self {
        debug_assert!(!s.chars().any(char::is_whitespace));
        Ident(s)
    }

    fn parse(s: &'s str) -> Result<Self, IdentParseError<'s>> {
        if s.is_empty() {
            return Err(IdentParseError(s));
        }
        if s.chars().any(char::is_whitespace) {
            return Err(IdentParseError(s));
        }
        Ok(Ident::new(s))
    }

    pub fn as_str(&self) -> &'s str {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
struct TypeUnionParseError<'s>(&'s str);

impl<'s> std::fmt::Display for TypeUnionParseError<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid type union: '{}'", self.0)
    }
}

impl<'s> std::error::Error for TypeUnionParseError<'s> {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeUnion<'s> {
    content: &'s str,
    optional: bool,
}

impl<'s> TypeUnion<'s> {
    fn new(content: &'s str, optional: bool) -> Self {
        // Make sure that the type dont contain whitespace
        #[cfg(debug_assertions)]
        {
            content.split('|').map(str::trim).for_each(|s| {
                debug_assert!(!s.chars().any(char::is_whitespace));
            });
        }
        TypeUnion { content, optional }
    }

    /// Parse a type union and return the remaing of the line.
    ///
    /// The line should look something like:
    /// `number|string? attachment Optional attachment name or ID.`
    fn parse(line: &'s str) -> Result<(Self, &'s str), TypeUnionParseError<'s>> {
        let line = line.trim();
        let mut split = line.split_ascii_whitespace();

        let union_str = split.next().ok_or(TypeUnionParseError(line))?;
        let remaining = split.as_str();
        let optional = union_str.ends_with('?');
        let union_str = union_str.trim_end_matches('?');

        let valid = union_str
            .split('|')
            .map(str::trim)
            .all(|s| !s.chars().any(char::is_whitespace));

        if !valid {
            return Err(TypeUnionParseError(line));
        }

        Ok((Self::new(union_str, optional), remaining))
    }

    /// Iterator over all the types in this union.
    pub fn types(&self) -> impl Iterator<Item = Ident<'s>> {
        self.content.split('|').map(str::trim).map(Ident::new)
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
    Name(Ident<'s>),
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
        description: &'s str,
    },
    /// Parameter attribute. Example:
    /// -- @param type name this is the description
    Parameter {
        ty: TypeUnion<'s>,
        name: Ident<'s>,
        description: &'s str,
    },
    /// Return attribute. Example:
    /// -- @return type this is the description
    Return {
        ty: TypeUnion<'s>,
        description: &'s str,
    },
    /// A description fragment. This is a line that did not have a key. Example:
    /// --- This is a description
    /// -- This is also a description
    Description(&'s str),
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
    fn parse(line: &'s str) -> Result<Self, AttributeParseError> {
        let trimmed = line.trim();
        if trimmed.starts_with(TRIPLE) {
            return Ok(Self::Description(trimmed.trim_start_matches(TRIPLE).trim()));
        }

        if !trimmed.starts_with(DOUBLE) {
            return Err(AttributeParseError::InvalidAttributeLine(line));
        }

        let kvpair_str = trimmed.trim_start_matches(DOUBLE).trim_start();
        if !kvpair_str.starts_with('@') {
            return Ok(Self::Description(kvpair_str));
        }

        let (key_str, val_str) = kvpair_str.split_once(' ').unwrap_or((kvpair_str, ""));
        let key = key_str.trim_start_matches('@');

        match key {
            "name" => {
                let name = Ident::parse(val_str).map_err(AttributeParseError::InvalidName)?;
                Ok(Self::Name(name))
            }
            "class" => {
                let class = Ident::parse(val_str).map_err(AttributeParseError::InvalidClass)?;
                Ok(Self::Class(Class::from(class)))
            }
            "libtbl" => {
                let libtbl = Ident::parse(val_str).map_err(AttributeParseError::InvalidLibtbl)?;
                Ok(Self::Libtbl(libtbl))
            }
            // TODO: Check that the value is empty
            "server" => Ok(Self::Server),
            "client" => Ok(Self::Client),
            "shared" => Ok(Self::Shared),
            "field" => {
                let (name, description) = val_str.split_once(' ').unwrap_or((val_str, ""));
                let name = Ident::parse(name).map_err(AttributeParseError::InvalidField)?;
                Ok(Self::Field { name, description })
            }
            "param" => {
                let (ty, remain) =
                    TypeUnion::parse(val_str).map_err(AttributeParseError::InvalidParamTy)?;

                let (name, description) = remain.split_once(' ').unwrap_or((remain, ""));
                Ok(Self::Parameter {
                    ty,
                    name: Ident::new(name),
                    description,
                })
            }
            "return" => {
                let (ty, remain) =
                    TypeUnion::parse(val_str).map_err(AttributeParseError::InvalidParamTy)?;

                let description = remain.trim_start();
                Ok(Self::Return { ty, description })
            }
            _ => Ok(Self::Unknown {
                key: Ident::new(key),
                value: val_str,
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute<'s> {
    kind: AttributeKind<'s>,
    line: &'s str,
    line_number: usize,
}

impl<'s> Attribute<'s> {
    fn new(kind: AttributeKind<'s>, line: &'s str, line_number: usize) -> Self {
        Self {
            kind,
            line,
            line_number,
        }
    }

    pub fn kind(&self) -> &AttributeKind<'s> {
        &self.kind
    }

    pub fn line(&self) -> &'s str {
        self.line
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }
}

pub struct Section<'p, 's> {
    /// The starting line number for this section
    line_number: usize,
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
        line_number: usize,
        remaining: &'s str,
        class: Option<Class<'s>>,
        multiple_classes: bool,
        attributes: &'p [Attribute<'s>],
    ) -> Self {
        Self {
            line_number,
            remaining,
            class,
            multiple_classes,
            attributes,
        }
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn remaining(&self) -> &'s str {
        self.remaining
    }

    pub fn class(&self) -> Option<Class<'s>> {
        self.class
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
        'OUTER: while let Some(line) = self.peek_line() {
            let trimmed = line.trim();
            if !trimmed.starts_with(TRIPLE) {
                self.consume_line();
                continue;
            }

            // clear the state from parsing the previous item
            self.attributes.clear();
            self.spotted_class = None;
            self.multiple_classes = false;

            let section_line_number = self.current_line;
            while let Some(line) = self.peek_line() {
                let trimmed = line.trim();
                if !(trimmed.starts_with(TRIPLE) || trimmed.starts_with(DOUBLE)) {
                    break;
                }

                let kind = match AttributeKind::parse(line) {
                    Ok(kind) => kind,
                    Err(err) => {
                        todo!();
                        self.consume_remainig_doubles();
                        continue 'OUTER;
                    }
                };
                match kind {
                    AttributeKind::Class(c) => match self.spotted_class {
                        None => self.spotted_class = Some(c),
                        Some(_) => self.multiple_classes = true,
                    },
                    _ => {}
                }
                let attr = Attribute::new(kind, line, self.current_line);
                self.attributes.push(attr);
                self.consume_line();
            }
            let remaining = self.remaining();
            let section = Section::new(
                section_line_number,
                remaining,
                self.spotted_class,
                self.multiple_classes,
                &self.attributes,
            );
            return Some(Ok(section));
        }
        None
    }

    fn consume_remainig_doubles(&mut self) {
        while let Some(line) = self.peek_line() {
            if !line.trim().starts_with(DOUBLE) {
                break;
            }
            self.consume_line();
        }
    }

    fn expect_line_and_trim(&mut self) -> &'s str {
        self.consume_line_and_trim()
            .expect("expected line while parsing")
    }

    fn consume_line_and_trim(&mut self) -> Option<&'s str> {
        self.consume_line().map(str::trim)
    }

    fn consume_line(&mut self) -> Option<&'s str> {
        let line = self.lines.next()?;
        self.current_line += 1;
        Some(line)
    }

    fn peek_line(&self) -> Option<&'s str> {
        self.lines.clone().next()
    }

    fn remaining(&self) -> &'s str {
        self.lines.as_str()
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    fn parse_ident_valid() {
        let ident = Ident::parse("foo").unwrap();
        assert_eq!(ident.as_str(), "foo");
    }

    #[test]
    fn parse_ident_two_words() {
        let ident = Ident::parse("foo bar");
        assert!(ident.is_err());
    }

    #[test]
    fn parse_ident_space_end() {
        let ident = Ident::parse("foo ");
        assert!(ident.is_err());
    }

    #[test]
    fn parse_ident_empty() {
        let ident = Ident::parse("");
        assert!(ident.is_err());
    }

    #[test]
    fn parse_ident_whitespace() {
        let ident = Ident::parse("  ");
        assert!(ident.is_err());
    }

    #[test]
    fn parse_type_union_single_type() {
        let (tunion, remaining) = TypeUnion::parse("foo").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(tunion.optional(), false);
        assert_eq!(tunion.types().count(), 1);
        assert_eq!(tunion.types().next(), Some(Ident::new("foo")));
    }

    #[test]
    fn parse_type_union_double_type() {
        let (tunion, remaining) = TypeUnion::parse("foo|bar").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(tunion.optional(), false);
        assert_eq!(tunion.types().count(), 2);
        assert_eq!(tunion.types().next(), Some(Ident::new("foo")));
        assert_eq!(tunion.types().skip(1).next(), Some(Ident::new("bar")));
    }

    #[test]
    fn parse_type_union_double_with_remaining() {
        let (tunion, remaining) = TypeUnion::parse("foo|bar baz").unwrap();
        assert_eq!(remaining, "baz");
        assert_eq!(tunion.optional(), false);
        assert_eq!(tunion.types().count(), 2);
        assert_eq!(tunion.types().next(), Some(Ident::new("foo")));
        assert_eq!(tunion.types().skip(1).next(), Some(Ident::new("bar")));
    }

    #[test]
    fn parse_type_union_double_with_remaining_optional() {
        let (tunion, remaining) = TypeUnion::parse("foo|bar? baz").unwrap();
        assert_eq!(remaining, "baz");
        assert_eq!(tunion.optional(), true);
        assert_eq!(tunion.types().count(), 2);
        assert_eq!(tunion.types().next(), Some(Ident::new("foo")));
        assert_eq!(tunion.types().skip(1).next(), Some(Ident::new("bar")));
    }

    #[test]
    fn parse_attribute_name_valid() {
        let kind = AttributeKind::parse("-- @name foo").unwrap();
        assert_eq!(kind, AttributeKind::Name(Ident::new("foo")));
    }

    #[test]
    #[should_panic]
    fn parse_attribute_name_invalid() {
        AttributeKind::parse("-- @name foo bar").unwrap();
    }

    #[test]
    fn parse_attribute_class_valid() {
        let kind = AttributeKind::parse("-- @class foo").unwrap();
        assert_eq!(kind, AttributeKind::Class(Class::from(Ident::new("foo"))));
    }

    #[test]
    #[should_panic]
    fn parse_attribute_class_invalid() {
        AttributeKind::parse("-- @class foo bar").unwrap();
    }

    #[test]
    fn parse_attribute_libtbl_valid() {
        let kind = AttributeKind::parse("-- @libtbl foo").unwrap();
        assert_eq!(kind, AttributeKind::Libtbl(Ident::new("foo")));
    }

    #[test]
    #[should_panic]
    fn parse_attribute_libtbl_invalid() {
        AttributeKind::parse("-- @libtbl foo bar").unwrap();
    }

    #[test]
    fn parse_attribute_server_valid() {
        let kind = AttributeKind::parse("-- @server").unwrap();
        assert_eq!(kind, AttributeKind::Server);
    }

    #[test]
    fn parse_attribute_client_valid() {
        let kind = AttributeKind::parse("-- @client").unwrap();
        assert_eq!(kind, AttributeKind::Client);
    }

    #[test]
    fn parse_attribute_shared_valid() {
        let kind = AttributeKind::parse("-- @shared").unwrap();
        assert_eq!(kind, AttributeKind::Shared);
    }

    #[test]
    fn parse_attribute_parameter_valid() {
        let kind = AttributeKind::parse("-- @param foo bar desc desc").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Parameter {
                ty: TypeUnion::parse("foo").unwrap().0,
                name: Ident::new("bar"),
                description: "desc desc",
            }
        )
    }

    #[test]
    fn parse_attribute_return_valid() {
        let kind = AttributeKind::parse("-- @return foo bar desc desc").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Return {
                ty: TypeUnion::parse("foo").unwrap().0,
                description: "bar desc desc",
            }
        )
    }

    #[test]
    fn parse_attribute_unknown_valid_1() {
        let kind = AttributeKind::parse("-- @libtl foo").unwrap();
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
        let kind = AttributeKind::parse("-- @libtl foo bar").unwrap();
        assert_eq!(
            kind,
            AttributeKind::Unknown {
                key: Ident::new("libtl"),
                value: "foo bar"
            }
        );
    }
}
