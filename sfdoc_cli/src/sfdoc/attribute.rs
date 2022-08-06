const DASH: char = '-';
const HEADER: &str = "--- ";
const ATTRIBUTE: &str = "-- ";
const TRIPLE_DASH: &str = "---";
const DOUBLE_DASH: &str = "--";

#[derive(Debug, Clone, Copy)]
pub struct IdentParseError<'s>(&'s str);

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
    fn new_unchecked(value: &'s str) -> Self {
        debug_assert!(Self::parse(value).is_ok());
        Ident { value }
    }

    pub fn parse(value: &'s str) -> Result<Self, IdentParseError> {
        if value.is_empty() {
            return Err(IdentParseError(value));
        }
        if value.contains(' ') {
            return Err(IdentParseError(value));
        }
        Ok(Ident { value })
    }

    pub fn as_str(&self) -> &'s str {
        self.value
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Description<'s> {
    value: &'s str,
}

impl<'s> std::fmt::Display for Description<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<'s> From<&'s str> for Description<'s> {
    fn from(v: &'s str) -> Self {
        Self::new(v)
    }
}

impl<'s> AsRef<str> for Description<'s> {
    fn as_ref(&self) -> &'s str {
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

/// Class types that will decide how we parse the attributes
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

    pub fn ident(&self) -> Ident<'s> {
        self.ident
    }
}

impl<'s> From<Ident<'s>> for Class<'s> {
    fn from(c: Ident<'s>) -> Self {
        let kind = match c.as_str() {
            "hook" => ClassKind::Hook,
            "type" => ClassKind::Type,
            "table" => ClassKind::Table,
            "field" => ClassKind::Field,
            "library" => ClassKind::Library,
            "function" => ClassKind::Function,
            _ => ClassKind::Unknown,
        };
        Class::new(kind, c)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeUnionParseError<'s>(&'s str);

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
    fn parse(type_union: &'s str) -> Result<Self, TypeUnionParseError<'s>> {
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
        self.value.split('|').map(Ident::new_unchecked)
    }

    /// Is this type union optional.
    pub fn optional(&self) -> bool {
        self.optional
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field<'s> {
    name: Ident<'s>,
    description: Description<'s>,
}

impl<'s> Field<'s> {
    fn new(name: Ident<'s>, description: Description<'s>) -> Self {
        Self { name, description }
    }

    pub fn name(&self) -> Ident<'s> {
        self.name
    }

    pub fn description(&self) -> Description<'s> {
        self.description
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter<'s> {
    name: Ident<'s>,
    tunion: TypeUnion<'s>,
    description: Description<'s>,
}

impl<'s> Parameter<'s> {
    fn new(name: Ident<'s>, tunion: TypeUnion<'s>, description: Description<'s>) -> Self {
        Self {
            name,
            tunion,
            description,
        }
    }

    pub fn name(&self) -> Ident<'s> {
        self.name
    }

    pub fn type_union(&self) -> TypeUnion<'s> {
        self.tunion
    }

    pub fn description(&self) -> Description<'s> {
        self.description
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return<'s> {
    tunion: TypeUnion<'s>,
    description: Description<'s>,
}

impl<'s> Return<'s> {
    fn new(tunion: TypeUnion<'s>, description: Description<'s>) -> Self {
        Self {
            tunion,
            description,
        }
    }

    pub fn type_union(&self) -> TypeUnion<'s> {
        self.tunion
    }

    pub fn description(&self) -> Description<'s> {
        self.description
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Unknown<'s> {
    key: Ident<'s>,
    value: &'s str,
}

impl<'s> Unknown<'s> {
    fn new(key: Ident<'s>, value: &'s str) -> Self {
        Self { key, value }
    }

    pub fn key(&self) -> Ident<'s> {
        self.key
    }

    pub fn value(&self) -> &'s str {
        self.value
    }
}

#[derive(Debug, Clone)]
pub enum AttributeParseError<'s> {
    InvalidName(IdentParseError<'s>),
    InvalidClass(IdentParseError<'s>),
    InvalidLibtbl(IdentParseError<'s>),
    InvalidField(IdentParseError<'s>),
    InvalidParamTy(TypeUnionParseError<'s>),
    InvalidReturnTy(TypeUnionParseError<'s>),
    NotAttributeLine(&'s str),
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
            AttributeParseError::NotAttributeLine(err) => {
                write!(f, "Not an attribute line: {}", err)
            }
        }
    }
}

impl<'s> std::error::Error for AttributeParseError<'s> {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeKind<'s> {
    /// Header attribute. Example:
    /// --- This is the description
    Header {
        description: Description<'s>,
    },
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
    Field(Field<'s>),
    /// Parameter attribute. Example:
    /// -- @param type name this is the description
    Parameter(Parameter<'s>),
    /// Return attribute. Example:
    /// -- @return type this is the description
    Return(Return<'s>),
    /// A description fragment. This is a line that did not have a key. Example:
    /// --- This is a description
    /// -- This is also a description
    Description(Description<'s>),
    /// Unknown attribute. Example:
    /// -- @xyz ajwld awdjlaw
    Unknown(Unknown<'s>),
}

impl<'s> AttributeKind<'s> {
    /// Parse an attribute kind. `line` should look like:
    /// -- @key value
    /// or
    /// -- @key value value value ....
    /// or
    /// -- @key
    /// or
    /// --- this is a description
    /// or
    /// ---this is a description
    /// or
    /// -- this is a description
    /// or
    /// --this is a description
    fn new(line: &'s str) -> Result<Self, AttributeParseError<'s>> {
        let line = line.trim_start();
        if line.starts_with(HEADER) {
            return Ok(Self::Header {
                description: Description::from(line.trim_start_matches(DASH).trim_start()),
            });
        }

        if line.starts_with(TRIPLE_DASH)
            || (!line.starts_with(ATTRIBUTE) && line.starts_with(DOUBLE_DASH))
        {
            return Ok(Self::Description(Description::from(
                line.trim_start_matches(DASH).trim_start(),
            )));
        }

        if !line.starts_with(ATTRIBUTE) {
            return Err(AttributeParseError::NotAttributeLine(line));
        }

        let line = line.trim_start_matches(ATTRIBUTE).trim_start();
        if !line.starts_with('@') {
            return Ok(Self::Description(Description::from(line)));
        }

        let line = line.trim_start_matches('@');
        let (key, value) = line.split_once(' ').unwrap_or((line, ""));

        match key {
            "name" => {
                let (name, description) = value.split_once(' ').unwrap_or((value, ""));
                Ok(Self::Name {
                    name: Ident::parse(name).map_err(AttributeParseError::InvalidName)?,
                    description: Description::new(description),
                })
            }
            "class" => {
                let class = value.split_once(' ').unwrap_or((value, "")).0;
                let ident = Ident::parse(class).map_err(AttributeParseError::InvalidClass)?;
                Ok(Self::Class(Class::from(ident)))
            }
            "libtbl" => {
                let libtbl = value.split_once(' ').unwrap_or((value, "")).0;
                let ident = Ident::parse(libtbl).map_err(AttributeParseError::InvalidLibtbl)?;
                Ok(Self::Libtbl(ident))
            }
            // TODO: Check that the value is empty
            "server" => Ok(Self::Server),
            "client" => Ok(Self::Client),
            "shared" => Ok(Self::Shared),
            "field" => {
                let mut iter = value.split_ascii_whitespace();
                let name = iter.next().unwrap_or_default();
                let description = iter.as_str();

                let name = Ident::parse(name).map_err(AttributeParseError::InvalidField)?;
                let description = Description::from(description);
                let field = Field::new(name, description);

                Ok(Self::Field(field))
            }
            "param" => {
                let mut iter = value.split_ascii_whitespace();
                let tunion = iter.next().unwrap_or_default();
                let name = iter.next().unwrap_or_default();
                let description = iter.as_str();

                let name = Ident::new_unchecked(name);
                let tunion =
                    TypeUnion::parse(tunion).map_err(AttributeParseError::InvalidParamTy)?;
                let description = Description::new(description);
                let parameter = Parameter::new(name, tunion, description);

                Ok(Self::Parameter(parameter))
            }
            "return" => {
                let mut iter = value.split_ascii_whitespace();
                let tunion = iter.next().unwrap_or_default();
                let description = iter.as_str();

                let tunion =
                    TypeUnion::parse(tunion).map_err(AttributeParseError::InvalidReturnTy)?;
                let description = Description::from(description);
                let ret = Return::new(tunion, description);

                Ok(Self::Return(ret))
            }
            _ => Ok(Self::Unknown(Unknown::new(
                Ident::new_unchecked(key),
                value,
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute<'s> {
    kind: AttributeKind<'s>,
    value: &'s str,
}

impl<'s> Attribute<'s> {
    pub fn new(line: &'s str) -> Result<Self, AttributeParseError> {
        let kind = AttributeKind::new(line)?;
        Ok(Self { kind, value: line })
    }

    pub fn kind(&self) -> &AttributeKind<'s> {
        &self.kind
    }
}
