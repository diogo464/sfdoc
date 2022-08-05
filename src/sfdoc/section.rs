use std::{collections::HashMap, str::Split};

use super::{
    attribute::{
        Attribute, AttributeKind, AttributeParseError, Class, ClassKind, Ident, TypeUnion,
    },
    Field, Hook, Library, Method, Parameter, Realm, Return, Table, Type,
};

#[derive(Debug)]
pub enum SectionParseError<'s> {
    MissingName,
    DuplicateName,
    DuplicateClass,
    DuplicateRealm,
    ToManyTbls,
    MissingTbl,
    MissingNextLine,
    InvalidFunctionLine(&'s str),
    InvalidFunctionName(Ident<'s>),
    InvalidTableName(Ident<'s>),
    UnknownClass(Ident<'s>),
    UnknownAttribute(Ident<'s>),
    InvalidAttribute(AttributeParseError<'s>),
}

impl<'s> std::fmt::Display for SectionParseError<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SectionParseError::MissingName => write!(f, "missing name"),
            SectionParseError::DuplicateName => write!(f, "duplicate name"),
            SectionParseError::DuplicateClass => write!(f, "duplicate class"),
            SectionParseError::DuplicateRealm => write!(f, "duplicate realm"),
            SectionParseError::ToManyTbls => write!(f, "to many tbls"),
            SectionParseError::MissingTbl => write!(f, "missing tbl"),
            SectionParseError::MissingNextLine => write!(f, "missing next line"),
            SectionParseError::InvalidFunctionLine(line) => {
                write!(f, "invalid function line: {}", line)
            }
            SectionParseError::InvalidFunctionName(ident) => {
                write!(f, "invalid function name: {}", ident)
            }
            SectionParseError::InvalidTableName(ident) => {
                write!(f, "invalid table name: {}", ident)
            }
            SectionParseError::UnknownClass(ident) => write!(f, "unknown class: {}", ident),
            SectionParseError::UnknownAttribute(ident) => write!(f, "unknown attribute: {}", ident),
            SectionParseError::InvalidAttribute(err) => write!(f, "invalid attribute: {}", err),
        }
    }
}

impl<'s> std::error::Error for SectionParseError<'s> {}

#[derive(Debug, Clone)]
pub struct HookSection<'s> {
    pub ident: Ident<'s>,
    pub hook: Hook,
}

#[derive(Debug, Clone)]
pub struct TypeSection<'s> {
    pub ident: Ident<'s>,
    pub ty: Type,
    pub tbls: Vec<Ident<'s>>,
}

#[derive(Debug, Clone)]
pub struct TableSection<'s> {
    pub ident: Ident<'s>,
    pub table: Table,
    pub tbl: Ident<'s>,
}

#[derive(Debug, Clone)]
pub struct FieldSection<'s> {
    pub ident: Ident<'s>,
    pub field: Field,
    pub tbl: Ident<'s>,
}

#[derive(Debug, Clone)]
pub struct LibrarySection<'s> {
    pub ident: Ident<'s>,
    pub library: Library,
    pub tbls: Vec<Ident<'s>>,
}

#[derive(Debug, Clone)]
pub struct MethodSection<'s> {
    pub ident: Ident<'s>,
    pub method: Method,
    pub tbl: Ident<'s>,
}

#[derive(Debug, Clone)]
pub enum Section<'s> {
    Hook(HookSection<'s>),
    Type(TypeSection<'s>),
    Table(TableSection<'s>),
    Field(FieldSection<'s>),
    Library(LibrarySection<'s>),
    Method(MethodSection<'s>),
}

/// A documentation section in progress of being built.
#[derive(Debug, Clone)]
struct PartialSection<'s> {
    name: Option<Ident<'s>>,
    class: Option<Class<'s>>,
    top_level_description: String,
    parameters: Vec<Parameter>,
    returns: Vec<Return>,
    fields: Vec<Field>,
    tbls: Vec<Ident<'s>>,
    realm: Option<Realm>,
    default_realm: Realm,
    next_line: Option<&'s str>,
}

impl<'s> PartialSection<'s> {
    fn new(default_realm: Realm, next_line: Option<&'s str>) -> Self {
        Self {
            name: Default::default(),
            class: Default::default(),
            top_level_description: Default::default(),
            parameters: Default::default(),
            returns: Default::default(),
            fields: Default::default(),
            tbls: Default::default(),
            realm: Default::default(),
            default_realm,
            next_line,
        }
    }

    fn clear(&mut self) {
        self.name = Default::default();
        self.class = Default::default();
        self.top_level_description.clear();
        self.parameters.clear();
        self.returns.clear();
        self.fields.clear();
        self.tbls.clear();
        self.realm = Default::default();
        self.next_line = Default::default();
    }

    fn take_ident(&mut self) -> Option<Ident<'s>> {
        self.name.take()
    }

    fn take_ident_and_name(&mut self) -> Result<(Ident<'s>, String), SectionParseError<'s>> {
        match self.name.take() {
            Some(name) => Ok((name, name.to_string())),
            None => Err(SectionParseError::MissingName),
        }
    }

    fn take_description(&mut self) -> String {
        std::mem::take(&mut self.top_level_description)
    }

    fn get_realm(&self) -> Realm {
        self.realm.unwrap_or(self.default_realm)
    }

    fn take_parameters(&mut self) -> Vec<Parameter> {
        std::mem::take(&mut self.parameters)
    }

    fn take_returns(&mut self) -> Vec<Return> {
        std::mem::take(&mut self.returns)
    }

    fn take_fields_map(&mut self) -> HashMap<String, Field> {
        self.fields.drain(..).map(|f| (f.name.clone(), f)).collect()
    }

    fn take_tbls(&mut self) -> Vec<Ident<'s>> {
        std::mem::take(&mut self.tbls)
    }

    fn get_tbl(&mut self) -> Result<Ident<'s>, SectionParseError<'s>> {
        match self.tbls.as_slice() {
            &[] => Err(SectionParseError::MissingTbl),
            &[tbl] => Ok(tbl),
            _ => Err(SectionParseError::ToManyTbls),
        }
    }
}

#[derive(Debug)]
pub struct SectionParser<'s> {
    lines: Split<'s, char>,
    default_realm: Realm,
    attributes: Vec<Attribute<'s>>,
    section: PartialSection<'s>,
}

impl<'s> SectionParser<'s> {
    pub fn new(source: &'s str, default_realm: Realm) -> Self {
        Self {
            lines: source.split('\n'),
            default_realm,
            attributes: Vec::with_capacity(16),
            section: PartialSection::new(default_realm, None),
        }
    }

    pub fn next_section(&mut self) -> Option<Result<Section<'s>, SectionParseError<'s>>> {
        let mut next_line = None;
        while let Some(line) = self.lines.next() {
            match Attribute::new(line) {
                Ok(attr) => {
                    if self.attributes.is_empty()
                        && !std::matches!(attr.kind(), AttributeKind::Header { .. })
                    {
                        continue;
                    }
                    self.attributes.push(attr)
                }
                Err(err) => match err {
                    AttributeParseError::NotAttributeLine(_) => {
                        if self.attributes.is_empty() {
                            continue;
                        } else {
                            // Get the first non empty line
                            next_line = std::iter::once(line)
                                .chain(self.lines.clone())
                                .map(str::trim)
                                .filter(|s| !s.is_empty())
                                .next();
                            break;
                        }
                    }
                    _ => return Some(Err(SectionParseError::InvalidAttribute(err))),
                },
            }
        }

        if self.attributes.is_empty() {
            None
        } else {
            self.section.clear();
            self.section.next_line = next_line;
            let parse_result = self.parse_attributes();
            self.attributes.clear();
            Some(parse_result)
        }
    }

    fn parse_attributes(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let mut iter = self.attributes.iter().peekable();
        while let Some(attr) = iter.next() {
            match attr.kind() {
                AttributeKind::Header { description } => {
                    self.section
                        .top_level_description
                        .push_str(description.as_str());
                }
                AttributeKind::Name {
                    name,
                    description: _,
                } => match self.section.name {
                    None => self.section.name = Some(*name),
                    Some(_) => return Err(SectionParseError::DuplicateName),
                },
                AttributeKind::Class(c) => match self.section.class {
                    None => self.section.class = Some(*c),
                    Some(_) => return Err(SectionParseError::DuplicateClass),
                },
                AttributeKind::Libtbl(tbl) => self.section.tbls.push(*tbl),
                AttributeKind::Server => match self.section.realm {
                    None => self.section.realm = Some(Realm::Server),
                    Some(_) => return Err(SectionParseError::DuplicateRealm),
                },
                AttributeKind::Client => match self.section.realm {
                    None => self.section.realm = Some(Realm::Client),
                    Some(_) => return Err(SectionParseError::DuplicateRealm),
                },
                AttributeKind::Shared => match self.section.realm {
                    None => self.section.realm = Some(Realm::Shared),
                    Some(_) => return Err(SectionParseError::DuplicateRealm),
                },
                AttributeKind::Field { name, description } => self.section.fields.push(Field {
                    name: name.to_string(),
                    description: description.to_string(),
                }),
                AttributeKind::Parameter {
                    ty,
                    name,
                    description,
                } => {
                    let name = name.to_string();
                    let parameter_type = type_union_to_type(ty);
                    let optional = ty.optional();
                    let mut description = description.to_string();

                    while let Some(attr) = iter.peek() {
                        match attr.kind() {
                            AttributeKind::Description(desc)
                            | AttributeKind::Header { description: desc } => {
                                description.push_str(" ");
                                description.push_str(desc.as_str());
                                iter.next();
                            }
                            _ => break,
                        }
                    }

                    self.section.parameters.push(Parameter {
                        name,
                        ty: parameter_type,
                        optional,
                        description,
                    })
                }
                AttributeKind::Return { ty, description } => {
                    let parameter_type = type_union_to_type(ty);
                    let optional = ty.optional();
                    let mut description = description.to_string();

                    while let Some(attr) = iter.peek() {
                        match attr.kind() {
                            AttributeKind::Description(desc)
                            | AttributeKind::Header { description: desc } => {
                                description.push_str(" ");
                                description.push_str(desc.as_str());
                                iter.next();
                            }
                            _ => break,
                        }
                    }

                    self.section.returns.push(Return {
                        ty: parameter_type,
                        description,
                        optional,
                    })
                }
                AttributeKind::Description(desc) => {
                    if !self.section.top_level_description.is_empty() {
                        self.section.top_level_description.push('\n');
                    }
                    self.section.top_level_description.push_str(desc.as_str());
                }
                AttributeKind::Unknown { key, .. } => {
                    return Err(SectionParseError::UnknownAttribute(*key));
                }
            }
        }

        self.parse_section()
    }

    fn parse_section(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        match self.section.class.as_ref().map(Class::kind) {
            Some(ClassKind::Hook) => self.parse_hook(),
            Some(ClassKind::Type) => self.parse_type(),
            Some(ClassKind::Table) => self.parse_table(),
            Some(ClassKind::Field) => self.parse_field(),
            Some(ClassKind::Library) => self.parse_library(),
            Some(ClassKind::Function) => self.parse_function(),
            Some(ClassKind::Unknown) => Err(SectionParseError::UnknownClass(
                self.section.class.unwrap().ident(),
            )),
            None => self.parse_not_specified(),
        }
    }

    fn parse_hook(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let (ident, name) = self.section.take_ident_and_name()?;
        let section = HookSection {
            ident,
            hook: Hook {
                name,
                description: self.section.take_description(),
                realm: self.section.get_realm(),
                parameters: self.section.take_parameters(),
                returns: self.section.take_returns(),
            },
        };
        Ok(Section::Hook(section))
    }

    fn parse_type(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let (ident, name) = self.section.take_ident_and_name()?;
        let section = TypeSection {
            ident,
            ty: Type {
                name,
                description: self.section.take_description(),
                realm: self.section.get_realm(),
                methods: Default::default(),
                meta_methods: Default::default(),
            },
            tbls: self.section.take_tbls(),
        };
        Ok(Section::Type(section))
    }

    fn parse_table(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let (tbl, ident) = parse_table_name(&mut self.section)?;
        let section = TableSection {
            ident,
            table: Table {
                name: ident.to_string(),
                description: self.section.take_description(),
                realm: self.section.get_realm(),
                fields: self.section.take_fields_map(),
            },
            tbl,
        };
        Ok(Section::Table(section))
    }

    fn parse_field(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let (tbl, ident) = parse_table_name(&mut self.section)?;
        let section = FieldSection {
            ident,
            field: Field {
                name: ident.to_string(),
                description: self.section.take_description(),
            },
            tbl,
        };
        Ok(Section::Field(section))
    }

    fn parse_library(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let (ident, name) = self.section.take_ident_and_name()?;
        let section = LibrarySection {
            ident,
            library: Library {
                name,
                description: self.section.take_description(),
                realm: self.section.get_realm(),
                tables: Default::default(),
                methods: Default::default(),
                fields: Default::default(),
            },
            tbls: self.section.take_tbls(),
        };
        Ok(Section::Library(section))
    }

    fn parse_function(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let (tbl, ident) = parse_function_name(&mut self.section)?;
        let section = MethodSection {
            ident,
            method: Method {
                name: ident.to_string(),
                description: self.section.take_description(),
                realm: self.section.get_realm(),
                parameters: self.section.take_parameters(),
                returns: self.section.take_returns(),
                // TODO: parse from description
                deprecated: false,
            },
            tbl,
        };
        Ok(Section::Method(section))
    }

    // not_specified should be a method of a type
    fn parse_not_specified(&mut self) -> Result<Section<'s>, SectionParseError<'s>> {
        let (tbl, ident) = parse_function_name(&mut self.section)?;
        let section = MethodSection {
            ident,
            method: Method {
                name: ident.to_string(),
                description: self.section.take_description(),
                realm: self.section.get_realm(),
                parameters: self.section.take_parameters(),
                returns: self.section.take_returns(),
                // TODO: parse from description
                deprecated: false,
            },
            tbl,
        };
        Ok(Section::Method(section))
    }
}

fn type_union_to_type(type_union: &TypeUnion) -> String {
    let mut joint_type = String::new();
    for t in type_union.types() {
        if !joint_type.is_empty() {
            joint_type.push('|');
        }
        joint_type.push_str(t.as_str());
    }
    joint_type
}

fn parse_table_name<'s>(
    section: &mut PartialSection<'s>,
) -> Result<(Ident<'s>, Ident<'s>), SectionParseError<'s>> {
    match section.take_ident() {
        Some(name) => name
            .as_str()
            .split_once('.')
            .map(|(tbl, name)| (Ident::new(tbl).unwrap(), Ident::new(name).unwrap()))
            .ok_or_else(|| SectionParseError::InvalidTableName(name)),
        None => Err(SectionParseError::MissingName),
    }
}

fn parse_function_name<'s>(
    section: &mut PartialSection<'s>,
) -> Result<(Ident<'s>, Ident<'s>), SectionParseError<'s>> {
    match section.take_ident() {
        Some(name) => name
            .as_str()
            .split_once('.')
            .map(|(tbl, name)| (Ident::new(tbl).unwrap(), Ident::new(name).unwrap()))
            .ok_or_else(|| SectionParseError::InvalidFunctionName(name)),
        None => parse_next_line(section),
    }
}

fn parse_next_line<'s>(
    section: &PartialSection<'s>,
) -> Result<(Ident<'s>, Ident<'s>), SectionParseError<'s>> {
    let line = section
        .next_line
        .ok_or_else(|| SectionParseError::MissingNextLine)?;
    let x = line.trim().trim_start_matches("function").trim_start();
    let (tbl, func) = x
        .split_once(":")
        .or(x.split_once("."))
        .ok_or_else(|| SectionParseError::InvalidFunctionLine(line))?;

    let space = func.find(char::is_whitespace).unwrap_or(usize::MAX);
    let paren = func.find('(').unwrap_or(usize::MAX);
    let min = space.min(paren);
    if min == usize::MAX {
        return Err(SectionParseError::InvalidFunctionLine(line));
    }

    let func = &func[..min];
    Ok((
        Ident::new(tbl).map_err(|_| SectionParseError::InvalidFunctionLine(line))?,
        Ident::new(func).map_err(|_| SectionParseError::InvalidFunctionLine(line))?,
    ))
}
