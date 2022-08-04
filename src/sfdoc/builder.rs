use std::{
    assert_matches::debug_assert_matches,
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::sfdoc::{Field, Hook, Parameter, Return, Type};

use super::{diagnostic::DiagnosticEmitter, parser2::*, Docs, Library, Method, Realm, Table};

use anyhow::Result;

// https://stackoverflow.com/questions/34953711/unwrap-inner-type-when-enum-variant-is-known
macro_rules! attr_single {
    ($section:expr, $pattern:pat => $ext:expr, $msg:expr) => {
        #[allow(unused)]
        $section
            .attributes()
            .iter()
            .find(|v| std::matches!(v.kind(), $pattern))
            .map(|v| match v.kind() {
                $pattern => $ext,
                _ => unreachable!(),
            })
            .ok_or_else(|| DocBuilderError::new($section.line_number(), $msg))
    };
}

macro_rules! attr_name {
    ($section:expr, $msg:expr) => {
        attr_single!($section, AttributeKind::Name{name:n, ..} => n, $msg)
    };
}

macro_rules! attr_many {
    ($section:expr, $pattern:pat => $ext:expr) => {
        #[allow(unused)]
        $section
            .attributes()
            .iter()
            .filter(|v| std::matches!(v.kind(), $pattern))
            .map(|v| match v.kind() {
                $pattern => $ext,
                _ => unreachable!(),
            })
    };
}

macro_rules! attr_desc {
    ($section:expr, $msg:expr) => {{
        let mut fail_on_desc = false;
        let mut description = String::new();
        for attr in $section.attributes() {
            match attr.kind() {
                AttributeKind::Description(d) => {
                    if fail_on_desc {
                        return Err(DocBuilderError::new(
                            attr.line_number(),
                            "Found description line after attributes started",
                        ));
                    }
                    if !description.is_empty() {
                        description.push(' ');
                    }
                    description.push_str(d.as_str());
                }
                _ => fail_on_desc = true,
            }
        }
        description
    }};
}

macro_rules! attr_fields {
    ($section:expr) => {
        attr_many!($section, AttributeKind::Field{name, description} => Field{
            name: name.to_string(),
            description: description.as_str().to_owned(),
        }).collect::<Vec<_>>()
    };
}

macro_rules! attr_params {
    ($section:expr) => {
        {
            let mut params = Vec::new();
            #[allow(unused)]
            let param_iter = attr_many!($section, AttributeKind::Parameter { ty, name, description } => (ty, name, description));
            for (types, name, description) in param_iter {
                let mut ty = String::new();
                for t in types.types() {
                    if !ty.is_empty(){ty.push('|');}
                    ty.push_str(t.as_str());
                }
                params.push(Parameter {
                    name:name.as_str().to_string(),
                    ty,
                    description:description.as_str().to_owned(),
                    optional:types.optional(),
                });
            }
            params
        }
    };
}

macro_rules! attr_returns {
    ($section:expr) => {
        {
            let mut returns = Vec::new();
            #[allow(unused)]
            let returns_iter = attr_many!($section, AttributeKind::Return { ty, description } => (ty, description));
            for (types, description) in returns_iter {
                let mut ty = String::new();
                for t in types.types() {
                    if !ty.is_empty(){ty.push('|');}
                    ty.push_str(t.as_str());
                }
                returns.push(Return {
                    ty,
                    description:description.as_str().to_owned(),
                });
            }
            returns
        }
    };
}

macro_rules! attr_realm {
    ($section:expr, $def:expr) => {{
        let mut realm = $def;
        for attr in $section.attributes() {
            match attr.kind() {
                AttributeKind::Server | AttributeKind::Client | AttributeKind::Shared => {
                    realm = attribute_kind_to_realm(attr.kind());
                    break;
                }
                _ => {}
            }
        }
        realm
    }};
}

#[derive(Debug, Clone, Copy)]
pub struct LuaFile<'s> {
    path: &'s Path,
    source: &'s str,
}

impl<'s> Default for LuaFile<'s> {
    fn default() -> Self {
        Self {
            path: Path::new(""),
            source: Default::default(),
        }
    }
}

impl<'s> LuaFile<'s> {
    pub fn new(path: &'s Path, source: &'s str) -> Self {
        Self { path, source }
    }

    fn path(&self) -> &'s Path {
        self.path
    }

    fn realm(&self) -> Realm {
        Realm::Shared
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticLevel {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    path: PathBuf,
    level: DiagnosticLevel,
}

struct DocBuilderError {
    line_number: usize,
    message: String,
}

impl DocBuilderError {
    fn new(line_number: usize, message: impl Into<String>) -> Self {
        Self {
            line_number,
            message: message.into(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct DocBuilder<'s> {
    types: HashMap<&'s str, Type>,
    libraries: HashMap<&'s str, Library>,
    tbl_to_lib: HashMap<&'s str, &'s str>,
    tbl_to_type: HashMap<&'s str, &'s str>,
    tbl_methods: HashMap<&'s str, Vec<Method>>,
    tbl_tables: HashMap<&'s str, Vec<Table>>,
    lua_files: HashMap<&'s Path, LuaFile<'s>>,
    diagnostics: Vec<Diagnostic>,
    current_file: LuaFile<'s>,
}

impl<'s> DocBuilder<'s> {
    pub fn new() -> Self {
        Self {
            types: Default::default(),
            libraries: Default::default(),
            tbl_to_lib: Default::default(),
            tbl_to_type: Default::default(),
            tbl_methods: Default::default(),
            tbl_tables: Default::default(),
            lua_files: Default::default(),
            diagnostics: Default::default(),
            current_file: Default::default(),
        }
    }

    pub fn parse_file(&mut self, file: LuaFile<'s>) {
        // TODO: warning if already exists
        self.lua_files.insert(file.path(), file);
        self.current_file = file;
        let mut parser = Parser::new(file.source);
        while let Some(parse_result) = parser.next_section() {
            match parse_result {
                Ok(section) => self.parse_section(section),
                Err(err) => {
                    // TODO
                }
            }
        }
    }

    pub fn finish(self) -> (Docs, Vec<Diagnostic>) {
        todo!()
    }

    fn parse_section(&mut self, section: Section<'_, 's>) {
        if section.multiple_classes() {
            //diagnostics.error(
            //    section.line_number(),
            //    "Multiple classes are not allowed in a single section".to_string(),
            //);
            return;
        }

        log::debug!("Parsing section with class {:?}", section.class());
        // TODO: Remove Result from all of this functions and push diagnostics instead
        let result = match section.class().map(Class::kind) {
            Some(ClassKind::Hook) => self.parse_hook(section),
            Some(ClassKind::Type) => self.parse_type(section),
            Some(ClassKind::Table) => self.parse_table(section),
            Some(ClassKind::Library) => self.parse_library(section),
            Some(ClassKind::Function) => self.parse_function(section),
            Some(ClassKind::Unknown) => Err(DocBuilderError::new(
                section.line_number(),
                format!("Unknown section class: {:?}", section.class()),
            )),
            None => self.parse_not_specified(section),
        };
    }

    fn parse_hook(&mut self, section: Section<'_, 's>) -> Result<(), DocBuilderError> {
        let description = attr_desc!(section, "Hook missing description");
        let name = attr_name!(section, "Hook missing name")?;
        let parameters = attr_params!(section);
        let returns = attr_returns!(section);
        let realm = attr_realm!(section, self.current_file.realm());

        let hook = Hook {
            name: name.to_string(),
            description,
            parameters,
            returns,
            realm,
        };

        // TODO
        //if self.docs.hooks.contains_key(name.as_str()) {
        //    return Err(DocBuilderError::new(
        //        section.line_number(),
        //        format!("Duplicate hook : {}", name),
        //    ));
        //}

        //self.docs.hooks.insert(name.to_string(), hook);

        Ok(())
    }

    fn parse_type(&mut self, section: Section<'_, 's>) -> Result<(), DocBuilderError> {
        let name = attr_name!(section, "Type missing name")?;
        let description = attr_desc!(section, "Type missing description");
        let realm = attr_realm!(section, self.current_file.realm());
        let libtbls = attr_many!(section, AttributeKind::Libtbl(tbl) => tbl);

        for tbl in libtbls {
            self.tbl_to_type.insert(tbl.as_str(), name.as_str());
        }

        // TODO
        //self.docs.types.insert(
        //    name.to_string(),
        //    Type {
        //        name: name.to_string(),
        //        description,
        //        realm,
        //        methods: Default::default(),
        //        meta_methods: Default::default(),
        //    },
        //);

        Ok(())
    }

    fn parse_table(&mut self, section: Section<'_, 's>) -> Result<(), DocBuilderError> {
        let name = attr_name!(section, "Table missing name")?;
        let description = attr_desc!(section, "Table missing description");
        let fields = attr_fields!(section);
        let realm = attr_realm!(section, self.current_file.realm());

        let (tbl, name) = name.as_str().split_once('.').ok_or_else(|| {
            DocBuilderError::new(
                section.line_number(),
                "Table name must be in the format <lib>.<table>".to_string(),
            )
        })?;

        let table = Table {
            name: name.to_string(),
            description,
            realm,
            fields: fields.into_iter().map(|f| (f.name.clone(), f)).collect(),
        };
        log::trace!("parsed table: {:#?}", table);
        self.tbl_tables.entry(tbl).or_default().push(table);

        Ok(())
    }

    fn parse_library(&mut self, section: Section<'_, 's>) -> Result<(), DocBuilderError> {
        let name = attr_name!(section, "Library missing name")?;
        let description = attr_desc!(section, "Library missing description");
        let realm = attr_realm!(section, self.current_file.realm());
        let libtbls = attr_many!(section, AttributeKind::Libtbl(tbl) => tbl);

        log::debug!("found library: {}", name);
        for tbl in libtbls {
            log::debug!("mapping tbl '{}' to library '{}'", tbl, name);
            self.tbl_to_lib.insert(tbl.as_str(), name.as_str());
        }

        // TODO
        //self.docs.libraries.insert(
        //    name.to_string(),
        //    Library {
        //        name: name.to_string(),
        //        description,
        //        realm,
        //        tables: Default::default(),
        //        methods: Default::default(),
        //    },
        //);

        Ok(())
    }

    fn parse_function(&mut self, section: Section<'_, 's>) -> Result<(), DocBuilderError> {
        let name = attr_name!(section, "Function missing name");
        let description = attr_desc!(section, "Function missing description");
        let parameters = attr_params!(section);
        let returns = attr_returns!(section);
        let realm = attr_realm!(section, self.current_file.realm());
        let function_line = section.following_line().ok_or(DocBuilderError::new(
            section.line_number(),
            "Function missing function line".to_string(),
        ))?;

        let (tbl, name) = if let Ok(name) = name {
            name.as_str().split_once(".").ok_or_else(|| {
                DocBuilderError::new(section.line_number(), "Invalid function name")
            })?
        } else if let Some((tbl, name)) = function_line_to_table_and_function_name(function_line) {
            (tbl, name)
        } else {
            return Err(DocBuilderError::new(
                section.line_number(),
                "Function missing name".to_string(),
            ));
        };

        let method = Method {
            name: name.to_string(),
            description,
            parameters,
            returns,
            realm,
            deprecated: false,
        };

        log::debug!("found method: {} from {}", method.name, tbl);
        self.tbl_methods.entry(tbl).or_default().push(method);
        Ok(())
    }

    // not_specified should be a method of a type
    fn parse_not_specified(&mut self, section: Section<'_, 's>) -> Result<(), DocBuilderError> {
        let description = attr_desc!(section, "Method missing description");
        let realm = attr_realm!(section, self.current_file.realm());
        let parameters = attr_params!(section);
        let returns = attr_returns!(section);
        let function_line = section.following_line().ok_or_else(|| {
            DocBuilderError::new(
                section.line_number(),
                "Method missing function line following it".to_string(),
            )
        })?;
        let (table_name, function_name) = function_line_to_table_and_function_name(function_line)
            .ok_or_else(|| {
            DocBuilderError::new(
                section.line_number(),
                "Method missing table name".to_string(),
            )
        })?;

        let is_meta = table_name.contains("_meta");
        let method = Method {
            name: function_name.to_string(),
            description,
            parameters,
            returns,
            realm,
            deprecated: false,
        };

        //if let Some(lib_name) = self.tbl_to_lib.get(table_name) {
        //    self.docs
        //        .libraries
        //        .get_mut(*lib_name)
        //        .ok_or_else(|| {
        //            DocBuilderError::new(
        //                section.line_number(),
        //                format!("Library not found: {}", lib_name),
        //            )
        //        })?
        //        .methods
        //        .insert(method.name.clone(), method);
        //} else if let Some(ty_name) = self.tbl_to_type.get(table_name) {
        //    let ty = self.docs.types.get_mut(*ty_name).ok_or_else(|| {
        //        DocBuilderError::new(
        //            section.line_number(),
        //            format!("Type not found: {}", ty_name),
        //        )
        //    })?;
        //    if is_meta {
        //        ty.meta_methods.insert(method.name.clone(), method);
        //    } else {
        //        ty.methods.insert(method.name.clone(), method);
        //    }
        //} else {
        //    return Err(DocBuilderError::new(
        //        section.line_number(),
        //        format!("Table not found: {}", table_name),
        //    ));
        //}

        Ok(())
    }
}

fn attribute_kind_to_realm(kind: &AttributeKind) -> Realm {
    match kind {
        AttributeKind::Server => Realm::Server,
        AttributeKind::Client => Realm::Client,
        AttributeKind::Shared => Realm::Shared,
        _ => unreachable!(),
    }
}

fn function_line_to_table_and_function_name(line: &str) -> Option<(&str, &str)> {
    let x = line.trim().trim_start_matches("function").trim_start();
    let (tbl, func) = x.split_once(":").or(x.split_once("."))?;

    let space = func.find(char::is_whitespace).unwrap_or(usize::MAX);
    let paren = func.find('(').unwrap_or(usize::MAX);
    let min = space.min(paren);
    let func = &func[..min];
    Some((tbl, func))
}
