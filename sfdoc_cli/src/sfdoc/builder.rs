use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::sfdoc::{Hook, Type};

use super::{
    section::{
        FieldSection, HookSection, LibrarySection, MethodSection, Section, SectionParser,
        TableSection, TypeSection,
    },
    Docs, Field, Library, Method, Realm, Table,
};

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
    line: usize,
    message: String,
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
    hooks: HashMap<&'s str, Hook>,
    libraries: HashMap<&'s str, Library>,
    tbl_to_lib: HashMap<&'s str, &'s str>,
    tbl_to_type: HashMap<&'s str, &'s str>,
    tbl_methods: HashMap<&'s str, HashMap<&'s str, Method>>,
    tbl_tables: HashMap<&'s str, HashMap<&'s str, Table>>,
    tbl_fields: HashMap<&'s str, HashMap<&'s str, Field>>,
    lua_files: HashMap<&'s Path, LuaFile<'s>>,
    diagnostics: Vec<Diagnostic>,
}

impl<'s> DocBuilder<'s> {
    pub fn parse_file(&mut self, file: LuaFile<'s>) {
        self.lua_files.insert(file.path(), file);

        let mut parser = SectionParser::new(file.source, file.realm());
        while let Some(result) = parser.next_section() {
            match result {
                Ok(section) => self.parse_section(section),
                Err(err) => log::error!(
                    "Failed to parse section in {}: {}",
                    file.path.display(),
                    err
                ),
            }
        }
    }

    pub fn finish(self) -> (Docs, Vec<Diagnostic>) {
        let mut docs = Docs::default();

        // Insert hooks
        for hook in self.hooks.values() {
            docs.hooks.insert(hook.name.clone(), hook.clone());
        }

        // Insert types
        for ty in self.types.into_values() {
            docs.types.insert(ty.name.clone(), ty);
        }

        // Insert libraries
        for lib in self.libraries.values() {
            docs.libraries.insert(lib.name.clone(), lib.clone());
        }

        // Insert methods
        for (tbl, methods) in self.tbl_methods.into_iter() {
            let methods_map = match self.tbl_to_lib.get(tbl) {
                Some(lib) => &mut docs.libraries.get_mut(*lib).unwrap().methods,
                None => match self.tbl_to_type.get(tbl) {
                    Some(ty) => {
                        if tbl.contains("_meta") {
                            &mut docs.types.get_mut(*ty).unwrap().meta_methods
                        } else {
                            &mut docs.types.get_mut(*ty).unwrap().methods
                        }
                    }
                    None => {
                        log::warn!("No library or type found for tbl {}", tbl);
                        continue;
                    }
                },
            };
            for method in methods.into_values() {
                methods_map.insert(method.name.clone(), method);
            }
        }

        // Insert tables
        for (tbl, tables) in self.tbl_tables.into_iter() {
            if let Some(lib) = self.tbl_to_lib.get(tbl) {
                let library = &mut docs.libraries.get_mut(*lib).unwrap();
                for table in tables.into_values() {
                    library.tables.insert(table.name.clone(), table);
                }
            }
        }

        // Insert fields
        for (tbl, fields) in self.tbl_fields.into_iter() {
            if let Some(lib) = self.tbl_to_lib.get(tbl) {
                let library = &mut docs.libraries.get_mut(*lib).unwrap();
                for field in fields.into_values() {
                    library.fields.insert(field.name.clone(), field);
                }
            } else {
                log::warn!("No library found for tbl {}", tbl);
            }
        }

        (docs, self.diagnostics)
    }

    fn parse_section(&mut self, section: Section<'s>) {
        match section {
            Section::Hook(section) => self.parse_section_hook(section),
            Section::Type(section) => self.parse_section_type(section),
            Section::Table(section) => self.parse_section_table(section),
            Section::Field(section) => self.parse_section_field(section),
            Section::Library(section) => self.parse_section_library(section),
            Section::Method(section) => self.parse_section_method(section),
        }
    }

    fn parse_section_hook(&mut self, section: HookSection<'s>) {
        match self.hooks.contains_key(&section.ident.as_str()) {
            true => log::warn!("Duplicate hook: {}", section.ident),
            false => {
                self.hooks.insert(section.ident.as_str(), section.hook);
            }
        }
    }

    fn parse_section_type(&mut self, section: TypeSection<'s>) {
        if self.types.contains_key(&section.ident.as_str()) {
            log::warn!("Duplicate type: {}", section.ident);
            return;
        }

        for tbl in section.tbls.iter() {
            if self.tbl_to_type.contains_key(tbl.as_str())
                || self.tbl_to_lib.contains_key(tbl.as_str())
            {
                log::warn!("Duplicate type tbl: {}", tbl);
                continue;
            }
        }

        for tbl in section.tbls {
            self.tbl_to_type
                .insert(tbl.as_str(), section.ident.as_str());
        }

        self.types.insert(section.ident.as_str(), section.ty);
    }

    fn parse_section_table(&mut self, section: TableSection<'s>) {
        let tables = self.tbl_tables.entry(section.tbl.as_str()).or_default();
        if tables.contains_key(&section.ident.as_str()) {
            log::warn!("Duplicate table: {}", section.ident);
            return;
        }
        tables.insert(section.ident.as_str(), section.table);
    }

    fn parse_section_field(&mut self, section: FieldSection<'s>) {
        let fields = self.tbl_fields.entry(section.tbl.as_str()).or_default();
        if fields.contains_key(&section.ident.as_str()) {
            log::warn!("Duplicate field: {}", section.ident);
            return;
        }
        fields.insert(section.ident.as_str(), section.field);
    }

    fn parse_section_library(&mut self, section: LibrarySection<'s>) {
        if self.libraries.contains_key(&section.ident.as_str()) {
            log::warn!("Duplicate library: {}", section.ident);
            return;
        }

        for tbl in section.tbls.iter() {
            if self.tbl_to_lib.contains_key(tbl.as_str()) {
                log::warn!("Duplicate library tbl: {}", tbl);
                continue;
            }
        }

        for tbl in section.tbls {
            self.tbl_to_lib.insert(tbl.as_str(), section.ident.as_str());
        }

        self.libraries
            .insert(section.ident.as_str(), section.library);
    }

    fn parse_section_method(&mut self, section: MethodSection<'s>) {
        let methods = self.tbl_methods.entry(section.tbl.as_str()).or_default();
        if methods.contains_key(&section.ident.as_str()) {
            log::warn!("Duplicate method: {}", section.ident);
            return;
        }
        methods.insert(section.ident.as_str(), section.method);
    }
}
