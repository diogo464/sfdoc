use std::{collections::HashMap, path::Path};

use crate::{
    item,
    section::{
        FieldSection, HookSection, LibrarySection, MethodSection, Section, SectionBuilder,
        SectionDiagnostic, SectionStatus, TableSection, TypeSection,
    },
    source::{Source, SourceIndexer, SourceReader},
    Diagnostic, Docs, Field, Hook, Library, LuaFile, Method, Table, Type,
};

#[derive(Debug, Clone)]
struct DocBuilderFile<'s> {
    path: &'s Path,
    source: Source<'s>,
    indexer: SourceIndexer,
}

#[derive(Debug, Default, Clone)]
pub struct DocBuilder<'s> {
    files: HashMap<&'s Path, DocBuilderFile<'s>>,
    types: HashMap<String, Type>,
    hooks: HashMap<String, Hook>,
    libraries: HashMap<String, Library>,
    tbl_to_lib: HashMap<String, String>,
    tbl_to_type: HashMap<String, String>,
    tbl_methods: HashMap<String, HashMap<String, Method>>,
    tbl_tables: HashMap<String, HashMap<String, Table>>,
    tbl_fields: HashMap<String, HashMap<String, Field>>,
    diagnostics: Vec<Diagnostic>,
    current_file: Option<&'s Path>,
}

impl<'s> DocBuilder<'s> {
    pub fn parse_file(&mut self, file: LuaFile<'s>) {
        if self.files.contains_key(file.path) {
            log::warn!("{} already parsed", file.path.display());
            return;
        }

        self.files.insert(
            file.path,
            DocBuilderFile {
                path: file.path,
                source: Source::new(file.source),
                indexer: SourceIndexer::new(file.source),
            },
        );

        log::info!("Parsing file {}", file.path.display());

        self.current_file = Some(file.path);
        let source = Source::new(file.source);
        let mut reader = SourceReader::new(source);
        let mut section_builder = SectionBuilder::new(source, file.realm());
        while !reader.is_eof() {
            match reader.parse::<item::Item, _>() {
                Ok(item) => {
                    self.diagnostic_item(&item);
                    match section_builder.feed_item(item) {
                        SectionStatus::FeedMore => {}
                        SectionStatus::Complete(section) => {
                            self.parse_section(section);
                            for diagnostic in section_builder.drain_diagnostics() {
                                self.diagnostic_section(diagnostic);
                            }
                        }
                    }
                }
                Err(err) => {
                    reader.skip_line();
                    self.diagnostic_item_parse_error(err);
                }
            }
        }
    }

    pub fn finish(mut self) -> (Docs, Vec<Diagnostic>) {
        let mut docs = Docs::default();

        self.diagnostic_check_unknown_types();
        self.diagnostic_required_after_optional();

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
            let methods_map = match self.tbl_to_lib.get(&tbl) {
                Some(lib) => &mut docs.libraries.get_mut(lib).unwrap().methods,
                None => match self.tbl_to_type.get(&tbl) {
                    Some(ty) => {
                        if tbl.contains("_meta") {
                            &mut docs.types.get_mut(ty).unwrap().meta_methods
                        } else {
                            &mut docs.types.get_mut(ty).unwrap().methods
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
            if let Some(lib) = self.tbl_to_lib.get(&tbl) {
                let library = &mut docs.libraries.get_mut(lib).unwrap();
                for table in tables.into_values() {
                    library.tables.insert(table.name.clone(), table);
                }
            }
        }

        // Insert fields
        for (tbl, fields) in self.tbl_fields.into_iter() {
            if let Some(lib) = self.tbl_to_lib.get(&tbl) {
                let library = &mut docs.libraries.get_mut(lib).unwrap();
                for field in fields.into_values() {
                    library.fields.insert(field.name.clone(), field);
                }
            } else {
                log::warn!("No library found for tbl {}", tbl);
            }
        }

        (docs, self.diagnostics)
    }

    fn parse_section(&mut self, section: Section) {
        match section {
            Section::Hook(section) => self.parse_section_hook(section),
            Section::Type(section) => self.parse_section_type(section),
            Section::Table(section) => self.parse_section_table(section),
            Section::Field(section) => self.parse_section_field(section),
            Section::Library(section) => self.parse_section_library(section),
            Section::Method(section) => self.parse_section_method(section),
        }
    }

    fn parse_section_hook(&mut self, section: HookSection) {
        match self.hooks.contains_key(&section.name) {
            true => log::warn!("Duplicate hook: {}", section.name),
            false => {
                self.hooks.insert(
                    section.name.clone(),
                    Hook {
                        name: section.name,
                        description: section.description,
                        realm: section.realm,
                        parameters: section.parameters,
                        returns: section.returns,
                    },
                );
            }
        }
    }

    fn parse_section_type(&mut self, section: TypeSection) {
        if self.types.contains_key(&section.name) {
            log::warn!("Duplicate type: {}", section.name);
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
            self.tbl_to_type.insert(tbl.clone(), section.name.clone());
        }

        self.types.insert(
            section.name.clone(),
            Type {
                name: section.name,
                description: section.description,
                realm: section.realm,
                methods: Default::default(),
                meta_methods: Default::default(),
            },
        );
    }

    fn parse_section_table(&mut self, section: TableSection) {
        let tables = self.tbl_tables.entry(section.tbl).or_default();
        if tables.contains_key(&section.name) {
            log::warn!("Duplicate table: {}", section.name);
            return;
        }
        tables.insert(
            section.name.clone(),
            Table {
                name: section.name,
                description: section.description,
                realm: section.realm,
                fields: section
                    .fields
                    .into_iter()
                    .map(|f| (f.name.clone(), f))
                    .collect(),
            },
        );
    }

    fn parse_section_field(&mut self, section: FieldSection) {
        let fields = self.tbl_fields.entry(section.tbl).or_default();
        if fields.contains_key(&section.name) {
            log::warn!("Duplicate field: {}", section.name);
            return;
        }
        fields.insert(
            section.name.clone(),
            Field {
                name: section.name,
                description: section.description,
            },
        );
    }

    fn parse_section_library(&mut self, section: LibrarySection) {
        if self.libraries.contains_key(&section.name) {
            log::warn!("Duplicate library: {}", section.name);
            return;
        }

        for tbl in section.tbls.iter() {
            if self.tbl_to_lib.contains_key(tbl.as_str()) {
                log::warn!("Duplicate library tbl: {}", tbl);
                continue;
            }
        }

        for tbl in section.tbls {
            self.tbl_to_lib.insert(tbl, section.name.clone());
        }

        self.libraries.insert(
            section.name.clone(),
            Library {
                name: section.name,
                description: section.description,
                realm: section.realm,
                tables: Default::default(),
                methods: Default::default(),
                fields: Default::default(),
            },
        );
    }

    fn parse_section_method(&mut self, section: MethodSection) {
        let methods = self.tbl_methods.entry(section.tbl.clone()).or_default();
        if methods.contains_key(&section.name) {
            log::warn!("Duplicate method: {}", section.name);
            return;
        }
        methods.insert(
            section.name.clone(),
            Method {
                name: section.name,
                description: section.description,
                realm: section.realm,
                parameters: section.parameters,
                returns: section.returns,
                deprecated: section.deprecated,
            },
        );
    }
}

impl<'s> DocBuilder<'s> {
    fn diagnostic_item(&mut self, item: &item::Item) {}

    fn diagnostic_item_parse_error(&mut self, err: item::ItemParseError) {
        let file = self.get_current_file();
        let location = file.indexer.locate(err.span().begin());
        log::error!(
            "File {}\n{:?}\n{}",
            file.path.display(),
            location,
            err.message()
        );
    }

    fn diagnostic_section(&mut self, diagnostic: SectionDiagnostic) {
        let file = self.get_current_file();
        let location = file.indexer.locate(diagnostic.span().begin());
        log::error!(
            "File {}\nLocation {:?}\n{}",
            file.path.display(),
            location,
            diagnostic.message()
        );
    }

    fn diagnostic_check_unknown_types(&mut self) {
        for (tbl, methods) in self.tbl_methods.iter() {
            for method in methods.values() {
                let parameter_types = method
                    .parameters()
                    .iter()
                    .map(|p| p.types().into_iter())
                    .flatten();
                let return_types = method
                    .returns()
                    .iter()
                    .map(|p| p.types().into_iter())
                    .flatten();
                let types = parameter_types.chain(return_types);
                for ty in types {
                    if !is_well_known_type(ty) && !self.types.contains_key(ty) {
                        log::warn!("Unknown type in {}.{}: {}", tbl, method.name(), ty);
                    }
                }
            }
        }
    }

    fn diagnostic_required_after_optional(&mut self) {
        for (tbl, methods) in self.tbl_methods.iter() {
            for method in methods.values() {
                let mut optional = false;
                for param in method.parameters() {
                    optional |= param.optional();
                    if !param.optional() && optional {
                        log::warn!(
                            "Required parameter after optional parameter in {}.{}: {}",
                            tbl,
                            method.name(),
                            param.name()
                        );
                    }
                }
            }
        }
    }

    fn get_current_file(&self) -> &DocBuilderFile<'s> {
        &self.files[self.current_file.unwrap()]
    }
}

fn is_well_known_type(ty: &str) -> bool {
    match ty {
        "table" | "boolean" | "string" | "integer" | "number" | "any" | "function" | "..."
        | "nil" | "thread" => true,
        _ => false,
    }
}