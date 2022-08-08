use std::cell::{Cell, RefCell};

use crate::{
    item,
    source::{Source, Span, Spanned},
    Field, Hook, Parameter, Realm, Return, Type,
};

#[derive(Debug, Clone)]
pub struct SectionDiagnostic {
    span: Span,
    message: String,
}

impl SectionDiagnostic {
    fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

pub trait SectionDiagnosticEmitter {
    fn emit(&mut self, diagnostic: SectionDiagnostic);
}

#[derive(Debug, Clone)]
pub struct HookSection {
    pub span: Span,
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub parameters: Vec<Parameter>,
    pub returns: Vec<Return>,
}

#[derive(Debug, Clone)]
pub struct TypeSection {
    pub span: Span,
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub tbls: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TableSection {
    pub span: Span,
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub fields: Vec<Field>,
    pub tbl: String,
}

#[derive(Debug, Clone)]
pub struct FieldSection {
    pub span: Span,
    pub name: String,
    pub description: String,
    pub tbl: String,
}

#[derive(Debug, Clone)]
pub struct LibrarySection {
    pub span: Span,
    pub name: String,
    pub realm: Realm,
    pub description: String,
    pub tbls: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MethodSection {
    pub span: Span,
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub parameters: Vec<Parameter>,
    pub returns: Vec<Return>,
    pub deprecated: bool,
    pub tbl: String,
}

#[derive(Debug, Clone)]
pub enum Section {
    Hook(HookSection),
    Type(TypeSection),
    Table(TableSection),
    Field(FieldSection),
    Library(LibrarySection),
    Method(MethodSection),
}

struct SectionBuilder2<'a, E> {
    source: Source<'a>,
    section: item::Section,
    default_realm: Realm,
    emitter: RefCell<&'a mut E>,
}

impl<'a, E: SectionDiagnosticEmitter> SectionBuilder2<'a, E> {
    fn build(mut self) -> Option<Section> {
        let class_kind = self.get_class_kind();
        match class_kind {
            Some(item::ClassKind::Hook) => self.build_hook(),
            Some(item::ClassKind::Type) => self.build_type(),
            Some(item::ClassKind::Table) => self.build_table(),
            Some(item::ClassKind::Field) => self.build_field(),
            Some(item::ClassKind::Library) => self.build_library(),
            Some(item::ClassKind::Unknown) => {
                self.emit_diagnostic("unknown class kind".to_string());
                None
            }
            Some(item::ClassKind::Function) | None => self.build_method(),
        }
    }

    fn build_hook(self) -> Option<Section> {
        let name = self.get_name()?;
        let description = self.get_header_description();
        let realm = self.get_realm();
        let parameters = self.get_parameters();
        let returns = self.get_returns();

        Some(Section::Hook(HookSection {
            span: self.section.span(),
            name,
            description,
            realm,
            parameters,
            returns,
        }))
    }

    fn build_type(self) -> Option<Section> {
        let name = self.get_name()?;
        let description = self.get_header_description();
        let realm = self.get_realm();
        let tbls = self.get_tbls();

        Some(Section::Type(TypeSection {
            span: self.section.span(),
            name,
            description,
            realm,
            tbls,
        }))
    }

    fn build_table(self) -> Option<Section> {
        let (name, tbl) = self.get_name_and_tbl()?;
        let description = self.get_header_description();
        let realm = self.get_realm();
        let fields = self.get_fields();

        Some(Section::Table(TableSection {
            span: self.section.span(),
            name,
            description,
            realm,
            fields,
            tbl,
        }))
    }

    fn build_field(self) -> Option<Section> {
        let (name, tbl) = self.get_name_and_tbl()?;
        let description = self.get_header_description();

        Some(Section::Field(FieldSection {
            span: self.section.span(),
            name,
            description,
            tbl,
        }))
    }

    fn build_library(self) -> Option<Section> {
        let name = self.get_name()?;
        let description = self.get_header_description();
        let realm = self.get_realm();
        let tbls = self.get_tbls();

        Some(Section::Library(LibrarySection {
            span: self.section.span(),
            name,
            description,
            realm,
            tbls,
        }))
    }

    fn build_method(self) -> Option<Section> {
        let (name, tbl) = self.get_method_name_and_tbl()?;
        let description = self.get_header_description();
        let realm = self.get_realm();
        let parameters = self.get_parameters();
        let returns = self.get_returns();

        Some(Section::Method(MethodSection {
            span: self.section.span(),
            name,
            description,
            realm,
            parameters,
            returns,
            deprecated: false,
            tbl,
        }))
    }

    fn get_class_kind(&mut self) -> Option<item::ClassKind> {
        let mut class_kind = None;
        for attr in self.section.attributes() {
            match attr {
                item::Attribute::Class(c) => {
                    if class_kind.is_some() {
                        self.emit_diagnostic("multiple class attributes");
                    }
                    class_kind = Some(c.value().kind());
                }
                _ => {}
            }
        }
        class_kind
    }

    fn get_path(&self) -> Option<&item::Path> {
        let mut name = None;
        for attr in self.section.attributes() {
            match attr {
                item::Attribute::Name(n) => {
                    if name.is_some() {
                        self.emit_diagnostic("multiple name attributes");
                    }
                    name = Some(n.value());
                }
                _ => {}
            }
        }
        name
    }

    fn get_name(&self) -> Option<String> {
        let path = self.get_path()?;
        let name = self.source.lookup(&path.name()).to_owned();
        if path.table().is_some() {
            self.emit_diagnostic("section cannot contain a table in the name");
        }
        Some(name)
    }

    fn get_name_and_tbl(&self) -> Option<(String, String)> {
        let path = self.get_path()?;
        let name = self.source.lookup(&path.name()).to_owned();
        let tbl = match path.table() {
            Some(tbl) => {
                let tbl = self.source.lookup(&tbl).to_owned();
                if tbl.is_empty() {
                    self.emit_diagnostic("table name cannot be empty");
                    return None;
                }
                tbl
            }
            None => {
                self.emit_diagnostic("section name must be in a table");
                return None;
            }
        };
        Some((name, tbl))
    }

    fn get_header_description(&self) -> String {
        self.description_to_string(self.section.header())
    }

    fn get_parameters(&self) -> Vec<Parameter> {
        let mut parameters = Vec::new();
        for attr in self.section.attributes() {
            let param = match attr {
                item::Attribute::Parameter(attr) => attr.value(),
                _ => continue,
            };

            parameters.push(Parameter {
                name: self.source.lookup(&param.name()).to_owned(),
                description: self.description_to_string(param.description()),
                types: self.type_union_to_types_vec(param.type_union()),
                optional: param.type_union().optional(),
            });
        }
        parameters
    }

    fn get_returns(&self) -> Vec<Return> {
        let mut parameters = Vec::new();
        for attr in self.section.attributes() {
            let ret = match attr {
                item::Attribute::Return(attr) => attr.value(),
                _ => continue,
            };

            parameters.push(Return {
                description: self.description_to_string(ret.description()),
                types: self.type_union_to_types_vec(ret.type_union()),
                optional: ret.type_union().optional(),
            });
        }
        parameters
    }

    fn get_tbls(&self) -> Vec<String> {
        let mut tbls = Vec::new();
        for attr in self.section.attributes() {
            let tbl = match attr {
                item::Attribute::Libtbl(attr) => attr.value(),
                _ => continue,
            };

            tbls.push(self.source.lookup(tbl).to_owned());
        }
        tbls
    }

    fn get_fields(&self) -> Vec<Field> {
        let mut fields = Vec::new();
        for attr in self.section.attributes() {
            let field = match attr {
                item::Attribute::Field(attr) => attr.value(),
                _ => continue,
            };

            fields.push(Field {
                name: self.source.lookup(&field.name()).to_owned(),
                description: self.description_to_string(field.description()),
            });
        }
        fields
    }

    fn get_realm(&self) -> Realm {
        let mut realm = None;
        for attr in self.section.attributes() {
            match attr {
                item::Attribute::Server(_)
                | item::Attribute::Client(_)
                | item::Attribute::Shared(_) => {
                    if realm.is_some() {
                        self.emit_diagnostic("multiple realm attributes");
                    }
                    realm = Some(attr_to_realm(attr));
                }
                _ => {}
            }
        }
        realm.unwrap_or(self.default_realm)
    }

    fn get_method_name_and_tbl(&self) -> Option<(String, String)> {
        match self.get_path() {
            Some(path) => {
                let name = self.source.lookup(&path.name()).to_owned();
                let tbl = match path.table() {
                    Some(tbl) => {
                        let tbl = self.source.lookup(&tbl).to_owned();
                        if tbl.is_empty() {
                            self.emit_diagnostic(
                                "method name must contain the table it is in".to_string(),
                            );
                            return None;
                        }
                        tbl
                    }
                    None => {
                        self.emit_diagnostic("section name must be in a table".to_string());
                        return None;
                    }
                };
                Some((name, tbl))
            }
            None => {
                let source_line = self.source.lookup(&self.section.source_line());
                self.parse_source_line(source_line)
            }
        }
    }

    fn description_to_string(&self, description: &item::Description) -> String {
        let mut description = String::new();
        for span in self.section.header().spans() {
            if !description.is_empty() {
                description.push('\n');
            }
            description.push_str(self.source.lookup(&span));
        }
        description
    }

    fn type_union_to_types_vec(&self, type_union: &item::TypeUnion) -> Vec<String> {
        let mut types = Vec::new();
        for ty in type_union.types() {
            types.push(self.source.lookup(ty).to_owned());
        }
        types
    }

    fn parse_source_line(&self, line: &str) -> Option<(String, String)> {
        let x = line.trim().trim_start_matches("function").trim_start();
        let (tbl, func) = x.split_once(":").or(x.split_once("."))?;

        let space = func.find(char::is_whitespace).unwrap_or(usize::MAX);
        let paren = func.find('(').unwrap_or(usize::MAX);
        let min = space.min(paren);
        if min == usize::MAX {
            return None;
        }

        let func = &func[..min];
        Some((func.to_owned(), tbl.to_owned()))
    }

    fn emit_diagnostic(&self, message: impl Into<String>) {
        self.emitter
            .borrow_mut()
            .emit(SectionDiagnostic::new(self.section.span(), message.into()));
    }
}

fn attr_to_realm(attr: &item::Attribute) -> Realm {
    match attr {
        item::Attribute::Server(r) => Realm::Server,
        item::Attribute::Client(r) => Realm::Client,
        item::Attribute::Shared(r) => Realm::Shared,
        _ => unreachable!(),
    }
}

pub fn build_section<'a>(
    source: Source<'a>,
    section: item::Section,
    default_realm: Realm,
    diagnostic_emitter: &mut impl SectionDiagnosticEmitter,
) -> Option<Section> {
    let builder = SectionBuilder2 {
        source,
        section,
        default_realm,
        emitter: RefCell::new(diagnostic_emitter),
    };
    builder.build()
}

#[derive(Debug, Clone)]
pub enum SectionStatus {
    FeedMore,
    Complete(Section),
}

/// A parameter item with a variable sized description.
#[derive(Debug, Clone)]
struct SectionParam {
    item: item::Parameter,
    description: String,
}

/// A return item with a variable sized description.
#[derive(Debug, Clone)]
struct SectionReturn {
    item: item::Return,
    description: String,
}

/// A field item with a variable sized description.
#[derive(Debug, Clone)]
struct SectionField {
    item: item::Field,
    description: String,
}

/// An item whose description might grow beyond the description that was in its own line.
/// This can happen if there are comment lines after an item
#[derive(Debug, Clone)]
enum SectionItem {
    Param(SectionParam),
    Return(SectionReturn),
    Field(SectionField),
}

impl SectionItem {
    fn push_description(&mut self, desc: &str) {
        match self {
            SectionItem::Param(v) => v.description.push_str(desc),
            SectionItem::Return(v) => v.description.push_str(desc),
            SectionItem::Field(v) => v.description.push_str(desc),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct SectionRealm {
    _span: Span,
    realm: Realm,
}

#[derive(Debug, Clone)]
pub struct SectionBuilder<'s> {
    source: Source<'s>,
    default_realm: Realm,
    diagnostics: Vec<SectionDiagnostic>,

    /// Are we building a section? This begins as false and turns true when we find a header, then
    /// turns false again when we find a source code line meaning we have no more comments items
    /// (commented lines) to handle
    building: bool,
    span: Span,
    /// only contains descriptions, params and returns
    description: String,
    name: Option<item::Path>,
    class: Option<item::Class>,
    realm: Option<SectionRealm>,
    items: Vec<SectionItem>,
    tbls: Vec<item::Ident>,
    source_line: Option<Span>,
}

impl<'s> SectionBuilder<'s> {
    pub fn new(source: Source<'s>, default_realm: Realm) -> Self {
        Self {
            source,
            default_realm,
            diagnostics: Vec::new(),

            building: false,
            span: Span::default(),
            description: String::new(),
            name: Default::default(),
            class: Default::default(),
            realm: Default::default(),
            items: Vec::new(),
            tbls: Vec::new(),
            source_line: Default::default(),
        }
    }

    pub fn feed_item(&mut self, item: item::Item) -> SectionStatus {
        let (kind, span) = item.split();

        if !self.building {
            match kind {
                item::ItemKind::Header(_) => self.building = true,
                item::ItemKind::SourceCode | item::ItemKind::Description(_) => {
                    return SectionStatus::FeedMore
                }
                _ => {
                    self.diagnostics.push(SectionDiagnostic::new(
                        span,
                        "Found attribute before section header",
                    ));
                    return SectionStatus::FeedMore;
                }
            }
        }

        match kind {
            item::ItemKind::Header(desc) => self.handle_header(span, desc),
            item::ItemKind::Description(desc) => self.handle_description(span, desc),
            item::ItemKind::Name(kv) => self.handle_name(span, kv.split().1),
            item::ItemKind::Class(kv) => self.handle_class(span, kv.split().1),
            item::ItemKind::Libtbl(kv) => self.handle_libtbl(span, kv.split().1),
            item::ItemKind::Server(_) => self.handle_server(span),
            item::ItemKind::Client(_) => self.handle_client(span),
            item::ItemKind::Shared(_) => self.handle_shared(span),
            item::ItemKind::Field(kv) => self.handle_field(span, kv.split().1),
            item::ItemKind::Parameter(param) => self.handle_parameter(span, param.split().1),
            item::ItemKind::Return(ret) => self.handle_return(span, ret.split().1),
            item::ItemKind::Unknown(_) => self.handle_unknown(span),
            item::ItemKind::SourceCode => self.handle_source_code(span),
        }
    }

    pub fn drain_diagnostics(&mut self) -> impl Iterator<Item = SectionDiagnostic> + '_ {
        self.diagnostics.drain(..)
    }

    fn handle_header(&mut self, span: Span, desc: item::Text) -> SectionStatus {
        self.handle_description(span, desc)
    }

    fn handle_description(&mut self, _span: Span, desc: item::Text) -> SectionStatus {
        // If there are no params/returns then add this description to the item's main description
        let desc = self.source.lookup(&desc);
        match self.items.last_mut() {
            Some(item) => item.push_description(desc),
            None => self.push_description(desc),
        }
        SectionStatus::FeedMore
    }

    fn handle_name(&mut self, span: Span, path: item::Path) -> SectionStatus {
        match self.name {
            Some(_) => {
                self.handle_unknown(span);
                self.diagnostics.push(SectionDiagnostic::new(
                    span,
                    "Name was already defined in this section",
                ));
            }
            None => self.name = Some(path),
        }
        SectionStatus::FeedMore
    }

    fn handle_class(&mut self, span: Span, path: item::Class) -> SectionStatus {
        match self.class {
            Some(_) => {
                self.handle_unknown(span);
                self.diagnostics.push(SectionDiagnostic::new(
                    span,
                    "Class was already defined in this section",
                ));
            }
            None => self.class = Some(path),
        }
        SectionStatus::FeedMore
    }

    fn handle_libtbl(&mut self, _span: Span, name: item::Ident) -> SectionStatus {
        self.tbls.push(name);
        SectionStatus::FeedMore
    }

    fn handle_server(&mut self, span: Span) -> SectionStatus {
        self.set_realm(span, Realm::Server)
    }

    fn handle_client(&mut self, span: Span) -> SectionStatus {
        self.set_realm(span, Realm::Client)
    }

    fn handle_shared(&mut self, span: Span) -> SectionStatus {
        self.set_realm(span, Realm::Shared)
    }

    fn handle_field(&mut self, _span: Span, field: item::Field) -> SectionStatus {
        self.items.push(SectionItem::Field(SectionField {
            item: field,
            description: Default::default(),
        }));
        SectionStatus::FeedMore
    }

    fn handle_parameter(&mut self, _span: Span, param: item::Parameter) -> SectionStatus {
        self.items.push(SectionItem::Param(SectionParam {
            item: param,
            description: Default::default(),
        }));
        SectionStatus::FeedMore
    }

    fn handle_return(&mut self, _span: Span, ret: item::Return) -> SectionStatus {
        self.items.push(SectionItem::Return(SectionReturn {
            item: ret,
            description: Default::default(),
        }));
        SectionStatus::FeedMore
    }

    fn handle_unknown(&mut self, span: Span) -> SectionStatus {
        self.diagnostics
            .push(SectionDiagnostic::new(span, "Unknown item in this section"));
        SectionStatus::FeedMore
    }

    fn handle_source_code(&mut self, span: Span) -> SectionStatus {
        self.source_line = Some(span);
        self.finish_and_clear()
    }

    fn push_description(&mut self, desc: &str) {
        if !self.description.is_empty() {
            self.description.push('\n');
        }
        self.description.push_str(desc);
    }

    fn set_realm(&mut self, span: Span, realm: Realm) -> SectionStatus {
        match self.realm {
            Some(_) => {
                self.handle_unknown(span);
                self.diagnostics.push(SectionDiagnostic::new(
                    span,
                    "Realm was already defined in this section",
                ));
            }
            None => self.realm = Some(SectionRealm { _span: span, realm }),
        }
        SectionStatus::FeedMore
    }

    fn finish_and_clear(&mut self) -> SectionStatus {
        let class = self
            .class
            .as_ref()
            .map(item::Class::kind)
            .unwrap_or(item::ClassKind::Unknown);

        let status = match class {
            item::ClassKind::Hook => self.finish_hook(),
            item::ClassKind::Type => self.finish_type(),
            item::ClassKind::Table => self.finish_table(),
            item::ClassKind::Field => self.finish_field(),
            item::ClassKind::Library => self.finish_library(),
            item::ClassKind::Function => self.finish_function(),
            item::ClassKind::Unknown => self.finish_unknown(),
        };

        self.clear();
        status
    }

    fn finish_hook(&mut self) -> SectionStatus {
        let name = match self.name {
            Some(name) => self.source.lookup(&name).to_owned(),
            None => {
                self.handle_unknown(self.source_line.unwrap_or(Span::default()));
                self.diagnostics.push(SectionDiagnostic::new(
                    self.source_line.unwrap_or(Span::default()),
                    "Hook has no name",
                ));
                return SectionStatus::FeedMore;
            }
        };
        let (params, rets, _) = self.assemble_items();
        let realm = self.get_realm();

        SectionStatus::Complete(Section::Hook(HookSection {
            span: self.span,
            name,
            description: std::mem::take(&mut self.description),
            realm,
            parameters: params,
            returns: rets,
        }))
    }

    fn finish_type(&mut self) -> SectionStatus {
        let name = match self.name {
            Some(name) => self.source.lookup(&name).to_owned(),
            None => {
                self.handle_unknown(self.source_line.unwrap_or(Span::default()));
                self.diagnostics.push(SectionDiagnostic::new(
                    self.source_line.unwrap_or(Span::default()),
                    "Type has no name",
                ));
                return SectionStatus::FeedMore;
            }
        };

        SectionStatus::Complete(Section::Type(TypeSection {
            span: self.span,
            name,
            description: std::mem::take(&mut self.description),
            realm: self.get_realm(),
            tbls: self.assemble_tbls(),
        }))
    }

    fn finish_table(&mut self) -> SectionStatus {
        let name = match self.name {
            Some(name) => self.source.lookup(&name).to_owned(),
            None => {
                self.handle_unknown(self.source_line.unwrap_or(Span::default()));
                self.diagnostics.push(SectionDiagnostic::new(
                    self.source_line.unwrap_or(Span::default()),
                    "Table has no name",
                ));
                return SectionStatus::FeedMore;
            }
        };
        let (_, _, fields) = self.assemble_items();
        let tbls = self.assemble_tbls();

        if tbls.is_empty() {
            self.handle_unknown(self.source_line.unwrap_or(Span::default()));
            self.diagnostics.push(SectionDiagnostic::new(
                self.source_line.unwrap_or(Span::default()),
                "Table has no tbl",
            ));
            return SectionStatus::FeedMore;
        }

        if tbls.len() > 1 {
            self.handle_unknown(self.source_line.unwrap_or(Span::default()));
            self.diagnostics.push(SectionDiagnostic::new(
                self.source_line.unwrap_or(Span::default()),
                "Table has more than one tbl",
            ));
            return SectionStatus::FeedMore;
        }

        SectionStatus::Complete(Section::Table(TableSection {
            span: self.span,
            name,
            description: std::mem::take(&mut self.description),
            realm: self.get_realm(),
            fields,
            tbl: tbls.into_iter().next().unwrap(),
        }))
    }

    fn finish_field(&mut self) -> SectionStatus {
        let (parent, name) = match self.get_parent_tbl_and_name() {
            Ok((parent, name)) => (parent, name),
            Err(diag) => {
                self.diagnostics.push(diag);
                return SectionStatus::FeedMore;
            }
        };

        SectionStatus::Complete(Section::Field(FieldSection {
            span: self.span,
            name,
            description: std::mem::take(&mut self.description),
            tbl: parent,
        }))
    }

    fn finish_library(&mut self) -> SectionStatus {
        let name = match self.name {
            Some(name) => self.source.lookup(&name).to_owned(),
            None => {
                self.handle_unknown(self.source_line.unwrap_or(Span::default()));
                self.diagnostics.push(SectionDiagnostic::new(
                    self.source_line.unwrap_or(Span::default()),
                    "Library has no name",
                ));
                return SectionStatus::FeedMore;
            }
        };

        SectionStatus::Complete(Section::Library(LibrarySection {
            span: self.span,
            name,
            realm: self.get_realm(),
            description: std::mem::take(&mut self.description),
            tbls: self.assemble_tbls(),
        }))
    }

    fn finish_function(&mut self) -> SectionStatus {
        let (parent, name) = match self.get_parent_tbl_and_name() {
            Ok((parent, name)) => (parent, name),
            Err(diag) => {
                self.diagnostics.push(diag);
                return SectionStatus::FeedMore;
            }
        };
        let (params, rets, _) = self.assemble_items();

        SectionStatus::Complete(Section::Method(MethodSection {
            span: self.span,
            name,
            description: std::mem::take(&mut self.description),
            realm: self.get_realm(),
            parameters: params,
            returns: rets,
            deprecated: false,
            tbl: parent,
        }))
    }

    fn finish_unknown(&mut self) -> SectionStatus {
        self.finish_function()
    }

    fn clear(&mut self) {
        self.building = false;
        self.span = Default::default();
        self.description.clear();
        self.name = None;
        self.class = None;
        self.realm = None;
        self.items.clear();
        self.tbls.clear();
        self.source_line = None;
    }

    fn assemble_items(&mut self) -> (Vec<Parameter>, Vec<Return>, Vec<Field>) {
        let mut params = Vec::new();
        let mut rets = Vec::new();
        let mut fields = Vec::new();

        for item in self.items.drain(..) {
            match item {
                SectionItem::Param(param) => {
                    params.push(Parameter {
                        name: self.source.lookup(&param.item.name()).to_owned(),
                        description: param.description,
                        types: type_union_to_type_strings(&self.source, param.item.type_union()),
                        optional: param.item.type_union().optional(),
                    });
                }
                SectionItem::Return(ret) => rets.push(Return {
                    types: type_union_to_type_strings(&self.source, ret.item.type_union()),
                    description: ret.description,
                    optional: ret.item.type_union().optional(),
                }),
                SectionItem::Field(field) => {
                    fields.push(Field {
                        name: self.source.lookup(&field.item.name()).to_owned(),
                        description: field.description,
                    });
                }
            }
        }

        (params, rets, fields)
    }

    fn get_parent_tbl_and_name(&self) -> Result<(String, String), SectionDiagnostic> {
        match self.name {
            Some(path) => {
                let tbl = match path.table() {
                    Some(ident) => self.source.lookup(&ident).to_owned(),
                    None => String::new(),
                };
                let name = self.source.lookup(&path.name()).to_owned();
                Ok((tbl, name))
            }
            None => match self.source_line {
                Some(span) => {
                    let line = self.source.lookup(&span);
                    parse_source_line(span, line)
                }
                None => Err(SectionDiagnostic::new(
                    self.source_line.unwrap_or(Span::default()),
                    "No name",
                )),
            },
        }
    }

    fn assemble_tbls(&mut self) -> Vec<String> {
        let mut tbls = Vec::new();
        for tbl in self.tbls.drain(..) {
            tbls.push(self.source.lookup(&tbl).to_owned());
        }
        tbls
    }

    fn get_realm(&self) -> Realm {
        self.realm.map(|r| r.realm).unwrap_or(self.default_realm)
    }
}

fn type_union_to_type_strings(source: &Source, type_union: &item::TypeUnion) -> Vec<String> {
    let mut types = Vec::new();
    for ident in type_union.types() {
        types.push(source.lookup(ident).to_owned());
    }
    types
}

fn parse_source_line(span: Span, line: &str) -> Result<(String, String), SectionDiagnostic> {
    let x = line.trim().trim_start_matches("function").trim_start();
    let (tbl, func) = x
        .split_once(":")
        .or(x.split_once("."))
        .ok_or_else(|| SectionDiagnostic::new(span, "invalid function line"))?;

    let space = func.find(char::is_whitespace).unwrap_or(usize::MAX);
    let paren = func.find('(').unwrap_or(usize::MAX);
    let min = space.min(paren);
    if min == usize::MAX {
        return Err(SectionDiagnostic::new(span, "invalid function line"));
    }

    let func = &func[..min];
    Ok((tbl.to_owned(), func.to_owned()))
}

#[test]
fn test1() {
    let (tbl, name) =
        parse_source_line(Span::default(), "function bass_methods:getAverageBitRate()").unwrap();
    assert_eq!(tbl, "bass_methods");
    assert_eq!(name, "getAverageBitRate");
}

#[cfg(test)]
mod tests {
    use crate::{item, section::build_section, source::Source, Realm};

    use super::SectionDiagnosticEmitter;

    const EXAMPLE_FUNCTION_1: &str = r#"--- Used to select single values from a vararg or get the count of values in it.
-- @name builtins_library.select
-- @class function
-- @param any parameter
-- @param ... vararg Args to select from
-- @return any Returns a number or vararg, depending on the select method.
builtins_library.select = select
"#;

    const EXAMPLE_FUNCTION_2: &str = r#"--- Loads a sound channel from an URL.
-- @param string path URL path to play from.
-- @param string flags Flags for the sound (`3d`, `mono`, `noplay`, `noblock`). noblock will fail if the webserver doesn't provide file length.
-- @param function callback Function which is called when the sound channel is loaded. It'll get 3 arguments: `Bass` object, error number and name.
function bass_library.loadURL(path, flags, callback)
	checkpermission(instance, path, "bass.loadURL")
"#;

    struct TestDiagnosticEmitter;
    impl SectionDiagnosticEmitter for TestDiagnosticEmitter {
        fn emit(&mut self, diagnostic: super::SectionDiagnostic) {
            eprintln!("{diagnostic:?}")
        }
    }

    fn item_section_from_source(source: &str) -> item::Section {
        let source = Source::new(source);
        let section = item::ItemParser::new(source).next().unwrap();
        match section {
            Ok(item) => match item {
                item::Item2::Section(section) => section,
                _ => panic!("not a section"),
            },
            Err(e) => panic!("{e:?}"),
        }
    }

    #[test]
    fn build_example_function_1() {
        let source = Source::new(EXAMPLE_FUNCTION_1);
        let section = item_section_from_source(EXAMPLE_FUNCTION_1);
        let section =
            build_section(source, section, Realm::Shared, &mut TestDiagnosticEmitter).unwrap();
        let method = match section {
            super::Section::Method(method) => method,
            _ => panic!(),
        };

        assert_eq!(method.tbl, "builtins_library");
        assert_eq!(method.name, "select");
        assert_eq!(
            method.description,
            "Used to select single values from a vararg or get the count of values in it."
        );
        assert_eq!(method.realm, Realm::Shared);
        assert_eq!(method.parameters.len(), 2);
        assert_eq!(method.returns.len(), 1);
    }

    #[test]
    fn build_example_function_2() {
        let source = Source::new(EXAMPLE_FUNCTION_2);
        let section = item_section_from_source(EXAMPLE_FUNCTION_2);
        let section =
            build_section(source, section, Realm::Shared, &mut TestDiagnosticEmitter).unwrap();
        let method = match section {
            super::Section::Method(method) => method,
            _ => panic!(),
        };

        assert_eq!(method.tbl, "bass_library");
        assert_eq!(method.name, "loadURL");
        assert_eq!(method.description, "Loads a sound channel from an URL.");
        assert_eq!(method.realm, Realm::Shared);
        assert_eq!(method.parameters.len(), 3);
        assert_eq!(method.returns.len(), 0);
    }
}
