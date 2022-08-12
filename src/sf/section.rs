use std::cell::RefCell;

use super::{
    item,
    source::{Source, Span, Spanned},
    Field, Parameter, Realm, Return,
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
            if let item::Attribute::Class(c) = attr {
                if class_kind.is_some() {
                    self.emit_diagnostic("multiple class attributes");
                }
                class_kind = Some(c.value().kind());
            }
        }
        class_kind
    }

    fn get_path(&self) -> Option<&item::Path> {
        let mut name = None;
        for attr in self.section.attributes() {
            if let item::Attribute::Name(n) = attr {
                if name.is_some() {
                    self.emit_diagnostic("multiple name attributes");
                }
                name = Some(n.value());
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
        let mut desc = String::new();
        for span in description.spans() {
            if !desc.is_empty() {
                desc.push('\n');
            }
            desc.push_str(self.source.lookup(&span));
        }
        desc
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
        let (tbl, func) = x.split_once(':').or_else(|| x.split_once('.'))?;

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
        item::Attribute::Server(_) => Realm::Server,
        item::Attribute::Client(_) => Realm::Client,
        item::Attribute::Shared(_) => Realm::Shared,
        _ => unreachable!(),
    }
}

pub fn build_section(
    source: Source,
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

#[cfg(test)]
mod tests {
    use super::super::{item, section::build_section, source::Source, Realm};

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
