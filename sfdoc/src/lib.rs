#![feature(never_type)]
#![feature(anonymous_lifetime_in_impl_trait)]

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use builder::DocBuilder;
use serde::{Deserialize, Serialize};

#[macro_use]
mod source;

mod builder;
mod diagnostic;
pub use diagnostic::*;
mod item;
mod section;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Realm {
    Client,
    Server,
    Shared,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DocLocation {
    path: PathBuf,
    line: u32,
    column: u32,
}

impl DocLocation {
    pub fn new(path: PathBuf, line: u32, column: u32) -> Self {
        Self { path, line, column }
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Docs {
    libraries: HashMap<String, Library>,
    hooks: HashMap<String, Hook>,
    types: HashMap<String, Type>,
}

impl Docs {
    pub fn libraries(&self) -> impl Iterator<Item = &Library> {
        self.libraries.values()
    }

    pub fn get_library(&self, name: impl AsRef<str>) -> Option<&Library> {
        self.libraries.get(name.as_ref())
    }

    pub fn hooks(&self) -> impl Iterator<Item = &Hook> {
        self.hooks.values()
    }

    pub fn get_hook(&self, name: impl AsRef<str>) -> Option<&Hook> {
        self.hooks.get(name.as_ref())
    }

    pub fn types(&self) -> impl Iterator<Item = &Type> {
        self.types.values()
    }

    pub fn get_type(&self, name: impl AsRef<str>) -> Option<&Type> {
        self.types.get(name.as_ref())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Library {
    name: String,
    description: String,
    realm: Realm,
    tables: HashMap<String, Table>,
    methods: HashMap<String, Method>,
    fields: HashMap<String, Field>,
}

impl Library {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn description(&self) -> &str {
        self.description.as_ref()
    }

    pub fn realm(&self) -> Realm {
        self.realm
    }

    pub fn tables(&self) -> impl Iterator<Item = &Table> {
        self.tables.values()
    }

    pub fn get_table(&self, name: impl AsRef<str>) -> Option<&Table> {
        self.tables.get(name.as_ref())
    }

    pub fn methods(&self) -> impl Iterator<Item = &Method> {
        self.methods.values()
    }

    pub fn get_method(&self, name: impl AsRef<str>) -> Option<&Method> {
        self.methods.get(name.as_ref())
    }

    pub fn fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.values()
    }

    pub fn get_field(&self, name: impl AsRef<str>) -> Option<&Field> {
        self.fields.get(name.as_ref())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hook {
    name: String,
    description: String,
    realm: Realm,
    parameters: Vec<Parameter>,
    returns: Vec<Return>,
}

impl Hook {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn description(&self) -> &str {
        self.description.as_ref()
    }

    pub fn realm(&self) -> Realm {
        self.realm
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }

    pub fn returns(&self) -> &[Return] {
        &self.returns
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Type {
    name: String,
    description: String,
    realm: Realm,
    methods: HashMap<String, Method>,
    meta_methods: HashMap<String, Method>,
}

impl Type {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
    pub fn description(&self) -> &str {
        self.description.as_ref()
    }
    pub fn realm(&self) -> Realm {
        self.realm
    }
    pub fn methods(&self) -> impl Iterator<Item = &Method> {
        self.methods.values()
    }
    pub fn get_method(&self, name: impl AsRef<str>) -> Option<&Method> {
        self.methods.get(name.as_ref())
    }
    pub fn meta_methods(&self) -> impl Iterator<Item = &Method> {
        self.meta_methods.values()
    }
    pub fn get_meta_method(&self, name: impl AsRef<str>) -> Option<&Method> {
        self.meta_methods.get(name.as_ref())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Table {
    name: String,
    description: String,
    realm: Realm,
    fields: HashMap<String, Field>,
}

impl Table {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
    pub fn description(&self) -> &str {
        self.description.as_ref()
    }
    pub fn realm(&self) -> Realm {
        self.realm
    }
    pub fn fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.values()
    }
    pub fn get_field(&self, name: impl AsRef<str>) -> Option<&Field> {
        self.fields.get(name.as_ref())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    name: String,
    description: String,
}

impl Field {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
    pub fn description(&self) -> &str {
        self.description.as_ref()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Method {
    name: String,
    description: String,
    realm: Realm,
    parameters: Vec<Parameter>,
    returns: Vec<Return>,
    deprecated: bool,
}

impl Method {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
    pub fn description(&self) -> &str {
        self.description.as_ref()
    }
    pub fn realm(&self) -> Realm {
        self.realm
    }
    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }
    pub fn returns(&self) -> &[Return] {
        &self.returns
    }
    pub fn deprecated(&self) -> bool {
        self.deprecated
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Parameter {
    name: String,
    description: String,
    types: Vec<String>,
    optional: bool,
}

impl Parameter {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
    pub fn description(&self) -> &str {
        self.description.as_ref()
    }
    pub fn types(&self) -> Types {
        Types(&self.types)
    }
    pub fn optional(&self) -> bool {
        self.optional
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Return {
    types: Vec<String>,
    description: String,
    optional: bool,
}

impl Return {
    pub fn types(&self) -> Types {
        Types(&self.types)
    }
    pub fn description(&self) -> &str {
        self.description.as_ref()
    }
    pub fn optional(&self) -> bool {
        self.optional
    }
}

#[derive(Debug)]
pub struct Types<'a>(&'a [String]);

impl<'a> Types<'a> {
    pub fn new(types: &'a [String]) -> Self {
        Types(types)
    }
}

impl<'a> std::fmt::Display for Types<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pipe = false;
        for t in self.0 {
            if pipe {
                write!(f, "|")?;
            }
            write!(f, "{}", t)?;
            pipe = true;
        }
        Ok(())
    }
}

impl<'a> IntoIterator for Types<'a> {
    type Item = &'a str;
    type IntoIter = TypesIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TypesIter(self.0.iter())
    }
}

pub struct TypesIter<'a>(std::slice::Iter<'a, String>);

impl<'a> Iterator for TypesIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|s| s.as_ref())
    }
}

pub fn document(files: impl Iterator<Item = LuaFile<'_>>) -> (Docs, Vec<Diagnostic>) {
    let mut builder = DocBuilder::default();
    for file in files {
        builder.parse_file(file);
    }
    builder.finish()
}

pub fn document_paths(
    paths: impl IntoIterator<Item = impl AsRef<Path>>,
) -> std::io::Result<(Docs, Vec<Diagnostic>)> {
    let mut files = Vec::new();
    for path in paths {
        let path = path.as_ref();
        path_to_vec(path, &mut files);
    }
    document_files(files)
}

fn document_files(file_paths: Vec<PathBuf>) -> std::io::Result<(Docs, Vec<Diagnostic>)> {
    let mut files = Vec::new();
    for path in file_paths {
        let content = std::fs::read_to_string(&path).unwrap();
        files.push((path, content));
    }
    Ok(document(
        files
            .iter()
            .map(|(path, source)| LuaFile::new(path, source)),
    ))
}

fn path_to_vec(path: &Path, vec: &mut Vec<PathBuf>) {
    if path.is_file() {
        vec.push(path.canonicalize().expect("failed to canonicalize path"));
    } else if path.is_dir() {
        for entry in path.read_dir().expect("failed to read dir") {
            let entry = entry.expect("failed to read entry");
            let entry_path = entry.path();
            path_to_vec(&entry_path, vec);
        }
    } else {
        log::warn!("{} is not a file or dir", path.display());
    }
}
