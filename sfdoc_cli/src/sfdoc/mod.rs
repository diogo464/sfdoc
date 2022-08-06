mod attribute;
mod builder;
mod diagnostic;
mod section;
mod source;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

use self::builder::DocBuilder;

pub use self::builder::{Diagnostic, DiagnosticLevel, LuaFile};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Realm {
    Client,
    Server,
    Shared,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Docs {
    pub libraries: HashMap<String, Library>,
    pub hooks: HashMap<String, Hook>,
    pub types: HashMap<String, Type>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Library {
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub tables: HashMap<String, Table>,
    pub methods: HashMap<String, Method>,
    pub fields: HashMap<String, Field>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hook {
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub parameters: Vec<Parameter>,
    pub returns: Vec<Return>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Type {
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub methods: HashMap<String, Method>,
    pub meta_methods: HashMap<String, Method>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Table {
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub fields: HashMap<String, Field>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Method {
    pub name: String,
    pub description: String,
    pub realm: Realm,
    pub parameters: Vec<Parameter>,
    pub returns: Vec<Return>,
    pub deprecated: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub description: String,
    #[serde(rename = "type")]
    pub ty: String,
    pub optional: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Return {
    #[serde(rename = "type")]
    pub ty: String,
    pub description: String,
    pub optional: bool,
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

#[cfg(test)]
mod tests {
    //use super::*;

    const EXAMPLE_VALID_CLASS_TYPE: &str = r#"
--- Npc type
-- @name Npc
-- @class type
-- @libtbl npc_methods
-- @libtbl npc_meta
SF.RegisterType("Npc", false, true, debug.getregistry().NPC, "Entity")

return function(instance)
"#;

    const EXAMPLE_VALID_HOOKS: &str = r#"

-- ------------------------- Hook Documentation ------------------------- --

--- Called when an input on a wired SF chip is written to
-- @name input
-- @class hook
-- @param string input The input name
-- @param any value The value of the input

--- Called when a high speed device reads from a wired SF chip
-- @name readcell
-- @class hook
-- @server
-- @param any address The address requested
-- @return any The value read

--- Called when a high speed device writes to a wired SF chip
-- @name writecell
-- @class hook
-- @param any address The address written to
-- @param table data The data being written

end
"#;

    const EXAMPLE_VALID_LIBRARY: &str = r#"

--- Lua table library https://wiki.garrysmod.com/page/Category:table
-- @name table
-- @class library
-- @libtbl table_library
SF.RegisterLibrary("table")

return function(instance)

local table_library = instance.Libraries.table

"#;

    //#[test]
    //fn parse_type() {
    //    let docs = parse(EXAMPLE_VALID_CLASS_TYPE).unwrap();
    //    assert_eq!(docs.types.len(), 1);
    //    let ty = docs.types.get("Npc").unwrap();
    //    assert_eq!(ty.name, "Npc");
    //    assert_eq!(ty.description, "Npc type");
    //    assert_eq!(ty.realm, Realm::Shared);
    //}

    //#[test]
    //fn parse_hooks_valid() {
    //    let docs = parse(EXAMPLE_VALID_HOOKS).unwrap();
    //    assert_eq!(docs.hooks.len(), 3);
    //}

    //#[test]
    //fn parse_library_valid() {
    //    let docs = parse(EXAMPLE_VALID_LIBRARY).unwrap();
    //    assert_eq!(docs.libraries.len(), 1);
    //    assert_eq!(docs.libraries.get("table").unwrap().name, "table");
    //}
}
