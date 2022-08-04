use std::{assert_matches::debug_assert_matches, collections::HashMap};

use crate::sfdoc::{Hook, Parameter, Return, Type};

use super::{tokenizer::*, Docs, Library, Realm};

use anyhow::Result;

macro_rules! attr_parse {
    ($class:expr, $item:expr, $frealm:expr, $scratch:expr, $($value:ident),*) => {
        {
            // Declare the variables that will hold the requests values
            $(
                let mut $value = attr_parse!(@ $value);
            )*

            // Have we seen a class attribute
            let mut seen_class = false;


            // Parse the attributes
            for attr in $item.attributes() {
                let kind = attr.kind();

                $(
                    attr_parse!(! $value, &mut $value, &kind);
                )*

                match kind {
                    AttributeKind::Class(c) => {
                        if seen_class {
                            anyhow::bail!("Duplicate class attribute");
                        }
                        if Class::from(*c) != $class {
                            anyhow::bail!("Class attribute does not match class");
                        }
                        seen_class = true;
                        continue;
                    }
                    AttributeKind::Libtbl(_) =>{
                        $scratch.push(*attr);
                        continue;
                    }
                    _ => {}
                }

                // If none of the parsers matched, then this is an invalid attribute
                anyhow::bail!("Invalid attribute for class {:?}: {:?}", $class, kind);
            }

            // Validate the parsed value
            $(
                let $value = attr_parse!(% $value, $frealm, $value);
            )*

            // Return the parsed and validated values
            ($($value),*)
        }
    };

    // Default value for the named values
    (@ name) => {Option::<&'s str>::None};
    (@ description) => {String::new()};
    (@ realm) => {Option::<Realm>::None};
    (@ parameters) => {Vec::<Parameter>::new()};
    (@ returns) => {Vec::<Return>::new()};

    // Parser for the named values
    (! name, $storage:expr, $kind:expr) => {
        match $kind {
            AttributeKind::Name(n) => {
                if $storage.is_some() {
                    anyhow::bail!("Duplicate name attribute");
                }
                *$storage = Some(*n);
                continue;
            }
            _ => {}
        }
    };
    (! description, $storage:expr, $kind:expr) => {
        match $kind {
            AttributeKind::DescriptionFragment(d) => {
                if !$storage.is_empty() {
                    $storage.push(' ');
                }
                $storage.push_str(d);
                continue;
            }
            _ => {}
        }
    };
    (! realm, $storage:expr, $kind:expr) => {
        match $kind {
            AttributeKind::Server |
            AttributeKind::Client |
            AttributeKind::Shared => {
                let r = attribute_kind_to_realm($kind);
                if $storage.is_some() {
                    anyhow::bail!("Duplicate realm attribute");
                }
                *$storage = Some(r);
                continue;
            }
            _ => {}
        }
    };
    (! parameters, $storage:expr, $kind:expr) => {
        match $kind {
            AttributeKind::Param { .. } => {
                let param = attribute_kind_to_parameter($kind)?;
                $storage.push(param);
                continue
            },
            _ => {}
        }
    };
    (! returns, $storage:expr, $kind:expr) => {
        match $kind {
            AttributeKind::Return { .. } => {
                let ret = attribute_kind_to_return($kind)?;
                $storage.push(ret);
                continue
            },
            _ => {}
        }
    };

    // Validator for the named values
    (% name, $frealm:expr, $storage:expr) => {
        match $storage {
            Some(v) => v,
            None => anyhow::bail!("Missing name"),
        }
    };
    (% description, $frealm:expr, $storage:expr) => {$storage};
    (% realm, $frealm:expr, $storage:expr) => {
        $storage.unwrap_or($frealm)
    };
    (% parameters, $frealm:expr, $storage:expr) => {$storage};
    (% returns, $frealm:expr, $storage:expr) => {$storage};
}

pub struct Parser<'s> {
    docs: &'s mut Docs,
    file_realm: Realm,
    tbl_to_lib: HashMap<&'s str, &'s str>,
    tbl_to_type: HashMap<&'s str, &'s str>,
    scratch_attrs: Vec<Attribute<'s>>,
}

impl<'s> Parser<'s> {
    pub fn new(docs: &'s mut Docs, file_realm: Realm) -> Self {
        Self {
            docs,
            file_realm,
            tbl_to_lib: HashMap::new(),
            tbl_to_type: Default::default(),
            scratch_attrs: Vec::with_capacity(8),
        }
    }

    pub fn parse_section(&mut self, section: Section<'_, 's>) -> Result<()> {
        match section.class() {
            Class::Hook => self.parse_hook(section),
            Class::Type => self.parse_type(section),
            Class::Table => todo!(),
            Class::Library => self.parse_library(section),
            Class::Function => self.parse_function(section),
            Class::NotSpecified => self.parse_not_specified(section),
            Class::Unknown(_) => {
                log::warn!("Unknown section class: {:?}", section.class());
                Ok(())
            }
        }
    }

    fn parse_hook(&mut self, section: Section<'_, 's>) -> Result<()> {
        let (name, description, realm, parameters, returns) = attr_parse!(
            Class::Hook,
            section,
            self.file_realm,
            &mut self.scratch_attrs,
            name,
            description,
            realm,
            parameters,
            returns
        );

        if self.docs.hooks.contains_key(name) {
            anyhow::bail!("Duplicate hook: {}", name);
        }

        let hook = Hook {
            name: name.to_string(),
            description,
            realm,
            parameters,
            returns,
        };

        self.docs.hooks.insert(name.to_string(), hook);

        Ok(())
    }

    fn parse_type(&mut self, section: Section<'_, 's>) -> Result<()> {
        debug_assert!(self.scratch_attrs.is_empty());
        debug_assert_matches!(section.class(), Class::Type);

        let (name, description, realm) = attr_parse!(
            Class::Type,
            section,
            self.file_realm,
            &mut self.scratch_attrs,
            name,
            description,
            realm
        );

        if self.docs.types.contains_key(name) {
            // TODO: emit error
            log::warn!("Type already exists: {}", name);
            return Ok(());
        }

        for attr in self.scratch_attrs.drain(..) {
            match attr.kind() {
                AttributeKind::Libtbl(tbl) => {
                    self.tbl_to_type.insert(tbl, name);
                }
                _ => unreachable!(),
            }
        }

        self.docs.types.insert(
            name.to_string(),
            Type {
                name: name.to_string(),
                description,
                realm,
                methods: Default::default(),
                meta_methods: Default::default(),
            },
        );

        Ok(())
    }

    fn parse_library(&mut self, section: Section<'_, 's>) -> Result<()> {
        debug_assert!(self.scratch_attrs.is_empty());
        debug_assert_matches!(section.class(), Class::Library);

        let (name, description, realm) = attr_parse!(
            Class::Library,
            section,
            self.file_realm,
            &mut self.scratch_attrs,
            name,
            description,
            realm
        );

        if self.docs.libraries.contains_key(name) {
            anyhow::bail!("Duplicate library: {}", name);
        }

        let library = Library {
            name: name.to_string(),
            description,
            realm,
            methods: Default::default(),
            tables: Default::default(),
        };

        for attr in self.scratch_attrs.drain(..) {
            match attr.kind() {
                AttributeKind::Libtbl(tbl) => {
                    self.tbl_to_lib.insert(tbl, name);
                }
                _ => unreachable!(),
            }
        }

        self.docs.libraries.insert(name.to_string(), library);

        Ok(())
    }

    fn parse_function(&mut self, section: Section<'_, 's>) -> Result<()> {
        debug_assert_matches!(section.class(), Class::Function);

        let (name, description, realm, parameters, returns) = attr_parse!(
            Class::Function,
            section,
            self.file_realm,
            &mut self.scratch_attrs,
            name,
            description,
            realm,
            parameters,
            returns
        );

        //if self.docs.functions.contains_key(name) {
        //    // TODO: emit warning
        //    log::warn!("Duplicate function: {}", name);
        //    anyhow::bail!("Duplicate function: {}", name);
        //}

        Ok(())
    }

    // not_specified should be a method of a type
    fn parse_not_specified(&mut self, section: Section) -> Result<()> {
        debug_assert_matches!(section.class(), Class::NotSpecified);
        todo!()
    }
}

fn attribute_kind_to_parameter(kind: &AttributeKind) -> Result<Parameter> {
    match kind {
        AttributeKind::Param { ty, name, desc } => Ok(Parameter {
            ty: ty.to_string(),
            name: name.to_string(),
            description: desc.to_string(),
            optional: false,
        }),
        _ => unreachable!(),
    }
}

fn attribute_kind_to_return(kind: &AttributeKind) -> Result<Return> {
    match kind {
        AttributeKind::Return { ty, desc } => Ok(Return {
            ty: ty.to_string(),
            description: desc.to_string(),
        }),
        _ => unreachable!(),
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

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn parse_type() {
        let mut docs = Docs::default();
        let mut parser = Parser::new(&mut docs, Realm::Server);
        let mut tokenizer = Tokenizer::new(EXAMPLE_VALID_CLASS_TYPE);
        let section = tokenizer.next_section().unwrap();
        parser.parse_type(section).unwrap();

        assert_eq!(docs.types.len(), 1);
        let ty = docs.types.get("Npc").unwrap();
        assert_eq!(ty.name, "Npc");
        assert_eq!(ty.description, "Npc type");
        assert_eq!(ty.realm, Realm::Server);
    }

    #[test]
    fn parse_hooks_valid() {
        let mut docs = Docs::default();
        let mut parser = Parser::new(&mut docs, Realm::Server);
        let mut tokenizer = Tokenizer::new(EXAMPLE_VALID_HOOKS);
        while let Some(section) = tokenizer.next_section() {
            parser.parse_section(section).unwrap();
        }

        assert_eq!(docs.hooks.len(), 3);
    }

    #[test]
    fn parse_library_valid() {
        let mut docs = Docs::default();
        let mut parser = Parser::new(&mut docs, Realm::Server);
        let mut tokenizer = Tokenizer::new(EXAMPLE_VALID_LIBRARY);
        while let Some(section) = tokenizer.next_section() {
            parser.parse_section(section).unwrap();
        }

        assert_eq!(docs.libraries.len(), 1);
        assert_eq!(docs.libraries.get("table").unwrap().name, "table");
    }
}
