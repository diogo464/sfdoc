use std::str::Split;

use anyhow::Result;

const TRIPLE: &str = "---";
const DOUBLE: &str = "--";

/// Class types that will decide how we parse the attributes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Class<'s> {
    Hook,
    Type,
    Table,
    Library,
    Function,
    NotSpecified,
    Unknown(&'s str),
}

impl<'s> From<&'s str> for Class<'s> {
    fn from(c: &'s str) -> Self {
        match c {
            "hook" => Class::Hook,
            "type" => Class::Type,
            "table" => Class::Table,
            "library" => Class::Library,
            "function" => Class::Function,
            _ => Class::Unknown(c),
        }
    }
}

/// Attributes are defined as comments in lua source code.
/// They begin after a triple dash comment `---` (the description)
/// and have the format `-- @key value`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttributeKind<'s> {
    Name(&'s str),
    Class(&'s str),
    Libtbl(&'s str),
    Server,
    Client,
    Shared,
    Param {
        ty: &'s str,
        name: &'s str,
        desc: &'s str,
    },
    Return {
        ty: &'s str,
        desc: &'s str,
    },
    DescriptionFragment(&'s str),
    Unknown {
        key: &'s str,
        value: &'s str,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Attribute<'s> {
    line: usize,
    kind: AttributeKind<'s>,
}

impl<'s> Attribute<'s> {
    fn new(line: usize, kind: AttributeKind<'s>) -> Self {
        Self { line, kind }
    }

    pub fn line_number(&self) -> usize {
        self.line
    }

    pub fn kind(&self) -> &AttributeKind<'s> {
        &self.kind
    }
}

/// This is everthing that should be required to parse a documented item.
pub struct Section<'p, 's> {
    start_line: usize,
    class: Class<'s>,
    attributes: &'p [Attribute<'s>],
    remain: &'s str,
}

impl<'p, 's> Section<'p, 's> {
    fn new(
        start_line: usize,
        class: Class<'s>,
        attributes: &'p [Attribute<'s>],
        remain: &'s str,
    ) -> Self {
        Self {
            start_line,
            class,
            attributes,
            remain,
        }
    }

    pub fn line_number(&self) -> usize {
        self.start_line
    }

    pub fn class(&self) -> Class {
        self.class
    }

    pub fn attributes(&self) -> &[Attribute<'s>] {
        self.attributes
    }

    pub fn next_line(&self) -> Option<&'s str> {
        self.remain.lines().next()
    }
}

pub struct Tokenizer<'s> {
    lines: Split<'s, char>,
    current_line: usize,

    // parse state
    attributes: Vec<Attribute<'s>>,
    spotted_class: Class<'s>,
}

impl<'s> Tokenizer<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            lines: source.split('\n'),
            current_line: 0,

            attributes: Vec::with_capacity(16),
            spotted_class: Class::NotSpecified,
        }
    }

    pub fn next_section(&mut self) -> Option<Section<'_, 's>> {
        while let Some(line) = self.peek_line() {
            let trimmed = line.trim();
            if !trimmed.starts_with(TRIPLE) {
                self.consume_line();
                continue;
            }

            // clear the state from parsing the previous item
            self.attributes.clear();
            self.spotted_class = Class::NotSpecified;

            let start_line = self.current_line;
            self.parse_opener();
            if self.parse_attributes() {
                return Some(Section::new(
                    start_line,
                    self.spotted_class,
                    &self.attributes,
                    self.remaining(),
                ));
            }
        }
        None
    }

    /// Parse the opener line.
    /// This line is just a description fragment staring with `---`.
    fn parse_opener(&mut self) {
        let line = self.expect_line_and_trim();
        let kind = parse_opener(line);
        let attr = Attribute::new(self.current_line - 1, kind);
        self.attributes.push(attr);
    }

    /// Parse the attributes. The class attribute is also used to set the value of `spotted_class`.
    ///
    /// Returns success/failure.
    fn parse_attributes(&mut self) -> bool {
        while let Some(line) = self.peek_line() {
            let trimmed = line.trim();
            if !trimmed.starts_with(DOUBLE) {
                break;
            }

            let lnumber = self.current_line;
            let line = self.expect_line_and_trim();
            let kind = parse_attribute_kind(line);

            match kind {
                Ok(attr) => {
                    if let AttributeKind::Class(class) = attr {
                        self.spotted_class = Class::from(class);
                    }
                    self.attributes.push(Attribute::new(lnumber, attr))
                }
                Err(_) => {
                    // consume any remaining attributes
                    while let Some(line) = self.peek_line() {
                        if !line.trim().starts_with(DOUBLE) {
                            break;
                        }
                        self.consume_line();
                    }
                    return false;
                }
            }
        }
        true
    }

    fn expect_line_and_trim(&mut self) -> &'s str {
        self.consume_line_and_trim()
            .expect("expected line while parsing")
    }

    fn consume_line_and_trim(&mut self) -> Option<&'s str> {
        self.consume_line().map(str::trim)
    }

    fn consume_line(&mut self) -> Option<&'s str> {
        match self.lines.next() {
            Some(line) => {
                self.current_line += 1;
                Some(line)
            }
            None => todo!(),
        }
    }

    fn peek_line(&self) -> Option<&'s str> {
        self.lines.clone().next()
    }

    fn remaining(&self) -> &'s str {
        self.lines.as_str()
    }
}

fn parse_opener<'s>(trimmed_line: &'s str) -> AttributeKind<'s> {
    debug_assert!(trimmed_line.starts_with(TRIPLE));
    let desc = trimmed_line.split_once(TRIPLE).unwrap().1.trim();
    AttributeKind::DescriptionFragment(desc)
}

// parse the attribute, if is not valid, signal an error
fn parse_attribute_kind<'s>(trimmed_line: &'s str) -> Result<AttributeKind<'s>, ()> {
    debug_assert!(trimmed_line.starts_with(DOUBLE));

    // key_val should look something like `@name Npc`
    let key_val = trimmed_line.trim_start_matches(DOUBLE).trim_start();
    let (key_at, value_untrimmed) = key_val.split_once(' ').unwrap_or((key_val, ""));
    let value = value_untrimmed.trim();
    if !key_at.starts_with("@") {
        return Ok(AttributeKind::DescriptionFragment(value));
    }
    let key = key_at.trim_start_matches('@');
    match key {
        "name" => {
            if !value.is_empty() {
                Ok(AttributeKind::Name(value))
            } else {
                Err(())
            }
        }
        "class" => {
            if !value.is_empty() {
                Ok(AttributeKind::Class(value))
            } else {
                Err(())
            }
        }
        "libtbl" => {
            if !value.is_empty() {
                Ok(AttributeKind::Libtbl(value))
            } else {
                Err(())
            }
        }
        "server" => Ok(AttributeKind::Server),
        "client" => Ok(AttributeKind::Client),
        "shared" => Ok(AttributeKind::Shared),
        "param" => {
            let mut split = value.split_ascii_whitespace();
            let ty = split.next().ok_or(())?;
            let name = split.next().unwrap_or_default();
            let desc = split.as_str();
            Ok(AttributeKind::Param { ty, name, desc })
        }
        "return" => {
            let mut split = value.split_ascii_whitespace();
            let ty = split.next().ok_or(())?;
            let desc = split.as_str();
            Ok(AttributeKind::Return { ty, desc })
        }
        _ => Ok(AttributeKind::Unknown { key, value }),
    }
}

#[cfg(test)]
mod tests {
    use super::{Attribute, AttributeKind, Class, Tokenizer};

    #[test]
    fn opener_valid() {
        let line = "--- This is a description";
        let attr = super::parse_opener(line);
        assert_eq!(
            attr,
            AttributeKind::DescriptionFragment("This is a description")
        );
    }

    #[test]
    fn opener_no_space() {
        let line = "---This is a description";
        let attr = super::parse_opener(line);
        assert_eq!(
            attr,
            AttributeKind::DescriptionFragment("This is a description")
        );
    }

    #[test]
    fn opener_empty() {
        let line = "---";
        let attr = super::parse_opener(line);
        assert_eq!(attr, AttributeKind::DescriptionFragment(""));
    }

    #[test]
    fn description_empty_with_space() {
        let line = "--- ";
        let attr = super::parse_opener(line);
        assert_eq!(attr, AttributeKind::DescriptionFragment(""));
    }

    #[test]
    fn attribute_name_valid() {
        let line = "-- @name Npc";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Ok(AttributeKind::Name("Npc")));
    }

    #[test]
    fn attribute_name_empty() {
        let line = "-- @name";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Err(()));
    }

    #[test]
    fn attribute_name_empty_with_space() {
        let line = "-- @name ";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Err(()));
    }

    #[test]
    fn attribute_class_valid() {
        let line = "-- @class Npc";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Ok(AttributeKind::Class("Npc")));
    }

    #[test]
    fn attribute_class_empty() {
        let line = "-- @class";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Err(()));
    }

    #[test]
    fn attribute_class_empty_with_space() {
        let line = "-- @class ";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Err(()));
    }

    #[test]
    fn attribute_libtbl_valid() {
        let line = "-- @libtbl npc_methods";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Ok(AttributeKind::Libtbl("npc_methods")));
    }

    #[test]
    fn attribute_libtbl_empty() {
        let line = "-- @libtbl";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Err(()));
    }

    #[test]
    fn parse_client_valid() {
        let line = "-- @client";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Ok(AttributeKind::Client));
    }

    #[test]
    fn parse_server_valid() {
        let line = "-- @server";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Ok(AttributeKind::Server));
    }

    #[test]
    fn parse_shared_valid() {
        let line = "-- @shared";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Ok(AttributeKind::Shared));
    }

    #[test]
    fn parse_param_valid() {
        let line = "-- @param int foo description";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(
            attr,
            Ok(AttributeKind::Param {
                ty: "int",
                name: "foo",
                desc: "description"
            })
        );
    }

    #[test]
    fn parse_param_only_type() {
        let line = "-- @param int";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(
            attr,
            Ok(AttributeKind::Param {
                ty: "int",
                name: "",
                desc: ""
            })
        );
    }

    #[test]
    fn parse_param_invalid() {
        let line = "-- @param";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Err(()));
    }

    #[test]
    fn parse_return_valid() {
        let line = "-- @return int description";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(
            attr,
            Ok(AttributeKind::Return {
                ty: "int",
                desc: "description"
            })
        );
    }

    #[test]
    fn parse_return_only_type() {
        let line = "-- @return int";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(
            attr,
            Ok(AttributeKind::Return {
                ty: "int",
                desc: ""
            })
        );
    }

    #[test]
    fn parse_return_invalid() {
        let line = "-- @return";
        let attr = super::parse_attribute_kind(line);
        assert_eq!(attr, Err(()));
    }

    #[test]
    fn parse_item_valid() {
        let testing_code = r#"
--- Npc type
-- @name Npc
-- @class type
-- @libtbl npc_methods
-- @libtbl npc_meta
SF.RegisterType("Npc", false, true, debug.getregistry().NPC, "Entity")

return function(instance)
local checkpermission = instance.player ~= SF.Superuser and SF.Permissions.check or function() end

if SERVER then
	--- Adds a relationship to the npc
	-- @server
	-- @param string str The relationship string. http://wiki.facepunch.com/gmod/NPC:AddRelationship
	function npc_methods:addRelationship(str)
		local npc = getnpc(self)
		[D_NU] = "neutral",
	}

	--- Adds a relationship to the npc with an entity
	-- @server
	-- @param Entity ent The target entity
	-- @param string disp String of the relationship. ("hate", "fear", "like", "neutral")
	-- @param number priority How strong the relationship is. Higher number is stronger
	function npc_methods:addEntityRelationship(ent, disp, priority)
		local npc = getnpc(self)
"#;

        let mut parser = Tokenizer::new(testing_code);

        let item = parser.next_section().unwrap();
        assert_eq!(item.start_line, 1);
        assert_eq!(item.class, Class::Type);
        assert_eq!(
            item.attributes,
            &[
                Attribute::new(1, AttributeKind::DescriptionFragment("Npc type")),
                Attribute::new(2, AttributeKind::Name("Npc")),
                Attribute::new(3, AttributeKind::Class("type")),
                Attribute::new(4, AttributeKind::Libtbl("npc_methods")),
                Attribute::new(5, AttributeKind::Libtbl("npc_meta")),
            ]
        );
    }

    #[test]
    fn parse_hooks_valid() {
        let testing_code = r#"

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

        let mut parser = Tokenizer::new(testing_code);

        let item = parser.next_section().unwrap();
        assert_eq!(item.start_line, 4);
        assert_eq!(item.class, Class::Hook);

        let item = parser.next_section().unwrap();
        assert_eq!(item.start_line, 10);
        assert_eq!(item.class, Class::Hook);

        let item = parser.next_section().unwrap();
        assert_eq!(item.start_line, 17);
        assert_eq!(item.class, Class::Hook);
    }
}
