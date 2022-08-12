use std::io::{Result, Write};

use crate::sf::{self, Library, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LibraryKind {
    Normal,
    Prelude,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum MethodKind<'s> {
    Table(&'s str),
    Type(&'s str),
    Standalone,
}

struct MetaHeader;

impl std::fmt::Display for MetaHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "---@meta")
    }
}

enum DescriptionKind {
    Inline,
    Newline,
    Return,
}

struct Description<'s> {
    kind: DescriptionKind,
    description: &'s str,
}

impl<'s> Description<'s> {
    fn new(kind: DescriptionKind, description: &'s str) -> Self {
        Self { kind, description }
    }
}

impl<'s> std::fmt::Display for Description<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc = self.description.trim();
        if desc.is_empty() {
            return Ok(());
        }

        let mut newline = false;
        match self.kind {
            DescriptionKind::Inline => write!(f, " ")?,
            DescriptionKind::Newline => write!(f, "--- ")?,
            DescriptionKind::Return => write!(f, " # ")?,
        }
        for line in desc.lines() {
            if newline {
                write!(f, "\n--- ")?;
            }
            newline = true;
            write!(f, "{}", line)?;
        }
        Ok(())
    }
}

struct Realm(sf::Realm);

impl Realm {
    fn new(realm: sf::Realm) -> Self {
        Self(realm)
    }
}

impl std::fmt::Display for Realm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "--- Realm: ")?;
        match self.0 {
            sf::Realm::Client => write!(f, "client"),
            sf::Realm::Server => write!(f, "server"),
            sf::Realm::Shared => write!(f, "shared"),
        }
    }
}

struct Method<'s> {
    kind: MethodKind<'s>,
    method: &'s sf::Method,
}

impl<'s> Method<'s> {
    pub fn new(kind: MethodKind<'s>, method: &'s sf::Method) -> Self {
        Self { kind, method }
    }
}

impl<'s> std::fmt::Display for Method<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}",
            Description::new(DescriptionKind::Newline, self.method.description())
        )?;
        writeln!(f, "{}", Realm(self.method.realm()))?;
        for param in self.method.parameters() {
            writeln!(f, "{}", Parameter::from(param))?;
        }
        for ret in self.method.returns() {
            writeln!(f, "{}", Return::from(ret))?;
        }
        writeln!(
            f,
            "{}",
            FunctionDecl::new(
                self.kind,
                self.method.name(),
                self.method.parameters().iter().map(|p| p.name())
            )
        )?;
        Ok(())
    }
}

struct Parameter<'s, T> {
    name: &'s str,
    optional: bool,
    types: T,
    description: &'s str,
}

impl<'s, T: std::fmt::Display + 's> Parameter<'s, T> {
    fn new(name: &'s str, optional: bool, types: T, description: &'s str) -> Self {
        Self {
            name,
            optional,
            types,
            description,
        }
    }
}

impl<'s> From<&'s sf::Parameter> for Parameter<'s, sf::Types<'s>> {
    fn from(p: &'s sf::Parameter) -> Self {
        Self::new(p.name(), p.optional(), p.types(), p.description())
    }
}

impl<'s, T: std::fmt::Display + 's> std::fmt::Display for Parameter<'s, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "---@param {}", ParameterName::new(self.name))?;
        if self.optional {
            write!(f, "?")?;
        }
        write!(f, " {}", self.types)?;
        write!(
            f,
            "{}",
            Description::new(DescriptionKind::Inline, self.description)
        )?;
        Ok(())
    }
}

struct ParameterName<'s>(&'s str);

impl<'s> ParameterName<'s> {
    fn new(name: &'s str) -> Self {
        Self(name)
    }
}

impl<'s> std::fmt::Display for ParameterName<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if is_reserved_keyword(self.0) {
            write!(f, "_")?;
        }
        Ok(())
    }
}

struct Return<'s, T: std::fmt::Display + 's> {
    types: T,
    description: &'s str,
}

impl<'s, T: std::fmt::Display + 's> Return<'s, T> {
    fn new(types: T, description: &'s str) -> Self {
        Self { types, description }
    }
}

impl<'s> From<&'s sf::Return> for Return<'s, sf::Types<'s>> {
    fn from(r: &'s sf::Return) -> Self {
        Self::new(r.types(), r.description())
    }
}

impl<'s, T: std::fmt::Display + 's> std::fmt::Display for Return<'s, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "---@return {}", self.types)?;
        write!(
            f,
            "{}",
            Description::new(DescriptionKind::Return, self.description)
        )?;
        Ok(())
    }
}

struct FunctionDecl<'s, I> {
    kind: MethodKind<'s>,
    name: &'s str,
    params: I,
}

impl<'s, I: Iterator<Item = &'s str> + Clone> FunctionDecl<'s, I> {
    fn new(kind: MethodKind<'s>, name: &'s str, params: I) -> Self {
        Self { kind, name, params }
    }
}

impl<'s, I: Iterator<Item = &'s str> + Clone> std::fmt::Display for FunctionDecl<'s, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function ")?;
        match self.kind {
            MethodKind::Table(n) => write!(f, "{}.{}", n, self.name)?,
            MethodKind::Type(n) => write!(f, "{}:{}", n, self.name)?,
            MethodKind::Standalone => write!(f, "{}", self.name)?,
        }
        write!(f, "(")?;
        for (i, param) in self.params.clone().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ParameterName::new(param))?;
        }
        write!(f, ") end")?;
        Ok(())
    }
}

struct Class<'s>(&'s str);

impl<'s> Class<'s> {
    fn new(class: &'s str) -> Self {
        Self(class)
    }
}

impl<'s> std::fmt::Display for Class<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "---@class {}", self.0)
    }
}

enum OperationKind {
    UnaryMinus,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl std::fmt::Display for OperationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperationKind::UnaryMinus => write!(f, "unm"),
            OperationKind::Add => write!(f, "add"),
            OperationKind::Subtract => write!(f, "sub"),
            OperationKind::Multiply => write!(f, "mul"),
            OperationKind::Divide => write!(f, "div"),
        }
    }
}

struct Operation<'s> {
    kind: OperationKind,
    input: Option<&'s str>,
    output: &'s str,
}

impl<'s> Operation<'s> {
    fn new(kind: OperationKind, input: Option<&'s str>, output: &'s str) -> Self {
        Self {
            kind,
            input,
            output,
        }
    }
}

impl<'s> std::fmt::Display for Operation<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "---@operator {}", self.kind)?;
        if let Some(input) = self.input {
            write!(f, "({})", input)?;
        }
        write!(f, ":{}", self.output)?;
        Ok(())
    }
}

pub fn generate_lib(writer: &mut impl Write, library: &Library, kind: LibraryKind) -> Result<()> {
    writeln!(writer, "{}", MetaHeader)?;
    let method_kind = match kind {
        LibraryKind::Normal => {
            writeln!(
                writer,
                "{}",
                Description::new(DescriptionKind::Newline, library.description())
            )?;
            writeln!(writer, "{}", Realm::new(library.realm()))?;
            writeln!(writer, "{} = {{}}", library.name())?;
            writeln!(writer)?;
            MethodKind::Table(library.name())
        }
        LibraryKind::Prelude => MethodKind::Standalone,
    };

    for method in library.methods() {
        writeln!(writer, "{}", Method::new(method_kind, method))?;
        writeln!(writer)?;
    }

    Ok(())
}

pub fn generate_ty(writer: &mut impl Write, ty: &Type) -> Result<()> {
    writeln!(writer, "{}", MetaHeader)?;
    writeln!(
        writer,
        "{}",
        Description::new(DescriptionKind::Newline, ty.description())
    )?;
    writeln!(writer, "{}", Realm::new(ty.realm()))?;
    write!(writer, "{}", Class::new(ty.name()))?;
    if let Some(index) = ty.get_meta_method("__index") {
        let in_ty = index.parameters()[0].types();
        let out_ty = index.returns()[0].types();
        write!(writer, ": {{ [{}]: {} }}", in_ty, out_ty)?;
    }
    writeln!(writer)?;

    for method in ty.meta_methods() {
        let input_type = method.parameters().get(0).map(|p| p.types().to_string());
        let output_type = method.returns().get(0).map(|p| p.types().to_string());

        let input_type = input_type.as_deref();
        let output_type = match output_type.as_deref() {
            Some(output_type) => output_type,
            None => {
                // TODO: handle this
                continue;
            }
        };

        let operation = match method.name() {
            "__add" => Operation::new(OperationKind::Add, input_type, output_type),
            "__div" => Operation::new(OperationKind::Divide, input_type, output_type),
            "__mul" => Operation::new(OperationKind::Multiply, input_type, output_type),
            "__unm" => Operation::new(OperationKind::UnaryMinus, None, output_type),
            "__sub" => Operation::new(OperationKind::Subtract, input_type, output_type),
            _ => continue,
        };
        writeln!(writer, "{}", operation)?;
    }
    writeln!(writer, "{} = {{}}", ty.name())?;
    writeln!(writer)?;

    for method in ty.methods() {
        let method_kind = MethodKind::Type(ty.name());
        writeln!(writer, "{}", Method::new(method_kind, method))?;
        writeln!(writer)?;
    }

    Ok(())
}

fn is_reserved_keyword(v: &str) -> bool {
    std::matches!(
        v,
        "and"
            | "break"
            | "do"
            | "else"
            | "elseif"
            | "end"
            | "false"
            | "for"
            | "function"
            | "if"
            | "in"
            | "local"
            | "nil"
            | "not"
            | "or"
            | "repeat"
            | "return"
            | "then"
            | "true"
            | "until"
            | "while"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_metaheader() {
        assert_eq!(MetaHeader.to_string(), "---@meta");
    }

    #[test]
    fn display_description() {
        assert_eq!(
            Description::new(DescriptionKind::Newline, "foo").to_string(),
            "--- foo"
        );
        assert_eq!(
            Description::new(DescriptionKind::Newline, "foo\nbar").to_string(),
            "--- foo\n--- bar"
        );
        assert_eq!(
            Description::new(DescriptionKind::Newline, "foo\nbar\nbaz").to_string(),
            "--- foo\n--- bar\n--- baz"
        );

        assert_eq!(
            Description::new(DescriptionKind::Inline, "foo").to_string(),
            " foo"
        );
        assert_eq!(
            Description::new(DescriptionKind::Inline, "foo\nbar").to_string(),
            " foo\n--- bar"
        );
        assert_eq!(
            Description::new(DescriptionKind::Inline, "foo\nbar\nbaz").to_string(),
            " foo\n--- bar\n--- baz"
        );

        assert_eq!(
            Description::new(DescriptionKind::Return, "foo").to_string(),
            " # foo"
        );
        assert_eq!(
            Description::new(DescriptionKind::Return, "foo\nbar").to_string(),
            " # foo\n--- bar"
        );
        assert_eq!(
            Description::new(DescriptionKind::Return, "foo\nbar\nbaz").to_string(),
            " # foo\n--- bar\n--- baz"
        );
    }

    #[test]
    fn display_realm() {
        assert_eq!(
            Realm::new(sf::Realm::Server).to_string(),
            "--- Realm: server"
        );
        assert_eq!(
            Realm::new(sf::Realm::Client).to_string(),
            "--- Realm: client"
        );
        assert_eq!(
            Realm::new(sf::Realm::Shared).to_string(),
            "--- Realm: shared"
        );
    }

    #[test]
    fn display_param() {
        assert_eq!(
            Parameter::new("foo", false, &"number", "foo",).to_string(),
            "---@param foo number foo"
        );

        assert_eq!(
            Parameter::new("foo", true, &"number", "").to_string(),
            "---@param foo? number"
        );

        assert_eq!(
            Parameter::new("foo", true, &"number", "",).to_string(),
            "---@param foo? number"
        );

        assert_eq!(
            Parameter::new("foo", true, &"number", "foo\nbar",).to_string(),
            "---@param foo? number foo\n--- bar"
        );

        assert_eq!(
            Parameter::new("end", true, &"number", "foo\nbar",).to_string(),
            "---@param end_? number foo\n--- bar"
        );
    }

    #[test]
    fn display_return() {
        assert_eq!(
            Return::new("number", "foo").to_string(),
            "---@return number # foo"
        );

        assert_eq!(Return::new("number", "",).to_string(), "---@return number");

        assert_eq!(Return::new("number", "",).to_string(), "---@return number");

        assert_eq!(
            Return::new("number", "foo\nbar",).to_string(),
            "---@return number # foo\n--- bar"
        );

        assert_eq!(
            Return::new("number", "foo",).to_string(),
            "---@return number # foo"
        );
    }

    #[test]
    fn display_functiondecl() {
        assert_eq!(
            FunctionDecl::new(MethodKind::Standalone, "foo", ["p1", "p2"].into_iter()).to_string(),
            "function foo(p1, p2) end"
        );

        assert_eq!(
            FunctionDecl::new(MethodKind::Standalone, "foo", ["end", "p2"].into_iter()).to_string(),
            "function foo(end_, p2) end"
        );
    }

    #[test]
    fn display_class() {
        assert_eq!(Class::new("Foo").to_string(), "---@class Foo");
    }

    #[test]
    fn annotation_operator() {
        assert_eq!(
            Operation::new(OperationKind::Add, Some("number"), "number",).to_string(),
            "---@operator add(number):number"
        );

        assert_eq!(
            Operation::new(OperationKind::Add, None, "number").to_string(),
            "---@operator add:number"
        );
    }
}
