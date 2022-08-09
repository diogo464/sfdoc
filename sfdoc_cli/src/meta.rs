use std::io::{Result, Write};

use sfdoc::{Library, Method, Parameter, Realm, Return, Type};

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

enum Operation {
    UnaryMinus,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::UnaryMinus => write!(f, "unm"),
            Operation::Add => write!(f, "add"),
            Operation::Subtract => write!(f, "sub"),
            Operation::Multiply => write!(f, "mul"),
            Operation::Divide => write!(f, "div"),
        }
    }
}

enum Annotation<'s> {
    Meta,
    Class {
        name: &'s str,
    },
    Deprecated,
    Field {
        name: &'s str,
        ty: &'s str,
        description: Option<&'s str>,
    },
    Operator {
        operation: Operation,
        input_type: Option<&'s str>,
        output_type: &'s str,
    },
    Param {
        name: &'s str,
        optional: bool,
        ty: &'s str,
        description: Option<&'s str>,
    },
    Return {
        ty: &'s str,
        name: Option<&'s str>,
        description: Option<&'s str>,
    },
}

impl<'s> Annotation<'s> {
    fn format_description_opt(
        f: &mut std::fmt::Formatter,
        description: Option<&'s str>,
    ) -> std::fmt::Result {
        let mut newline = false;
        if let Some(description) = description {
            for line in description.lines() {
                if newline {
                    write!(f, "\n--- ")?;
                } else {
                    write!(f, " ")?;
                }
                newline = true;
                write!(f, "{}", line.trim())?;
            }
        }
        Ok(())
    }
}

impl<'s> std::fmt::Display for Annotation<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Annotation::Meta => write!(f, "---@meta"),
            Annotation::Class { name } => write!(f, "---@class {}", name),
            Annotation::Deprecated => write!(f, "---@deprecated"),
            Annotation::Field {
                name,
                ty,
                description,
            } => {
                write!(f, "---@field {} {}", name, ty)?;
                Self::format_description_opt(f, *description)?;
                Ok(())
            }
            Annotation::Operator {
                operation,
                input_type,
                output_type,
            } => {
                write!(f, "---@operator {}", operation)?;
                if let Some(input_type) = input_type {
                    write!(f, "({})", input_type)?;
                }
                write!(f, ":{}", output_type)
            }
            Annotation::Param {
                name,
                optional,
                ty,
                description,
            } => {
                write!(f, "---@param {}", name)?;
                if *optional {
                    write!(f, "?")?;
                }
                write!(f, " {}", ty)?;
                Self::format_description_opt(f, *description)?;
                Ok(())
            }
            Annotation::Return {
                ty,
                name,
                description,
            } => {
                write!(f, "---@return {}", ty)?;
                if let Some(name) = name {
                    write!(f, " {}", name)?;
                }
                if description.is_some() {
                    write!(f, " #")?;
                    Self::format_description_opt(f, *description)?;
                }
                Ok(())
            }
        }
    }
}

pub fn generate_lib(writer: &mut impl Write, library: &Library, kind: LibraryKind) -> Result<()> {
    writeln!(writer, "{}", Annotation::Meta)?;
    let method_kind = match kind {
        LibraryKind::Normal => {
            write_description(writer, library.description())?;
            write_realm(writer, library.realm())?;
            writeln!(writer, "{} = {{}}", library.name())?;
            MethodKind::Table(library.name())
        }
        LibraryKind::Prelude => MethodKind::Standalone,
    };

    for method in library.methods() {
        write_method(writer, method_kind, method)?;
    }

    Ok(())
}

pub fn generate_ty(writer: &mut impl Write, ty: &Type) -> Result<()> {
    writeln!(writer, "{}", Annotation::Meta)?;
    write_description(writer, ty.description())?;
    write_realm(writer, ty.realm())?;
    writeln!(writer, "{}", Annotation::Class { name: ty.name() })?;
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

        let operator = match method.name() {
            "__add" => Annotation::Operator {
                operation: Operation::Add,
                input_type,
                output_type,
            },
            "__div" => Annotation::Operator {
                operation: Operation::Divide,
                input_type,
                output_type,
            },
            "__mul" => Annotation::Operator {
                operation: Operation::Multiply,
                input_type,
                output_type,
            },
            "__unm" => Annotation::Operator {
                operation: Operation::UnaryMinus,
                input_type: None,
                output_type,
            },
            "__sub" => Annotation::Operator {
                operation: Operation::Subtract,
                input_type,
                output_type,
            },
            _ => continue,
        };
        writeln!(writer, "{}", operator)?;
    }
    writeln!(writer, "{} = {{}}", ty.name())?;
    writeln!(writer)?;

    for method in ty.methods() {
        write_method(writer, MethodKind::Type(ty.name()), method)?;
    }

    Ok(())
}

fn write_method(writer: &mut impl Write, kind: MethodKind, method: &Method) -> Result<()> {
    write_description(writer, method.description())?;
    write_realm(writer, method.realm())?;
    for param in method.parameters() {
        write_parameter(writer, param)?;
    }
    for ret in method.returns() {
        write_return(writer, ret)?;
    }
    write!(writer, "function ")?;
    match kind {
        MethodKind::Table(n) => write!(writer, "{}.{}", n, method.name())?,
        MethodKind::Type(n) => write!(writer, "{}:{}", n, method.name())?,
        MethodKind::Standalone => write!(writer, "{}", method.name())?,
    }
    write!(writer, "(")?;
    for (i, param) in method.parameters().iter().enumerate() {
        if i > 0 {
            write!(writer, ", ")?;
        }
        write!(writer, "{}", param.name())?;
    }
    writeln!(writer, ") end")?;
    writeln!(writer)?;
    Ok(())
}

fn write_parameter(writer: &mut impl Write, param: &Parameter) -> Result<()> {
    writeln!(
        writer,
        "{}",
        Annotation::Param {
            name: param.name(),
            optional: param.optional(),
            ty: &param.types().to_string(),
            description: Some(param.description())
        }
    )
}

fn write_return(writer: &mut impl Write, ret: &Return) -> Result<()> {
    writeln!(
        writer,
        "{}",
        Annotation::Return {
            ty: &ret.types().to_string(),
            name: None,
            description: Some(ret.description()),
        }
    )
}

fn write_description(writer: &mut impl Write, description: &str) -> Result<()> {
    for line in description.lines() {
        writeln!(writer, "--- {}", line)?;
        writeln!(writer, "---")?;
    }
    Ok(())
}

fn write_realm(writer: &mut impl Write, realm: Realm) -> Result<()> {
    match realm {
        Realm::Client => writeln!(writer, "--- Realm => client\n---"),
        Realm::Server => writeln!(writer, "--- Realm => server\n---"),
        Realm::Shared => writeln!(writer, "--- Realm => shared\n---"),
    }
}
