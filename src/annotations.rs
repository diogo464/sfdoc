use std::io::{Result, Write};

use crate::sfdoc::{Library, Method, Parameter, Realm, Return, Type};

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

pub fn generate_lib(writer: &mut impl Write, library: &Library, kind: LibraryKind) -> Result<()> {
    writeln!(writer, "{}", Annotation::Meta)?;
    let method_kind = match kind {
        LibraryKind::Normal => {
            write_description(writer, &library.description)?;
            write_realm(writer, library.realm)?;
            writeln!(writer, "{} = {{}}", library.name)?;
            MethodKind::Table(&library.name)
        }
        LibraryKind::Prelude => MethodKind::Standalone,
    };

    for method in library.methods.values() {
        write_method(writer, method_kind, method)?;
    }

    Ok(())
}

pub fn generate_ty(writer: &mut impl Write, ty: &Type) -> Result<()> {
    writeln!(writer, "{}", Annotation::Meta)?;
    write_description(writer, &ty.description)?;
    write_realm(writer, ty.realm)?;
    writeln!(writer, "{}", Annotation::Class { name: &ty.name })?;
    writeln!(writer, "{} = {{}}", ty.name)?;
    writeln!(writer)?;

    for method in ty.methods.values() {
        write_method(writer, MethodKind::Type(&ty.name), method)?;
    }

    Ok(())
}

fn write_method(writer: &mut impl Write, kind: MethodKind, method: &Method) -> Result<()> {
    write_description(writer, &method.description)?;
    write_realm(writer, method.realm)?;
    for param in &method.parameters {
        write_parameter(writer, param)?;
    }
    for ret in &method.returns {
        write_return(writer, ret)?;
    }
    write!(writer, "function ")?;
    match kind {
        MethodKind::Table(n) => write!(writer, "{}.{}", n, method.name)?,
        MethodKind::Type(n) => write!(writer, "{}:{}", n, method.name)?,
        MethodKind::Standalone => write!(writer, "{}", method.name)?,
    }
    write!(writer, "(")?;
    for (i, param) in method.parameters.iter().enumerate() {
        if i > 0 {
            write!(writer, ", ")?;
        }
        write!(writer, "{}", param.name)?;
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
            name: &param.name,
            optional: param.optional,
            ty: &param.ty,
            description: Some(&param.description)
        }
    )
}

fn write_return(writer: &mut impl Write, ret: &Return) -> Result<()> {
    writeln!(
        writer,
        "{}",
        Annotation::Return {
            ty: &ret.ty,
            name: None,
            description: Some(&ret.description),
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

//pub struct TableWriter<W> {
//    writer: W,
//}
//
//impl<W: Write> TableWriter<W> {
//    pub fn field(&mut self, name: &str, ty: &str, description: &str) -> Result<()> {
//        todo!()
//    }
//}
//
//pub struct MethodWriter<'w, W> {
//    writer: &'w mut MetaWriter<W>,
//}
//
//impl<'w, W: Write> MethodWriter<'w, W> {
//    fn new(writer: &'w mut MetaWriter<W>, name: &str) -> Result<Self> {
//        Ok(Self { writer })
//    }
//
//    pub fn description(&mut self, description: &str) -> Result<&mut Self> {
//        todo!()
//    }
//
//    pub fn deprecated(&mut self, deprecated: bool) -> Result<&mut Self> {
//        todo!()
//    }
//
//    pub fn parameter(&mut self, name: &str, ty: &str, description: &str) -> Result<&mut Self> {
//        todo!()
//    }
//
//    pub fn returnv(&mut self, name: &str, ty: &str, description: &str) -> Result<&mut Self> {
//        todo!()
//    }
//}
//
//pub struct TypeWriter<'w, W> {
//    writer: &'w mut MetaWriter<W>,
//}
//
//impl<'w, W: Write> TypeWriter<'w, W> {
//    fn new(writer: &'w mut MetaWriter<W>) -> Self {
//        Self { writer }
//    }
//
//    pub fn description(&mut self, description: &str) -> Result<&mut Self> {
//        todo!()
//    }
//
//    pub fn method(&mut self, name: &str) -> Result<MethodWriter<W>> {
//        todo!()
//    }
//}
//
//pub struct MetaWriter<W> {
//    writer: W,
//}
//
//impl<W: Write> MetaWriter<W> {
//    pub fn new(writer: W) -> Self {
//        Self { writer }
//    }
//
//    pub fn field(&mut self, name: &str, description: &str) -> Result<()> {
//        todo!()
//    }
//
//    pub fn method(&mut self, name: &str) -> Result<MethodWriter<W>> {
//        MethodWriter::new(self, name)
//    }
//
//    pub fn table(&mut self, name: &str) -> Result<TableWriter<W>> {
//        todo!()
//    }
//
//    pub fn ty(&mut self, name: &str) -> Result<TypeWriter<W>> {
//        todo!()
//    }
//
//    pub fn finish(&mut self) -> Result<()> {
//        todo!()
//    }
//}

//pub trait FieldWriter {
//    fn field(&mut self, name: &str, ty: &str, description: &str);
//}
//
//pub trait TableWriter: FieldWriter {}
//
//pub trait MethodWriter {
//    fn deprecated(&mut self);
//    fn description(&mut self, description: &str);
//    fn parameter(&mut self, name: &str, ty: &str, description: &str);
//    fn returnv(&mut self, name: &str, ty: &str, description: &str);
//}
//
//pub trait TypeWriter {
//    type MethodWriter: MethodWriter;
//
//    fn method(&mut self, name: &str) -> Self::MethodWriter;
//    fn operation(&mut self, operation: &str, input_type: &str, output_type: &str);
//}
//
//pub trait MetaWriter: FieldWriter {
//    type TypeWriter: TypeWriter;
//    type TableWriter: TableWriter;
//
//    fn ty(&mut self, name: &str) -> Self::TypeWriter;
//    fn table(&mut self, name: &str) -> Self::TableWriter;
//}
//
//pub trait Documentable {
//    fn document(&self, write: impl MetaWriter);
//}

//use crate::sfdoc::{Docs, Library};
//
pub enum Operation<'s> {
    UnaryMinus,
    Add,
    Subtract,
    Multiply,
    Divide,
    Other(&'s str),
}

impl<'s> std::fmt::Display for Operation<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::UnaryMinus => write!(f, "unm"),
            Operation::Add => write!(f, "add"),
            Operation::Subtract => write!(f, "sub"),
            Operation::Multiply => write!(f, "mul"),
            Operation::Divide => write!(f, "div"),
            Operation::Other(s) => write!(f, "{}", s),
        }
    }
}

pub enum Annotation<'s> {
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
        operation: Operation<'s>,
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
                if let Some(description) = description {
                    write!(f, " {}", description)?;
                }
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
                if let Some(description) = description {
                    write!(f, " {}", description)?;
                }
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
                } else if description.is_some() {
                    write!(f, " #")?;
                }
                if let Some(description) = description {
                    write!(f, " {}", description)?;
                }
                Ok(())
            }
        }
    }
}
//
//pub struct MetaFileWriter<W> {
//    writer: W,
//}
//
//pub struct LibraryWriter<W> {
//    writer: W,
//}
//
//impl<W: Write> LibraryWriter<W> {
//    pub fn new(mut writer: W, name: &str) -> std::io::Result<Self> {
//        writeln!(writer, "{}", Annotation::Meta)?;
//        writeln!(writer)?;
//        Ok(Self { writer })
//    }
//
//    pub fn finish(mut self) -> std::io::Result<()> {
//        self.writer.flush()
//    }
//}
//
//pub struct AnnotatedLibrary {
//    pub name: String,
//    pub content: String,
//}

//pub fn annotate(docs: &Docs) -> Vec<AnnotatedLibrary> {
//    docs.libraries.values().map(annotate_library).collect()
//}

//fn annotate_library(library: &Library) -> AnnotatedLibrary {
//    let mut content = String::new();
//
//    writeln!(content, "---@meta");
//
//    // TODO: Library description
//    // TODO: Library realm
//
//    for table in library.tables.values() {
//        writeln!(content, "---@class {}", table.name);
//        for field in table.fields.values() {
//            writeln!(
//                content,
//                "---@field {} any {}",
//                field.name, field.description
//            );
//        }
//    }
//
//    for method in library.methods.values() {
//        writeln!(
//            content,
//            "--- {}",
//            method.description.replace("\n", "\n--- ")
//        );
//        for parameter in method.parameters.iter() {
//            writeln!(
//                content,
//                "---@param {} {} {}",
//                parameter.name, parameter.ty, parameter.description
//            );
//        }
//        for return_ in method.returns.iter() {
//            writeln!(content, "---@return {} {}", return_.ty, return_.description);
//        }
//        write!(content, "local function m.{}(", method.name);
//        for (i, parameter) in method.parameters.iter().enumerate() {
//            if i > 0 {
//                write!(content, ", ");
//            }
//            write!(content, "{}", parameter.name);
//        }
//        writeln!(content, ") end");
//        writeln!(content, "");
//    }
//
//    AnnotatedLibrary {
//        name: library.name.clone(),
//        content,
//    }
//}
