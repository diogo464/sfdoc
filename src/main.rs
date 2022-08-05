#![feature(pattern)]
#![feature(assert_matches)]
#![feature(str_split_as_str)]
#![feature(str_split_whitespace_as_str)]
#![feature(stmt_expr_attributes)]
#![feature(anonymous_lifetime_in_impl_trait)]

pub mod annotations;
pub mod sfdoc;

use std::{collections::HashMap, path::PathBuf};

use annotations::LibraryKind;
use clap::{Parser, Subcommand, ValueHint};
use sfdoc::{Docs, Library};

#[derive(Debug, Parser)]
struct Args {
    #[clap(subcommand)]
    subcommand: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Parse(ParseArgs),
    Annotations(AnnotationsArgs),
}

#[derive(Debug, Parser)]
struct ParseArgs {
    #[clap(required = true, value_hint = ValueHint::AnyPath)]
    /// Path to directories or lua files to parse.
    /// Directories will be recursively searched for lua files.
    paths: Vec<PathBuf>,
}

#[derive(Debug, Parser)]
struct AnnotationsArgs {
    #[clap(default_value = "docs.json", value_hint = ValueHint::FilePath)]
    /// Path to the file containing the json serialized documentation.
    /// Directories will be recursively searched for lua files.
    docs: PathBuf,

    #[clap(long, default_value = "sfdocs", value_hint = ValueHint::AnyPath)]
    /// Directory to write the generated documentation to.
    output: PathBuf,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let args = Args::parse();

    match args.subcommand {
        Command::Parse(parse) => command_parse(parse),
        Command::Annotations(annotations) => command_annotations(annotations),
    }
}

fn command_parse(parse: ParseArgs) -> anyhow::Result<()> {
    let (docs, _diags) = sfdoc::document_paths(&parse.paths).unwrap();
    //let docs = Docs {
    //    libraries: HashMap::from_iter(std::iter::once((
    //        "math".into(),
    //        Library {
    //            name: "math".into(),
    //            description: "A library of math functions".into(),
    //            realm: sfdoc::Realm::Shared,
    //            tables: Default::default(),
    //            methods: Default::default(),
    //            fields: Default::default(),
    //        },
    //    ))),
    //    ..Default::default()
    //};

    let mut stdout = std::io::stdout().lock();
    serde_json::to_writer_pretty(&mut stdout, &docs)?;

    Ok(())
}

fn command_annotations(annotations: AnnotationsArgs) -> anyhow::Result<()> {
    let content = std::fs::read(annotations.docs)?;
    let docs = serde_json::from_slice::<Docs>(&content)?;
    let basedir = annotations.output;
    let mut buffer = Vec::<u8>::with_capacity(512 * 1024);

    std::fs::create_dir_all(&basedir)?;
    for ty in docs.types.values() {
        let filename = format!("type_{}.lua", ty.name);
        buffer.clear();
        annotations::generate_ty(&mut buffer, ty)?;
        std::fs::write(basedir.join(filename), &buffer)?;
    }

    for lib in docs.libraries.values() {
        let filename = format!("{}.lua", lib.name);
        let library_kind = if lib.name == "builtins" {
            LibraryKind::Prelude
        } else {
            LibraryKind::Normal
        };
        buffer.clear();
        annotations::generate_lib(&mut buffer, lib, library_kind)?;
        std::fs::write(basedir.join(filename), &buffer)?;
    }

    Ok(())
}
