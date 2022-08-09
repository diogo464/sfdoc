#![feature(pattern)]
#![feature(assert_matches)]
#![feature(str_split_as_str)]
#![feature(str_split_whitespace_as_str)]
#![feature(stmt_expr_attributes)]
#![feature(anonymous_lifetime_in_impl_trait)]

pub mod meta;
pub mod snippet;

use std::path::{Path, PathBuf};

use anyhow::Context;
use clap::{Parser, Subcommand, ValueHint};
use meta::LibraryKind;
use sfdoc::Docs;
use snippet::Snippets;

const DEFAULT_METADATA_FILE: &str = "docs.json";

#[derive(Debug, Parser)]
struct Args {
    #[clap(subcommand)]
    subcommand: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Parse(ParseArgs),
    GenMeta(GenMetaArgs),
    Snippets(SnippetsArgs),
}

/// Parse the documentation and generate a json file with the metadata.
#[derive(Debug, Parser)]
struct ParseArgs {
    /// Path to directories or lua files to parse.
    /// Directories will be recursively searched for lua files.
    ///
    /// Ex: <starfall dir>/lua/starfall/libs_*
    #[clap(required = true, value_hint = ValueHint::AnyPath)]
    paths: Vec<PathBuf>,
}

/// Generate the meta files from the parsed documentation.
/// The meta files are used to provide the lua language server with type information.
#[derive(Debug, Parser)]
struct GenMetaArgs {
    /// Path to the file containing the json serialized documentation.
    #[clap(default_value = DEFAULT_METADATA_FILE, value_hint = ValueHint::FilePath)]
    docs: PathBuf,

    /// Directory to write the generated documentation to.
    #[clap(long, default_value = "sfdocs", value_hint = ValueHint::AnyPath)]
    output: PathBuf,
}

/// Generate snippets for visual studio code from the parsed documentation.
#[derive(Debug, Parser)]
struct SnippetsArgs {
    /// Path to the file containing the json serialized documentation.
    #[clap(default_value = DEFAULT_METADATA_FILE, value_hint = ValueHint::FilePath)]
    docs: PathBuf,

    /// Output file for the snippets.
    #[clap(default_value = "hooks.code-snippets", value_hint = ValueHint::FilePath)]
    output: PathBuf,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let args = Args::parse();

    match args.subcommand {
        Command::Parse(parse) => command_parse(parse),
        Command::GenMeta(annotations) => command_annotations(annotations),
        Command::Snippets(snippets) => command_snippets(snippets),
    }
}

fn command_parse(args: ParseArgs) -> anyhow::Result<()> {
    let (docs, diags) = sfdoc::document_paths(&args.paths).unwrap();
    let mut stdout = std::io::stdout().lock();
    serde_json::to_writer_pretty(&mut stdout, &docs)?;
    for diag in diags {
        match diag.level() {
            sfdoc::DiagnosticLevel::Warning => log::warn!(
                "In {} at {}:{}\nMessage: '{}'",
                diag.path().display(),
                diag.location().line(),
                diag.location().column(),
                diag.message()
            ),
            sfdoc::DiagnosticLevel::Error => log::error!(
                "In {} at {}:{}\nMessage: '{}'",
                diag.path().display(),
                diag.location().line(),
                diag.location().column(),
                diag.message()
            ),
        }
    }
    Ok(())
}

fn command_annotations(args: GenMetaArgs) -> anyhow::Result<()> {
    let docs = read_docs_from_path(&args.docs)?;
    let basedir = args.output;
    let mut buffer = Vec::<u8>::with_capacity(512 * 1024);

    std::fs::create_dir_all(&basedir)?;
    for ty in docs.types() {
        let filename = format!("type_{}.lua", ty.name());
        buffer.clear();
        meta::generate_ty(&mut buffer, ty)?;
        std::fs::write(basedir.join(filename), &buffer)?;
    }

    for lib in docs.libraries() {
        let filename = format!("{}.lua", lib.name());
        let library_kind = if lib.name() == "builtins" {
            LibraryKind::Prelude
        } else {
            LibraryKind::Normal
        };
        buffer.clear();
        meta::generate_lib(&mut buffer, lib, library_kind)?;
        std::fs::write(basedir.join(filename), &buffer)?;
    }

    Ok(())
}

fn command_snippets(args: SnippetsArgs) -> anyhow::Result<()> {
    let docs = read_docs_from_path(&args.docs)?;
    let mut snippets = Snippets::default();
    for hook in docs.hooks() {
        snippets.add(hook);
    }
    let content = serde_json::to_string_pretty(&snippets)?;
    std::fs::write(args.output, &content)?;
    Ok(())
}

fn read_docs_from_path(path: &Path) -> anyhow::Result<Docs> {
    let content = std::fs::read(path)
        .with_context(|| format!("failed to open file at: {}", path.display()))?;
    let docs = serde_json::from_slice(&content).context("failed to parse json")?;
    Ok(docs)
}
