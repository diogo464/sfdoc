#![feature(assert_matches)]
#![feature(str_split_as_str)]
#![feature(str_split_whitespace_as_str)]
#![feature(stmt_expr_attributes)]
#![feature(anonymous_lifetime_in_impl_trait)]

pub mod sfdoc;

use std::path::PathBuf;

use clap::{Parser, Subcommand, ValueHint};

#[derive(Debug, Parser)]
struct Args {
    #[clap(subcommand)]
    subcommand: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Parse(ParseArgs),
}

#[derive(Debug, Parser)]
struct ParseArgs {
    #[clap(required = true, value_hint = ValueHint::AnyPath)]
    /// Path to directories or lua files to parse.
    /// Directories will be recursively searched for lua files.
    paths: Vec<PathBuf>,
}

fn main() {
    env_logger::init();

    let args = Args::parse();

    match args.subcommand {
        Command::Parse(parse) => {
            println!("{:?}", parse);
            let content = std::fs::read_to_string(&parse.paths[0]).unwrap();
            let docs = sfdoc::parse(&content).unwrap();
            println!("{:#?}", docs);
        }
    }
}
