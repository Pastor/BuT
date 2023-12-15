extern crate but;

use std::fmt::Debug;
use std::fs;

use clap::Parser;

use but::helpers::ident::{DisplayIdent, FormatterIdent};

#[derive(Debug, Parser)]
#[clap(name = "BuT tooling", about, verbatim_doc_comment)]
struct Args {
    #[clap(short, long)]
    source_file: String,
}

fn main() {
    let args: Args = Args::parse();
    let path = args.source_file;
    let source = match fs::read_to_string(path.clone()) {
        Ok(source) => source,
        Err(err) => panic!("{}", err.to_string()),
    };

    let unit = match but::parse(&source, 0) {
        Ok(unit) => unit,
        Err(errors) => panic!(
            "{path:?}: \n\t{}",
            errors
                .iter()
                .map(|diag| format!("{diag:?}"))
                .collect::<Vec<_>>()
                .join("\n\t")
        ),
    };
    println!("{}", FormatterIdent::new(2, Box::new(unit.0)));
}
