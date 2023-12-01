extern crate BuT;

use std::fs;

use clap::Parser;

#[derive(Debug, Parser)]
#[clap(name = "BuT tooling", about, verbatim_doc_comment)]
struct Args {
    #[clap(short, long)]
    source_file: String,
}

fn main() {
    let args: Args = Args::parse();
    println!("File   : {}", args.source_file);

    let source_text =
        fs::read_to_string(args.source_file).expect("Unable to read the formula file");
    // let formula = grammar::FormulaParser::new()
    //     .parse(&source_text)
    //     .expect("Unable to parse the formula file");
    // println!("Formula: {}", formula);
    println!("{source_text:?}");
}
