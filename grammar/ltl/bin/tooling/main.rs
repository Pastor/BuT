extern crate ltl;

use std::fs;

use clap::Parser;

use ltl::ast::grammar;

#[derive(Debug, Parser)]
#[clap(name = "LTL tooling", about, verbatim_doc_comment)]
struct Args {
    #[clap(short, long)]
    formula_file: String,
}

fn main() {
    let args: Args = Args::parse();
    println!("File   : {}", args.formula_file);

    let formula_text =
        fs::read_to_string(args.formula_file).expect("Unable to read the formula file");
    let formula = grammar::FormulaParser::new()
        .parse(&formula_text)
        .expect("Unable to parse the formula file");
    println!("Formula: {}", formula);
}