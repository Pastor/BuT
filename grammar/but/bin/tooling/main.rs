extern crate but;

use std::fmt::Debug;
use std::path::Path;
use std::{fs, process};

use clap::Parser;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use but::helpers::ident::FormatterIdent;

#[derive(Debug, Parser)]
#[clap(name = "BuT tooling", about, verbatim_doc_comment)]
struct Args {
    #[clap(short, long)]
    source_file: String,
}

fn main() -> anyhow::Result<()> {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();
    let args: Args = Args::parse();
    let path = args.source_file;
    let source = match fs::read_to_string(path.clone()) {
        Ok(source) => source,
        Err(err) => panic!("{}", err.to_string()),
    };
    let path = Path::new(&path);

    let unit = match but::parse(&source, 0) {
        Ok(unit) => unit,
        Err(errors) => {
            let file = SimpleFile::new(path.file_name().unwrap().to_str().unwrap(), source);
            for x in errors {
                let message = x.message;
                let diagnostic = Diagnostic::error()
                    .with_code("E0001")
                    .with_message(message.clone())
                    .with_labels(vec![Label::primary((), x.loc.start()..x.loc.end())
                        .with_message(message.clone())])
                    .with_notes(x.notes.iter().map(move |n| n.message.clone()).collect());

                term::emit(&mut writer.lock(), &config, &file, &diagnostic)?;
            }
            process::exit(-1)
        }
    };
    println!("{}", FormatterIdent::new(2, Box::new(unit.0)));
    Ok(())
}
