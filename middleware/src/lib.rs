use but_grammar::ast::{SourceUnit, SourceUnitPart};
use but_grammar::diagnostics::Diagnostic;

use crate::tree::TreeDefinition;
use crate::unit::Unit;

mod tree;
mod unit;
mod context;
mod unit2;

pub fn parse(unit: SourceUnit) -> Result<Unit, Vec<Diagnostic>> {
    let mut global_enums = vec![];
    let mut models = vec![];
    let mut global_types = vec![];
    let mut global_properties = vec![];
    let mut global_variables = vec![];
    let mut global_functions = vec![];
    let mut global_structs = vec![];
    let mut global_imports = vec![];
    for part in unit.0 {
        match part {
            SourceUnitPart::ImportDirective(im) => { global_imports.push(im); }
            SourceUnitPart::EnumDefinition(ed) => { global_enums.push(ed); }
            SourceUnitPart::StructDefinition(sd) => { global_structs.push(sd); }
            SourceUnitPart::ErrorDefinition(_) => { println!("Skip error"); }
            SourceUnitPart::FunctionDefinition(fd) => { global_functions.push(fd); }
            SourceUnitPart::FormulaDefinition(_) => { println!("Skip formula"); }
            SourceUnitPart::VariableDefinition(vd) => { global_variables.push(vd); }
            SourceUnitPart::AnnotationDefinition(_) => { println!("Skip annotation"); }
            SourceUnitPart::PropertyDefinition(pd) => { global_properties.push(pd); }
            SourceUnitPart::ModelDefinition(md) => { models.push(md); }
            SourceUnitPart::TypeDefinition(td) => { global_types.push(td); }
            SourceUnitPart::Using(_) => { println!("Skip using"); }
            SourceUnitPart::StraySemicolon(_) => {}
        }
    }
    let mut diagnostics = vec![];
    let tree_definition = TreeDefinition::new(
        models, global_enums, global_types, global_properties, global_variables, global_structs, global_functions,
        &mut diagnostics,
    );
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }
    tree_definition.build_tree()
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;
    use std::vec::IntoIter;

    use walkdir::{DirEntry, WalkDir};

    use but_grammar::diagnostics::Diagnostic;
    use but_grammar::parse;

    use crate::unit::Unit;

    #[test]
    fn test_semantics_failed() {
        let semantic_tests =
            WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("tests_data/semantic/failed"))
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
                .into_iter();
        let errors = process_sources(semantic_tests);
        for error_text in errors.clone() {
            println!("[SEMANTIC] Failed: {}", error_text);
        }
        assert!(errors.len() > 0, "Has no errors?");
    }

    #[test]
    fn test_semantics_success() {
        let semantic_tests =
            WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("tests_data/semantic/success"))
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
                .into_iter();
        let errors = process_sources(semantic_tests);
        for error_text in errors.clone() {
            println!("[SEMANTIC] Failed: {}", error_text);
        }
        assert!(errors.is_empty(), "{}", errors.join("\n"));
    }

    fn process_sources(semantic_failed_tests: IntoIter<DirEntry>) -> Vec<String> {
        let source_delimiter = regex::Regex::new(r"====.*====").unwrap();
        let error_matcher = regex::Regex::new(r"// ----\r?\n// \w+( \d+)?:").unwrap();
        let syntax_test = false;
        semantic_failed_tests
            .into_iter()
            .map::<Result<_, String>, _>(|entry| {
                if entry.file_name().to_string_lossy().ends_with(".but") {
                    let source = match fs::read_to_string(entry.path()) {
                        Ok(source) => source,
                        Err(err) if matches!(err.kind(), std::io::ErrorKind::InvalidData) => {
                            return Ok(vec![]);
                        }
                        Err(err) => return Err(err.to_string()),
                    };

                    let expect_error = syntax_test && error_matcher.is_match(&source);

                    Ok(source_delimiter
                        .split(&source)
                        .filter(|source_part| !source_part.is_empty())
                        .map(|part| {
                            (
                                entry.path().to_string_lossy().to_string(),
                                expect_error,
                                part.to_string(),
                            )
                        })
                        .collect::<Vec<_>>())
                } else {
                    Ok(vec![])
                }
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .flatten()
            .filter_map(|(path, expect_error, source_part)| {
                let src = source_part.to_string();
                let result: Result<Unit, Vec<Diagnostic>> = match parse(&source_part, 0) {
                    Ok((unit, _comments)) => {
                        match crate::parse(unit) {
                            Ok(result) => Ok(result),
                            Err(err) => Err(err),
                        }
                    }
                    Err(diagnostics) => {
                        Err(diagnostics)
                    }
                };


                if let (Err(err), false) = (
                    result.map_err(|diags| {
                        process_diagnostics(path, src, diags)
                    }),
                    expect_error,
                ) {
                    return Some(err);
                }

                None
            })
            .collect::<Vec<_>>()
    }

    fn process_diagnostics(path: String, src: String, diagnostics: Vec<Diagnostic>) -> String {
        format!(
            "{:?}:\n\t{}\n\t{}",
            path,
            diagnostics
                .iter()
                .map(|diag| format!("{diag:?}"))
                .collect::<Vec<_>>()
                .join("\n\t"),
            diagnostics
                .iter()
                .map(|d| {
                    let loc = d.loc;
                    let mut chars = src.chars();
                    let mut start_line = loc.start();
                    let mut end_line = loc.end();
                    'position: loop {
                        //TODO: Зачем?
                        if start_line == 0 {
                            start_line = 0;
                            break;
                        }
                        if end_line >= src.len() {
                            end_line = src.len() - 1;
                            break;
                        }
                        let option = chars.nth(start_line);
                        match option {
                            None => break 'position,
                            Some(c) if c == '\n' => {
                                start_line += 1;
                                break 'position;
                            }
                            _ => {}
                        }

                        let option = chars.nth(end_line);
                        match option {
                            None => break 'position,
                            Some(c) if c == '\n' => {
                                end_line -= 1;
                                break 'position;
                            }
                            _ => {}
                        }
                        start_line -= 1;
                        end_line += 1;
                    }
                    format!("{}", &src[start_line..end_line])
                })
                .collect::<Vec<_>>()
                .join("\n\t"),
        )
    }
}