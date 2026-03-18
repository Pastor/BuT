use crate::model::Model;
use but_grammar::ast::SourceUnit;
use but_grammar::diagnostics::Diagnostic;

pub mod bitaccess;
pub mod include;

mod context;
mod model;
mod tree;
mod types;

pub fn processing(unit: SourceUnit) -> Result<SourceUnit, Vec<Diagnostic>> {
    let model = Model::new(unit.clone());
    let main = model.analyze()?;
    Ok(unit)
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;
    use std::vec::IntoIter;

    use but_grammar::ast::SourceUnit;
    use but_grammar::diagnostics::Diagnostic;
    use but_grammar::parse;
    use walkdir::{DirEntry, WalkDir};

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
                let result: Result<SourceUnit, Vec<Diagnostic>> = match parse(&source_part, 0) {
                    Ok((unit, _comments)) => match crate::processing(unit) {
                        Ok(result) => Ok(result),
                        Err(err) => Err(err),
                    },
                    Err(diagnostics) => Err(diagnostics),
                };

                if let (Err(err), false) = (
                    result.map_err(|diags| process_diagnostics(path, src, diags)),
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
