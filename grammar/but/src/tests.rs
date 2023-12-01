use std::{fs, path::Path, thread};
use std::sync::mpsc;
use std::time::Duration;

use pretty_assertions::assert_eq;
use walkdir::WalkDir;

use crate::ast::*;
use crate::but;
use crate::diagnostics::{Diagnostic, ErrorType::ParserError, Level::Error};
use crate::lexer::Lexer;
use crate::Loc::Source;

#[test]
fn parser_error_recovery() {
    let src = r#"import * as sesa frum "sesa";
pragma sesa_pragma;
usingg sesa for *;
contract 9c {
    uint256 0sesa_glb = 90;
    9uint256 sesa_glb = 90;
    uint256 sesa_glb = 90id;

    event 1sesa_event(uint 0invalid_param_id);
    event sesa_event(3uint invalid_param_type);

    error 1sesa_error(uint 0invalid_param_id);
    error sesa_error(3uint invalid_param_type);

    struct 2sesa_struct {
        uint256 3sesa_struct_mem;
    }

    function 4sesa_func() public! pure {
        uint 3sesa_var = 3sesa_id + id;
        9uint sesa= 4b;
        if (true)
    }
}
"#;

    if let Err(errors) = crate::parse(src, 0) {
        assert_eq!(
            errors,
            vec![
                Diagnostic { loc: Source(0, 17, 21), level: Error, ty: ParserError, message: "'frum' found where 'from' expected".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 48, 49), level: Error, ty: ParserError, message: r#"unrecognised token ';', expected string"#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 62, 65), level: Error, ty: ParserError, message: r#"unrecognised token 'for', expected "(", ";", "=""#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 78, 79), level: Error, ty: ParserError, message: r#"unrecognised token '9', expected "case", "default", "leave", "revert", "switch", identifier"#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 95, 96), level: Error, ty: ParserError, message: "unrecognised token '0', expected \"(\", \"++\", \"--\", \".\", \"[\", \"case\", \"constant\", \"default\", \"external\", \"immutable\", \"internal\", \"leave\", \"override\", \"private\", \"public\", \"revert\", \"switch\", \"{\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 116, 123), level: Error, ty: ParserError, message: "unrecognised token 'uint256', expected \"++\", \"--\", \".\", \"[\", \"case\", \"default\", \"leave\", \"switch\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 403, 404), level: Error, ty: ParserError, message: "unrecognised token '3', expected \"(\", \"++\", \"--\", \".\", \"[\", \"case\", \"constant\", \"default\", \"external\", \"immutable\", \"internal\", \"leave\", \"override\", \"private\", \"public\", \"revert\", \"switch\", \"{\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 441, 442), level: Error, ty: ParserError, message: r#"unrecognised token '4', expected "(", "case", "default", "leave", "revert", "switch", identifier"#.to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 460, 461), level: Error, ty: ParserError, message: "unrecognised token '!', expected \";\", \"case\", \"constant\", \"default\", \"external\", \"immutable\", \"internal\", \"leave\", \"override\", \"payable\", \"private\", \"public\", \"pure\", \"return\", \"returns\", \"revert\", \"switch\", \"view\", \"virtual\", \"{\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 482, 483), level: Error, ty: ParserError, message: "unrecognised token '3', expected \"!=\", \"%\", \"%=\", \"&\", \"&&\", \"&=\", \"(\", \"*\", \"**\", \"*=\", \"+\", \"++\", \"+=\", \"-\", \"--\", \"-=\", \".\", \"/\", \"/=\", \";\", \"<\", \"<<\", \"<<=\", \"<=\", \"=\", \"==\", \">\", \">=\", \">>\", \">>=\", \"?\", \"[\", \"^\", \"^=\", \"calldata\", \"case\", \"default\", \"leave\", \"memory\", \"revert\", \"storage\", \"switch\", \"{\", \"|\", \"|=\", \"||\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 518, 522), level: Error, ty: ParserError, message: "unrecognised token 'uint256', expected \"!=\", \"%\", \"%=\", \"&\", \"&&\", \"&=\", \"*\", \"**\", \"*=\", \"+\", \"++\", \"+=\", \"-\", \"--\", \"-=\", \".\", \"/\", \"/=\", \";\", \"<\", \"<<\", \"<<=\", \"<=\", \"=\", \"==\", \">\", \">=\", \">>\", \">>=\", \"?\", \"[\", \"^\", \"^=\", \"case\", \"default\", \"leave\", \"switch\", \"|\", \"|=\", \"||\", identifier".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 555, 556), level: Error, ty: ParserError, message: "unrecognised token '}', expected \"!\", \"(\", \"+\", \"++\", \"-\", \"--\", \"[\", \"address\", \"assembly\", \"bool\", \"break\", \"byte\", \"bytes\", \"case\", \"continue\", \"default\", \"delete\", \"do\", \"emit\", \"false\", \"for\", \"function\", \"if\", \"leave\", \"mapping\", \"new\", \"payable\", \"return\", \"revert\", \"string\", \"switch\", \"true\", \"try\", \"type\", \"unchecked\", \"while\", \"{\", \"~\", Bytes, Int, Uint, address, hexnumber, hexstring, identifier, number, rational, string".to_string(), notes: vec![] },
                Diagnostic { loc: Source(0, 557, 558), level: Error, ty: ParserError, message: "unrecognised token '}', expected \"(\", \";\", \"[\", \"abstract\", \"address\", \"bool\", \"byte\", \"bytes\", \"case\", \"contract\", \"default\", \"enum\", \"event\", \"false\", \"function\", \"import\", \"interface\", \"leave\", \"library\", \"mapping\", \"payable\", \"pragma\", \"string\", \"struct\", \"switch\", \"true\", \"type\", \"using\", Bytes, Int, Uint, address, annotation, hexnumber, hexstring, identifier, number, rational, string".to_string(), notes: vec![] },
            ]
        )
    }
}

#[test]
fn parse_function_assembly() {
    let src = r#"
        fn try_recover() {
            assembly {

            }
        }
        "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn parse_random_doc_comment() {
    let src = r#"
 const /** x */ dev : /** x */ usize  = /** x */1 /** x */ + /** x */2/** x */;
    "#;

    let (actual_parse_tree, _) = crate::parse(src, 0).unwrap();
    assert_eq!(actual_parse_tree.0.len(), 1);
}

#[test]
fn test_lib_but() {
    fn timeout_after<T, F>(d: Duration, f: F) -> Result<T, String>
        where
            T: Send + 'static,
            F: FnOnce() -> T,
            F: Send + 'static,
    {
        let (done_tx, done_rx) = mpsc::channel();
        let handle = thread::spawn(move || {
            let val = f();
            done_tx.send(()).expect("Unable to send completion signal");
            val
        });

        match done_rx.recv_timeout(d) {
            Ok(_) => Ok(handle.join().expect("Thread panicked")),
            Err(_) => Err(format!("Thread timeout-ed after {d:?}")),
        }
    }

    let source_delimiter = regex::Regex::new(r"====.*====").unwrap();
    let error_matcher = regex::Regex::new(r"// ----\r?\n// \w+( \d+)?:").unwrap();

    let semantic_tests =
        WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests_data/BuT/semantic"))
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .map(|entry| (false, entry));

    let syntax_tests =
        WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests_data/BuT/syntax"))
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .map(|entry| (true, entry));

    let errors =
        semantic_tests
            .into_iter()
            .chain(syntax_tests)
            .map::<Result<_, String>, _>(|(syntax_test, entry)| {
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
                let result = match timeout_after(Duration::from_secs(5), move || {
                    crate::parse(&source_part, 0)
                }) {
                    Ok(result) => result,
                    Err(err) => return Some(format!("{path:?}: \n\t{err}")),
                };

                if let (Err(err), false) = (
                    result.map_err(|diags| {
                        format!(
                            "{:?}:\n\t{}",
                            path,
                            diags
                                .iter()
                                .map(|diag| format!("{diag:?}"))
                                .collect::<Vec<_>>()
                                .join("\n\t")
                        )
                    }),
                    expect_error,
                ) {
                    return Some(err);
                }

                None
            })
            .collect::<Vec<_>>();

    assert!(errors.is_empty(), "{}", errors.join("\n"));
}
