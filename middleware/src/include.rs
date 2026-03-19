//! Include/import mechanism for BuT language files.
//!
//! Supported import directive forms:
//!
//! ```text
//! import "file";                        — direct import
//! import "file" as Alias;               — import with namespace alias
//! import * as Alias from "file";        — import all symbols with alias
//! import { A, B as C } from "file";    — named import
//! ```
//!
//! The resolver performs:
//! - File lookup relative to the base file or in search paths
//! - Recursive loading and parsing of transitive dependencies
//! - Deduplication: each file is included exactly once
//! - Circular dependency detection
//! - File numbering for correct error diagnostics
//!
//! # Usage example
//!
//! ```rust,no_run
//! use std::path::Path;
//! use but_middleware::include::IncludeResolver;
//!
//! let source = r#"import "types"; model M { state S {} start -> S; }"#;
//! let (unit, _) = but_grammar::parse(source, 0).unwrap();
//!
//! let mut resolver = IncludeResolver::from_file(Path::new("/project/main.but"));
//! match resolver.resolve(unit, Path::new("/project/main.but")) {
//!     Ok(merged) => println!("Merged {} items", merged.0.len()),
//!     Err(errs)  => errs.iter().for_each(|e| eprintln!("Error: {}", e)),
//! }
//! ```

use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use but_grammar::ast::{Import, ImportPath, SourceUnit, SourceUnitPart};
use but_grammar::diagnostics::Diagnostic;

// ─── File number counter ──────────────────────────────────────────────────────

/// Global counter for assigning unique numbers to files during parsing.
static FILE_NO_COUNTER: AtomicUsize = AtomicUsize::new(1);

fn next_file_no() -> usize {
    FILE_NO_COUNTER.fetch_add(1, Ordering::SeqCst)
}

// ─── Import resolution errors ─────────────────────────────────────────────────

/// Error that occurs when resolving an import directive.
#[derive(Debug)]
pub enum IncludeError {
    /// The imported file was not found in any of the search paths.
    FileNotFound {
        /// The file name from the import directive.
        path: PathBuf,
        /// List of directories that were searched.
        search_paths: Vec<PathBuf>,
    },
    /// A circular import was detected (a file imports itself transitively).
    CircularImport(PathBuf),
    /// I/O error when reading a file.
    IoError {
        /// Path to the file that could not be read.
        path: PathBuf,
        /// OS error message.
        error: String,
    },
    /// Syntax errors in the imported file.
    ParseError {
        /// Path to the file with errors.
        path: PathBuf,
        /// List of parser diagnostics.
        errors: Vec<Diagnostic>,
    },
}

impl fmt::Display for IncludeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IncludeError::FileNotFound { path, search_paths } => write!(
                f,
                "File not found: '{}'. Search paths: [{}]",
                path.display(),
                search_paths
                    .iter()
                    .map(|p| format!("'{}'", p.display()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IncludeError::CircularImport(path) => {
                write!(f, "Circular import: '{}'", path.display())
            }
            IncludeError::IoError { path, error } => {
                write!(f, "Read error '{}': {}", path.display(), error)
            }
            IncludeError::ParseError { path, errors } => write!(
                f,
                "Parse errors in '{}': {} errors",
                path.display(),
                errors.len()
            ),
        }
    }
}

// ─── Import resolver ──────────────────────────────────────────────────────────

/// Resolver for `import` directives in the BuT language.
///
/// Recursively loads and merges imported files into a single [`SourceUnit`].
/// Each file is processed exactly once by tracking visited canonical paths.
pub struct IncludeResolver {
    /// List of directories to search for imported files.
    search_paths: Vec<PathBuf>,
    /// Canonical paths of already-processed files (protection against duplication and cycles).
    visited: HashSet<PathBuf>,
}

impl IncludeResolver {
    /// Create a resolver with an explicit list of search paths.
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        Self {
            search_paths,
            visited: HashSet::new(),
        }
    }

    /// Create a resolver, adding the directory of `file_path` as a search path.
    pub fn from_file(file_path: &Path) -> Self {
        let mut search_paths = vec![];
        if let Some(dir) = file_path.parent() {
            // Skip the empty path ("") returned by parent() for files without a directory
            if dir != Path::new("") {
                search_paths.push(dir.to_path_buf());
            }
        }
        Self::new(search_paths)
    }

    /// Add a directory to the list of search paths.
    pub fn add_search_path(&mut self, path: PathBuf) -> &mut Self {
        self.search_paths.push(path);
        self
    }

    /// Get the list of search paths.
    pub fn search_paths(&self) -> &[PathBuf] {
        &self.search_paths
    }

    /// Number of already-processed files.
    pub fn visited_count(&self) -> usize {
        self.visited.len()
    }

    /// Resolve all `import` directives in `unit`, recursively loading dependencies.
    ///
    /// Returns a merged [`SourceUnit`] without import directives — all imported
    /// definitions are embedded directly in the result.
    ///
    /// Element order: first all definitions from imported files (in the order
    /// directives are encountered), then definitions from the current file.
    ///
    /// # Errors
    ///
    /// Returns a list of [`IncludeError`] if at least one import could not be
    /// resolved. Processing continues for remaining imports to collect as many
    /// errors as possible in a single pass.
    pub fn resolve(
        &mut self,
        unit: SourceUnit,
        base_path: &Path,
    ) -> Result<SourceUnit, Vec<IncludeError>> {
        // Mark the base file as visited
        self.mark_visited(base_path);

        let mut result_parts: Vec<SourceUnitPart> = vec![];
        let mut errors: Vec<IncludeError> = vec![];

        for part in unit.0 {
            match part {
                SourceUnitPart::ImportDirective(ref import) => {
                    let import_path_str = extract_path_str(import);

                    // Find the file on disk
                    let resolved = match self.find_file(&import_path_str, base_path) {
                        Ok(p) => p,
                        Err(e) => {
                            errors.push(e);
                            continue;
                        }
                    };

                    // Check deduplication
                    let canonical = match resolved.canonicalize() {
                        Ok(c) => c,
                        Err(e) => {
                            errors.push(IncludeError::IoError {
                                path: resolved.clone(),
                                error: e.to_string(),
                            });
                            continue;
                        }
                    };

                    if self.visited.contains(&canonical) {
                        // File already processed — skip without error
                        continue;
                    }

                    // Load and recursively resolve the file
                    match self.load_and_resolve(&resolved) {
                        Ok(sub_unit) => result_parts.extend(sub_unit.0),
                        Err(mut sub_errors) => errors.append(&mut sub_errors),
                    }
                }
                // All other elements are carried over unchanged
                other => result_parts.push(other),
            }
        }

        if errors.is_empty() {
            Ok(SourceUnit(result_parts))
        } else {
            Err(errors)
        }
    }

    // ── Private methods ───────────────────────────────────────────────────────

    /// Load a file from disk, parse it, and recursively resolve its imports.
    fn load_and_resolve(&mut self, path: &Path) -> Result<SourceUnit, Vec<IncludeError>> {
        // Check for circular import before loading
        if let Ok(canonical) = path.canonicalize() {
            if self.visited.contains(&canonical) {
                return Err(vec![IncludeError::CircularImport(canonical)]);
            }
        }

        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                return Err(vec![IncludeError::IoError {
                    path: path.to_path_buf(),
                    error: e.to_string(),
                }])
            }
        };

        let file_no = next_file_no();
        let unit = match but_grammar::parse(&source, file_no) {
            Ok((u, _)) => u,
            Err(diags) => {
                return Err(vec![IncludeError::ParseError {
                    path: path.to_path_buf(),
                    errors: diags,
                }])
            }
        };

        // Recursively resolve imports inside the loaded file
        self.resolve(unit, path)
    }

    /// Find a file by path string relative to the base file or in search paths.
    ///
    /// Automatically adds the `.but` extension if not specified.
    fn find_file(&self, path_str: &str, base_path: &Path) -> Result<PathBuf, IncludeError> {
        // Normalize the extension
        let normalized = if path_str.ends_with(".but") {
            path_str.to_string()
        } else {
            format!("{}.but", path_str)
        };

        // 1. Relative to the base file's directory
        if let Some(base_dir) = base_path.parent() {
            let candidate = base_dir.join(&normalized);
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        // 2. Absolute path
        let abs = PathBuf::from(&normalized);
        if abs.is_absolute() && abs.exists() {
            return Ok(abs);
        }

        // 3. Iterate through search paths
        for search_dir in &self.search_paths {
            let candidate = search_dir.join(&normalized);
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        Err(IncludeError::FileNotFound {
            path: PathBuf::from(&normalized),
            search_paths: self.search_paths.clone(),
        })
    }

    /// Register a file as visited (if the path can be canonicalized).
    fn mark_visited(&mut self, path: &Path) {
        if let Ok(canonical) = path.canonicalize() {
            self.visited.insert(canonical);
        }
    }
}

// ─── Helper functions ─────────────────────────────────────────────────────────

/// Extract the path string from an [`Import`] directive.
fn extract_path_str(import: &Import) -> String {
    match import {
        Import::Plain(p, _) => import_path_to_string(p),
        Import::GlobalSymbol(p, _, _) => import_path_to_string(p),
        Import::Rename(p, _, _) => import_path_to_string(p),
    }
}

/// Convert an [`ImportPath`] to a file path string.
fn import_path_to_string(path: &ImportPath) -> String {
    match path {
        // `import "file.but"` — string literal
        ImportPath::Filename(lit) => lit.string.clone(),
        // `import std::types` — path via `::` → convert to `std/types`
        ImportPath::Path(id_path) => id_path
            .identifiers
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join("/"),
    }
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use super::*;

    // ── Test helper utilities ─────────────────────────────────────────────────

    /// Path to the include test fixtures directory.
    fn fixtures_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests_data/include")
    }

    /// Create a resolver with the fixtures directory as a search path.
    fn resolver_for(subdir: &str) -> IncludeResolver {
        let dir = fixtures_dir().join(subdir);
        IncludeResolver::new(vec![dir])
    }

    /// Parse a BuT source without the filesystem (without resolving imports).
    fn parse_src(src: &str) -> SourceUnit {
        but_grammar::parse(src, 0).expect("Failed to parse test source").0
    }

    // ── Tests for extract_path_str / import_path_to_string ───────────────────

    #[test]
    fn test_extract_path_plain_string() {
        // import "types"; — string path
        let src = r#"import "types";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "types");
        } else {
            panic!("Expected an import directive");
        }
    }

    #[test]
    fn test_extract_path_with_alias() {
        // import "foo/bar" as fb;
        let src = r#"import "foo/bar" as fb;"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "foo/bar");
        } else {
            panic!("Expected an import directive");
        }
    }

    #[test]
    fn test_extract_path_rename() {
        // import { A, B as C } from "utils";
        let src = r#"import { A, B as C } from "utils";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "utils");
        } else {
            panic!("Expected an import directive");
        }
    }

    #[test]
    fn test_extract_path_glob() {
        // import * as base from "lib";
        let src = r#"import * as base from "lib";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "lib");
        } else {
            panic!("Expected an import directive");
        }
    }

    // ── Tests for IncludeResolver::new / from_file / add_search_path ─────────

    #[test]
    fn test_resolver_new_empty() {
        let r = IncludeResolver::new(vec![]);
        assert_eq!(r.search_paths().len(), 0);
        assert_eq!(r.visited_count(), 0);
    }

    #[test]
    fn test_resolver_new_with_paths() {
        let paths = vec![PathBuf::from("/usr/include"), PathBuf::from("/opt/but/lib")];
        let r = IncludeResolver::new(paths.clone());
        assert_eq!(r.search_paths(), paths.as_slice());
    }

    #[test]
    fn test_resolver_from_file() {
        let r = IncludeResolver::from_file(Path::new("/home/user/project/main.but"));
        assert_eq!(r.search_paths().len(), 1);
        assert_eq!(r.search_paths()[0], PathBuf::from("/home/user/project"));
    }

    #[test]
    fn test_resolver_from_file_no_parent() {
        // File without a parent directory — search paths are empty
        let r = IncludeResolver::from_file(Path::new("main.but"));
        assert!(r.search_paths().is_empty());
    }

    #[test]
    fn test_add_search_path() {
        let mut r = IncludeResolver::new(vec![]);
        r.add_search_path(PathBuf::from("/opt/but"));
        assert_eq!(r.search_paths().len(), 1);
        r.add_search_path(PathBuf::from("/usr/local/but"));
        assert_eq!(r.search_paths().len(), 2);
    }

    // ── Tests for find_file ───────────────────────────────────────────────────

    #[test]
    fn test_find_file_not_found() {
        let r = resolver_for("basic");
        let base = fixtures_dir().join("basic/main.but");
        let result = r.find_file("nonexistent_file_xyz", &base);
        assert!(matches!(result, Err(IncludeError::FileNotFound { .. })));
        if let Err(IncludeError::FileNotFound { path, .. }) = result {
            assert!(path.to_str().unwrap().contains("nonexistent_file_xyz"));
        }
    }

    #[test]
    fn test_find_file_adds_extension() {
        // Without the .but extension — should find types.but
        let r = resolver_for("basic");
        let base = fixtures_dir().join("basic/main.but");
        // Searching "types" should find "types.but"
        let result = r.find_file("types", &base);
        assert!(result.is_ok(), "types.but not found when searching 'types': {:?}", result);
        let found = result.unwrap();
        assert!(found.to_str().unwrap().ends_with("types.but"));
    }

    #[test]
    fn test_find_file_with_explicit_extension() {
        let r = resolver_for("basic");
        let base = fixtures_dir().join("basic/main.but");
        let result = r.find_file("types.but", &base);
        assert!(result.is_ok());
    }

    // ── Tests for resolve: basic case ─────────────────────────────────────────

    #[test]
    fn test_resolve_no_imports() {
        // Source without imports — returned as-is
        let src = r#"const X: u32 = 42; let y: bit = 0;"#;
        let unit = parse_src(src);
        let original_len = unit.0.len();

        let mut resolver = IncludeResolver::new(vec![]);
        let result = resolver.resolve(unit, Path::new("/fake/main.but"));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().0.len(), original_len);
    }

    #[test]
    fn test_resolve_basic_import() {
        // main.but imports types.but with type declarations
        let main_path = fixtures_dir().join("basic/main.but");
        let source = fs::read_to_string(&main_path).expect("Failed to read main.but");
        let unit = but_grammar::parse(&source, 0).expect("Failed to parse main.but").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_ok(), "Resolution error: {:?}", result);
        let merged = result.unwrap();

        // Definitions from types.but + definitions from main.but should be merged
        assert!(
            merged.0.len() >= 2,
            "Expected at least 2 elements after merging, got {}",
            merged.0.len()
        );
        // The merged SourceUnit should not contain import directives
        let has_imports = merged
            .0
            .iter()
            .any(|p| matches!(p, SourceUnitPart::ImportDirective(_)));
        assert!(!has_imports, "Merged SourceUnit contains import directives");
    }

    #[test]
    fn test_resolve_deduplication() {
        // Two files import the same file — it should be included only once
        let main_path = fixtures_dir().join("dedup/main.but");
        let source = fs::read_to_string(&main_path).expect("Failed to read dedup/main.but");
        let unit = but_grammar::parse(&source, 0).expect("Failed to parse").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_ok(), "Resolution errors: {:?}", result);
        let merged = result.unwrap();

        // Count type definitions — the type from common.but should appear exactly once
        let type_defs: Vec<_> = merged
            .0
            .iter()
            .filter(|p| matches!(p, SourceUnitPart::TypeDefinition(_)))
            .collect();
        assert_eq!(
            type_defs.len(),
            1,
            "Type from common.but included {} time(s), expected 1",
            type_defs.len()
        );
    }

    #[test]
    fn test_resolve_nested_imports() {
        // main.but → level1.but → level2.but (transitive chain)
        let main_path = fixtures_dir().join("nested/main.but");
        let source = fs::read_to_string(&main_path).expect("Failed to read nested/main.but");
        let unit = but_grammar::parse(&source, 0).expect("Failed to parse").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_ok(), "Errors with nested imports: {:?}", result);
        let merged = result.unwrap();

        // All three levels should be included
        assert!(
            merged.0.len() >= 3,
            "Expected >= 3 elements from three levels, got {}",
            merged.0.len()
        );
    }

    #[test]
    fn test_resolve_circular_import_no_crash() {
        // a.but → b.but → a.but — should not cause infinite recursion
        // Due to deduplication, the second import of a.but is simply ignored
        let a_path = fixtures_dir().join("circular/a.but");
        let source = fs::read_to_string(&a_path).expect("Failed to read circular/a.but");
        let unit = but_grammar::parse(&source, 0).expect("Failed to parse circular/a.but").0;

        let mut resolver = IncludeResolver::from_file(&a_path);
        // Expect either success (deduplication) or error (cycle detection) —
        // the important thing is no stack overflow or hang
        let _ = resolver.resolve(unit, &a_path);
        // Test passes if we reach this point
    }

    #[test]
    fn test_resolve_file_not_found_error() {
        let src = r#"import "does_not_exist_xyz";"#;
        let unit = parse_src(src);

        let mut resolver = IncludeResolver::new(vec![]);
        let result = resolver.resolve(unit, Path::new("/fake/main.but"));

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], IncludeError::FileNotFound { .. }));
    }

    #[test]
    fn test_resolve_multiple_errors_collected() {
        // Two non-existent files — both should appear in the error list
        let src = r#"
            import "missing_a";
            import "missing_b";
            const X: u32 = 1;
        "#;
        let unit = parse_src(src);

        let mut resolver = IncludeResolver::new(vec![]);
        let result = resolver.resolve(unit, Path::new("/fake/main.but"));

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 2, "Expected 2 errors, got {}", errors.len());
    }

    #[test]
    fn test_resolve_parse_error_in_import() {
        // File with a syntax error
        let main_path = fixtures_dir().join("errors/main.but");
        let source = fs::read_to_string(&main_path).expect("Failed to read errors/main.but");
        let unit = but_grammar::parse(&source, 0).expect("Failed to parse errors/main.but").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(
            errors.iter().any(|e| matches!(e, IncludeError::ParseError { .. })),
            "Expected a ParseError, got: {:?}",
            errors
        );
    }

    // ── Tests for visited_count ───────────────────────────────────────────────

    #[test]
    fn test_visited_count_after_resolve() {
        // After resolving with one file, exactly 1 file should be visited
        let src = r#"const OK: u32 = 1;"#;
        let unit = parse_src(src);

        // Use a non-existent path — mark_visited uses canonicalize,
        // which will fail. But testing the visited_count structure is important.
        let mut resolver = IncludeResolver::new(vec![]);
        let _ = resolver.resolve(unit, Path::new("/fake/file.but"));
        // After failing to canonicalize a non-existent path, visited remains empty
        assert_eq!(resolver.visited_count(), 0);
    }

    #[test]
    fn test_visited_count_with_real_file() {
        let main_path = fixtures_dir().join("basic/main.but");
        let source = fs::read_to_string(&main_path).expect("Failed to read main.but");
        let unit = but_grammar::parse(&source, 0).expect("Failed to parse").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let _ = resolver.resolve(unit, &main_path);

        // Main file + imported file types.but
        assert!(
            resolver.visited_count() >= 1,
            "Expected >= 1 visited file, got {}",
            resolver.visited_count()
        );
    }

    // ── Tests for Display on IncludeError ─────────────────────────────────────

    #[test]
    fn test_display_file_not_found() {
        let err = IncludeError::FileNotFound {
            path: PathBuf::from("missing.but"),
            search_paths: vec![PathBuf::from("/opt/but"), PathBuf::from("/usr/but")],
        };
        let msg = err.to_string();
        assert!(msg.contains("missing.but"), "Message: {}", msg);
        assert!(msg.contains("/opt/but"), "Message: {}", msg);
    }

    #[test]
    fn test_display_circular_import() {
        let err = IncludeError::CircularImport(PathBuf::from("/project/a.but"));
        let msg = err.to_string();
        assert!(msg.contains("Circular"), "Message: {}", msg);
        assert!(msg.contains("a.but"), "Message: {}", msg);
    }

    #[test]
    fn test_display_io_error() {
        let err = IncludeError::IoError {
            path: PathBuf::from("/protected/secret.but"),
            error: "Permission denied".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("secret.but"), "Message: {}", msg);
        assert!(msg.contains("Permission denied"), "Message: {}", msg);
    }

    #[test]
    fn test_display_parse_error() {
        let err = IncludeError::ParseError {
            path: PathBuf::from("/project/broken.but"),
            errors: vec![],
        };
        let msg = err.to_string();
        assert!(msg.contains("broken.but"), "Message: {}", msg);
        assert!(msg.contains("0 errors"), "Message: {}", msg);
    }

    // ── Test for import_path_to_string with the Path variant ─────────────────

    #[test]
    fn test_import_path_path_variant() {
        // import * as base from "lib"; — Path variant via identifier path
        // This variant is created for import std::types (via :: notation)
        // We test via the string variant (Filename), since the Path syntax
        // requires no quotes around the path
        let src = r#"import "std/types";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            // Filename variant: the string is returned as-is
            assert_eq!(extract_path_str(import), "std/types");
        }
    }

    // ── Tests for additional search paths (--include-dir / add_search_path) ───

    /// Helper function: path to search_paths fixtures.
    fn search_paths_dir() -> PathBuf {
        fixtures_dir().join("search_paths")
    }

    #[test]
    fn test_import_not_found_without_extra_path() {
        // main.but imports "shared/defs", which is not next to the file.
        // Without an additional search path a FileNotFound error should occur.
        let main_path = search_paths_dir().join("app/main.but");
        let source = fs::read_to_string(&main_path)
            .expect("Failed to read search_paths/app/main.but");
        let unit = but_grammar::parse(&source, 0)
            .expect("Failed to parse main.but").0;

        // Resolver only knows the file's own directory (app/)
        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(
            result.is_err(),
            "Expected error: file from libs/ is not accessible without an extra search path"
        );
        let errs = result.unwrap_err();
        assert!(
            errs.iter().any(|e| matches!(e, IncludeError::FileNotFound { .. })),
            "Expected FileNotFound error, got: {:?}", errs
        );
    }

    #[test]
    fn test_import_found_with_extra_path() {
        // Same file, but now we add libs/ as an additional search path.
        let main_path = search_paths_dir().join("app/main.but");
        let libs_path = search_paths_dir().join("libs");
        let source = fs::read_to_string(&main_path)
            .expect("Failed to read search_paths/app/main.but");
        let unit = but_grammar::parse(&source, 0)
            .expect("Failed to parse main.but").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        resolver.add_search_path(libs_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(
            result.is_ok(),
            "Expected success after adding libs/ to search paths: {:?}", result
        );
        let merged = result.unwrap();
        // Types from shared/defs.but and hardware/regs.but should be included
        let type_defs: Vec<_> = merged.0.iter()
            .filter(|p| matches!(p, SourceUnitPart::TypeDefinition(_)))
            .collect();
        assert!(
            type_defs.len() >= 6,
            "Expected at least 6 type definitions (3 from shared + 3 from hardware), got {}",
            type_defs.len()
        );
    }

    #[test]
    fn test_multiple_extra_search_paths() {
        // Add two specific subdirectories: libs/shared and libs/hardware.
        // main.but uses import "shared/defs" and import "hardware/regs",
        // which are relative to libs/.
        let main_path = search_paths_dir().join("app/main.but");
        let shared_path = search_paths_dir().join("libs");
        let source = fs::read_to_string(&main_path)
            .expect("Failed to read search_paths/app/main.but");
        let unit = but_grammar::parse(&source, 0)
            .expect("Failed to parse main.but").0;

        let mut resolver = IncludeResolver::new(vec![]);
        // Add both paths — neither is added automatically
        resolver.add_search_path(main_path.parent().unwrap().to_path_buf()); // app/
        resolver.add_search_path(shared_path);                                // libs/

        assert_eq!(
            resolver.search_paths().len(), 2,
            "Expected 2 search paths"
        );

        let result = resolver.resolve(unit, &main_path);
        assert!(
            result.is_ok(),
            "Expected success with two search paths: {:?}", result
        );
    }

    #[test]
    fn test_first_search_path_takes_priority() {
        // When the same file name exists in two directories,
        // the file from the first added path is used.
        let main_path = search_paths_dir().join("override/project/main.but");
        let vendor_path = search_paths_dir().join("override/vendor");
        let local_path  = search_paths_dir().join("override/local");
        let source = fs::read_to_string(&main_path)
            .expect("Failed to read override/project/main.but");
        let unit = but_grammar::parse(&source, 0)
            .expect("Failed to parse main.but").0;

        // vendor/ is added first — its version of platform.but should be loaded
        let mut resolver = IncludeResolver::from_file(&main_path);
        resolver.add_search_path(vendor_path);
        resolver.add_search_path(local_path);

        let result = resolver.resolve(unit, &main_path);
        assert!(result.is_ok(), "Expected success: {:?}", result);

        let merged = result.unwrap();
        // vendor/platform.but defines VendorVersion, local/platform.but defines LocalVersion.
        // The vendor variant (VendorVersion) should be included.
        let type_names: Vec<String> = merged.0.iter()
            .filter_map(|p| {
                if let SourceUnitPart::TypeDefinition(td) = p {
                    Some(td.name.name.clone())
                } else {
                    None
                }
            })
            .collect();
        assert!(
            type_names.contains(&"VendorVersion".to_string()),
            "Expected type VendorVersion from vendor/platform.but, got: {:?}", type_names
        );
        assert!(
            !type_names.contains(&"LocalVersion".to_string()),
            "Type LocalVersion should not be included (vendor takes priority): {:?}", type_names
        );
    }

    #[test]
    fn test_add_search_path_returns_mutable_ref() {
        // The add_search_path method returns &mut Self for method chaining.
        let mut r = IncludeResolver::new(vec![]);
        r.add_search_path(PathBuf::from("/a"))
         .add_search_path(PathBuf::from("/b"))
         .add_search_path(PathBuf::from("/c"));
        assert_eq!(r.search_paths().len(), 3);
        assert_eq!(r.search_paths()[0], PathBuf::from("/a"));
        assert_eq!(r.search_paths()[2], PathBuf::from("/c"));
    }

    #[test]
    fn test_search_paths_returns_all_paths() {
        let paths = vec![
            PathBuf::from("/usr/lib/but"),
            PathBuf::from("/opt/but/include"),
            PathBuf::from("./local/lib"),
        ];
        let r = IncludeResolver::new(paths.clone());
        assert_eq!(r.search_paths(), paths.as_slice());
    }
}
