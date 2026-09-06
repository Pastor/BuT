//! Экспорт корпуса матрицы для плагинов редакторов.

use std::path::PathBuf;

use super::matrix_probes::{case_name, cases, library_files, source};

/// Корень репозитория: подъём от каталога набора.
fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("у крейта есть родитель")
        .to_path_buf()
}

/// Пишет корпус матрицы в `target/matrix-corpus/`.
#[test]
fn matrix_corpus_is_exported_for_editor_plugins() {
    let dir = repo_root().join("target").join("matrix-corpus");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог корпуса");

    let all = cases();
    for (shape, touch, kind) in &all {
        let name = case_name(*shape, *touch, *kind);
        let case_dir = dir.join(&name);
        std::fs::create_dir_all(&case_dir).expect("каталог случая");
        std::fs::write(case_dir.join("probe.takt"), source(*shape, *touch, *kind))
            .expect("запись пробы");
        // Подключаемые файлы кладутся рядом: разбор плагина обязан пережить и
        // импортирующий файл, и библиотеку.
        for file in library_files(*touch) {
            std::fs::write(case_dir.join(file.name), file.text).expect("запись библиотеки");
        }
    }

    let written = std::fs::read_dir(&dir).expect("каталог читается").count();
    assert_eq!(
        written,
        all.len(),
        "корпус матрицы выгружен не полностью: {written} из {}",
        all.len()
    );
}
