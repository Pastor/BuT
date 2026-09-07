//! Приёмка форматирования LSP на всём корпусе `examples/`.
//!
//! Вынесено в отдельный файл (а не в `lsp_tests.rs`): тот в реестре
//! `scripts/module-size-baseline.txt` (узаконенный долг) и расти не имеет права.
//!
//! "Reformat Code" в IntelliJ идёт через `takt-lsp` `textDocument/formatting` ->
//! [`takt_lang::lsp::formatting_edits`] -> тот же [`takt_lang::format::format_source`],
//! что и `taktc fmt`. Значит результат реформата в IDE совпадает с `taktc fmt` **по
//! построению**. Этот тест закрывает   ("байт-в-байт равно `taktc fmt`") на
//! всём корпусе, а не на одной строке (её проверяет `a6_lsp_and_cli_share_one_core` в
//! `lsp_tests.rs`).

#![cfg(feature = "lsp")]

/// Что получит IDE от `formatting_edits`, применяя полнодиапазонную правку (`0..len`) к
/// документу: либо новый текст правки, либо исходник, если правок нет (документ уже
/// каноничен).
fn apply_full_document(source: &str) -> String {
    match takt_lang::lsp::formatting_edits(source).expect("форматирование удалось")
    {
        None => source.to_string(),
        Some(mut edits) => {
            assert_eq!(edits.len(), 1, "ожидается одна полнодиапазонная правка");
            edits.remove(0).new_text
        }
    }
}

/// Приёмка A2: на **всём корпусе** `examples/` -
///
/// 1. **Идемпотентность.** Канон корпуса (его стережёт `taktc fmt --check` в
///    `precheck.sh`) через LSP не меняется - IDE не пометит файл изменённым на
///    ровном месте.
/// 2. **Тавтология по построению.** На **возмущённом** входе то, что IDE получит
///    от сервера, байт-в-байт равно выводу ядра (`= taktc fmt`). Тавтологию
///    обходят классы дефектов, невидимые глазами в `runIde` (лишний перевод
///    строки, подмена EOL, потеря байта) - здесь они провалили бы assert.
#[test]
fn a2_reformat_matches_lamc_fmt_over_corpus() {
    let examples = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("у grammar/ есть родитель — корень репозитория")
        .join("examples");
    let mut checked = 0usize;
    for entry in std::fs::read_dir(&examples).expect("examples/ читается") {
        let path = entry.expect("запись каталога").path();
        if path.extension().and_then(|e| e.to_str()) != Some("takt") {
            continue;
        }
        let source = std::fs::read_to_string(&path).expect("файл примера читается");
        let name = path.file_name().unwrap().to_string_lossy();

        // (1) канон корпуса -> правок нет.
        assert!(
            takt_lang::lsp::formatting_edits(&source)
                .expect("канон форматируется")
                .is_none(),
            "{name}: канон корпуса обязан идти через LSP без правок"
        );

        // (2) возмущённый вход: то, что получит IDE, == вывод ядра (= taktc fmt).
        let roughed = format!("\n\n  {}\n\n", source.replace(";\n", ";\n\n\n"));
        let from_core = takt_lang::format::format_source(&roughed)
            .unwrap_or_else(|e| panic!("{name}: ядро форматирует возмущённый вход: {e:?}"));
        let to_ide = apply_full_document(&roughed);
        assert_eq!(
            to_ide, from_core,
            "{name}: IDE обязана получить ровно вывод ядра (= taktc fmt)"
        );
        checked += 1;
    }
    assert!(checked > 0, "корпус examples/ не должен быть пуст");
}
