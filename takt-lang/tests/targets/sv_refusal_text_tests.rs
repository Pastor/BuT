//! Текст отказа цели `sv` не склеивается с шаблоном.

use std::path::PathBuf;

/// Вход: досрочный возврат - самое развёрнутое сообщение цели.
const SRC: &str = "fn make(k: u8) -> u8 { if k > 0 { return 9; } return 8; }\n\
     var r: u8 := 0;\nout o: u8 at 0;\n\
     start Run { always { r := make(1); o := r; } ref Run: r = 9; }\n";

/// Отказ читается как предложение, а объяснение политики - заметкой.
#[test]
fn refusal_text_reads_as_a_sentence() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0339_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let diagnostic = takt_lang::compile_to_sv(
        "refusal",
        SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect_err("досрочный возврат цель 'sv' не переводит");

    assert_eq!(diagnostic.code.as_deref(), Some("SV-002"));
    assert!(
        diagnostic
            .message
            .starts_with("Не транслируется в SystemVerilog: "),
        "шаблон обязан быть ПРЕФИКСОМ, как у целей `st` и `rust`:\n{}",
        diagnostic.message
    );
    assert!(
        !diagnostic.message.contains("Молчаливо пропустить"),
        "объяснение политики обязано быть заметкой, а не хвостом сообщения:\n{}",
        diagnostic.message
    );
    assert!(
        diagnostic.message.trim_end().ends_with("стоял в конце"),
        "сообщение обязано кончаться текстом автора отказа:\n{}",
        diagnostic.message
    );
    assert_eq!(
        diagnostic.notes.len(),
        1,
        "объяснение политики обязано доехать заметкой: {:?}",
        diagnostic.notes
    );
}

/// Носитель шаблона один.
///
/// Проверка грепом, а не типом: вторая копия - обычная функция, компилятор её не
/// запретит. Падает **списком** мест.
#[test]
fn sv002_has_a_single_definition() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/generator/sv");
    let mut places = Vec::new();
    for entry in std::fs::read_dir(&root).expect("каталог цели sv") {
        let path = entry.expect("запись каталога").path();
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }
        let text = std::fs::read_to_string(&path).expect("чтение модуля");
        for (i, line) in text.lines().enumerate() {
            if line.contains("fn sv002(") {
                places.push(format!("{}:{}", path.display(), i + 1));
            }
        }
    }
    assert_eq!(
        places.len(),
        1,
        "шаблон SV-002 обязан иметь ОДИН носитель, найдено: {places:#?}"
    );
    // Частные обёртки (`sv002_type`, `sv002_width`) строят текст и зовут общий носитель -
    // их наличие законно; тест ловит именно вторую **реализацию** шаблона (`fn
    // sv002(`).
}
