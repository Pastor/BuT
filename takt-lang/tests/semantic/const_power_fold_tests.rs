//! Целая степень в инициализаторе константы.
//!
//! ## Что доказывается
//!
//! Оператор `**` язык принимает с, эталон его исполняет, цели переводят - не знал о нём
//! **только** константный вычислитель.
//!
//! | Потребитель | До фичи |
//! |---|---|
//! | эталон | `v = 0` - значение теряется **молча** |
//! | `c`, `c-hal` | `CC-023` - "узел не прошёл понижение" |
//! | `st`, `st-at` | `SPAN : UINT;` - инициализатор потерян **молча** (`iec2c` принимает) |
//! | `rust` | `(2).wrapping_pow((8) as u32)` - **`rustc` отвергает** (`E0689`) |
//! | `sv`, `sv-mmio` | `SV-002` |
//!
//! Проверяется **вывод целей**, а не факт компиляции: у цели `st` дефект был именно в
//! том, что валидный файл нёс неверное значение. Значенческую сверку с эталоном ведёт
//! `takt-sim` (`conformance_const_power_tests`).

use std::sync::atomic::{AtomicUsize, Ordering};

use takt_lang::generator::GenerateOptions;

/// Порождает код цели во временном каталоге и возвращает его текст.
///
/// Каталог уникален по вызову: тесты идут параллельно.
fn compile(target: &str, src: &str) -> String {
    static SEQ: AtomicUsize = AtomicUsize::new(0);
    let dir = std::env::temp_dir().join(format!(
        "takt_power_fold_{}_{}_{}",
        target.replace('-', "_"),
        std::process::id(),
        SEQ.fetch_add(1, Ordering::Relaxed)
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание временного каталога");
    let out = dir.to_str().expect("путь каталога");
    let opts = GenerateOptions::default();
    let result = match target {
        "c" => takt_lang::compile_to_c("power.takt", src, out, &[], &opts),
        "rust" => takt_lang::compile_to_rust("power.takt", src, out, &[], &opts),
        "st" => takt_lang::compile_to_st("power.takt", src, out, &[], &opts),
        other => panic!("цель '{other}' в этом тесте не предусмотрена"),
    };
    result.unwrap_or_else(|e| panic!("цель '{target}' отказала: {e:?}"));
    let text = std::fs::read_dir(&dir)
        .expect("чтение каталога")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file())
        .map(|e| std::fs::read_to_string(e.path()).unwrap_or_default())
        .collect::<Vec<_>>()
        .join("\n");
    let _ = std::fs::remove_dir_all(&dir);
    text
}

/// Модель с объявлениями-предметом; `lamp` нужен, чтобы значение использовалось
/// (неиспользуемое объявление цели не эмитят - ловушка 0301).
fn model(decls: &str) -> String {
    format!(
        "model M {{\n    out lamp: u8 at 0x600;\n{decls}\n    \
         start S {{ always {{ lamp := probe; }} }}\n}}\nstart Main = M;\n"
    )
}

/// Степень в инициализаторе вычисляется - и одинаково у всех целей.
#[test]
fn power_initializer_is_folded_for_every_target() {
    let src = model("    var probe: u16 := 2 ** 8;");
    for target in ["c", "rust", "st"] {
        let out = compile(target, &src);
        assert!(
            out.contains("256"),
            "цель '{target}': ожидалось свёрнутое значение 256\n{out}"
        );
        assert!(
            !out.contains("2 ** 8") && !out.contains("wrapping_pow"),
            "цель '{target}': степень доехала до вывода — свёртки не было\n{out}"
        );
    }
}

/// Цель `st` больше не теряет инициализатор молча.
///
/// Отдельная проверка, а не частный случай предыдущей: до фичи `st` печатала `probe :
/// UINT;` **без** значения, и такой файл `iec2c` принимал, а `cc` собирал - валидный
/// вывод с неверным значением.
#[test]
fn st_no_longer_drops_the_power_initializer() {
    let out = compile("st", &model("    var probe: u16 := 2 ** 8;"));
    assert!(
        out.contains("probe : UINT := 256;"),
        "цель st обязана объявить probe со значением 256:\n{out}"
    );
}

/// Цель `rust` печатает число, а не вызов метода у литерала.
///
/// `(2).wrapping_pow(...)` - не "стилистика": `rustc` отвергает такой код (`E0689`,
/// ambiguous numeric type) при **нулевом** коде возврата `taktc`.
#[test]
fn rust_prints_a_number_not_a_method_on_a_literal() {
    let out = compile("rust", &model("    var probe: u16 := 2 ** 8;"));
    assert!(
        out.contains("256"),
        "цель rust обязана напечатать свёрнутое значение:\n{out}"
    );
    assert!(
        !out.contains("wrapping_pow"),
        "у литеральной базы `wrapping_pow` не компилируется (E0689)\n{out}"
    );
}

/// Значение нормируется по типу приёмника - (обёртка 0127).
///
/// Контроль того, что фича не завела **своей** нормировки: `2 ** 8` в `u8` обязано дать
/// `0`, ровно как `200 + 100` даёт `44`.
#[test]
fn folded_power_is_normalised_by_the_declared_type() {
    let out = compile("c", &model("    var probe: u8 := 2 ** 8;"));
    assert!(
        out.contains("probe = 0") || out.contains("probe;"),
        "обёртка по типу приёмника обязана дать 0:\n{out}"
    );
    assert!(
        !out.contains("256"),
        "значение обязано быть нормировано типом u8, а не оставлено 256\n{out}"
    );
}

/// Показатель, которого не бывает у целой степени, оставляет запись как есть.
///
/// Граница названа: фича вычисляет то, что вычислимо, и **не заводит нового отказа** -
/// при отрицательном показателе поведение прежнее (`CC-023` у цели `c`, то есть узел до
/// неё доезжает неразвёрнутым).
#[test]
fn negative_exponent_is_left_unfolded() {
    let src = model("    const NEG: i32 := 2 ** -1;\n    var probe: i32 := NEG;");
    let dir = std::env::temp_dir().join(format!("takt_power_neg_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание временного каталога");
    let opts = GenerateOptions::default();
    let result = takt_lang::compile_to_c(
        "power.takt",
        &src,
        dir.to_str().expect("путь каталога"),
        &[],
        &opts,
    );
    let _ = std::fs::remove_dir_all(&dir);
    let err = result.expect_err("отрицательный показатель цель c не переводит");
    assert_eq!(
        err.code.as_deref(),
        Some("CC-023"),
        "ожидался прежний отказ цели, а не новая диагностика: {err:?}"
    );
}

/// Вывод для степени **тождествен** выводу для готового литерала.
///
/// Это тест класса, а не входа: доказывается, что за границей семантики степени не
/// существует вовсе, - значит расходиться потребителям **не по чему** (тот же приём,
/// каким 0143 проверяет константную выдержку). Сверка трасс на одном входе такого
/// утверждения не даёт.
#[test]
fn folded_power_is_indistinguishable_from_the_literal() {
    let power = model("    var probe: u16 := 2 ** 8;");
    let literal = model("    var probe: u16 := 256;");
    for target in ["c", "rust", "st"] {
        assert_eq!(
            compile(target, &power),
            compile(target, &literal),
            "цель '{target}': вывод для `2 ** 8` обязан совпасть с выводом для `256`"
        );
    }
}
