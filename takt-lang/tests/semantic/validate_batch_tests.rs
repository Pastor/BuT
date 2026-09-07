//! проверка `validate` высказывается обо **всех** нарушениях.
//!
//! накопила диагностики **между** проверками: `validate_model_all` собирает все, а не
//! первую. Внутри каждой проверки при этом остался ранний возврат - цикл с `?`, - и две
//! неверные переменные давали **одно** сообщение. Пользователь снова получал цикл
//! "правка -> компиляция", от которого накопление избавляет лишь наполовину.
//!
//! # Правило накопления
//!
//! **Одна диагностика на элемент; высказываются все элементы.** Элемент - это
//! объявление, ребро перехода, именованное условие, оператор `address`. Внутри
//! одного выражения ранний выход **сохранён**: вторая ошибка в том же выражении
//! почти всегда следствие первой - тот же довод, которым оставила
//! терминальными стадии построения дерева.
//!
//! **Граница видна в тестах.** `SE-002` (ссылка на несуществующее состояние) и `SE-003`
//! (имя не найдено) приходят **не** из `validate`, а из стадий построения дерева, и те
//! терминальны по решению языка. Их проверки здесь стоят затем, чтобы граница осталась
//! решением, а не случайностью.

use std::path::PathBuf;
use std::process::Command;

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0151_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Коды ошибок компиляции в порядке печати.
fn error_codes(tag: &str, source: &str) -> Vec<String> {
    let dir = work_dir(tag);
    let path = dir.join("probe.takt");
    std::fs::write(&path, source).expect("запись фикстуры");
    let out = taktc()
        .arg("compile")
        .arg(&path)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    String::from_utf8_lossy(&out.stderr)
        .lines()
        .filter(|l| l.contains("Ошибка компиляции ["))
        .filter_map(|l| l.split('[').nth(1)?.split(']').next().map(str::to_string))
        .collect()
}

/// Два неразрешённых условия перехода - **два** сообщения.
///
/// Тот самый случай, с которого начался разбор: `ref T: qqq > 0;` в одном состоянии и
/// `ref S: www > 0;` в другом давали одну `SE-025`.
#[test]
fn two_unresolved_conditions_give_two_diagnostics() {
    let codes = error_codes(
        "conds",
        "model M {\n    start S {\n        ref T: qqq > 0;\n    }\n    state T {\n        ref S: www > 0;\n    }\n}\nstart Main = M;\n",
    );
    assert_eq!(
        codes,
        vec!["SE-025", "SE-025"],
        "оба ребра обязаны высказаться"
    );
}

/// Два неразрешённых **именованных** условия (`cond`) - **два** сообщения.
///
/// Отдельно от предыдущего теста: `SE-025` рождается в двух местах - на ребре перехода
/// (`validate_state_references`) и в объявлении `cond` (`validate_conditions`). Мутация
/// показала, что первый тест второе место не покрывает: ранний выход там возвращался
/// незамеченным.
#[test]
fn two_unresolved_named_conditions_give_two_diagnostics() {
    let codes = error_codes(
        "named_conds",
        "cond A = qqq > 0;\ncond B = www > 0;\nstart S {\n    ref T: A;\n}\nstate T;\n",
    );
    assert_eq!(codes, vec!["SE-025", "SE-025"], "оба объявления `cond`");
}

/// Две переменные `bit` с недопустимым значением - **два** сообщения.
#[test]
fn two_bad_bit_values_give_two_diagnostics() {
    let codes = error_codes("bits", "var x: bit := 5;\nvar y: bit := 7;\nstart S;\n");
    assert_eq!(codes, vec!["SE-035", "SE-035"]);
}

/// Два массива недопустимого размера - **два** сообщения.
#[test]
fn two_bad_array_sizes_give_two_diagnostics() {
    let codes = error_codes(
        "arrays",
        "var a: [bit;99999] := 0;\nvar b: [bit;99999] := 0;\nstart S;\n",
    );
    assert_eq!(codes, vec!["SE-038", "SE-038"]);
}

/// Два оператора `address` на несуществующие порты - **два** сообщения.
#[test]
fn two_dangling_address_defs_give_two_diagnostics() {
    let codes = error_codes(
        "addr",
        "out a: bit;\naddress nope1 = 0x10;\naddress nope2 = 0x20;\nstart S {\n    always {\n        a := 1;\n    }\n}\n",
    );
    assert_eq!(codes, vec!["SE-048", "SE-048"]);
}

/// Два обращения к ячейке в инициализаторах - **два** сообщения.
#[test]
fn two_anon_initializers_give_two_diagnostics() {
    let codes = error_codes(
        "anon",
        "var a: u8 := #0x100 as u8;\nvar b: u8 := #0x200 as u8;\nstart S;\n",
    );
    assert_eq!(codes, vec!["SE-099", "SE-099"]);
}

/// Разные проверки складываются: накопление не потеряно.
#[test]
fn different_checks_still_accumulate() {
    let codes = error_codes(
        "mixed",
        "in a: bit;\nvar x: bit := 5;\nstart S {\n    always {\n        a := 1;\n    }\n    ref T: qqq > 0;\n}\nstate T;\n",
    );
    assert!(
        codes.len() >= 3,
        "ожидались три разных нарушения: {codes:?}"
    );
    for expected in ["SE-026", "SE-035", "SE-025"] {
        assert!(
            codes.iter().any(|c| c == expected),
            "потеряна диагностика {expected}: {codes:?}"
        );
    }
}

/// **Граница:** стадии построения дерева терминальны.
///
/// `SE-002` и `SE-003` рождаются там, а не в `validate`, поэтому второе нарушение того
/// же вида молчит. Это не дефект накопления: после ошибки построения дерево неполно, и
/// продолжение дало бы сообщения о следствиях.
#[test]
fn build_stage_errors_stay_terminal() {
    let refs = error_codes(
        "stage_refs",
        "start S {\n    ref Nope1;\n}\nstate T {\n    ref Nope2;\n}\n",
    );
    assert_eq!(refs, vec!["SE-002"], "стадия построения — первая ошибка");

    let names = error_codes(
        "stage_names",
        "var a: u8 := zzz1;\nvar b: u8 := zzz2;\nstart S;\n",
    );
    assert_eq!(names, vec!["SE-003"], "стадия построения — первая ошибка");
}

/// **Одна диагностика на элемент.** Внутри одного выражения ранний выход
/// сохранён: вторая ошибка там - следствие первой.
#[test]
fn one_diagnostic_per_element() {
    let codes = error_codes(
        "per_element",
        "var x: bit := 5;\nstart S {\n    ref T: qqq > 0;\n}\nstate T;\n",
    );
    assert_eq!(
        codes.iter().filter(|c| *c == "SE-035").count(),
        1,
        "одно объявление — одно сообщение: {codes:?}"
    );
}

/// Порядок диагностик - по позиции в файле (свойство `normalize`).
///
/// Накопление не должно сделать наблюдаемым порядок обхода `BTreeMap`.
#[test]
fn diagnostics_are_ordered_by_position() {
    let dir = work_dir("order");
    let path = dir.join("probe.takt");
    std::fs::write(
        &path,
        "var x: bit := 5;\nvar y: bit := 7;\nvar z: bit := 9;\nstart S;\n",
    )
    .expect("запись фикстуры");
    let out = taktc()
        .arg("compile")
        .arg(&path)
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    let stderr = String::from_utf8_lossy(&out.stderr);
    let lines: Vec<u32> = stderr
        .lines()
        .filter(|l| l.contains("[SE-035]"))
        .filter_map(|l| l.split(':').nth(1)?.parse().ok())
        .collect();

    assert_eq!(lines.len(), 3, "три объявления — три сообщения: {stderr}");
    let mut sorted = lines.clone();
    sorted.sort_unstable();
    assert_eq!(lines, sorted, "порядок обязан идти по позиции: {lines:?}");
}
