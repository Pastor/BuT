//! Трансляция `S(Модель) = Состояние` в цель `c`.
//!
//! # Что охраняется
//!
//! `S` - встроенная функция языка: **текущее состояние** модели
//! (`semantic/builtin.rs`). Семантика конструкцию полностью поддерживает и разрешает
//! имя состояния **в области видимости модели-аргумента** (`validate.rs`, диагностика
//! `SE-033`) - именно поэтому имя остаётся `Unresolved` в общем конвейере, что и
//! закреплено критическим инвариантом (`CLAUDE.md`: проход `resolve_state_references`
//! запрещён).
//!
//! Генератор C её **не переводил**: `S(...)` приходит как `ConditionNode::Function`, а
//! не `Model`, и упирался в `CC-003`.
//!
//! # Ожидания захвачены зондом
//!
//! Все строки C - из реального вывода `taktc`, не угаданы (`CLAUDE.md`). Каждая
//! исправлениетура дополнительно **компилируется** `cc -std=c11 -Wall -Werror`: верный путь к
//! полю в C - это то, что подтверждает только компилятор.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::{GenerateOptions, compile_to_c};

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_state_ref_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn generate(
    src: &str,
    name: &str,
    dir: &Path,
) -> Result<String, takt_lang::diagnostics::Diagnostic> {
    compile_to_c(
        name,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )?;
    Ok(std::fs::read_to_string(dir.join(format!("{name}.c"))).expect(".c порождён"))
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Компилирует порождённый C настоящим компилятором.
///
/// Путь к полю чужой структуры проверяется только так: строка вида
/// `model->entry.ping0.state` выглядит правдоподобно и в тесте на `contains` прошла бы -
/// а `cc` отвечает "no member named 'entry'".
fn assert_c_compiles(dir: &Path, name: &str) {
    if !cc_available() {
        eprintln!("[ПРОПУСК] компилятор `cc` не найден — {name} не проверен сборкой");
        return;
    }
    let out = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-c"])
        .arg(dir.join(format!("{name}.c")))
        .arg("-o")
        .arg(dir.join("out.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Модель-**сестра**: обе под-модели корня, условие в одной про состояние другой.
///
/// Путь обязан идти через `main`: своя структура (`SrefPong`) поля `entry` не имеет.
/// Именно здесь прежняя безусловная база `model->` давала невалидный C.
const SIBLING: &str = r#"
model Ping {
    var n: u8 := 0;
    start Run {
        always { n := n + 1; }
        ref Done: n > 2;
    }
    state Done;
}

model Pong {
    var m: u8 := 0;
    start Go {
        always { m := 1; }
        ref Stop: S(Ping) = Done;
    }
    state Stop;
}

start Entry = (Ping | Pong);
"#;

#[test]
fn sibling_model_state_goes_through_main() {
    let dir = temp_dir("sibling");
    let c = generate(SIBLING, "sref", &dir).expect("порождение C");
    assert!(
        c.contains("if (main->entry.ping0.state == SREF_PING_DONE) {"),
        "состояние модели-сестры адресуется от корня:\n{c}"
    );
    assert_c_compiles(&dir, "sref");
}

/// Под-модель **владельца**: путь идёт через свою структуру.
const NESTED: &str = r#"
model Outer {
    model Inner {
        var k: u8 := 0;
        start Work { always { k := k + 1; } ref Fin: k > 1; }
        state Fin;
    }
    var q: u8 := 0;
    start Hold = Inner {
        next Ready;
    }
    state Ready {
        always { q := 1; }
        ref Done: S(Inner) = Fin;
    }
    state Done;
}
start Entry = Outer;
"#;

#[test]
fn own_submodel_state_goes_through_model() {
    let dir = temp_dir("nested");
    let c = generate(NESTED, "nest", &dir).expect("порождение C");
    assert!(
        c.contains("if (model->hold.state == NEST_OUTER_INNER_FIN) {"),
        "состояние своей под-модели адресуется через свою структуру:\n{c}"
    );
    assert_c_compiles(&dir, "nest");
}

/// **Вложенная** сестра: обе модели - дети общего предка, который сам не корень.
///
/// Прямого пути у владельца нет, но корень владеет всем по значению, поэтому адрес
/// собирается цепочкой полей от него.
const DEEP_SIBLING: &str = r#"
model Mid {
    model Target {
        var k: u8 := 0;
        start Work { always { k := k + 1; } ref Fin: k > 1; }
        state Fin;
    }
    model Watcher {
        var q: u8 := 0;
        start Go {
            always { q := 1; }
            ref Stop: S(Target) = Fin;
        }
        state Stop;
    }
    start Run = (Target | Watcher);
}
start Entry = Mid;
"#;

#[test]
fn deeply_nested_sibling_state_is_addressed_from_root() {
    let dir = temp_dir("deep");
    let c = generate(DEEP_SIBLING, "deep2", &dir).expect("порождение C");
    assert!(
        c.contains("if (main->entry.run.target0.state == DEEP2_MID_TARGET_FIN) {"),
        "состояние вложенной сестры собирается цепочкой от корня:\n{c}"
    );
    assert_c_compiles(&dir, "deep2");
}

/// Отрицание: `S(М) != Состояние` - та же трансляция с `!=`.
///
/// Ветки `=` и `!=` были дословными копиями и слиты в одну; тест охраняет от того,
/// чтобы при слиянии оператор не "слипся" в `==`.
#[test]
fn not_equal_uses_not_equal_operator() {
    let dir = temp_dir("neq");
    let src = SIBLING.replace("S(Ping) = Done", "S(Ping) != Done");
    let c = generate(&src, "sref", &dir).expect("порождение C");
    assert!(
        c.contains("if (main->entry.ping0.state != SREF_PING_DONE) {"),
        "`!=` обязан остаться `!=`:\n{c}"
    );
    assert_c_compiles(&dir, "sref");
}

/// **Скобочная форма `S(...)` канонизируется - скобки прозрачны**.
///
/// До 0074 обёртка `Parenthesis` ломала распознавание паттерна `S(Модель) = Состояние`
/// и давала `SE-025`. Теперь `resolve_condition` снимает прозрачные скобки в трёх
/// позициях паттерна (вокруг `S(...)`, вокруг модели-аргумента, вокруг имени
/// состояния), поэтому любая скобочная форма даёт C, **байт-в-байт равный**
/// бесскобочной `S(Ping) = Done`.
///
/// Это и есть граница из: старый тест
/// `parenthesised_state_of_is_rejected_by_semantics` инвертирован - "отвергается" стало
/// "принимается и канонично".
#[test]
fn parenthesised_state_of_is_canonical() {
    let canon = generate(SIBLING, "sref", &temp_dir("paren_canon")).expect("эталон C");
    for form in [
        "(S(Ping)) = Done",
        "S((Ping)) = Done",
        "S(Ping) = (Done)",
        "((S((Ping)))) = (Done)",
    ] {
        let dir = temp_dir("paren");
        let src = SIBLING.replace("S(Ping) = Done", form);
        let c = generate(&src, "sref", &dir)
            .unwrap_or_else(|e| panic!("форма `{form}` должна компилироваться, получено {e:?}"));
        assert_eq!(
            c, canon,
            "форма `{form}`: C обязан быть байт-в-байт равен бесскобочному эталону"
        );
        assert_c_compiles(&dir, "sref");
    }
}

/// Отрицание с любой скобочной формой - так же канонично (та же трансляция `!=`).
#[test]
fn parenthesised_state_of_not_equal_is_canonical() {
    let canon_src = SIBLING.replace("S(Ping) = Done", "S(Ping) != Done");
    let canon = generate(&canon_src, "sref", &temp_dir("pneq_canon")).expect("эталон C");
    for form in ["(S(Ping)) != Done", "S((Ping)) != (Done)"] {
        let dir = temp_dir("pneq");
        let src = SIBLING.replace("S(Ping) = Done", form);
        let c = generate(&src, "sref", &dir)
            .unwrap_or_else(|e| panic!("форма `{form}` должна компилироваться, получено {e:?}"));
        assert_eq!(
            c, canon,
            "форма `{form}`: C обязан совпасть с бесскобочным `!=`"
        );
        assert_c_compiles(&dir, "sref");
    }
}

/// Скобочная форма с несуществующим состоянием отсекается **семантикой** (`SE-033`),
/// как и бесскобочная - канонизация не меняет диагностику по существу, лишь снимает
/// обёртку до сопоставления.
#[test]
fn parenthesised_unknown_state_is_se033() {
    let dir = temp_dir("paren_se033");
    let src = SIBLING.replace("S(Ping) = Done", "(S(Ping)) = NoSuchState");
    let diag = generate(&src, "sref", &dir).expect_err("состояния нет — ожидается отказ");
    assert_eq!(
        diag.code.as_deref(),
        Some("SE-033"),
        "скобочная форма с неизвестным состоянием: ожидалась SE-033, получено {diag:?}"
    );
}

/// Состояние, которого у модели-аргумента нет, отсекает **семантика** (`SE-033`) - до
/// кодогенерации: имя ищется в области видимости аргумента.
///
/// Тест закрепляет разделение ответственности: генератору такие входы не приходят, и
/// `CC-011` для этого случая - не его забота.
#[test]
fn unknown_state_of_argument_model_is_semantic_error() {
    let dir = temp_dir("se033");
    let src = SIBLING.replace("S(Ping) = Done", "S(Ping) = NoSuchState");
    let diag = generate(&src, "sref", &dir).expect_err("состояния нет — ожидается отказ");
    assert_eq!(
        diag.code.as_deref(),
        Some("SE-033"),
        "ожидалась семантическая диагностика, получено: {diag:?}"
    );
}
