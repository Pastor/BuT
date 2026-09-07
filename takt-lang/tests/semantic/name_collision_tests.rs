//! Столкновения имён при отображении в пространство имён цели.
//!
//! # Три формы, три разных дефекта
//!
//! Карточка называла их "тремя формами одной коллизии"; замер 2026-08-03 показал, что
//! причины и охват разные:
//!
//! | Форма | Причина | Кто ломался | Лечение |
//! |---|---|---|---|
//! | К1 | рассинхрон нормализации имени поля | только `c` | кодоген (эта фича) |
//! | К2 | порт и состояние делят перечислители C | только `c` | префикс `PORT_` в цели `c` |
//! | К3 | состояние-тёзка дочерней модели | **все четыре** цели | отказ семантики - `SE-100` |
//!
//! К2 и К3 лечатся по-разному не по вкусу. К2 ломает одну цель, а запись при этом
//! **естественна** (светофор: порты `red/yellow/green`, состояния `Red/Yellow/Green` -
//! пример документа), поэтому имена разводятся в цели, а не запрещаются. К3 не работает
//! нигде - там запрет живёт в языке.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// К1: имя состояния из **двух** слов (односложное дефект скрывает).
const COMPOUND_STATE: &str = "model M {\n\
                              \x20   var n: u8 := 0;\n\
                              \x20   start Run { always { n := n + 1; } ref Run; }\n\
                              }\n\
                              start TwoWords = M;\n";

/// К2: порт-тёзка состояния той же модели.
const PORT_STATE: &str = "model M {\n\
                          \x20   out settled: bit at 0x600:0;\n\
                          \x20   start Run { ref Settled: 1 = 1; }\n\
                          \x20   state Settled { always { settled := 1; } }\n\
                          }\n\
                          start Main = M;\n";

/// К3: состояние-тёзка **дочерней** модели того же владельца.
const STATE_CHILD_MODEL: &str = "model Pid {\n\
                                 \x20   var n: u8 := 0;\n\
                                 \x20   start Run { always { n := n + 1; } ref Run; }\n\
                                 }\n\
                                 start Pid = Pid;\n";

/// Контрпример к К3: состояние-тёзка **соседней** модели - законно и работает.
const STATE_SIBLING_MODEL: &str = "model Ping {\n\
                                   \x20   var n: u8 := 0;\n\
                                   \x20   start Go { always { n := n + 1; } ref Go; }\n\
                                   }\n\
                                   model Toggle {\n\
                                   \x20   start Ping = Ping { next Done; }\n\
                                   \x20   state Done;\n\
                                   }\n\
                                   start Main = Toggle;\n";

fn build_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .to_string();
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0195_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Код диагностики, которой цель ответила на исходник (или `None` при успехе).
fn target_error(target: &str, tag: &str, source: &str) -> Option<String> {
    let dir = build_dir(tag);
    let path = dir.to_str().expect("путь в UTF-8");
    let opts = GenerateOptions::default();
    let result = match target {
        "c" => takt_lang::compile_to_c(tag, source, path, &[], &opts),
        "rust" => takt_lang::compile_to_rust(tag, source, path, &[], &opts),
        "sv" => takt_lang::compile_to_sv(tag, source, path, &[], &opts),
        "st" => takt_lang::compile_to_st(tag, source, path, &[], &opts),
        other => panic!("неизвестная цель '{other}'"),
    };
    result
        .err()
        .map(|d| format!("{}|{}", d.code.unwrap_or_else(|| "?".into()), d.notes.len()))
}

// -- К1: рассинхрон нормализации ----------------------------------------------

/// **R1/R2:** составное имя состояния даёт C, который собирает `cc`.
///
/// Одно имя, две функции - /0193.
#[test]
fn compound_state_name_produces_compilable_c() {
    let dir = build_dir("k1");
    takt_lang::compile_to_c(
        "two_words",
        COMPOUND_STATE,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c принимает составное имя состояния");

    if !cc_available() {
        eprintln!("[ПРОПУСК] compound_state_name_produces_compilable_c: `cc` не найден");
        return;
    }
    let out = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(&dir)
        .arg("-c")
        .arg(dir.join("two_words.c"))
        .arg("-o")
        .arg(dir.join("two_words.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C обязан компилироваться: имя поля печатается ТОЙ ЖЕ \
         функцией, что и его объявление.\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

// -- К2: разведено префиксом, отказа нет --------------------------------------

/// **R3:** порт-тёзка состояния переводится целью `c`, и вывод собирает `cc`.
///
/// Первая редакция фичи отвергала эту форму (`CC-022`). Отказ снят решением 2026-08-03:
/// предкоммит показал, что под запрет попадает **пример документа** - светофор с
/// портами `red/yellow/green` и состояниями `Red/Yellow/Green`. Это не редкое
/// столкновение, а самое естественное именование автомата, и запрещать его нельзя.
/// Перечислители разведены сегментом `PORT_` (`c_names::port_enum_variant`).
#[test]
fn port_named_like_state_compiles_in_c() {
    let dir = build_dir("k2c");
    takt_lang::compile_to_c(
        "k2c",
        PORT_STATE,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c обязана принять порт-тёзку состояния");

    if !cc_available() {
        eprintln!("[ПРОПУСК] port_named_like_state_compiles_in_c: `cc` не найден");
        return;
    }
    let out = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(&dir)
        .arg("-c")
        .arg(dir.join("k2c.c"))
        .arg("-o")
        .arg(dir.join("k2c.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порт и состояние с одним именем обязаны давать РАЗНЫЕ перечислители: \
         прежде оба печатались одним элементом и `cc` отвечал `redefinition of \
         enumerator`.\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Имя перечислителя порта несёт сегмент `PORT_` - разводящий признак.
///
/// Проверяется **текстом**: сборка `cc` выше доказывает отсутствие столкновения, но не
/// то, что разведение сделано именно так. Имя видно пользователю (оно в сигнатуре
/// HAL-колбэка), поэтому его форма - контракт.
#[test]
fn port_enum_variant_carries_port_segment() {
    let dir = build_dir("k2seg");
    takt_lang::compile_to_c(
        "k2seg",
        PORT_STATE,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let header = std::fs::read_to_string(dir.join("k2seg.h")).expect("заголовок");
    assert!(
        header.contains("K2SEG_M_PORT_SETTLED"),
        "перечислитель порта обязан нести сегмент `PORT_`:\n{header}"
    );
    assert!(
        header.contains("K2SEG_M_SETTLED"),
        "перечислитель состояния остаётся без сегмента — этим они и разведены:\n{header}"
    );
}

// -- К3: отказ принадлежит языку ----------------------------------------------

/// **R4:** состояние-тёзка дочерней модели отвергается **семантикой**,
/// то есть одинаково всеми целями.
#[test]
fn state_named_like_child_model_is_rejected_everywhere() {
    for target in ["c", "rust", "sv", "st"] {
        assert_eq!(
            target_error(target, &format!("k3_{target}"), STATE_CHILD_MODEL).as_deref(),
            Some("SE-100|1"),
            "цель {target}: отказ обязан приходить от семантики с примечанием — \
             эта форма не работает НИ В ОДНОЙ цели"
        );
    }
}

/// **R5:** состояние-тёзка **соседней** модели законно и переводится.
///
/// Граница дефекта, а не перестраховка: первая редакция проверки условия не имела и
/// отвергала эту форму - поймал прогон теста инварианта `S(Ping) = End`, а не чтение
/// кода.
#[test]
fn state_named_like_sibling_model_stays_legal() {
    for target in ["c", "rust", "sv", "st"] {
        assert_eq!(
            target_error(target, &format!("k3ok_{target}"), STATE_SIBLING_MODEL),
            None,
            "цель {target} обязана принять состояние-тёзку соседней модели"
        );
    }
}
