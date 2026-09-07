//! Форма идентификатора у цели `st`: подчёркивание.
//!
//! # Что проверяется
//!
//! - отказ `ST-026` в каждой позиции имени (тест падает **списком**);
//! - контроли: законные формы переводятся, и `iec2c` вывод **принимает**;
//! - отказ принадлежит цели: прочие то же имя переводят.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Исправлениетура корпуса: подчёркивание в конце имени переменной.
const FIXTURE: &str = "tests/data/st/invalid/name_trailing_underscore.takt";

/// Позиции, где имя автора доезжает до вывода `st`.
///
/// Каждая строка - свой путь печати: переменная, константа, функция, порт и поле
/// структуры приходят в вывод разными печатниками, и воронок проверки имени **две**.
const BAD: &[(&str, &str)] = &[
    (
        "константа",
        "out sum: u8 at 0x2000;\nconst CAP_: u8 := 5;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; sum := ticks + CAP_; } ref Run; }\n",
    ),
    (
        "функция",
        "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         fn twice_(v: u8) -> u8 { return v + v; }\n\
         start Run { always { ticks := ticks + 1; sum := twice_(ticks); } ref Run; }\n",
    ),
    (
        "порт",
        "out out_: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; out_ := ticks; } ref Run; }\n",
    ),
    (
        "поле структуры",
        "struct Pair { lo_: u8, hi: u8 }\nout sum: u8 at 0x2000;\n\
         var p: Pair := {1, 2};\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; sum := p.lo_ + ticks; } ref Run; }\n",
    ),
    (
        "два подчёркивания подряд",
        "out sum: u8 at 0x2000;\nvar a__b: u8 := 1;\n\
         start Run { always { a__b := a__b + 1; sum := a__b; } ref Run; }\n",
    ),
];

/// Формы, которые арбитр принимает: цель обязана переводить их по-прежнему.
const GOOD: &[&str] = &["ok_name", "a_b_c", "_lead"];

/// Все входы отказа: позиции из таблицы плюс фикстура корпуса.
///
/// Вход корпуса обязателен: код, которого корпус не даёт, не виден проверкам, гоняющим
/// чужие инструменты, и требует записи в реестре недостижимых.
fn bad_sources() -> Vec<(String, String)> {
    let mut out = vec![(
        "фикстура корпуса".to_string(),
        std::fs::read_to_string(FIXTURE).expect("фикстура читается"),
    )];
    out.extend(
        BAD.iter()
            .map(|(kind, source)| ((*kind).to_string(), (*source).to_string())),
    );
    out
}

fn compile_st(
    tag: &str,
    source: &str,
) -> Result<std::path::PathBuf, takt_lang::diagnostics::Diagnostic> {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0504_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    takt_lang::compile_to_st(
        "probe",
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .map(|_| dir)
}

/// Каждая позиция имени отвергается `ST-026`; тест падает **списком**.
#[test]
fn every_position_is_refused() {
    let mut missed = Vec::new();
    for (kind, source) in bad_sources() {
        let tag = kind.replace(' ', "_");
        match compile_st(&tag, &source) {
            Ok(dir) => {
                let _ = std::fs::remove_dir_all(&dir);
                missed.push(format!("{kind}: цель приняла вход — вывод невалиден"));
            }
            Err(err) if err.code.as_deref() != Some("ST-026") => {
                missed.push(format!("{kind}: код {:?}, ожидался ST-026", err.code));
            }
            Err(_) => {}
        }
    }
    assert!(missed.is_empty(), "не отвергнуто:\n{}", missed.join("\n"));
}

/// Отказ называет форму и говорит, что модель валидна для прочих целей.
#[test]
fn refusal_names_the_form_and_scope() {
    let err = compile_st("text", BAD[0].1).expect_err("ожидался отказ цели");
    assert!(
        err.message.contains("подчёркивание в конце"),
        "отказ обязан назвать форму: {}",
        err.message
    );
    assert!(
        err.message.contains("остаётся валидной"),
        "отказ принадлежит цели — текст обязан это сказать: {}",
        err.message
    );
    let doubled = compile_st(
        "dbl",
        BAD.iter()
            .find(|(kind, _)| *kind == "два подчёркивания подряд")
            .expect("вид есть в таблице")
            .1,
    )
    .expect_err("ожидался отказ цели");
    assert!(
        doubled.message.contains("два подчёркивания подряд"),
        "вторая форма обязана называться своей: {}",
        doubled.message
    );
}

/// **Контроли:** законные формы переводятся, и `iec2c` вывод принимает.
///
/// Без них правило читалось бы как "цель не переводит имён с подчёркиванием"; `_lead`
/// здесь ключевой - стандарт его запрещает, а арбитр принимает, и правило берётся у
/// арбитра.
#[test]
fn legal_forms_are_still_translated() {
    let prefix = std::env::var("IEC2C_PREFIX")
        .unwrap_or_else(|_| format!("{}/.local", std::env::var("HOME").unwrap_or_default()));
    let iec2c = std::path::Path::new(&prefix).join("bin").join("iec2c");
    let lib = std::path::Path::new(&prefix)
        .join("share")
        .join("matiec")
        .join("lib");
    let arbiter = iec2c.is_file() && lib.join("ieclib.txt").is_file();
    if !arbiter {
        eprintln!("[ПРОПУСК] арбитра iec2c нет — проверяется только перевод");
    }
    let mut broken = Vec::new();
    for name in GOOD {
        let source = format!(
            "out sum: u8 at 0x2000;\nvar {name}: u8 := 1;\n\
             start Run {{ always {{ {name} := {name} + 1; sum := {name}; }} ref Run; }}\n"
        );
        let dir = match compile_st(&format!("ok_{}", name.trim_start_matches('_')), &source) {
            Ok(dir) => dir,
            Err(err) => {
                broken.push(format!("{name}: цель отказала — {:?}", err.code));
                continue;
            }
        };
        if arbiter {
            let out_dir = dir.join("iec");
            std::fs::create_dir_all(&out_dir).expect("каталог iec2c");
            let out = Command::new(&iec2c)
                .arg("-I")
                .arg(&lib)
                .arg("-T")
                .arg(&out_dir)
                .arg(dir.join("probe.st"))
                .output()
                .expect("запуск iec2c");
            let text = String::from_utf8_lossy(&out.stderr).to_string()
                + &String::from_utf8_lossy(&out.stdout);
            if text.contains("error") {
                broken.push(format!("{name}: iec2c отверг вывод — {text}"));
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(broken.is_empty(), "законные формы:\n{}", broken.join("\n"));
}

/// **Контроль:** отказ принадлежит цели - прочие то же имя переводят.
#[test]
fn other_targets_accept_the_same_name() {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0504_others_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let path = dir.to_str().expect("путь");
    takt_lang::compile_to_c("probe", BAD[0].1, path, &[], &GenerateOptions::default())
        .expect("цель `c` обязана переводить имя с подчёркиванием в конце");
    takt_lang::compile_to_rust("probe", BAD[0].1, path, &[], &GenerateOptions::default())
        .expect("цель `rust` обязана переводить это имя");
    takt_lang::compile_to_sv("probe", BAD[0].1, path, &[], &GenerateOptions::default())
        .expect("цель `sv` обязана переводить это имя");
    let _ = std::fs::remove_dir_all(&dir);
}
