//! Столкновение имён внутри одного POU у цели `st`.
//!
//! # Что тестытся
//!
//! Отказ `ST-025` на **каждый** вид пары (тест падает **списком** - новый вид
//! объявления, забытый в накопителе, обязан быть назван) и **контроли**: без них
//! правило читалось бы как "цель `st` не переводит похожие имена".

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Пары имён, которые цель печатает в один POU.
///
/// Каждая строка - **отдельный вид** объявления, а не вариация одного: переменная,
/// порт, константа и два служебных имени цели приходят в POU разными путями, и
/// накопитель занятых обязан видеть их все.
const CLASHES: &[(&str, &str)] = &[
    (
        "переменная ↔ константа",
        "const LEVEL: u8 := 7;\n\
         var level: u8 := 0;\n\
         start Run { always { level := LEVEL; } next Done; }\n\
         state Done { }\n",
    ),
    (
        "порт ↔ переменная",
        "in LEVEL: u8;\n\
         var level: u8 := 0;\n\
         start Run { always { level := LEVEL; } next Done; }\n\
         state Done { }\n",
    ),
    (
        "порт ↔ константа",
        "in level: u8;\n\
         const LEVEL: u8 := 7;\n\
         var seen: u8 := 0;\n\
         start Run { always { seen := level + LEVEL; } next Done; }\n\
         state Done { }\n",
    ),
    (
        "порт ↔ порт",
        "in LEVEL: u8;\n\
         out level: u8;\n\
         start Run { always { level := LEVEL; } next Done; }\n\
         state Done { }\n",
    ),
    (
        "служебное 'state' цели",
        "var State: u8 := 0;\n\
         start Run { always { State := State + 1; } next Done; }\n\
         state Done { }\n",
    ),
    (
        "служебное 'is_done' цели",
        "var is_done: u8 := 0;\n\
         start Run { always { is_done := is_done + 1; } next Done; }\n\
         state Done { }\n",
    ),
];

/// Входы, которые цель обязана переводить по-прежнему.
///
/// Контроли обязательны: без них "пары отвергаются" означало бы лишь, что цель
/// отвергает всё похожее. Каждый вход ниже `iec2c` **принимает** - проверено прогоном
/// 2026-09-02.
const CONTROLS: &[(&str, &str)] = &[
    (
        "различные имена",
        "const HEIGHT: u8 := 7;\n\
         var level: u8 := 0;\n\
         start Run { always { level := HEIGHT; } next Done; }\n\
         state Done { }\n",
    ),
    (
        "столкновение при НЕиспользуемом объявлении",
        "const LEVEL: u8 := 7;\n\
         var level: u8 := 0;\n\
         var n: u8 := 0;\n\
         start Run { always { n := n + 1; } next Done; }\n\
         state Done { }\n",
    ),
    // Половина пары используется, половина - нет: только такой вход отличает "проверка
    // после фильтра" от "проверки до него". Вариант выше, где не используются оба
    // имени, мутацию "проверять до фильтра" пропускал - цель не печатает ни одного из
    // имён, и столкновению неоткуда взяться.
    (
        "используемая константа + НЕиспользуемая переменная того же имени",
        "const LEVEL: u8 := 7;\n\
         var level: u8 := 0;\n\
         var n: u8 := 0;\n\
         start Run { always { n := n + LEVEL; } next Done; }\n\
         state Done { }\n",
    ),
    (
        "переменная ↔ имя функции",
        "fn LEVEL(a: u8) -> u8 { return a + 1; }\n\
         var level: u8 := 0;\n\
         start Run { always { level := LEVEL(level); } next Done; }\n\
         state Done { }\n",
    ),
    (
        "переменная ↔ имя состояния",
        "var level: u8 := 0;\n\
         start Run { always { level := level + 1; } next LEVEL; }\n\
         state LEVEL { }\n",
    ),
    (
        "переменная ↔ имя вложенной модели",
        "model Level { var t: u8 := 0; start S { always { t := t + 1; } ref S; } }\n\
         var level: u8 := 0;\n\
         start Run = Level;\n",
    ),
];

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
        .join(format!("takt_0480_{tag}_{thread}"));
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

/// Каждая пара отвергается `ST-025`; тест падает **списком**.
#[test]
fn every_kind_of_clash_is_refused() {
    let mut missed = Vec::new();
    for (kind, source) in CLASHES {
        match compile_st(&kind.replace(' ', "_").replace('↔', "x"), source) {
            Ok(dir) => {
                let _ = std::fs::remove_dir_all(&dir);
                missed.push(format!(
                    "{kind}: цель приняла вход — вывод невалиден для iec2c"
                ));
            }
            Err(err) if err.code.as_deref() != Some("ST-025") => {
                missed.push(format!("{kind}: код {:?}, ожидался ST-025", err.code));
            }
            Err(_) => {}
        }
    }
    assert!(missed.is_empty(), "не отвергнуто:\n{}", missed.join("\n"));
}

/// Отказ называет **обе** стороны пары и сообщает, что модель валидна для прочих целей.
///
/// Без имён отказ бесполезен: автор видит одно объявление и не знает, с чем оно
/// столкнулось (урок `ST-023`, `ST-024`).
#[test]
fn refusal_names_both_sides_and_scope() {
    let err = compile_st("text", CLASHES[0].1).expect_err("ожидался отказ цели");
    assert!(
        err.message.contains("'level'") && err.message.contains("'LEVEL'"),
        "отказ обязан назвать оба имени: {}",
        err.message
    );
    assert!(
        err.message.contains("остаётся валидной"),
        "отказ принадлежит цели — текст обязан это сказать: {}",
        err.message
    );
}

/// **Контроли:** цель по-прежнему переводит, и `iec2c` вывод принимает.
#[test]
fn controls_are_still_translated_and_accepted() {
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
    for (kind, source) in CONTROLS {
        let tag = kind.replace(' ', "_").replace('↔', "x");
        let dir = match compile_st(&tag, source) {
            Ok(dir) => dir,
            Err(err) => {
                broken.push(format!(
                    "{kind}: цель отказала — {:?} {}",
                    err.code, err.message
                ));
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
            if !out.status.success() {
                broken.push(format!(
                    "{kind}: iec2c отверг вывод:\n{}",
                    String::from_utf8_lossy(&out.stdout)
                ));
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        broken.is_empty(),
        "контроли сломаны:\n{}",
        broken.join("\n")
    );
}
