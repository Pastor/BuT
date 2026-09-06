//! Разворот составного порта во всех местах обращения.
//!
//! # Что тестытся
//!
//! - **список мест** - перебором, падающим списком: место, забытое обходом,
//!   печатает обращение к порту, которого в дереве уже нет;
//! - **глубина пути** - вложенная структура и массив внутри структуры;
//! - **судьи направления и индексации** - `SE-026` приходит на вложенную запись
//!   во входной порт (прежде молчал), `SE-027` не приходит на вложенную запись
//!   в выходной (прежде отвергал законное), `SE-117` не приходит на элемент
//!   порта-массива в условии (прежде отвергал законное).

use std::path::PathBuf;
use std::process::Command;

use takt_lang::generator::GenerateOptions;

/// Шапка проб: порт структурного типа и выход, куда попадает значение.
const HEAD: &str = "struct Pair { lo: u8, hi: u8 }\n\
     in cfg: Pair at 0x1000;\n\
     out sum: u8 at 0x2000;\n\
     var ticks: u8 := 0;\n";

/// Места обращения к части порта: `(имя, тело модели после шапки)`.
///
/// Путь здесь длиной **один** - намеренно: место проверяется отдельно от глубины, иначе
/// провал не сказал бы, что именно сломано.
const SITES: &[(&str, &str)] = &[
    (
        "тело блока",
        "start Run { always { ticks := ticks + 1; sum := cfg.lo; } ref Run; }",
    ),
    (
        "условие ребра",
        "start Run { always { ticks := ticks + 1; sum := ticks; } ref Hot: cfg.hi > 3; ref Run; }\n\
         state Hot { always { sum := 9; } ref Hot; }",
    ),
    (
        "именованное условие",
        "cond hot = cfg.hi > 3;\n\
         start Run { always { ticks := ticks + 1; if hot { sum := 1; } } ref Run; }",
    ),
    (
        "условие if",
        "start Run { always { ticks := ticks + 1; if cfg.lo > 1 { sum := 1; } } ref Run; }",
    ),
    (
        "условие loop",
        "start Run { always { ticks := ticks + 1; var i: u8 := 0; \
         loop i < cfg.lo { i := i + 1; } sum := i; } ref Run; }",
    ),
    (
        "условие for",
        "start Run { always { ticks := ticks + 1; var s: u8 := 0; \
         for var i: u8 := 0; i < cfg.lo; i := i + 1 { s := s + 1; } sum := s; } ref Run; }",
    ),
    (
        "разбираемое выражение match",
        "start Run { always { ticks := ticks + 1; \
         match cfg.lo { 0 => { sum := 1; } _ => { sum := 2; } } } ref Run; }",
    ),
    (
        "охранная формула",
        "start Run { : cfg.hi < 200; always { ticks := ticks + 1; sum := ticks; } ref Run; }",
    ),
    (
        "аргумент вызова",
        "fn pick(x: u8) -> u8 { return x + 1; }\n\
         start Run { always { ticks := ticks + 1; sum := pick(cfg.lo); } ref Run; }",
    ),
    (
        "тело every",
        "start Run { every 2ms { sum := cfg.lo; } always { ticks := ticks + 1; } ref Run; }",
    ),
    (
        "тело enter",
        "start Run { enter { sum := cfg.lo; } always { ticks := ticks + 1; } ref Run; }",
    ),
];

/// Путь глубже одного шага: вложенная структура и массив внутри структуры.
const DEEP: &[(&str, &str)] = &[
    (
        "поле вложенной структуры",
        "struct Inner { a: u8, b: u8 }\nstruct Outer { head: u8, tail: Inner }\n\
         in cfg: Outer at 0x1000;\nout sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; sum := cfg.head + cfg.tail.b + ticks; } \
         ref Hot: cfg.tail.b > 3; ref Run; }\nstate Hot { always { sum := 9; } ref Hot; }",
    ),
    (
        "элемент массива в поле структуры",
        "struct Bank { head: u8, xs: [u8;2] }\n\
         in cfg: Bank at 0x1000;\nout sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; sum := cfg.head + cfg.xs[1] + ticks; } \
         ref Hot: cfg.xs[0] > 3; ref Run; }\nstate Hot { always { sum := 9; } ref Hot; }",
    ),
];

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0500_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Порождает C и возвращает текст `.c` вместе с каталогом.
fn c_output(tag: &str, source: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_c(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|d| panic!("порождение C ({tag}): {}", d.message));
    let text = std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение вывода");
    (dir, text)
}

/// Код диагностики компиляции целью `c` (`None` - приняла).
fn refusal(source: &str) -> Option<String> {
    let dir = out_dir("judge");
    let result = takt_lang::compile_to_c(
        "judge",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    );
    let _ = std::fs::remove_dir_all(&dir);
    result.err().and_then(|d| d.code)
}

/// Ни в одном месте обращения не остаётся имени неразвёрнутого порта.
///
/// Тест падает **списком**: пропущенное место - это невалидный вывод у четырёх целей, и
/// знать надо все сразу, а не первое по алфавиту.
#[test]
fn every_access_site_is_split() {
    let mut missed = Vec::new();
    for (index, (name, body)) in SITES.iter().enumerate() {
        let tag = format!("sites{index}");
        let (dir, text) = c_output(&tag, &format!("{HEAD}{body}\n"));
        // Имя развёрнутого порта несёт сегмент поля (`...PORT_CFG_LO`), голое
        // `...PORT_CFG` объявления не имеет: запятая отделяет его от аргумента.
        if text.contains(&format!("{}_PORT_CFG,", tag.to_uppercase())) {
            missed.push((*name).to_string());
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        missed.is_empty(),
        "разворот не дошёл до мест обращения: {}",
        missed.join(", ")
    );
}

/// Путь глубже одного шага сопоставляется с листом целиком.
#[test]
fn deep_path_reaches_the_leaf() {
    let mut missed = Vec::new();
    for (index, (name, source)) in DEEP.iter().enumerate() {
        let tag = format!("deep{index}");
        let (dir, text) = c_output(&tag, &format!("{source}\n"));
        if text.contains(&format!("{}_PORT_CFG,", tag.to_uppercase())) {
            missed.push((*name).to_string());
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        missed.is_empty(),
        "путь не доведён до листа: {}",
        missed.join(", ")
    );
}

/// Вывод для вложенного порта принимает `cc` флагами проверки цели.
///
/// Текстовая проверка формы валидности не доказывает, а класс был именно невалидным
/// выводом при нулевом коде возврата.
#[test]
fn generated_c_passes_the_gate_tool() {
    if !Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("[ПРОПУСК] generated_c_passes_the_gate_tool: нет cc");
        return;
    }
    for (index, (name, source)) in DEEP.iter().enumerate() {
        let tag = format!("deepcc{index}");
        let (dir, _) = c_output(&tag, &format!("{source}\n"));
        let out = Command::new("cc")
            .args([
                "-std=c11",
                "-Wall",
                "-Wextra",
                "-Wno-unused-parameter",
                "-Werror",
                "-c",
                "-o",
            ])
            .arg(dir.join("obj.o"))
            .arg(dir.join(format!("{tag}.c")))
            .arg("-I")
            .arg(&dir)
            .output()
            .expect("запуск cc");
        assert!(
            out.status.success(),
            "cc обязан принять вывод ({name}):\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}

/// Вложенная запись во входной порт отвергается `SE-026`.
///
/// Прежде она принималась молча, и цель печатала присваивание результату чтения - вывод
/// отвергал уже `cc`. Одноуровневая форма отвергалась всегда: расходились не правила, а
/// глубина разбора.
#[test]
fn nested_write_to_input_port_is_refused() {
    let source = "struct Inner { a: u8, b: u8 }\nstruct Outer { head: u8, tail: Inner }\n\
         in cfg: Outer at 0x1000;\nout sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; cfg.tail.b := ticks; sum := ticks; } ref Run; }\n";
    assert_eq!(
        refusal(source).as_deref(),
        Some("SE-026"),
        "запись во входной порт запрещена на любой глубине пути"
    );
}

/// Вложенная запись в выходной порт законна: `SE-027` на ней не приходит.
#[test]
fn nested_write_to_output_port_is_allowed() {
    let source = "struct Inner { a: u8, b: u8 }\nstruct Outer { head: u8, tail: Inner }\n\
         out res: Outer at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; res.head := ticks; \
         res.tail.b := ticks + 1; } ref Run; }\n";
    assert_eq!(
        refusal(source),
        None,
        "левая часть — место записи, а не чтение, на любой глубине пути"
    );
}

/// Элемент порта-массива в условии законен: `SE-117` на нём не приходит.
///
/// Контроль границы: та же запись в теле блока принималась и прежде - расходились
/// дерево условий и дерево выражений, а не правило языка.
#[test]
fn array_port_element_in_condition_is_accepted() {
    let source = "in bus: [u8;2] at 0x1000;\nout sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; sum := ticks + bus[0]; } \
         ref Hot: bus[1] > 3; ref Run; }\nstate Hot { always { sum := 9; } ref Hot; }\n";
    assert_eq!(
        refusal(source),
        None,
        "элемент порта-массива индексируется и в условии"
    );
}

/// **Контроль:** сама проверка `SE-117` не снята - неизвестное имя и скалярная
/// переменная в условии индексироваться по-прежнему не могут.
///
/// Без него правка судьи читалась бы как "пропускать всё": расширение вида объявления
/// (порт наравне с переменной) не должно снимать проверку.
#[test]
fn condition_index_still_judged_for_other_bases() {
    let unknown = "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; sum := ticks; } \
         ref Hot: missing[1] > 3; ref Run; }\nstate Hot { always { sum := 9; } ref Hot; }\n";
    assert_eq!(
        refusal(unknown).as_deref(),
        Some("SE-117"),
        "неизвестное имя индексировать нельзя"
    );
    let scalar = "out sum: u8 at 0x2000;\nvar ticks: u8 := 0;\nvar plain: u8 := 1;\n\
         start Run { always { ticks := ticks + 1; sum := ticks + plain; } \
         ref Hot: plain[1] > 3; ref Run; }\nstate Hot { always { sum := 9; } ref Hot; }\n";
    assert_eq!(
        refusal(scalar).as_deref(),
        Some("SE-117"),
        "скалярную переменную индексировать нельзя и в условии"
    );
}
