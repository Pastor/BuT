//! Узел-Поддерево составного порта в позиции значения.
//!
//! # Что проверяется
//!
//! - чтение узла поднимается во временную, и в выводе нет имени
//!   неразвёрнутого порта;
//! - запись в узел раздаётся листьям - и значением, и вложенным агрегатом;
//! - `cc` флагами проверки принимает вывод;
//! - позиция, где временную объявить негде, отвечает `SE-130` - с позицией;
//! - обход, названный текстом отказа, работает.

use std::path::PathBuf;
use std::process::Command;

use takt_lang::generator::GenerateOptions;

/// Шапка: порт вложенного структурного типа и выход.
const HEAD: &str = "struct Inner { a: u8, b: u8 }\n\
     struct Outer { head: u8, tail: Inner }\n\
     in cfg: Outer at 0x1000;\n\
     out sum: u8 at 0x2000;\n\
     var ticks: u8 := 0;\n";

/// Чтение узла: поддерево, порт целиком, элемент вложенного массива.
const READS: &[(&str, &str)] = &[
    (
        "поддерево-структура",
        "var holder: Inner;\nstart Run { always { ticks := ticks + 1; \
         holder := cfg.tail; sum := holder.b + ticks; } ref Run; }",
    ),
    (
        "порт целиком",
        "var whole: Outer;\nstart Run { always { ticks := ticks + 1; \
         whole := cfg; sum := whole.head + ticks; } ref Run; }",
    ),
    (
        "аргумент вызова",
        "fn pick(v: Inner) -> u8 { return v.a + v.b; }\n\
         start Run { always { ticks := ticks + 1; sum := pick(cfg.tail) + ticks; } ref Run; }",
    ),
];

/// Запись в узел: значением и вложенным агрегатом.
const WRITES: &[(&str, &str)] = &[
    (
        "узел ← значение",
        "struct Inner { a: u8, b: u8 }\nstruct Outer { head: u8, tail: Inner }\n\
         out res: Outer at 0x2000;\nvar holder: Inner := {5, 6};\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; res.tail := holder; } ref Run; }",
    ),
    (
        "порт ← вложенный агрегат",
        "struct Inner { a: u8, b: u8 }\nstruct Outer { head: u8, tail: Inner }\n\
         out res: Outer at 0x2000;\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; res := {ticks, {2, 3}}; } ref Run; }",
    ),
];

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0501_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

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

/// Код отказа компиляции целью `c` (`None` - приняла).
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

/// Чтение узла не оставляет в выводе имени неразвёрнутого порта.
#[test]
fn subtree_read_is_lifted() {
    let mut missed = Vec::new();
    for (index, (name, body)) in READS.iter().enumerate() {
        let tag = format!("rd0501{index}");
        let (dir, text) = c_output(&tag, &format!("{HEAD}{body}\n"));
        if text.contains(&format!("{}_PORT_CFG,", tag.to_uppercase())) {
            missed.push((*name).to_string());
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        missed.is_empty(),
        "узел обязан собираться из листьев: {}",
        missed.join(", ")
    );
}

/// Запись в узел раздаётся листьям.
#[test]
fn subtree_write_reaches_leaves() {
    let mut missed = Vec::new();
    for (index, (name, source)) in WRITES.iter().enumerate() {
        let tag = format!("wr0501{index}");
        let (dir, text) = c_output(&tag, &format!("{source}\n"));
        let prefix = tag.to_uppercase();
        if text.contains(&format!("{prefix}_PORT_RES,"))
            || !text.contains(&format!("{prefix}_PORT_RES_TAIL_B"))
        {
            missed.push((*name).to_string());
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        missed.is_empty(),
        "запись обязана дойти до каждого листа: {}",
        missed.join(", ")
    );
}

/// Вложенный массив-порт: узлом здесь является внутренний массив.
///
/// Отдельный набор: массив разворачивается всем целям, поэтому у него класс шире - те
/// же `st` и `sv`, которые структурный порт печатают сами.
#[test]
fn array_subtree_is_lifted() {
    let source = "in bus: [[u8;2];2] at 0x1000;\nout sum: u8 at 0x2000;\n\
         var row: [u8;2];\nvar ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; row := bus[0]; sum := row[1] + ticks; } \
         ref Run; }\n";
    let (dir, text) = c_output("ar0501", source);
    assert!(
        !text.contains("AR0501_PORT_BUS,"),
        "внутренний массив обязан собираться из листьев:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Вывод для узла принимает `cc` флагами проверки цели.
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
    for (index, (name, body)) in READS.iter().enumerate() {
        let tag = format!("cc0501{index}");
        let (dir, _) = c_output(&tag, &format!("{HEAD}{body}\n"));
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

/// Исправлениетура корпуса: узел-поддерево в условии ребра.
///
/// Вход живёт в корпусе, а не строкой в тесте: код, которого корпус не даёт, не виден
/// проверкам, гоняющим чужие инструменты (реестр покрытия).
const FIXTURE: &str = "tests/data/subtree0501/subtree_in_condition.takt";

fn fixture() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

/// Узел в условии ребра отвергается `SE-130`, а не печатается невалидным.
#[test]
fn subtree_in_condition_is_refused() {
    assert_eq!(
        refusal(&fixture()).as_deref(),
        Some("SE-130"),
        "в условии временную объявить негде — отказ обязан называть это"
    );
}

/// Отказ несёт позицию использования, а не координату "начало файла".
#[test]
fn refusal_carries_position() {
    let source = fixture();
    let dir = out_dir("pos");
    let err = takt_lang::compile_to_c(
        "pos",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("узел в условии отвергается");
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        matches!(err.loc, takt_lang::diagnostics::Location::Source(..)),
        "позиция обязана быть позицией использования: {:?}",
        err.loc
    );
}

/// **Контроль:** обход, названный текстом отказа, работает.
///
/// Без него отказ обещал бы то, чего нет: узел переносится в переменную модели, и её
/// присваивание этот же проход разворачивает по листьям.
#[test]
fn named_workaround_compiles() {
    let source = format!(
        "{HEAD}var seen: Inner;\n\
         start Run {{ always {{ ticks := ticks + 1; seen := cfg.tail; sum := ticks; }} \
         ref Hot: seen.b > 3; ref Run; }}\n\
         state Hot {{ always {{ sum := 9; }} ref Hot; }}\n"
    );
    assert_eq!(refusal(&source), None, "обход обязан компилироваться");
}

/// **Контроль:** тот же узел у переменной правкой не затронут.
#[test]
fn variable_subtree_is_untouched() {
    let source = "struct Inner { a: u8, b: u8 }\nstruct Outer { head: u8, tail: Inner }\n\
         out sum: u8 at 0x2000;\nvar src: Outer := {1, {2, 3}};\nvar holder: Inner;\n\
         var ticks: u8 := 0;\n\
         start Run { always { ticks := ticks + 1; holder := src.tail; \
         sum := holder.b + ticks; } ref Run; }\n";
    let (dir, text) = c_output("vr0501", source);
    assert!(
        text.contains("model->holder = model->src.tail;"),
        "у переменной узел печатается прямым доступом:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
