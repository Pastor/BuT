//! Безусловное ребро завершает цепочку - цель `c`.
//!
//! ## Что здесь ловится
//!
//! Ветвь `case` закрывается общим `break;`, поэтому в выводе выходила пара `break;
//! break;` (7 мест в корпусе `examples/generated/c`), а рёбра, записанные после
//! безусловного, печатались недостижимым кодом.
//!
//! Три остальные цели этот вопрос уже решают одинаково: `rust_tick.rs` (`return
//! Ok(true)` с комментарием "в C это молча"), `st_model.rs` ("цепочка на нём
//! заканчивается"), `sv_fsm.rs`. Отставала одна цель - /0193/0195.

use takt_lang::GenerateOptions;

/// Каталог фикстур фичи.
const FIXTURES: &str = "tests/data/cbreak0213";

/// Каталог вывода, уникальный по имени потока: тесты идут параллельно, а каждый прогон
/// начинается с очистки своего каталога.
fn out_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt0213_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Компилирует исходник целью `c` и возвращает текст порождённого `.c`.
fn generate_c(unit: &str, source: &str, search: &[String], tag: &str) -> String {
    let dir = out_dir(tag);
    takt_lang::compile_to_c(
        unit,
        source,
        dir.to_str().expect("путь в UTF-8"),
        search,
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|d| panic!("{unit}: цель c обязана скомпилировать вход: {d:?}"));
    std::fs::read_to_string(dir.join(format!("{unit}.c"))).expect("порождённый .c читается")
}

/// Пары `break;` подряд в тексте: список номеров строк (1-based) второго из пары.
fn redundant_breaks(text: &str) -> Vec<usize> {
    let lines: Vec<&str> = text.lines().collect();
    lines
        .windows(2)
        .enumerate()
        .filter(|(_, w)| w[0].trim() == "break;" && w[1].trim() == "break;")
        .map(|(idx, _)| idx + 2)
        .collect()
}

/// Читает фикстуру фичи.
fn fixture(name: &str) -> (String, String) {
    let path = format!("{FIXTURES}/{name}.takt");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("не прочитать {path}: {e}"));
    (name.to_string(), source)
}

// -- R1/R2 на корпусе ---------------------------------------------------------

/// Ни один пример корпуса не даёт двух `break;` подряд.
///
/// Падает списком "файл: строки", а не первым встреченным местом.
#[test]
fn corpus_has_no_two_breaks_in_a_row() {
    let mut offenders: Vec<String> = Vec::new();
    let mut checked = 0;
    for entry in std::fs::read_dir("../examples").expect("каталог examples") {
        let path = entry.expect("запись каталога").path();
        if path.extension().and_then(|e| e.to_str()) != Some("takt") {
            continue;
        }
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("имя файла")
            .to_string();
        let source = std::fs::read_to_string(&path).expect("исходник читается");
        // Каталог корпуса - путь поиска: пример вправе подключать соседа
        // (`pid_heater.takt` -> `pid_law.takt`), а исходник передаётся строкой.
        let search = ["../examples/include".to_string(), "../examples".to_string()];
        let text = generate_c(&name, &source, &search, &format!("corpus_{name}"));
        let lines = redundant_breaks(&text);
        if !lines.is_empty() {
            offenders.push(format!("{name}.c: строки {lines:?}"));
        }
        checked += 1;
    }
    assert!(
        checked >= 5,
        "ожидалось ≥5 примеров корпуса, найдено {checked}"
    );
    assert!(
        offenders.is_empty(),
        "в порождённом C есть недостижимый `break;` после безусловного перехода:\n  {}",
        offenders.join("\n  ")
    );
}

// -- R1/R2 на фикстуре: хвост после безусловного ребра ------------------------

/// Ребро, записанное после безусловного, целью `c` не печатается.
///
/// Корпус этот класс не покрывает - записи в нём нет ни одной.
#[test]
fn edge_after_unconditional_is_not_emitted() {
    let (unit, source) = fixture("tail_after_unconditional");
    let text = generate_c(&unit, &source, &[], "tail");

    let run = case_body(&text, "TAIL_AFTER_UNCONDITIONAL_RUN");
    assert!(
        run.contains("model->state = TAIL_AFTER_UNCONDITIONAL_DONE;"),
        "безусловный переход обязан остаться:\n{run}"
    );
    assert!(
        !run.contains("TAIL_AFTER_UNCONDITIONAL_LATE"),
        "ребро после безусловного недостижимо и печататься не должно:\n{run}"
    );
    assert_eq!(
        run.matches("break;").count(),
        1,
        "у ветви обязан остаться ровно один `break;` — общий, закрывающий `case`:\n{run}"
    );
    assert!(
        redundant_breaks(&text).is_empty(),
        "двух `break;` подряд быть не должно:\n{text}"
    );
}

// -- Что править было нельзя -------------------------------------------

/// У условного ребра `break;` остаётся - он выходит из `switch`
/// изнутри блока `if`, и без него ветвь провалилась бы в соседнюю.
#[test]
fn conditional_edge_keeps_its_break() {
    let (unit, source) = fixture("conditional_and_terminal");
    let text = generate_c(&unit, &source, &[], "cond");

    let wait = case_body(&text, "CONDITIONAL_AND_TERMINAL_WAIT");
    assert!(
        wait.contains("if (") && wait.contains("model->state = CONDITIONAL_AND_TERMINAL_STOP;"),
        "условный переход печатается блоком `if`:\n{wait}"
    );
    assert_eq!(
        wait.matches("break;").count(),
        2,
        "ожидались два `break;`: внутри блока `if` и общий в конце ветви:\n{wait}"
    );
}

/// Терминальное состояние по-прежнему уходит в `_END` и
/// закрывается единственным `break;`.
#[test]
fn terminal_state_falls_to_end_with_single_break() {
    let (unit, source) = fixture("conditional_and_terminal");
    let text = generate_c(&unit, &source, &[], "term");

    let stop = case_body(&text, "CONDITIONAL_AND_TERMINAL_STOP");
    assert!(
        stop.contains("model->state = CONDITIONAL_AND_TERMINAL_END;"),
        "терминальное состояние обязано уходить в _END:\n{stop}"
    );
    assert_eq!(
        stop.matches("break;").count(),
        1,
        "у терминальной ветви ровно один `break;`:\n{stop}"
    );
}

/// Тело ветви `case <variant>: { ... }` порождённого `switch`.
///
/// Границей служит следующая строка `case ` либо `default:` - вложенных `switch` в теле
/// ветви состояния цель `c` не печатает.
fn case_body(text: &str, variant: &str) -> String {
    let head = format!("case {variant}: {{");
    let mut body = Vec::new();
    let mut inside = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed == head {
            inside = true;
            continue;
        }
        if inside {
            if trimmed.starts_with("case ") || trimmed.starts_with("default:") {
                break;
            }
            body.push(line);
        }
    }
    assert!(inside, "в выводе нет ветви `{head}`:\n{text}");
    body.join("\n")
}
