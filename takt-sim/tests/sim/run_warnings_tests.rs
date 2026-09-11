//! Эталон не разговаривает с пользователем из библиотеки.
//!
//! Предупреждения прогона и вывод программы возвращаются вызывающему, а печатает их
//! он - то же правило, что у генераторов. До перевода два предупреждения
//! (`SIM-037` о форме сценария и `SIM-032` о длине позиционного массива) и вывод
//! встроенной функции `debug` уходили `eprintln!` прямо из библиотеки, и потребитель
//! без консоли не получал их вовсе: печать внутри модуля для страницы не существует.
//!
//! Здесь проверяется само правило - что печати в библиотеке не осталось, - и
//! согласованность двух каналов: напечатанная строка собирается тем же носителем,
//! который отдаёт возврат.

use std::path::{Path, PathBuf};

/// Печать в исходнике эталона осталась только там, где ей место.
///
/// Греп по исходнику, а не по поведению: перехватить `stderr` собственного процесса
/// надёжно нельзя, а печать была именно текстом в двух модулях. Тест падает
/// **списком** мест, чтобы третья копия не завелась незамеченной.
///
/// Разрешены два места: `bin/` (это и есть вызывающий) и `runner/mod.rs`, где живёт
/// `run` - цикл печати трассы. Всё остальное обязано возвращать.
#[test]
fn library_does_not_print() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src");
    // Разрешённые места названы поимённо, а не оговоркой про исключения:
    //
    // - `bin/` - это и есть вызывающий;
    // - `runner/mod.rs` - там живёт `run`, цикл печати трассы.
    let allowed = [PathBuf::from("bin"), PathBuf::from("runner").join("mod.rs")];
    let mut offenders = Vec::new();
    let mut stack = vec![root.clone()];
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir).expect("каталог src") {
            let path = entry.expect("запись каталога").path();
            if path.is_dir() {
                stack.push(path);
                continue;
            }
            if path.extension().and_then(|e| e.to_str()) != Some("rs") {
                continue;
            }
            let rel = path.strip_prefix(&root).expect("путь внутри src");
            if allowed.iter().any(|a| rel.starts_with(a)) {
                continue;
            }
            let text = std::fs::read_to_string(&path).expect("файл читается");
            // Тестовый код модуля отсекается: в нём печать - отладка пробы, а не
            // канал диагностики. Комментарии отсекаются тоже, иначе тест ловит
            // рассказ о печати вместо самой печати.
            let code = text.split("#[cfg(test)]").next().unwrap_or(&text);
            let prints = code.lines().any(|line| {
                let trimmed = line.trim_start();
                !trimmed.starts_with("//")
                    && (trimmed.contains("eprintln!") || trimmed.contains("println!"))
            });
            if prints {
                offenders.push(rel.display().to_string());
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "эталон печатает из библиотеки — предупреждения и вывод программы возвращают \
         вызывающему: {offenders:?}"
    );
}

/// Напечатанное совпадает с возвращённым.
///
/// Два канала легко разъезжаются: печать остаётся в `run`, возврат уходит потребителю
/// без консоли, и разойдись у них формат - в консоли и на странице один код выглядел бы
/// по-разному. Строку собирает один носитель, и здесь это проверяется прогоном
/// настоящего бинарника против возврата того же сценария.
#[test]
fn printed_matches_returned() {
    let fixtures = Path::new("tests/data/named0132");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([
            fixtures.join("panel.takt").to_str().expect("путь модели"),
            "-s",
            fixtures
                .join("short_positional.json")
                .to_str()
                .expect("путь сценария"),
            "--steps",
            "2",
        ])
        .output()
        .expect("запуск симулятора");
    let printed = String::from_utf8_lossy(&out.stderr);

    let source = std::fs::read_to_string(fixtures.join("panel.takt")).expect("модель");
    let scenario =
        std::fs::read_to_string(fixtures.join("short_positional.json")).expect("сценарий");
    let returned = super::named_port_scenario_tests::collect_warnings_of(&source, &scenario, 2);
    assert!(!returned.is_empty(), "сценарий обязан дать предупреждения");

    for warning in &returned {
        let line = takt_sim::trace::warning_line(warning);
        assert!(
            printed.contains(&line),
            "напечатанное расходится с возвращённым:\n  строка: {line}\n  печать: {printed}"
        );
    }
}
