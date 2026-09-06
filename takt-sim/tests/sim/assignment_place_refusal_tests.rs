//! Текст `SIM-017` называет то, за что он остался отвечать.

use std::path::PathBuf;
use std::process::Command;

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_sim_0249_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Гоняет модель эталоном, возвращая (код возврата, stderr + stdout).
fn run(tag: &str, source: &str) -> (Option<i32>, String) {
    let dir = work_dir(tag);
    let path = dir.join("probe.takt");
    std::fs::write(&path, source).expect("запись пробы");
    let result = Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .arg(&path)
        .arg("-n")
        .arg("3")
        .output()
        .expect("запуск takt-sim");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&result.stderr),
        String::from_utf8_lossy(&result.stdout)
    );
    (result.status.code(), text)
}

/// Что обязан нести текст отказа: саму форму и способ её обойти.
const MUST_NAME: &[&str] = &["x[a:b]", "x[a] :="];

/// **Запись среза - `SIM-017`, и текст называет форму и обход.**
#[test]
fn slice_write_refusal_names_the_form_and_the_way_out() {
    let (_, text) = run(
        "slice",
        "var arr: [u8; 4] := { 0, 0, 0, 0 };\nstart Run { always { arr[0:2] := 1; } }\n",
    );
    assert!(text.contains("SIM-017"), "ожидался SIM-017: {text}");
    let missing: Vec<&str> = MUST_NAME
        .iter()
        .filter(|form| !text.contains(**form))
        .copied()
        .collect();
    assert!(
        missing.is_empty(),
        "текст отказа не называет это: {missing:?}; текст: {text}"
    );
    assert!(
        !text.contains("пока не поддерж"),
        "срез не переводит ни одна цель — обещать будущую работу нечем: {text}"
    );
}

/// **Запись разряда отказом больше не является**.
///
/// Контрпример к предыдущему тесту: пока текст перечислял две формы, "починка" могла
/// оставить разряд за отказом и остаться незамеченной.
#[test]
fn bit_write_is_no_longer_refused() {
    let (code, text) = run(
        "bit",
        "var b: u8 := 0;\nstart Run { always { b.2 := 1; } }\n",
    );
    assert!(
        !text.contains("SIM-017"),
        "эталон исполняет запись разряда с фичи 0250: {text}"
    );
    assert_eq!(code, Some(0), "прогон обязан завершиться успехом: {text}");
}

/// **Текст не обещает отказа там, где эталон умеет.**
///
/// Исходная формулировка перечисляла три места и тем утверждала, что порт и ячейка
/// `#АДРЕС` не поддержаны. Тест ловит возврат к такому обещанию: прогон, где записаны
/// переменная, поле, элемент, разряд и порт, обязан пройти.
#[test]
fn places_the_reference_does_execute_are_not_refused() {
    let (code, text) = run(
        "ok",
        "struct Pt { x: u8, y: u8 }\n\
         var n: u8 := 0;\n\
         var p: Pt := { 1, 2 };\n\
         var arr: [u8; 4] := { 0, 0, 0, 0 };\n\
         var flags: u8 := 0;\n\
         out led: bit at 0x100:3;\n\
         start Run { always { n := 1; p.x := 2; arr[1] := 3; flags.3 := 1; led := 1; } }\n",
    );
    assert!(
        !text.contains("SIM-017"),
        "эти места эталон исполняет: {text}"
    );
    assert_eq!(code, Some(0), "прогон обязан завершиться успехом: {text}");
}
