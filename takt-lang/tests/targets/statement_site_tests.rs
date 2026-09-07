//! Отказ цели указывает на **оператор** - у всех четырёх целей.
//!
//! # Устройство
//!
//! Носитель стал **потоковым и общим**: конструкторы отказов у `rust`, `st` и `sv` -
//! свободные функции без доступа к контексту, и протаскивать карту через полсотни
//! вызовов значило бы менять сигнатуры ради координаты.

use std::path::PathBuf;
use std::process::Command;

const DIR: &str = "tests/data/site0308";

/// Каталог сборки уникален по тесту.
fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_site0308_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Компилирует фикстуру целью и возвращает stderr (фикстуры непереводимы).
fn refusal(fixture: &str, target: &str) -> String {
    let dir = out_dir(&format!("{}_{target}", fixture.trim_end_matches(".takt")));
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .args(["compile", "-t", target, "-o"])
        .arg(&dir)
        .arg(format!("{DIR}/{fixture}"))
        .output()
        .expect("запуск taktc");
    assert!(
        !out.status.success(),
        "{fixture} целью {target} обязана отвергаться"
    );
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// Предмет: срез в теле состояния - координата оператора у трёх целей.
///
/// Строка 16 фикстуры - `res := mem[1:2];`; строка 7 - объявление `mem`, откуда
/// координату брал бы вывод по операнду.
#[test]
fn slice_in_statement_points_at_the_statement() {
    for target in ["c", "rust", "st"] {
        let err = refusal("slice_stmt.takt", target);
        assert!(
            err.contains("slice_stmt.takt:16:"),
            "цель {target} обязана указать строку оператора:\n{err}"
        );
    }
}

/// Срез массива: то же для цели `sv`.
///
/// Прежде здесь стояло возведение в степень - научила ей обе цели, и проверка потеряла
/// бы предмет. Взят срез: его не переводит ни одна цель, и на нём же проверяется
/// координата у `c`, `rust` и `st` выше.
///
/// Отдельная фикстура для `sv` больше не нужна: агрегат массива она переводит с, то
/// есть до среза доходит.
#[test]
fn slice_in_statement_points_at_the_statement_for_sv() {
    let err = refusal("slice_stmt.takt", "sv");
    assert!(
        err.contains("slice_stmt.takt:16:"),
        "цель sv обязана указать строку оператора:\n{err}"
    );
}

/// **Контроль:** вне оператора позиция оператора не подставляется.
///
/// Без него правка читалась бы как "координата берётся всегда", и протёкшая позиция из
/// соседней генерации выглядела бы достоверной ложью.
///
/// Цель здесь одна - `rust`: на том же входе `c` отказывает раньше и по своей причине
/// (`CC-017`, скалярный инициализатор массива), а `st` его переводит. Брать "все цели"
/// значило бы проверять разные вещи под одним именем.
///
/// Исправлениетура **пользуется** `res`: неиспользуемое объявление штатный фильтр выбрасывает
/// из вывода (ловушка пробы), и первая редакция теста мерила отсутствие отказа вместо
/// его координаты.
#[test]
fn refusal_outside_a_statement_keeps_its_own_position() {
    let err = refusal("slice_init.takt", "rust");
    assert!(
        !err.contains("slice_init.takt:12:") && !err.contains("slice_init.takt:13:"),
        "координата тела состояния в отказ инициализатора попасть не могла:\n{err}"
    );
}
