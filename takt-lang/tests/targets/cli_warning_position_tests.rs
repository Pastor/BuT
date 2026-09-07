//! предупреждение `taktc compile` несёт позицию.
//!
//! Без разрешения номера файла предупреждение печатается **без координат вовсе**:
//!
//! ```text
//! Предупреждение [SE-036]: переменная 'BadVar' объявлена, но нигде не используется
//! ```
//!
//! -- тогда как ошибка того же прогона выглядела как
//! `probe.takt:2:17: Ошибка компиляции [SE-034]: ...`. Координата в диагностике
//! **была** (`var.loc()`); терялась она на печати: `format_warning` зовёт
//! `position_prefix`, а тот без пути к файлу возвращает пустую строку. Путь
//! предупреждению никто не ставил.
//!
//! # Что здесь ловится
//!
//! 1. **Позиция доезжает** - у предупреждений "своего" файла и у целей `c-hal`
//!    (там формат вообще был написан литералом, второй копией).
//! 2. **Путь берётся из реестра файлов, а не из имени входа.** У диагностики
//!    импортированного файла смещения принадлежат **чужому** тексту: штамп "путь
//!    входа" дал бы верный код с неверными координатами. Тест на импорт держит
//!    именно это.
//! 3. **Форма префикса та же, что у ошибки** - формат диагностики есть её
//!    свойство, а не свойство того, кто печатает.
//! 4. **`--quiet` по-прежнему глушит** - в том числе на пути целей `c-hal`.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Неиспользуемая переменная (`SE-036`) - предупреждение "своего" файла.
const UNUSED_VAR: &str = "\
model M {
    var unused_var: u8 := 1;
    start S;
}
start Main = M;
";

/// Однобитный порт без позиции бита (`SE-090`) - предупреждение цели `c-hal`, где
/// формат печати был отдельной копией.
const BIT_PORT_WITHOUT_POSITION: &str = "\
model M {
    out lamp: bit at 0x40000000;
    in button: bit at 0x40000004;
    start S {
        always {
            lamp := button;
        }
    }
}
start Main = M;
";

/// Библиотека с неиспользуемой переменной: предупреждение принадлежит **ей**, а не
/// импортёру.
const LIBRARY: &str = "\
var lib_unused: u8 := 7;
model Lib {
    start S;
}
";

const IMPORTER: &str = "\
import \"lib.takt\";
model M {
    start S;
}
start Main = M;
";

/// Файл с семантической ошибкой (`SE-034`) - эталон формы префикса.
const BROKEN: &str = "\
model M {
    var v: NoSuchType := 1;
    start S {
        always {
            v := 1;
        }
    }
}
start Main = M;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0228_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// stderr прогона `taktc compile <файл>` из каталога теста.
fn compile_stderr(dir: &Path, file: &str, extra: &[&str]) -> String {
    let out = taktc()
        .arg("compile")
        .args(extra)
        .arg(dir.join(file))
        .arg("-o")
        .arg(dir.join("out"))
        .output()
        .expect("запуск taktc compile");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// Строка предупреждения с данным кодом.
fn warning_line(stderr: &str, code: &str) -> String {
    stderr
        .lines()
        .find(|l| l.contains(&format!("Предупреждение [{code}]")))
        .unwrap_or_else(|| panic!("нет предупреждения {code} в выводе:\n{stderr}"))
        .to_string()
}

/// Разбирает префикс `путь:строка:колонка: ` в тройку.
///
/// Возвращает `None`, если префикса нет вовсе - ровно то состояние, в котором
/// предупреждения жили до фичи.
fn position_of(line: &str) -> Option<(String, u32, u32)> {
    let (head, _) = line.split_once(": Предупреждение")?;
    let mut parts = head.rsplitn(3, ':');
    let column = parts.next()?.parse().ok()?;
    let line_no = parts.next()?.parse().ok()?;
    let path = parts.next()?.to_string();
    Some((path, line_no, column))
}

/// **Позиция доезжает до пользователя.**
///
/// Проверяются все три составляющие префикса: путь называет файл, строка и колонка
/// указывают на объявление.
#[test]
fn warning_carries_path_line_and_column() {
    let dir = work_dir("unused");
    std::fs::write(dir.join("probe.takt"), UNUSED_VAR).expect("запись фикстуры");
    let stderr = compile_stderr(&dir, "probe.takt", &[]);

    let line = warning_line(&stderr, "SE-036");
    let (path, line_no, column) =
        position_of(&line).unwrap_or_else(|| panic!("у предупреждения нет позиции: {line:?}"));
    assert!(
        path.ends_with("probe.takt"),
        "путь обязан называть файл: {path:?}"
    );
    assert_eq!((line_no, column), (2, 5), "позиция объявления: {line:?}");
}

/// **Цель `c-hal`: там формат печати был отдельной копией** (собственный
/// `eprintln!` вместо `format_warning`), поэтому позиции у неё не было даже после
/// правки общей точки. Копия устранена - путь ведёт через ту же функцию.
#[test]
fn hal_target_warning_carries_position() {
    let dir = work_dir("hal");
    std::fs::write(dir.join("probe.takt"), BIT_PORT_WITHOUT_POSITION).expect("запись фикстуры");
    let stderr = compile_stderr(&dir, "probe.takt", &["-t", "c-hal"]);

    let line = warning_line(&stderr, "SE-090");
    let (path, line_no, _) =
        position_of(&line).unwrap_or_else(|| panic!("у предупреждения нет позиции: {line:?}"));
    assert!(path.ends_with("probe.takt"), "путь: {path:?}");
    assert_eq!(
        line_no, 2,
        "первое предупреждение — о порте `lamp`: {line:?}"
    );
}

/// **Путь берётся из реестра файлов, а не из имени входа.**
///
/// Предупреждение об объявлении **библиотеки** обязано называть `lib.takt`: смещения
/// принадлежат её тексту, и подстановка пути импортёра дала бы верный код с
/// координатами, указывающими не туда.
#[test]
fn imported_warning_names_the_library_file() {
    let dir = work_dir("import");
    std::fs::write(dir.join("lib.takt"), LIBRARY).expect("запись библиотеки");
    std::fs::write(dir.join("probe.takt"), IMPORTER).expect("запись импортёра");
    let stderr = compile_stderr(&dir, "probe.takt", &[]);

    let line = warning_line(&stderr, "SE-036");
    let (path, _, _) =
        position_of(&line).unwrap_or_else(|| panic!("у предупреждения нет позиции: {line:?}"));
    assert!(
        path.ends_with("lib.takt"),
        "предупреждение обязано называть файл, которому принадлежит: {path:?}"
    );
    assert!(
        !path.ends_with("probe.takt"),
        "путь импортёра здесь означал бы неверные координаты: {path:?}"
    );
}

/// **Форма префикса - та же, что у ошибки.**
///
/// Сравниваются не тексты (они разные по существу), а **форма позиции**: имя файла,
/// строка и колонка на своих местах.
#[test]
fn warning_prefix_matches_error_prefix() {
    let dir = work_dir("shape");
    std::fs::write(dir.join("warn.takt"), UNUSED_VAR).expect("фикстура с предупреждением");
    std::fs::write(dir.join("err.takt"), BROKEN).expect("фикстура с ошибкой");

    let warn = compile_stderr(&dir, "warn.takt", &[]);
    let warn_line = warning_line(&warn, "SE-036");
    let (warn_path, _, _) = position_of(&warn_line).expect("позиция предупреждения");

    let err = compile_stderr(&dir, "err.takt", &[]);
    let err_line = err
        .lines()
        .find(|l| l.contains("Ошибка компиляции [SE-034]"))
        .unwrap_or_else(|| panic!("нет ошибки SE-034:\n{err}"))
        .to_string();
    let (err_path, _, _) = {
        let (head, _) = err_line
            .split_once(": Ошибка компиляции")
            .expect("префикс ошибки");
        let mut parts = head.rsplitn(3, ':');
        let column: u32 = parts.next().expect("колонка").parse().expect("число");
        let line_no: u32 = parts.next().expect("строка").parse().expect("число");
        (parts.next().expect("путь").to_string(), line_no, column)
    };

    assert!(warn_path.ends_with("warn.takt"), "путь: {warn_path:?}");
    assert!(err_path.ends_with("err.takt"), "путь: {err_path:?}");
}

/// **`--quiet` глушит и на пути целей `c-hal`.**
///
/// Правка задела именно эту ветвь печати, поэтому проверка стоит здесь, а не полагается
/// на соседний тест: тот гоняет цель `c`.
#[test]
fn quiet_suppresses_hal_warnings() {
    let dir = work_dir("quiet");
    std::fs::write(dir.join("probe.takt"), BIT_PORT_WITHOUT_POSITION).expect("запись фикстуры");
    let stderr = compile_stderr(&dir, "probe.takt", &["-t", "c-hal", "--quiet"]);

    assert!(
        !stderr.contains("Предупреждение"),
        "--quiet обязан заглушить предупреждения цели c-hal:\n{stderr}"
    );
}
