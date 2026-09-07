//! Цель `c` использует объявленные константы перечисления.
//!
//! ## Что здесь ловится
//!
//! Цель объявляла константы и тут же их **не использовала**:
//!
//! ```c
//! #define ENUM_EN_GO 1
//! ...
//! model->c = 1;              /* а это, оказывается, Go */
//! ```
//!
//! Читатель порождённого C не узнавал, что `1` - это `Go`, не заглянув в `.takt`. Ровно
//! то положение, из которого вывела цель `st`.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Каталог теста уникален по имени потока: тесты идут параллельно.
fn tmp(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace("::", "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0167_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Компилирует исходник целью `c` и возвращает текст `.c`.
fn compile_c(tag: &str, name: &str, source: &str) -> String {
    let dir = tmp(tag);
    takt_lang::compile_to_c(
        &format!("{name}.takt"),
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c");
    std::fs::read_to_string(dir.join(format!("{name}.c"))).expect("вывод цели c")
}

const SINGLE: &str = r#"
enum Command {
    Stop = 0,
    Go = 1,
    Halt = 2
}

var c: Command := 0;
var n: u8 := 0;

start Idle {
    always {
        n := n + 1;
        c := 1;
    }
    ref Run: c = 1;
}

state Run {
    enter { c := 2; }
    ref Idle: n >= 5;
}
"#;

/// Два перечисления одной модели с **одноимённым** вариантом - вход, на котором прежняя
/// форма имени давала дубль макроса.
const CLASHING: &str = r#"
enum Command { Stop = 0, Go = 1 }
enum Mode { Stop = 5, Run = 6 }

var c: Command := 0;
var m: Mode := 5;
var n: u8 := 0;

start Idle {
    always { n := n + 1; c := 1; m := 6; }
    ref Idle: n < 100;
}
"#;

/// A2: присваивание печатается **именем константы**, а не числом.
#[test]
fn assignment_uses_the_constant() {
    let text = compile_c("assign", "en", SINGLE);
    assert!(
        text.contains("model->c = ENUM_EN_COMMAND_GO;"),
        "ожидалось присваивание именем константы, получено:\n{text}"
    );
    assert!(
        text.contains("model->c = ENUM_EN_COMMAND_HALT;"),
        "то же для второго варианта:\n{text}"
    );
}

/// A2: начальная инициализация в `_init` - тоже присваивание.
///
/// Без этого вывод противоречил бы себе внутри одного файла: тело печатает имя, а
/// `_init` - число.
#[test]
fn initializer_uses_the_constant() {
    let text = compile_c("init", "en", SINGLE);
    assert!(
        text.contains("model->c = ENUM_EN_COMMAND_STOP;"),
        "ожидалась инициализация именем константы, получено:\n{text}"
    );
}

/// A3: сравнение с литералом - второе место, где известен тип.
#[test]
fn comparison_uses_the_constant() {
    let text = compile_c("cmp", "en", SINGLE);
    assert!(
        text.contains("model->c == ENUM_EN_COMMAND_GO"),
        "ожидалось сравнение именем константы, получено:\n{text}"
    );
}

/// A1: имя несёт сегмент перечисления, поэтому одноимённые варианты двух перечислений
/// **различимы**.
#[test]
fn constant_name_carries_the_enum_segment() {
    let text = compile_c("clash", "two", CLASHING);
    for expected in [
        "#define ENUM_TWO_COMMAND_STOP 0",
        "#define ENUM_TWO_MODE_STOP 5",
    ] {
        assert!(
            text.contains(expected),
            "ожидалось объявление {expected:?}, получено:\n{text}"
        );
    }
}

/// A1: и порождённый файл компилируется под флагами проверки проекта.
///
/// Прежде здесь был `-Wmacro-redefined`: дубль `#define` с разными значениями.
/// Держалось это лишь тем, что в корпусе одноимённых вариантов нет.
#[test]
fn clashing_variants_compile_under_werror() {
    let dir = tmp("werror");
    takt_lang::compile_to_c(
        "two.takt",
        CLASHING,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель c");
    let out = Command::new("cc")
        .arg("-c")
        .arg("-Wall")
        .arg("-Werror")
        .arg(dir.join("two.c"))
        .arg("-o")
        .arg(dir.join("two.o"))
        .current_dir(&dir)
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C обязан компилироваться под -Wall -Werror:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// A4: **не догадываемся.** Значение вне набора вариантов печатается числом -
/// перечислимой переменной можно присвоить произвольное число, и подмена его именем
/// "похожего" варианта была бы тихой ложью.
#[test]
fn value_outside_the_variants_stays_a_number() {
    let source = r#"
enum Command { Stop = 0, Go = 1 }

var c: Command := 0;
var n: u8 := 0;

start Idle {
    always { n := n + 1; c := 7; }
    ref Idle: n < 100;
}
"#;
    let text = compile_c("outside", "en", source);
    assert!(
        text.contains("model->c = 7;"),
        "значение вне набора вариантов обязано остаться числом:\n{text}"
    );
}

/// A5: продюсер и потребитель зовут **одну** функцию имени - значит всякая
/// использованная константа объявлена.
///
/// Тест на класс, а не на строку: разъехавшись, две формулы дали бы ссылку на
/// несуществующий макрос, и поймал бы это только компилятор C у пользователя.
#[test]
fn every_used_constant_is_declared() {
    for (tag, name, source) in [
        ("used_single", "en", SINGLE),
        ("used_clash", "two", CLASHING),
    ] {
        let text = compile_c(tag, name, source);
        let declared: Vec<String> = text
            .lines()
            .filter_map(|l| l.strip_prefix("#define "))
            .filter_map(|l| l.split_whitespace().next())
            .filter(|n| n.starts_with("ENUM_"))
            .map(|n| n.to_string())
            .collect();
        let used: Vec<String> = text
            .split(|c: char| !(c.is_alphanumeric() || c == '_'))
            .filter(|w| w.starts_with("ENUM_"))
            .map(|w| w.to_string())
            .collect();
        assert!(!declared.is_empty(), "в выводе нет объявлений: {text}");
        assert!(
            used.len() > declared.len(),
            "константы обязаны использоваться, а не только объявляться: {used:?}"
        );
        for u in &used {
            assert!(
                declared.contains(u),
                "использована необъявленная константа {u}: объявлены {declared:?}"
            );
        }
    }
}
