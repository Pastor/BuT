//! эталонная модель порождает **компилируемый** C.
//!
//! Тест `syntax_simple` (в `lib.rs`) стоял на модели-"всё сразу" (`SRC`), которая
//! **никогда** не давала компилируемый C: `out`-порт типа `u8` (= `[bit;8]`) с
//! бит-доступом использует `read_numeric`, которого нет в структуре, а `const`
//! типа `[bit;8]` с инициализатором `{...}` печатается макросом-массивом, но
//! бит-доступ трактует его как число (`{...} >> 5` - синтаксическая ошибка cc). Оба
//! дефекта упираются в семантику `[bit;N]` - "число или массив?".
//! Поэтому `syntax_simple` был вынужден проверять **строку** в `.c`, а не
//! компиляцию.
//!
//! Здесь эталон **разделён**: `parse_simple` в `lib.rs`
//! по-прежнему разбирает полный `SRC` (покрытие парсера всех конструкций), а
//! **компиляционная** проверка ведётся на модели [`SYNTH_SRC`], очищенной от
//! конструкций, которые генератор C не поддерживает. Ключевое, что тест охраняет,
//! -- перевод межмодельной ссылки `S(Ping) = End` - сохранено.

use std::process::Command;
use takt_lang::compile_to_c;
use takt_lang::generator::GenerateOptions;

/// Компилируемый эталон: композиция `(Ping | Pong) + Toggle`, межмодельная ссылка
/// `S(Ping) = End`, переходы `ref`/`next` по условию, состоянию и `cond`.
///
/// Конструкции, упирающиеся в семантику `[bit;N]` (numeric-порт с бит-доступом,
/// `const`-массив с бит-доступом), намеренно **опущены** - они живут в полном `SRC`
/// (`lib.rs::parse_simple`) и чинятся.
const SYNTH_SRC: &str = r#"
cond IsEmpty = it = 0;
out done: bit at 0x100:0;
in B1: bit at 0x200:6;
var it: bit := 0;

model Ping {
    start Start {
        ref End: B1;
        enter { done := true; }
    }
    state End;
}
model Pong {
    start Begin {
        ref Stop: S(Ping) = End;
    }
    state Stop;
}
model Toggle {
    start Entry { ref Ping: IsEmpty; }
    state Ping = Ping { next Pong; }
    state Pong = Pong { next Complete; }
    state Complete { ref End: true; }
    state End;
}
start Entry = (Ping | Pong) + Toggle;
"#;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Эталонная модель порождает C, который **компилируется** настоящим `cc`, и содержит
/// перевод `S(Ping) = End` сравнением состояния.
///
/// Это то, чего `syntax_simple` не мог, пока эталон не компилировался: успех
/// проверяется **компиляцией**, а не строкой (строка проверяется дополнительно - как
/// тест самого перевода).
#[test]
fn reference_model_compiles_and_translates_state_ref() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0075_reference");
    std::fs::create_dir_all(&dir).expect("каталог вывода");

    compile_to_c(
        "ThisIsMyModel",
        SYNTH_SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");

    let source = std::fs::read_to_string(dir.join("this_is_my_model.c")).expect(".c порождён");
    // Строка захвачена из реального вывода, не угадана. `Ping` - сестра владельца
    // условия (`Pong`) внутри `(Ping | Pong) + Toggle`, поэтому база пути - `main`.
    assert!(
        source.contains("if (main->entry_parallel0.ping0.state == THIS_IS_MY_MODEL_PING_END) {"),
        "условие `S(Ping) = End` обязано попасть в C сравнением состояния:\n{source}"
    );
    assert!(
        !source.contains("TODO") && !source.contains("FIXME"),
        "заглушек в выводе быть не должно:\n{source}"
    );

    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] reference_model_compiles_and_translates_state_ref: компилятор `cc` не найден"
        );
        return;
    }
    let obj = dir.join("reference.o");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(&dir)
        .arg("-c")
        .arg(dir.join("this_is_my_model.c"))
        .arg("-o")
        .arg(&obj)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "эталонный C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
}
