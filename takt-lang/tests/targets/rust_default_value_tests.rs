//! Цель `rust`: умолчание значения для структуры, длительности и `q`.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Структура, `q` и `duration` - всё без инициализаторов. `f` и `d` обязаны быть
/// Использованы: неиспользуемую переменную цель выбрасывает штатным фильтром, и
/// проверка на умолчание прошла бы на выводе, где переменной нет вовсе (ловушка пробы).
const PLAIN: &str = "struct Bb { m: u8 } \
                     struct Aa { b: Bb, n: u8 } \
                     var v: Aa; var f: q(4, 4); var d: duration; \
                     var g: q(4, 4) := 1.0 as q(4, 4); var e: duration := 1s; \
                     out o: u8 at 0x100; \
                     start Run { always { o := v.n; } ref Done: f > g; ref Done: d > e; } \
                     state Done { }";

/// Поле-перечисление: `derive(Default)` у перечисления не выводится.
const ENUM_FIELD: &str = "enum Mode { Idle, Work } \
                          struct Cfg { mode: Mode, n: u8 } \
                          var c: Cfg; out o: u8 at 0x100; \
                          start Run { always { o := c.n; } next Done; } \
                          state Done { }";

/// Поле-массив длиннее 32: `impl Default for [T; N]` дальше не идёт.
const LONG_ARRAY: &str = "struct Buf { data: [u8; 40], n: u8 } \
                          var b: Buf; out o: u8 at 0x100; \
                          start Run { always { o := b.n; } next Done; } \
                          state Done { }";

/// **Контрпример К1:** только скаляры - `derive` обязан остаться.
const SCALARS: &str = "struct Point { x: u8, y: u8 } \
                       var p: Point; out o: u8 at 0x100; \
                       start Run { always { o := p.x; } next Done; } \
                       state Done { }";

/// **Контрпример К2 + пример П6:** бит-вектор <= 64 - скаляр, > 64 - массив слов.
const BIT_VECTORS: &str = "struct Wide { w: [bit; 4096], n: u8 } \
                           struct Narrow { b: [bit; 40], n: u8 } \
                           var x: Wide; var y: Narrow; out o: u8 at 0x100; \
                           start Run { always { o := x.n + y.n; } next Done; } \
                           state Done { }";

/// **Контрпример К3:** авторский инициализатор умолчанием не подменяется.
const WITH_INIT: &str = "struct Bb { m: u8 } \
                         struct Aa { b: Bb, n: u8 } \
                         var v: Aa := {{7}, 3}; out o: u8 at 0x100; \
                         start Run { always { o := v.n; } next Done; } \
                         state Done { }";

fn build_dir(tag: &str) -> std::path::PathBuf {
    // Имя потока несёт `::` после объединения тестовых целей: двоеточие в пути каталога
    // недопустимо, поэтому вычищается.
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0351_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Порождает Rust для исходника и возвращает каталог и текст модуля.
fn generate(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|e| panic!("порождение Rust для '{tag}': {e:?}"));
    let path = dir.join(format!("{tag}.rs"));
    let text = std::fs::read_to_string(&path).expect("чтение порождённого модуля");
    (dir, text)
}

// -- Слой 1: эмиссия ----------------------------------------------------------

/// П1-П3: три типа получают умолчание, а не отказ `RS-014`.
#[test]
fn struct_duration_and_fixed_get_defaults() {
    let (_dir, text) = generate("plain", PLAIN);
    assert!(
        text.contains("v: Aa::default()"),
        "умолчание структуры — её собственный `Default`.\n{text}"
    );
    assert!(
        text.contains("f: 0,"),
        "умолчание `q(m, n)` — нулевой код: хранение `i{{W}}`, ноль значит 0.0.\n{text}"
    );
    assert!(
        text.contains("d: 0,"),
        "умолчание `duration` — ноль миллисекунд (цель печатает `u32` мс).\n{text}"
    );
}

/// П4: поле-перечисление - `derive` без `Default` плюс ручной импл.
///
/// Значение берётся у первого варианта - то же правило, что у `default_value` для
/// самого перечисления; второго носителя этого правила нет.
#[test]
fn struct_with_enum_field_gets_manual_default() {
    let (_dir, text) = generate("enumfield", ENUM_FIELD);
    assert!(
        !text.contains("#[derive(Debug, Clone, Copy, PartialEq, Default)]\npub struct Cfg"),
        "у структуры с полем-перечислением `Default` НЕ выводится: `derive` дал бы \
         `E0277` при нулевом коде возврата `taktc`.\n{text}"
    );
    assert!(
        text.contains("impl Default for Cfg"),
        "умолчание такой структуры печатается вручную.\n{text}"
    );
    assert!(
        text.contains("mode: Mode::Idle"),
        "умолчание перечислимого поля — его ПЕРВЫЙ вариант.\n{text}"
    );
}

/// П5: поле-массив длиннее 32 - та же форма.
#[test]
fn struct_with_long_array_gets_manual_default() {
    let (_dir, text) = generate("longarray", LONG_ARRAY);
    assert!(
        text.contains("impl Default for Buf"),
        "`impl Default for [T; N]` в стандартной библиотеке есть только до N = 32.\n{text}"
    );
    assert!(
        text.contains("data: [0; 40]"),
        "тело импла — литерал полей с умолчаниями.\n{text}"
    );
}

/// **К1:** структура со скалярными полями сохраняет `derive`.
///
/// Без этой проверки "починка", печатающая импл всегда, прошла бы остальные тесты и
/// уронила проверка цели: `clippy::derivable_impls` под `-D warnings` - отказ сборки.
#[test]
fn struct_with_scalar_fields_keeps_derive() {
    let (_dir, text) = generate("scalars", SCALARS);
    assert!(
        text.contains("#[derive(Debug, Clone, Copy, PartialEq, Default)]"),
        "там, где `Default` выводится, печать обязана остаться прежней.\n{text}"
    );
    assert!(
        !text.contains("impl Default for Point"),
        "ручной импл там, где `derive` выводится, — это `clippy::derivable_impls`, \
         то есть отказ гейта цели.\n{text}"
    );
}

/// **К2 + П6:** длина считается у напечатанного типа, а не у разрядов.
///
/// `[bit; 40]` - упакованный скаляр `u64`, `Default` у него есть; `[bit; 4096]` -
/// массив 64 слов, то есть длиннее 32.
#[test]
fn bit_vector_length_is_counted_in_printed_words() {
    let (_dir, text) = generate("bitvec", BIT_VECTORS);
    assert!(
        text.contains("pub b: u64"),
        "предусловие: `[bit; 40]` печатается упакованным скаляром.\n{text}"
    );
    assert!(
        !text.contains("impl Default for Narrow"),
        "у структуры со скалярным бит-вектором `Default` выводится.\n{text}"
    );
    assert!(
        text.contains("pub w: [u64; 64]"),
        "предусловие: `[bit; 4096]` печатается массивом слов.\n{text}"
    );
    assert!(
        text.contains("impl Default for Wide"),
        "64 слова — длиннее 32, `Default` не выводится.\n{text}"
    );
}

/// **К3:** авторский инициализатор умолчанием не подменяется.
#[test]
fn author_initializer_is_not_replaced_by_default() {
    let (_dir, text) = generate("withinit", WITH_INIT);
    assert!(
        text.contains("Aa { b: Bb { m: 7 }, n: 3 }"),
        "написанное автором значение печатается литералом, а не `::default()`.\n{text}"
    );
    assert!(
        !text.contains("v: Aa::default()"),
        "умолчание применяется только там, где инициализатора нет.\n{text}"
    );
}

// -- Слой 2: тот же проверка, что в precheck.sh -----------------------------------

fn clippy_available() -> bool {
    Command::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Все шесть фикстур принимаются `clippy -D warnings`.
#[test]
fn generated_modules_pass_clippy_gate() {
    if !clippy_available() {
        eprintln!("[ПРОПУСК] generated_modules_pass_clippy_gate: clippy-driver не найден");
        return;
    }
    for (tag, source) in [
        ("gate_plain", PLAIN),
        ("gate_enumfield", ENUM_FIELD),
        ("gate_longarray", LONG_ARRAY),
        ("gate_scalars", SCALARS),
        ("gate_bitvec", BIT_VECTORS),
        ("gate_withinit", WITH_INIT),
    ] {
        let (dir, _) = generate(tag, source);
        let wrapper = dir.join("gate.rs");
        let module = dir.join(format!("{tag}.rs"));
        std::fs::write(
            &wrapper,
            format!(
                "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
                module.display()
            ),
        )
        .expect("запись обёртки");

        let out = Command::new("clippy-driver")
            .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
            .arg(&wrapper)
            .arg("--out-dir")
            .arg(dir.join("out"))
            .output()
            .expect("запуск clippy-driver");

        assert!(
            out.status.success(),
            "порождённый Rust для '{tag}' обязан приниматься `clippy -D warnings` — \
             это тот же гейт, что в `precheck.sh`:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}
