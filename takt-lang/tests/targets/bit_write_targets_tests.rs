//! Запись разряда `x.N := v` в целях `rust` и `st`.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Носитель-переменная: установка, сброс, нулевой разряд и вычисляемое значение.
const VARS: &str = "in btn: bit at 0x10:0;\n\
                    var b: u8 := 0;\n\
                    var packed: [bit;8] := 0;\n\
                    var idx: u8 := 1;\n\
                    var arr: [u8; 3] := { 0, 0, 0 };\n\
                    start Idle {\n\
                        always {\n\
                            b.3 := 1;\n\
                            b.2 := 0;\n\
                            b.0 := btn;\n\
                            packed.6 := 1;\n\
                            arr[idx].4 := 1;\n\
                        }\n\
                    }\n";

/// Числовой выходной порт: установка разряда требует чтения.
const OUT_WORD_PORT: &str = "out bank: u8 at 0x200;\n\
                             var n: u8 := 0;\n\
                             start Idle { always { bank.3 := 1; n := n + 1; } }\n";

/// Однобитный выходной порт: чтения не требует - это запись самого порта.
const OUT_BIT_PORT: &str = "out led: bit at 0x100:3;\n\
                            var n: u8 := 0;\n\
                            start Idle { always { led.0 := 1; n := n + 1; } }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0250_targets_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

fn generate_rust(
    tag: &str,
    source: &str,
) -> Result<(PathBuf, String), takt_lang::diagnostics::Diagnostic> {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )?;
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение модуля");
    Ok((dir, text))
}

fn generate_st(
    tag: &str,
    source: &str,
) -> Result<(PathBuf, String), takt_lang::diagnostics::Diagnostic> {
    let dir = build_dir(tag);
    takt_lang::compile_to_st(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )?;
    let text = std::fs::read_to_string(dir.join(format!("{tag}.st"))).expect("чтение модуля");
    Ok((dir, text))
}

// -- Цель `rust`: слой 1, эмиссия ---------------------------------------------

/// **Запись разряда печатается установкой и сбросом маски.**
#[test]
fn rust_prints_mask_set_and_clear() {
    let (_dir, text) = generate_rust("bw_vars", VARS).expect("порождение Rust");
    let expected = [
        // Литеральная единица - идиоматичное `|=`.
        "self.b |= 1 << 3;",
        // Литеральный ноль - сброс; скобки у маски обязательны (`!1 << 2` разобралось
        // бы как `(!1) << 2`).
        "self.b &= !(1 << 2);",
        // Нулевой разряд: сдвига нет - маска нулевого разряда есть просто `1`.
        "self.b = if self.hal.read_bit(InBitPort::Btn) { self.b | 1 } else { self.b & !1 };",
        // Упакованный `[bit;8]` - тот же скаляр.
        "self.packed |= 1 << 6;",
        // Элемент массива - носитель наравне с переменной.
        "self.arr[self.idx as usize] |= 1 << 4;",
    ];
    let missing: Vec<&str> = expected
        .iter()
        .filter(|line| !text.contains(**line))
        .copied()
        .collect();
    assert!(
        missing.is_empty(),
        "цель `rust` не напечатала эти формы записи разряда: {missing:#?}\n{text}"
    );
}

/// **`1 << 0` в выводе не появляется.**
///
/// Довод здесь **не** "этого требует clippy": замер 2026-08-18 показал, что `clippy -D
/// warnings` сдвиг литерала пропускает (в отличие от `x >> 0` у чтения - там
/// `identity_op` действительно валит проверка). Причина проще: маска нулевого разряда есть
/// `1`, и печатать вместо неё сдвиг - засорять вывод. Нулевой разряд в корпусе обычен
/// (`SENSORS_CAB.0`), так что случай не теоретический.
#[test]
fn rust_never_shifts_by_zero() {
    let (_dir, text) = generate_rust("bw_zero", VARS).expect("порождение Rust");
    assert!(
        !text.contains("1 << 0"),
        "сдвиг на нуль — clippy::identity_op, гейт цели `rust` его отвергнет:\n{text}"
    );
}

/// **Числовой выходной порт - `RS-025` с причиной и обходом.**
#[test]
fn rust_refuses_word_output_port_bit_with_reason() {
    let error = generate_rust("bw_port", OUT_WORD_PORT).expect_err("порт-слово: ожидался отказ");
    assert_eq!(error.code.as_deref(), Some("RS-025"), "{error:?}");
    let text = &error.message;
    for expected in ["bank", "только запись", "целиком"] {
        assert!(
            text.contains(expected),
            "отказ обязан называть порт, причину и обход; нет '{expected}': {text}"
        );
    }
}

/// **Однобитный выходной порт чтения не требует** - запись идёт как есть.
///
/// Контрпример к предыдущему тесту: отказ "на всякий случай" для любого порта прошёл бы
/// его и отнял работающую форму.
#[test]
fn rust_writes_single_bit_output_port() {
    let (_dir, text) = generate_rust("bw_led", OUT_BIT_PORT).expect("порождение Rust");
    assert!(
        text.contains("self.hal.write_bit(OutBitPort::Led, true);"),
        "у однобитного порта разряд один, и запись в него — запись порта:\n{text}"
    );
}

// -- Цель `rust`: слой 2, настоящий clippy ------------------------------------

fn clippy_available() -> bool {
    Command::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Порождённый модуль принимается `clippy -D warnings` - как в проверке.
#[test]
fn rust_bit_write_passes_clippy_gate() {
    if !clippy_available() {
        eprintln!("[ПРОПУСК] rust_bit_write_passes_clippy_gate: clippy-driver не найден");
        return;
    }
    // Массив здесь не участвует: индексация литералом даёт `unnecessary_cast` (`arr[1
    // as usize]`) - дефект печатника индекса, существующий без всякой записи разряда и
    // вынесенный кандидатом.
    const CLIPPY_SRC: &str = "in btn: bit at 0x10:0;\n\
                              var b: u8 := 0;\n\
                              var packed: [bit;8] := 0;\n\
                              start Idle {\n\
                                  always {\n\
                                      b.3 := 1;\n\
                                      b.2 := 0;\n\
                                      b.0 := btn;\n\
                                      packed.6 := 1;\n\
                                  }\n\
                              }\n";
    let (dir, _) = generate_rust("bw_gate", CLIPPY_SRC).expect("порождение Rust");
    let wrapper = dir.join("gate.rs");
    std::fs::write(
        &wrapper,
        format!(
            "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
            dir.join("bw_gate.rs").display()
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
        "порождённый Rust с записью разряда обязан приниматься `clippy -D warnings`:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

// -- Цель `st`: слой 1, эмиссия -----------------------------------------------

/// **Запись разряда печатается маской, литералы - без `SEL`.**
#[test]
fn st_prints_masks_and_sel() {
    let (_dir, text) = generate_st("bw_st", VARS).expect("порождение ST");
    let expected = [
        "b := BYTE_TO_USINT(USINT_TO_BYTE(b) OR 16#08);",
        "b := BYTE_TO_USINT(USINT_TO_BYTE(b) AND 16#FB);",
        // Вычисляемое значение - развилка `SEL(G, IN0, IN1)`: при `G = FALSE` берётся
        // `IN0`, поэтому "сбросить" идёт вторым аргументом.
        "b := BYTE_TO_USINT(SEL(btn, USINT_TO_BYTE(b) AND 16#FE, USINT_TO_BYTE(b) OR 16#01));",
        "packed := BYTE_TO_USINT(USINT_TO_BYTE(packed) OR 16#40);",
        "arr[idx] := BYTE_TO_USINT(USINT_TO_BYTE(arr[idx]) OR 16#10);",
    ];
    let missing: Vec<&str> = expected
        .iter()
        .filter(|line| !text.contains(**line))
        .copied()
        .collect();
    assert!(
        missing.is_empty(),
        "цель `st` не напечатала эти формы записи разряда: {missing:#?}\n{text}"
    );
}

/// **`[bit;N <= 64]` - битовая строка и на чтении.**
///
/// Дефект жил независимо от записи: `r := v.2;` при `v: [bit;8]` давал `ST-011`, хотя
/// документ объявляет `[bit;8]` эквивалентным `u8`.
#[test]
fn st_reads_bit_of_packed_vector() {
    const READ_SRC: &str = "var v: [bit;8] := 5;\n\
                            var r: bit := 0;\n\
                            start Idle { always { r := v.2; } }\n";
    let (_dir, text) = generate_st("bw_read", READ_SRC).expect("чтение разряда [bit;8] в ST");
    assert!(
        text.contains("r := (USINT_TO_BYTE(v) AND 16#04) <> 16#00;"),
        "упакованный `[bit;8]` — скаляр, равный `u8`, и разряд у него читается:\n{text}"
    );
}

// -- Цель `st`: слой 2, настоящий iec2c ---------------------------------------

/// Прогоняет `iec2c` по порождённому ST (мягкий пропуск, если недоступен).
fn assert_st_valid(dir: &Path, name: &str) {
    let home = std::env::var("HOME").map(PathBuf::from).unwrap_or_default();
    let iec2c = home.join(".local/bin/iec2c");
    let lib = home.join(".local/share/matiec/lib");
    if !iec2c.exists() {
        eprintln!("[ПРОПУСК] iec2c недоступен — ST не проверен арбитром");
        return;
    }
    let out = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg("-T")
        .arg(dir)
        .arg(dir.join(format!("{name}.st")))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "порождённый ST с записью разряда не принят iec2c:\n{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Порождённый ST принимается арбитром `iec2c` - как в проверке.
#[test]
fn st_bit_write_is_accepted_by_iec2c() {
    let (dir, _) = generate_st("bw_iec", VARS).expect("порождение ST");
    assert_st_valid(&dir, "bw_iec");
}

// -- Границы: формы вне объёма отвечают прежними кодами -----------------------

/// **То, чего цель не умеет по другим причинам, кодов не меняет.**
///
/// Тест падает **списком**: "починка", превратившая любую из этих форм в невалидный
/// код, прошла бы все положительные тесты выше.
#[test]
fn out_of_scope_forms_keep_their_codes() {
    const FIELD: &str = "struct Pt { x: u8, y: u8 }\n\
                         var p: Pt := { 0, 0 };\n\
                         start Idle { always { p.x.2 := 1; } }\n";
    const ELEM: &str = "var arr: [u8; 3] := { 0, 0, 0 };\n\
                        start Idle { always { arr[1].2 := 1; } }\n";

    let mut wrong: Vec<String> = Vec::new();

    // Граница сдвинулась: структуры цели `rust` и `sv` теперь переводят, поэтому запись
    // разряда их поля больше не отвергается - она печатается (`self.p.x |= 1 << 2;` у
    // `rust`). Проверяется именно это: прежний отказ был свойством непереведённых
    // структур, а не записи разряда.
    match generate_rust("bw_b1", FIELD) {
        Ok(_) => {}
        other => wrong.push(format!(
            "rust/поле структуры (0293: переводится): {other:?}"
        )),
    }
    // Граница сдвинулась: агрегатный инициализатор массива цель
    // `sv` печатает шаблоном присваивания `'{...}`, и запись разряда элемента
    // больше не отвергается. Прежний отказ был свойством непереведённого
    // **инициализатора**, а не записи разряда - ровно как у структур в 0293.
    // Значения сверяются потактово (`conformance_sv_array_tests`).
    let tag = "bw_b3";
    let dir = build_dir(tag);
    let result = takt_lang::compile_to_sv(
        tag,
        ELEM,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    );
    match result {
        Ok(_) => {}
        other => wrong.push(format!("sv/элемент массива (0309: переводится): {other:?}")),
    }

    assert!(
        wrong.is_empty(),
        "границы обязаны отвечать прежними кодами, изменилось: {wrong:#?}"
    );
}
