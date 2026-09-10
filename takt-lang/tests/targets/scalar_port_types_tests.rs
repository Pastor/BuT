//! Скалярные типы, которые цели считали непредставимыми у порта.
//!
//! # Что проверяется
//!
//! - перевод обоих направлений всеми восемью целями (падает списком);
//! - у `rust` значение уходит в HAL целым (`as`), а приходит вариантом
//!   (`from_repr`), и восстановление **тотально**: число вне набора даёт
//!   первый по тексту вариант;
//! - `from_repr` печатается **по нужде** - при выходном порте её быть не
//!   должно, иначе `-D warnings` отвергнет неиспользуемую функцию;
//! - контроль: составной порт по-прежнему разворачивается по листам (его
//!   разворачивает `port_split`, до классификации портов он не доходит);
//! - то же для типа `duration`: он тоже скаляр - целое в
//!   миллисекундах, - и его порт переводят все восемь целей.

use takt_lang::generator::GenerateOptions;

/// Цели проекта, чей вывод проверяется.
const TARGETS: &[&str] = &["c", "c-hal", "st", "st-at", "rust", "sv", "sv-mmio"];

/// Модель с портом типа `duration` заданного направления.
///
/// Длительность - тот же класс, что перечисление: скаляр, чью ширину задаёт общий
/// носитель (`duration::VALUE_BITS`).
fn duration_source(direction: &str) -> String {
    let body = if direction == "in" {
        "in hold: duration at 0x300;\n\
         \x20   out level: u8 at 0x304;\n\
         \x20   start Cycle {\n\
         \x20       always { seen := hold; level := 1; }\n\
         \x20       ref Cycle: seen > 1s;\n\
         \x20   }\n"
    } else {
        "out hold: duration at 0x300;\n\
         \x20   start Cycle {\n\
         \x20       always { hold := seen; }\n\
         \x20       ref Cycle: seen > 1s;\n\
         \x20   }\n"
    };
    format!(
        "model Probe {{\n\
         \x20   var seen: duration := 2s;\n\
         \x20   {body}\
         }}\n\
         start Main = Probe;\n"
    )
}

/// Порт длительности переводят все восемь целей - в обоих направлениях.
#[test]
fn every_target_translates_duration_port() {
    let mut failed = Vec::new();
    for direction in ["in", "out"] {
        for target in TARGETS {
            let tag = format!("dur_{direction}_{target}");
            if let Err(err) = emit(target, &duration_source(direction), &tag) {
                failed.push(format!("{target} ({direction}): отказ {:?}", err.code));
            }
        }
    }
    assert!(failed.is_empty(), "не переведено:\n{}", failed.join("\n"));
}

/// Модель с портом-битвектором заданной ширины.
fn bitvector_source(width: u16) -> String {
    format!(
        "model Probe {{\n\
         \x20   var mask: [bit; {width}] := 0;\n\
         \x20   var ticks: u8 := 0;\n\
         \x20   out sig: [bit; {width}] at 0x300;\n\
         \x20   start Cycle {{\n\
         \x20       always {{ ticks := ticks + 1; sig := mask; }}\n\
         \x20       ref Cycle: ticks < 200;\n\
         \x20   }}\n\
         }}\n\
         start Main = Probe;\n"
    )
}

/// Бит-вектор до слова - скаляр: его порт переводят все восемь целей.
///
/// Пропусти носитель эту ветвь, и `st-at` ответит `ST-004` там, где прочие семь
/// потребителей переводят.
#[test]
fn every_target_translates_packed_bitvector_port() {
    let mut failed = Vec::new();
    for target in TARGETS {
        if let Err(err) = emit(target, &bitvector_source(12), &format!("bv_{target}")) {
            failed.push(format!("{target}: отказ {:?}", err.code));
        }
    }
    assert!(failed.is_empty(), "не переведено:\n{}", failed.join("\n"));
}

/// **Контроль:** бит-вектор шире слова скаляром не является.
///
/// Массив слов портом быть не может, и цели отвечают своими кодами - правило не
/// менялось. Без этого контроля "бит-вектор разрешён" читалось бы как "разрешён любой",
/// а `[bit;96]` в HAL-колбэк не ложится.
#[test]
fn wide_bitvector_port_is_still_refused() {
    let mut wrong = Vec::new();
    for target in ["c", "rust", "st-at"] {
        if emit(target, &bitvector_source(96), &format!("bv96_{target}")).is_ok() {
            wrong.push(format!("{target} принял порт [bit;96]"));
        }
    }
    assert!(wrong.is_empty(), "контроль:\n{}", wrong.join("\n"));
}

/// Модель с портом типа `q(m, n)` заданного направления.
///
/// Третий тип на той же оси: `q` хранится знаковым целым **машинной** ширины (`q(6, 6)` -
/// это 12 бит, а типа `i12` нет ни у одной цели).
fn fixed_source(direction: &str) -> String {
    let body = if direction == "in" {
        "in gain: q(8, 8) at 0x300;\n\
         \x20   out level: u8 at 0x304;\n\
         \x20   start Cycle {\n\
         \x20       always { seen := gain; level := 1; }\n\
         \x20       ref Cycle: ticks < 200;\n\
         \x20   }\n"
    } else {
        "out gain: q(8, 8) at 0x300;\n\
         \x20   start Cycle {\n\
         \x20       always { gain := seen; }\n\
         \x20       ref Cycle: ticks < 200;\n\
         \x20   }\n"
    };
    format!(
        "model Probe {{\n\
         \x20   var seen: q(8, 8) := 1.5;\n\
         \x20   var ticks: u8 := 0;\n\
         \x20   {body}\
         }}\n\
         start Main = Probe;\n"
    )
}

/// Порт `q(m, n)` переводят все восемь целей - в обоих направлениях.
#[test]
fn every_target_translates_fixed_port() {
    let mut failed = Vec::new();
    for direction in ["in", "out"] {
        for target in TARGETS {
            let tag = format!("q_{direction}_{target}");
            if let Err(err) = emit(target, &fixed_source(direction), &tag) {
                failed.push(format!("{target} ({direction}): отказ {:?}", err.code));
            }
        }
    }
    assert!(failed.is_empty(), "не переведено:\n{}", failed.join("\n"));
}

/// Модель с перечислимым портом заданного направления.
fn source(direction: &str) -> String {
    let body = if direction == "in" {
        "in mode: Mode at 0x300;\n\
         \x20   out level: u8 at 0x304;\n\
         \x20   start Cycle {\n\
         \x20       always {\n\
         \x20           seen := mode;\n\
         \x20           if seen = Run { level := 1; } else { level := 0; }\n\
         \x20       }\n\
         \x20       ref Cycle: seen != Halt;\n\
         \x20   }\n"
    } else {
        "out mode: Mode at 0x300;\n\
         \x20   start Cycle {\n\
         \x20       always {\n\
         \x20           if seen = Idle { seen := Run; } else { seen := Halt; }\n\
         \x20           mode := seen;\n\
         \x20       }\n\
         \x20       ref Cycle: seen != Halt;\n\
         \x20   }\n"
    };
    format!(
        "enum Mode {{ Idle, Run, Halt }}\n\
         model Probe {{\n\
         \x20   var seen: Mode := Idle;\n\
         \x20   {body}\
         }}\n\
         start Main = Probe;\n"
    )
}

fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0485_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Компилирует целью `name` и возвращает весь порождённый текст.
fn emit(name: &str, source: &str, tag: &str) -> Result<String, takt_lang::diagnostics::Diagnostic> {
    let dir = out_dir(tag);
    let path = dir.to_str().expect("путь");
    let mut options = GenerateOptions::default();
    let env = takt_lang::address_map::AddressEnv::default();
    let result = match name {
        "c" => takt_lang::compile_to_c("probe", source, path, &[], &options),
        "c-hal" => takt_lang::compile_to_c_hal("probe", source, path, &[], &[], &env, &options),
        "st" => takt_lang::compile_to_st("probe", source, path, &[], &options),
        "st-at" => takt_lang::compile_to_st_at("probe", source, path, &[], &[], &env, &options),
        "rust" => takt_lang::compile_to_rust("probe", source, path, &[], &options),
        "sv" => takt_lang::compile_to_sv("probe", source, path, &[], &options),
        "sv-mmio" => {
            options.hal = true;
            takt_lang::compile_to_sv_mmio("probe", source, path, &[], &[], &env, &options)
        }
        other => panic!("неизвестная цель {other}"),
    };
    result?;
    let mut text = String::new();
    for entry in std::fs::read_dir(&dir).expect("каталог вывода") {
        let file = entry.expect("файл").path();
        if file.is_file() {
            text.push_str(&std::fs::read_to_string(&file).unwrap_or_default());
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    Ok(text)
}

/// Перечислимый порт переводят все восемь целей - в обоих направлениях.
#[test]
fn every_target_translates_enum_port() {
    let mut failed = Vec::new();
    for direction in ["in", "out"] {
        for target in TARGETS {
            if let Err(err) = emit(target, &source(direction), &format!("{direction}_{target}")) {
                failed.push(format!("{target} ({direction}): отказ {:?}", err.code));
            }
        }
    }
    assert!(failed.is_empty(), "не переведено:\n{}", failed.join("\n"));
}

/// У цели `rust` значение уходит целым, а приходит вариантом.
///
/// Обе стороны обязательны: приведение при записи без восстановления при чтении дало бы
/// `rustc: mismatched types` - то есть отказ в чужом месте.
#[test]
fn rust_converts_at_the_hal_boundary() {
    let out = emit("rust", &source("out"), "rust_out").expect("выходной порт переводится");
    assert!(
        out.contains("as u8"),
        "значение перечисления обязано уходить в HAL целым:\n{out}"
    );
    assert!(
        !out.contains("from_repr"),
        "у выходного порта восстановление не нужно — неиспользуемая функция валит гейт:\n{out}"
    );

    let input = emit("rust", &source("in"), "rust_in").expect("входной порт переводится");
    assert!(
        input.contains("Mode::from_repr("),
        "значение с порта обязано восстанавливаться в вариант:\n{input}"
    );
    assert!(
        input.contains("_ => Self::Idle,"),
        "восстановление обязано быть тотальным: число вне набора даёт первый \
         по тексту вариант (правило 0391):\n{input}"
    );
}

/// У цели `st-at` перечислимый порт размещается как целое той же ширины.
#[test]
fn st_at_places_enum_as_integer() {
    let text = emit("st-at", &source("out"), "st_at").expect("порт размещается");
    assert!(
        text.contains("AT %QB"),
        "трёхвариантное перечисление ложится в байтовую локацию:\n{text}"
    );
}

/// **Контроль:** составной порт по-прежнему разворачивается по листам.
///
/// Контроль, ждущий отказа `RS-016` и `ST-004` на массиве и структуре, здесь неверен - и
/// была неверна: до классификации портов такой порт не доходит, его разворачивает
/// проход `port_split`. Отказ носителя на том, что не разворачивается (массив слов
/// `[bit;N>64]`), проверяет модульный тест `rust_port`. Здесь проверяется другое:
/// перечисление не сломало разворот - обе формы переводятся.
#[test]
fn composite_port_is_still_split() {
    let cases = [
        (
            "массив",
            "model Probe {\n\
             \x20   var seen: u8 := 0;\n\
             \x20   out data: [u8; 4] at 0x300;\n\
             \x20   start Cycle {\n\
             \x20       always { seen := seen + 1; data[0] := seen; }\n\
             \x20       ref Cycle: seen < 200;\n\
             \x20   }\n\
             }\n\
             start Main = Probe;\n",
        ),
        (
            "структура",
            "struct Pair { lo: u8, hi: u8 }\n\
             model Probe {\n\
             \x20   var seen: u8 := 0;\n\
             \x20   out pair: Pair at 0x300;\n\
             \x20   start Cycle {\n\
             \x20       always { seen := seen + 1; pair.lo := seen; }\n\
             \x20       ref Cycle: seen < 200;\n\
             \x20   }\n\
             }\n\
             start Main = Probe;\n",
        ),
    ];
    let mut wrong = Vec::new();
    for (kind, src) in cases {
        for target in ["rust", "st-at"] {
            if let Err(err) = emit(target, src, &format!("ctrl_{target}_{kind}")) {
                wrong.push(format!("{target}: порт-{kind} отвергнут {:?}", err.code));
            }
        }
    }
    assert!(wrong.is_empty(), "контроль:\n{}", wrong.join("\n"));
}
