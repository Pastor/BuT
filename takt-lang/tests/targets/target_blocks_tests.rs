//! Вставка `assembly` и блок формул `formula` у восьми целей.
//!
//! # Что проверяется
//!
//! - именованная вставка попадает в вывод **своей** цели и не попадает в
//!   чужие - по всем восьми целям, тест падает списком;
//! - блок формул не появляется ни в одном выводе и переводу не мешает;
//! - имя цели проверяется (`SE-129`), включая режимное имя;
//! - настоящий неразрешённый узел по-прежнему отвергается (регресс 0236):
//!   пропуск новых узлов не должен превратиться в пропуск дефекта.
//!
//! Значения здесь не сверяются - это предмет
//! `takt-sim/tests/conformance/conformance_target_blocks_tests.rs`: расхождение целей
//! между собой у именованной вставки **намеренно**, и трассой его судить нельзя.

use takt_lang::generator::GenerateOptions;

/// Цель: имя для тестового вывода, язык метки и функция компиляции.
struct Target {
    name: &'static str,
    /// Метка `assembly`, тело которой эта цель печатает.
    label: &'static str,
}

/// Восемь целей проекта; язык вывода у режимов тот же, что у базовой цели.
const TARGETS: &[Target] = &[
    Target {
        name: "c",
        label: "c",
    },
    Target {
        name: "c-hal",
        label: "c",
    },
    Target {
        name: "st",
        label: "st",
    },
    Target {
        name: "st-at",
        label: "st",
    },
    Target {
        name: "rust",
        label: "rust",
    },
    Target {
        name: "sv",
        label: "sv",
    },
    Target {
        name: "sv-mmio",
        label: "sv",
    },
    Target {
        name: "plantuml",
        label: "plantuml",
    },
];

/// Модель со вставкой, адресованной цели `label`, и блоком формул.
fn source(label: &str) -> String {
    format!(
        "model Probe {{\n\
         \x20   var level: u8 := 0;\n\
         \x20   out sig: u8 at 0x300;\n\
         \x20   start Fill {{\n\
         \x20       always {{\n\
         \x20           level := level + 1;\n\
         \x20           assembly \"{label}\" {{ level := level + 41; }}\n\
         \x20           formula {{ holds(level) }}\n\
         \x20           sig := level;\n\
         \x20       }}\n\
         \x20       ref Fill: level < 200;\n\
         \x20   }}\n\
         }}\n\
         start Main = Probe;\n"
    )
}

fn out_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0484_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Компилирует `source` целью `name` и возвращает весь порождённый текст.
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
        "plantuml" => takt_lang::compile_to_plantuml("probe", source, path, &[]),
        other => panic!("неизвестная цель {other}"),
    };
    result?;
    let mut text = String::new();
    for entry in std::fs::read_dir(&dir).expect("каталог вывода") {
        let file = entry.expect("файл вывода").path();
        if file.is_file() {
            text.push_str(&std::fs::read_to_string(&file).unwrap_or_default());
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    Ok(text)
}

/// Вставка попадает в вывод своей цели и не попадает в чужие.
///
/// Проверяются **обе** стороны: "печатает своя" без "не печатают чужие" прошло бы и у
/// цели, которая печатает вставку всегда.
#[test]
fn named_assembly_reaches_only_its_target() {
    let mut wrong = Vec::new();
    for owner in TARGETS {
        let src = source(owner.label);
        for target in TARGETS {
            let text = match emit(
                target.name,
                &src,
                &format!("{}_{}", owner.label, target.name),
            ) {
                Ok(text) => text,
                Err(err) => {
                    wrong.push(format!(
                        "метка '{}' → цель '{}': отказ {:?}",
                        owner.label, target.name, err.code
                    ));
                    continue;
                }
            };
            // `plantuml` тел не печатает вовсе - у него сравнивать нечего.
            if target.name == "plantuml" {
                continue;
            }
            let printed = text.contains("41");
            let expected = target.label == owner.label;
            if printed != expected {
                wrong.push(format!(
                    "метка '{}' → цель '{}': тело {}, ожидалось {}",
                    owner.label,
                    target.name,
                    if printed {
                        "напечатано"
                    } else {
                        "пропущено"
                    },
                    if expected {
                        "напечатано"
                    } else {
                        "пропущено"
                    }
                ));
            }
        }
    }
    assert!(
        wrong.is_empty(),
        "вставка адресуется неверно:\n{}",
        wrong.join("\n")
    );
}

/// Блок формул переводу не мешает и в выводе не появляется.
#[test]
fn formula_block_is_skipped_by_every_target() {
    let mut wrong = Vec::new();
    for target in TARGETS {
        match emit(
            target.name,
            &source("c"),
            &format!("formula_{}", target.name),
        ) {
            Ok(text) if text.contains("holds") => {
                wrong.push(format!("{}: блок формул попал в вывод", target.name));
            }
            Ok(_) => {}
            Err(err) => wrong.push(format!("{}: отказ {:?}", target.name, err.code)),
        }
    }
    assert!(wrong.is_empty(), "блок формул:\n{}", wrong.join("\n"));
}

/// Неизвестное и режимное имя цели отвергаются `SE-129`.
///
/// Режимное имя - не придирка: `c-hal` печатает тот же C, и автор, писавший его, ожидал
/// вставки именно туда. Молча недействующая метка хуже отказа.
#[test]
fn unknown_target_label_is_refused() {
    for label in ["cpp", "c-hal", "st-at", "sv-mmio", "С"] {
        let err = emit("c", &source(label), "bad").expect_err("ожидался отказ семантики");
        assert_eq!(
            err.code.as_deref(),
            Some("SE-129"),
            "метка '{label}': код {:?}",
            err.code
        );
    }
}

/// Контроль: та же модель без блоков переводится всеми целями.
#[test]
fn control_model_without_blocks_is_translated() {
    let src = "model Probe {\n\
               \x20   var level: u8 := 0;\n\
               \x20   out sig: u8 at 0x300;\n\
               \x20   start Fill {\n\
               \x20       always { level := level + 1; sig := level; }\n\
               \x20       ref Fill: level < 200;\n\
               \x20   }\n\
               }\n\
               start Main = Probe;\n";
    let mut failed = Vec::new();
    for target in TARGETS {
        if let Err(err) = emit(target.name, src, &format!("control_{}", target.name)) {
            failed.push(format!("{}: отказ {:?}", target.name, err.code));
        }
    }
    assert!(failed.is_empty(), "контроль:\n{}", failed.join("\n"));
}

// -- Места блоков выровнены --------------------------------------

/// Модель, где обе конструкции стоят на уровне модели и на уровне состояния.
///
/// Решением 2026-09-03 места выровнены.
fn source_all_places(label: &str) -> String {
    format!(
        "model Probe {{\n\
         \x20   var level: u8 := 0;\n\
         \x20   out sig: u8 at 0x300;\n\
         \x20   formula {{ holds(level) }}\n\
         \x20   assembly \"{label}\" {{ level := level + 37; }}\n\
         \x20   start Fill {{\n\
         \x20       always {{ sig := level; }}\n\
         \x20       formula {{ reachable(level) }}\n\
         \x20       assembly \"{label}\" {{ level := level + 41; }}\n\
         \x20       ref Fill: level < 200;\n\
         \x20   }}\n\
         }}\n\
         start Main = Probe;\n"
    )
}

/// Вставка уровня модели и уровня состояния доезжает до своей цели.
///
/// Тест падает списком: цель, потерявшая одно из мест, названа поимённо.
#[test]
fn assembly_reaches_target_from_model_and_state_level() {
    let mut lost = Vec::new();
    for target in TARGETS {
        let text = match emit(
            target.name,
            &source_all_places(target.label),
            &format!("places_{}", target.name),
        ) {
            Ok(text) => text,
            Err(d) => {
                lost.push(format!("{}: отказ {:?}", target.name, d.code));
                continue;
            }
        };
        // `plantuml` тел не печатает вовсе - у него сравнивать нечего.
        if target.name == "plantuml" {
            continue;
        }
        // Признак - Числа приращений: 1 и 40 в выводе иначе не встречаются, а форма
        // записи у целей разная (`+ 40` у C, `wrapping_add(40)` у Rust). Признак -
        // Числа приращений: 37 и 41 в выводе иначе не встречаются, а форма записи у
        // целей разная (`+ 41` у C, `wrapping_add(41)` у Rust).
        if !text.contains("37") {
            lost.push(format!("{}: вставка уровня модели потеряна", target.name));
        }
        if !text.contains("41") {
            lost.push(format!(
                "{}: вставка уровня состояния потеряна",
                target.name
            ));
        }
    }
    assert!(lost.is_empty(), "места вставки потеряны: {lost:?}");
}

/// Блок формул на обоих уровнях в вывод не попадает.
#[test]
fn formula_block_at_new_places_is_skipped() {
    let mut leaked = Vec::new();
    for target in TARGETS {
        let text = emit(
            target.name,
            &source_all_places(target.label),
            &format!("places_f_{}", target.name),
        )
        .unwrap_or_else(|d| panic!("{}: {d:?}", target.name));
        if text.contains("holds") || text.contains("reachable") {
            leaked.push(target.name);
        }
    }
    assert!(
        leaked.is_empty(),
        "формула — обязательство внешнему анализатору, в вывод она не идёт: {leaked:?}"
    );
}
