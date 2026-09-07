//! Все цели принимают параметризованный вход в обоих режимах -, (требование R8,
//! критерий A13 анализа).
//!
//! Проверяется **состав принимаемых программ**, а не форма вывода (её сверяют
//! `model_parameter_apply_tests` для `assign` и `model_parameter_const_tests` для
//! `specialize`) и не поведение (потактовая сверка - в
//! `takt-sim/tests/conformance_param_modes_tests.rs`).
//!
//! Восемь целей x два режима x два набора значений. Исключение **одно** и оно
//! содержательное: цель `sv` (и `sv-mmio`) уплощает композицию, поэтому два экземпляра
//! одной модели делят регистры - **разные** настройки в режиме `assign` невыразимы и
//! отвергаются `SV-016`. В `specialize` они законны: у каждой копии свои регистры.

use takt_lang::GenerateOptions;
use takt_lang::{
    compile_to_c, compile_to_c_hal, compile_to_plantuml, compile_to_rust, compile_to_st,
    compile_to_st_at, compile_to_sv, compile_to_sv_mmio,
};

/// Модель с параметром; два экземпляра с **разными** настройками.
const DIFFERENT: &str = "model Tuner {\n\
                         \x20   parameter gain: u8 := 1;\n\
                         \x20   var acc: u8 := 0;\n\
                         \x20   start Count {\n\
                         \x20       always { acc := acc + gain; }\n\
                         \x20       ref Count;\n\
                         \x20   }\n\
                         }\n\
                         start Main = Tuner(gain := 100) | Tuner(gain := 200);\n";

/// Тот же вход, но настройки **одинаковые**.
const EQUAL: &str = "model Tuner {\n\
                     \x20   parameter gain: u8 := 1;\n\
                     \x20   var acc: u8 := 0;\n\
                     \x20   start Count {\n\
                     \x20       always { acc := acc + gain; }\n\
                     \x20       ref Count;\n\
                     \x20   }\n\
                     }\n\
                     start Main = Tuner(gain := 100) | Tuner(gain := 100);\n";

/// Опции режима.
fn options(specialize: bool) -> GenerateOptions {
    // `GenerateOptions` - `#[non_exhaustive]`: поле правится после `default()`.
    let mut options = GenerateOptions::default();
    options.specialize = specialize;
    options
}

/// Каталог вывода для одного прогона - **уникальный по тесту**.
///
/// Ключ уникальности - имя потока (харнесс Rust называет поток именем теста), а не
/// только тег: оба теста файла зовут `try_target` с одними и теми же тегами (`sv-spec`
/// и т. д.), а каждый вызов начинается с `remove_dir_all` - то есть при параллельном
/// прогоне один тест сносил вывод другого прямо во время компиляции, и цель отвечала
/// `SV-001 (NotFound)`. Гонка пре-существующая: воспроизведена и до правок (1 падение
/// из 6 прогонов), поодиночке и под `--test-threads=1` тест зелёный всегда.
fn out_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .to_string();
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt-0185-07-{thread}-{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Компилирует вход целью `target` и возвращает код диагностики при отказе.
///
/// Цели-потребители адресов (`c-hal`/`st-at`/`sv-mmio`) зовутся своими входами: у
/// фикстуры портов нет, поэтому карта адресов пуста и разрешать нечего.
fn try_target(target: &str, src: &str, specialize: bool) -> Result<(), String> {
    let tag = format!("{target}-{}", if specialize { "spec" } else { "assign" });
    let dir = out_dir(&tag);
    let path = dir.to_str().expect("путь").to_string();
    let name = "modes.takt";
    let options = options(specialize);
    let result: Result<
        Vec<takt_lang::diagnostics::Diagnostic>,
        takt_lang::diagnostics::Diagnostic,
    > = match target {
        "c" => compile_to_c(name, src, &path, &[], &options),
        "c-hal" => compile_to_c_hal(name, src, &path, &[], &[], &Default::default(), &options),
        "st" => compile_to_st(name, src, &path, &[], &options),
        "st-at" => compile_to_st_at(name, src, &path, &[], &[], &Default::default(), &options),
        "rust" => compile_to_rust(name, src, &path, &[], &options),
        "sv" => compile_to_sv(name, src, &path, &[], &options),
        "sv-mmio" => compile_to_sv_mmio(name, src, &path, &[], &[], &Default::default(), &options),
        "plantuml" => compile_to_plantuml(name, src, &path, &[]),
        other => panic!("неизвестная цель '{other}' в тесте"),
    };
    result.map(|_| ()).map_err(|d| {
        format!(
            "{} ({})",
            d.code.unwrap_or_else(|| "?".to_string()),
            d.message
        )
    })
}

/// Все цели, включая потребителей адресов и диаграмму.
const TARGETS: [&str; 8] = [
    "c", "c-hal", "st", "st-at", "rust", "sv", "sv-mmio", "plantuml",
];

// --- A13: обе стороны флага на обоих наборах значений ------------------------

/// Одинаковые настройки принимают **все** цели в **обоих** режимах.
///
/// Прежде цели `sv`/`sv-mmio` отвергали и этот вход: `SV-016` сравнивал наборы
/// аргументов **вместе с позицией** в тексте, поэтому два текстуально одинаковых вызова
/// считались разными. Дефект (заявленное "одинаковые наборы законны" не выполнялось),
/// исправлен здесь - равенство аргумента есть равенство настройки (имя и значение).
#[test]
fn equal_settings_are_accepted_by_every_target_in_both_modes() {
    for specialize in [false, true] {
        for target in TARGETS {
            assert_eq!(
                try_target(target, EQUAL, specialize),
                Ok(()),
                "цель {target} обязана принять одинаковые настройки \
                 (specialize = {specialize})"
            );
        }
    }
}

/// Разные настройки: все цели принимают, кроме `sv`/`sv-mmio` в режиме `assign` - там
/// уплощение делает две настройки невыразимыми, и это громкий `SV-016`, а не молчаливая
/// победа последнего значения.
#[test]
fn different_settings_are_accepted_except_flattening_targets_under_assign() {
    for target in TARGETS {
        let flattening = matches!(target, "sv" | "sv-mmio");
        match try_target(target, DIFFERENT, false) {
            Ok(()) => assert!(
                !flattening,
                "цель {target} уплощает композицию и обязана была отвергнуть \
                 разные настройки в режиме assign"
            ),
            Err(message) => {
                assert!(
                    flattening,
                    "цель {target} обязана принять разные настройки в режиме \
                     assign, но отказала: {message}"
                );
                assert!(
                    message.starts_with("SV-016"),
                    "ожидался SV-016, получено: {message}"
                );
            }
        }
        // В `specialize` разные настройки законны для всех целей: у каждой копии свои
        // регистры/структуры.
        assert_eq!(
            try_target(target, DIFFERENT, true),
            Ok(()),
            "цель {target} обязана принять разные настройки в режиме specialize"
        );
    }
}
