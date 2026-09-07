//! Тесты разбора аргументов подкоманды `compile`.

use super::*;

/// Разделитель `-I` на текущей платформе; тесты параметризуются им.
const SEP: &str = if cfg!(windows) { ";" } else { ":" };

// -- split_include_dirs ----------------------------------------------------

/// На Unix: разделитель `:` (POSIX-стиль аналогично PATH).
#[cfg(not(windows))]
#[test]
fn split_colon_unix() {
    assert_eq!(split_include_dirs("/a:/b:/c"), vec!["/a", "/b", "/c"]);
}

/// На Windows: разделитель `;` (пути вида `C:\dir`).
#[cfg(windows)]
#[test]
fn split_semicolon_windows() {
    assert_eq!(
        split_include_dirs("C:\\lib;C:\\usr"),
        vec!["C:\\lib", "C:\\usr"]
    );
}

/// Пустые сегменты пропускаются.
#[cfg(not(windows))]
#[test]
fn split_empty_segments_skipped() {
    assert_eq!(split_include_dirs("/a::/b"), vec!["/a", "/b"]);
}

/// Пустая строка -> пустой вектор.
#[test]
fn split_empty_string() {
    let result: Vec<String> = split_include_dirs("");
    assert!(result.is_empty());
}

/// Один путь без разделителей.
#[test]
fn split_single_path() {
    assert_eq!(split_include_dirs("/only/one"), vec!["/only/one"]);
}

// -- parse_compile_args: позитивные случаи --------------------------------

/// Минимальный вызов: только входной файл.
#[test]
fn parse_minimal() {
    let args = vec!["main.takt".to_string()];
    let opts = parse_compile_args(&args).unwrap();
    assert_eq!(opts.input_file, "main.takt");
    assert_eq!(opts.target, "c");
    assert_eq!(opts.output_path, "output");
    assert!(opts.include_dirs.is_empty());
    assert!(!opts.verbose);
    assert!(!opts.quiet);
    assert_eq!(opts.tick_hz, None);
}

/// Флаг `-I` с разделителем платформы - два пути.
#[test]
fn parse_include_dirs_separator() {
    let args = vec![
        "-I".to_string(),
        format!("/lib/lam{SEP}/usr/lam"),
        "main.takt".to_string(),
    ];
    let opts = parse_compile_args(&args).unwrap();
    assert_eq!(opts.include_dirs, vec!["/lib/lam", "/usr/lam"]);
}

/// Флаг `-I` повторяется дважды - пути объединяются, порядок сохранён.
#[test]
fn parse_multiple_include_flags() {
    let args = vec![
        "-I".to_string(),
        "/a".to_string(),
        "-I".to_string(),
        "/b".to_string(),
        "main.takt".to_string(),
    ];
    let opts = parse_compile_args(&args).unwrap();
    assert_eq!(opts.include_dirs, vec!["/a", "/b"]);
}

/// Слитная форма: `-I/path` без пробела.
#[test]
fn parse_include_dir_glued() {
    let args = vec!["-I/lib/lam".to_string(), "main.takt".to_string()];
    let opts = parse_compile_args(&args).unwrap();
    assert_eq!(opts.include_dirs, vec!["/lib/lam"]);
}

/// Флаги `-t`, `-o` и `-I` все вместе.
#[test]
fn parse_full_args() {
    let args = vec![
        "--target".to_string(),
        "c".to_string(),
        "-I".to_string(),
        format!("/lib/lam{SEP}/usr/lam"),
        "main.takt".to_string(),
        "-o".to_string(),
        "build/".to_string(),
    ];
    let opts = parse_compile_args(&args).unwrap();
    assert_eq!(opts.target, "c");
    assert_eq!(opts.input_file, "main.takt");
    assert_eq!(opts.output_path, "build/");
    assert_eq!(opts.include_dirs, vec!["/lib/lam", "/usr/lam"]);
}

// -- parse_compile_args: контр-примеры (ошибки) ---------------------------

/// Нет входного файла -> ошибка.
#[test]
fn parse_missing_input_file_is_error() {
    let args: Vec<String> = vec![];
    assert!(parse_compile_args(&args).is_err());
}

/// Нет аргумента после `--target` -> ошибка с именем флага.
#[test]
fn parse_target_missing_value_is_error() {
    let err = parse_compile_args(&["--target".to_string()]).unwrap_err();
    assert!(err.contains("--target"), "сообщение: {err}");
}

/// Неизвестный флаг -> ошибка с его именем.
#[test]
fn parse_unknown_flag_is_error() {
    let args = vec!["main.takt".to_string(), "--unknown-flag".to_string()];
    let err = parse_compile_args(&args).unwrap_err();
    assert!(err.contains("--unknown-flag"), "сообщение: {err}");
}

// -- флаги --verbose / --quiet ---------------------------------------------

/// Одновременное указание `--verbose` и `--quiet` -> ошибка.
#[test]
fn parse_verbose_and_quiet_is_error() {
    let args = vec![
        "main.takt".to_string(),
        "--verbose".to_string(),
        "--quiet".to_string(),
    ];
    let err = parse_compile_args(&args).unwrap_err();
    assert!(
        err.contains("взаимоисключающие") || err.contains("verbose") && err.contains("quiet"),
        "сообщение: {err}"
    );
}

/// `--verbose` совместим с другими флагами.
#[test]
fn parse_verbose_with_other_flags() {
    let args = vec![
        "--verbose".to_string(),
        "-I".to_string(),
        "/lib".to_string(),
        "main.takt".to_string(),
    ];
    let opts = parse_compile_args(&args).unwrap();
    assert!(opts.verbose);
    assert!(!opts.quiet);
    assert_eq!(opts.include_dirs, vec!["/lib"]);
}

// -- --address-map и --define ----------------------------------------------

/// Флаг `--address-map` задаёт путь к внешней карте адресов.
#[test]
fn parse_address_map_flag() {
    let args = vec![
        "main.takt".to_string(),
        "--address-map".to_string(),
        "stm32.map".to_string(),
    ];
    let opts = parse_compile_args(&args).unwrap();
    assert_eq!(opts.address_map.as_deref(), Some("stm32.map"));
}

/// `--address-map` без аргумента - ошибка.
#[test]
fn address_map_requires_argument() {
    let err = parse_compile_args(&["--address-map".to_string()]).unwrap_err();
    assert!(err.contains("--address-map"), "сообщение: {err}");
}

/// Три формы флага `--define` дают одно и то же.
#[test]
fn define_flag_forms_are_equivalent() {
    let expected = vec!["N=0x1".to_string()];
    for args in [
        vec![
            "m.takt".to_string(),
            "--define".to_string(),
            "N=0x1".to_string(),
        ],
        vec!["m.takt".to_string(), "-D".to_string(), "N=0x1".to_string()],
        vec!["m.takt".to_string(), "-DN=0x1".to_string()],
    ] {
        let opts = parse_compile_args(&args).unwrap();
        assert_eq!(opts.defines, expected, "форма: {args:?}");
    }
}

/// Флаг `--define` повторяем - символы копятся.
#[test]
fn define_flag_is_repeatable() {
    let args = vec![
        "m.takt".to_string(),
        "-D".to_string(),
        "A=0x1".to_string(),
        "-DB=0x2".to_string(),
    ];
    let opts = parse_compile_args(&args).unwrap();
    assert_eq!(opts.defines, vec!["A=0x1".to_string(), "B=0x2".to_string()]);
}

/// Разбор `-D` не проглатывает чужие флаги.
#[test]
fn unknown_flag_is_still_rejected() {
    let args = vec!["m.takt".to_string(), "-Q".to_string(), "foo".to_string()];
    assert!(
        parse_compile_args(&args).is_err(),
        "-Q обязан остаться неизвестным"
    );
}

// -- float-width / float-as-q ---------------------------

/// Без флага - `W64` (double): эталон C совпадает с точностью симулятора.
#[test]
fn float_width_defaults_to_64() {
    let opts = parse_compile_args(&["main.takt".to_string()]).unwrap();
    assert_eq!(opts.float_width, crate::FloatWidth::W64);
}

/// `--float-width=32` -> `float`; форма с пробелом наравне со слитной.
#[test]
fn parse_float_width_both_forms() {
    let glued =
        parse_compile_args(&["main.takt".to_string(), "--float-width=32".to_string()]).unwrap();
    assert_eq!(glued.float_width, crate::FloatWidth::W32);
    let sep = parse_compile_args(&[
        "main.takt".to_string(),
        "--float-width".to_string(),
        "32".to_string(),
    ])
    .unwrap();
    assert_eq!(sep.float_width, crate::FloatWidth::W32);
}

/// `--float-width=16` - ошибка разбора, а не молчаливое умолчание.
#[test]
fn float_width_rejects_unsupported_value() {
    let err =
        parse_compile_args(&["main.takt".to_string(), "--float-width=16".to_string()]).unwrap_err();
    assert!(
        err.contains("16") && err.contains("32") && err.contains("64"),
        "сообщение обязано назвать и отвергнутое, и допустимые: {err}"
    );
}

/// `--float-as-q=10.22` -> `(10, 22)`; обе формы флага.
#[test]
fn parse_float_as_q_valid() {
    let slit =
        parse_compile_args(&["m.takt".to_string(), "--float-as-q=10.22".to_string()]).unwrap();
    assert_eq!(slit.float_as_q, Some((10, 22)));
    let sep = parse_compile_args(&[
        "m.takt".to_string(),
        "--float-as-q".to_string(),
        "8.8".to_string(),
    ])
    .unwrap();
    assert_eq!(sep.float_as_q, Some((8, 8)));
}

/// Контрпримеры границ и формата `--float-as-q` - ошибка CLI.
#[test]
fn float_as_q_rejects_out_of_bounds_and_bad_format() {
    for bad in ["40.40", "0.8", "8.0", "abc", "8", "8.x"] {
        let arg = format!("--float-as-q={bad}");
        let err = parse_compile_args(&["m.takt".to_string(), arg]).unwrap_err();
        assert!(err.contains("--float-as-q"), "для '{bad}' сообщение: {err}");
    }
}

/// `--float-embedded` - булев флаг.
#[test]
fn parse_float_embedded_flag() {
    let o = parse_compile_args(&[
        "m.takt".to_string(),
        "--float-as-q=8.8".to_string(),
        "--float-embedded".to_string(),
    ])
    .unwrap();
    assert!(o.float_embedded);
}

// -- --tick-hz -------------------------------------------------

/// По умолчанию частота не задана -> профиль "часы".
#[test]
fn tick_hz_absent_by_default() {
    let o = parse_compile_args(&["m.takt".to_string()]).unwrap();
    assert_eq!(o.tick_hz, None);
}

/// `--tick-hz=1000` и раздельная форма дают одно.
#[test]
fn parse_tick_hz_both_forms() {
    let glued = parse_compile_args(&["m.takt".to_string(), "--tick-hz=1000".to_string()]).unwrap();
    assert_eq!(glued.tick_hz, Some(1_000));
    let sep = parse_compile_args(&[
        "m.takt".to_string(),
        "--tick-hz".to_string(),
        "8000000".to_string(),
    ])
    .unwrap();
    assert_eq!(sep.tick_hz, Some(8_000_000));
}

/// Контрпримеры: ноль и нечисло - ошибка CLI, а не молчаливое умолчание.
#[test]
fn tick_hz_rejects_zero_and_non_number() {
    for bad in ["0", "abc", "1kHz", "-5"] {
        let arg = format!("--tick-hz={bad}");
        let err = parse_compile_args(&["m.takt".to_string(), arg]).unwrap_err();
        assert!(err.contains("--tick-hz"), "для '{bad}' сообщение: {err}");
    }
}

/// `--tick-hz` без аргумента - ошибка с именем флага.
#[test]
fn tick_hz_requires_argument() {
    let err = parse_compile_args(&["--tick-hz".to_string()]).unwrap_err();
    assert!(err.contains("--tick-hz"), "сообщение: {err}");
}

/// `--parameters=assign` - явное умолчание.
#[test]
fn parse_parameters_assign() {
    let o =
        parse_compile_args(&["in.takt".to_string(), "--parameters=assign".to_string()]).unwrap();
    assert_eq!(o.parameters, ParametersMode::Assign);
}

/// Без флага - режим `assign` (умолчание совпадает с явной формой байт-в-байт).
#[test]
fn parse_parameters_default_is_assign() {
    let o = parse_compile_args(&["in.takt".to_string()]).unwrap();
    assert_eq!(o.parameters, ParametersMode::Assign);
}

/// `--parameters=specialize` разбирается (реализация - ).
#[test]
fn parse_parameters_specialize() {
    let o = parse_compile_args(&["in.takt".to_string(), "--parameters=specialize".to_string()])
        .unwrap();
    assert_eq!(o.parameters, ParametersMode::Specialize);
}

/// Неизвестное значение - ошибка с перечислением допустимых, а не молчаливое умолчание.
#[test]
fn parse_parameters_unknown_value_is_an_error() {
    let err =
        parse_compile_args(&["in.takt".to_string(), "--parameters=bogus".to_string()]).unwrap_err();
    assert!(
        err.contains("assign") && err.contains("specialize"),
        "{err}"
    );
}

/// Флаг без значения - ошибка, а не умолчание.
#[test]
fn parse_parameters_without_value_is_an_error() {
    let err = parse_compile_args(&["in.takt".to_string(), "--parameters".to_string()]).unwrap_err();
    assert!(err.contains("--parameters="), "{err}");
}
