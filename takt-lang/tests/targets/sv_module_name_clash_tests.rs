//! Имя порта против имени модуля у цели `sv` - `SV-020`,.

use std::path::PathBuf;
use takt_lang::generator::GenerateOptions;

fn dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let path = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0515_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&path);
    std::fs::create_dir_all(&path).expect("каталог");
    path
}

/// Имя корневой модели (а с ним и модуля) берётся из первого аргумента.
fn compile_sv(name: &str, tag: &str, src: &str) -> Result<(), takt_lang::diagnostics::Diagnostic> {
    let d = dir(tag);
    takt_lang::compile_to_sv(
        name,
        src,
        d.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .map(|_| ())
}

const USED_PORT: &str = "out probe: u8 at 0x100;\n\
                         var t: u8 := 0;\n\
                         start Run { always { t := t + 1; probe := t; } ref Run; }\n";

/// Совпадение имени порта с именем модуля - отказ `SV-020`.
#[test]
fn port_named_like_module_is_refused() {
    let err = compile_sv("probe", "clash", USED_PORT).expect_err("verilator даёт VARHIDDEN");
    assert_eq!(err.code.as_deref(), Some("SV-020"), "{err:?}");
    assert!(
        err.message.contains("'c'") && err.message.contains("'rust'"),
        "отказ обязан назвать цели, где модель по-прежнему валидна:\n{}",
        err.message
    );
    assert!(
        !matches!(err.loc, takt_lang::diagnostics::Location::Builtin),
        "отказ обязан нести позицию порта: {:?}",
        err.loc
    );
}

/// Режим `sv-mmio` - тот же отказ.
///
/// Поэтому проверка смотрит на порты модели, а не на список портов модуля.
#[test]
fn mmio_mode_is_refused_too() {
    let d = dir("clash_mmio");
    let env = takt_lang::parse_defines(&[]).expect("окружение адресов");
    let err = takt_lang::compile_to_sv_mmio(
        "probe",
        USED_PORT,
        d.to_str().expect("путь в UTF-8"),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .expect_err("сигнал регистрового файла прячет имя модуля");
    assert_eq!(err.code.as_deref(), Some("SV-020"), "{err:?}");
}

/// **Контроль:** различные имена переводятся без отказа.
#[test]
fn distinct_names_are_translated() {
    compile_sv(
        "probe",
        "ok",
        "out led: u8 at 0x100;\n\
                               var t: u8 := 0;\n\
                               start Run { always { t := t + 1; led := t; } ref Run; }\n",
    )
    .expect("имя порта не совпадает с именем модуля");
}

/// **Контроль:** Неиспользуемый порт отказа не даёт.
///
/// До вывода он не доезжает (фильтр `UsageSet`), значит и прятать ему нечего: отказ был
/// бы ложным.
#[test]
fn unused_port_is_not_refused() {
    compile_sv(
        "probe",
        "unused",
        "out probe: u8 at 0x100;\n\
         out led: u8 at 0x104;\n\
         var t: u8 := 0;\n\
         start Run { always { t := t + 1; led := t; } ref Run; }\n",
    )
    .expect("неиспользуемый порт в выводе не объявляется");
}

/// **Контроль:** имя типа и имя функции конфликта не дают - это замерено.
///
/// `verilator` принимает и порт с именем `typedef`, и порт рядом с одноимённой
/// функцией; расширить правило на них значило бы отвергать корректные модели.
#[test]
fn type_and_function_names_are_left_alone() {
    compile_sv(
        "probe",
        "type_name",
        "enum Mode { Idle = 0, Run = 1 }\n\
         out mode: Mode at 0x100;\n\
         var t: u8 := 0;\n\
         start Go { always { t := t + 1; mode := Run; } ref Go; }\n",
    )
    .expect("порт с именем перечисления законен");
    compile_sv(
        "probe",
        "fn_name",
        "fn helper(v: u8) -> u8 { return v + 1; }\n\
         out helper_out: u8 at 0x100;\n\
         var t: u8 := 0;\n\
         start Go { always { t := helper(t); helper_out := t; } ref Go; }\n",
    )
    .expect("порт рядом с одноимённой функцией законен");
}

/// Фикстура корпуса даёт `SV-020` - код достижим проверками.
#[test]
fn corpus_fixture_reports_the_code() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/data/svclash0515/probe.takt"
    );
    let src = std::fs::read_to_string(path).expect("фикстура читается");
    let err = compile_sv("probe", "corpus", &src).expect_err("имя порта прячет имя модуля");
    assert_eq!(err.code.as_deref(), Some("SV-020"), "{err:?}");
}
