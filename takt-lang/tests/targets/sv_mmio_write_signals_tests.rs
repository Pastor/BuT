//! Регистровый интерфейс `sv-mmio` отражает состав портов.
//!
//! # Что здесь ловится
//!
//! `verilator --lint-only -Wall` отвечал двумя `UNUSEDSIGNAL` и **ненулевым** кодом
//! возврата - то есть уронил бы проверка, попади туда такая модель. В проверке её не было:
//! `SV_MMIO_TRANSLATABLE` состоял из одного `stacker`, у которого входные порты есть.

use takt_lang::GenerateOptions;

/// Каталог вывода, уникальный по имени потока (тесты идут параллельно, 0190).
fn out_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt0214_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Модель, у которой все адресованные порты - выходные.
const ONLY_OUT: &str = "out ready: bit at 0x600:0;\n\
                        var n: u8 := 0;\n\
                        start Run { always { n := n + 1; ready := 1; } ref Run; }\n";

/// Модель с входным портом - шине есть что писать.
const WITH_IN: &str = "in cmd: bit at 0x100:0;\n\
                       out ready: bit at 0x600:0;\n\
                       start Run { always { ready := cmd; } ref Run; }\n";

/// Компилирует исходник целью `sv-mmio` и возвращает текст модуля.
fn generate(unit: &str, source: &str, tag: &str, options: &GenerateOptions) -> String {
    let dir = out_dir(tag);
    takt_lang::compile_to_sv_mmio(
        unit,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[] as &[String],
        &[],
        &takt_lang::address_map::AddressEnv::default(),
        options,
    )
    .unwrap_or_else(|d| panic!("{unit}: цель sv-mmio обязана скомпилировать вход: {d:?}"));
    std::fs::read_to_string(dir.join(format!("{unit}.sv"))).expect("порождённый .sv читается")
}

/// **T1.** Без входных портов сигналы записи не эмитятся.
#[test]
fn read_only_core_has_no_write_signals() {
    let text = generate("ro_core", ONLY_OUT, "ro", &GenerateOptions::default());
    assert!(
        !text.contains("reg_wdata"),
        "у ядра только для чтения не должно быть входа данных записи:\n{text}"
    );
    assert!(
        !text.contains("reg_wen"),
        "у ядра только для чтения не должно быть строба записи:\n{text}"
    );
    assert!(
        text.contains("reg_rdata"),
        "чтение обязано остаться — шина видит выходной регистр:\n{text}"
    );
}

/// **T2.** При входном порте интерфейс прежний - все четыре сигнала.
#[test]
fn writable_core_keeps_full_interface() {
    let text = generate("rw_core", WITH_IN, "rw", &GenerateOptions::default());
    for signal in ["reg_addr", "reg_wdata", "reg_wen", "reg_rdata"] {
        assert!(
            text.contains(signal),
            "сигнал '{signal}' обязан остаться при наличии входного порта:\n{text}"
        );
    }
}

/// **T3.** Адаптер APB не заводит проводов к несуществующим выводам ядра.
///
/// Прежде обёртка подключала `.reg_wdata(...)`/`.reg_wen(...)` безусловно, и на ядре
/// без записи verilator отвечал `PINNOTFOUND` - пара "ядро + обёртка" вообще не
/// собиралась.
#[test]
fn apb_adapter_follows_core_interface() {
    let dir = out_dir("apb");
    // `GenerateOptions` помечен `#[non_exhaustive]`: снаружи крейта строится только
    // через `default()` с последующим присваиванием поля.
    let mut options = GenerateOptions::default();
    options.bus = Some(takt_lang::generator::Bus::Apb);
    takt_lang::compile_to_sv_mmio(
        "ro_apb",
        ONLY_OUT,
        dir.to_str().expect("путь в UTF-8"),
        &[] as &[String],
        &[],
        &takt_lang::address_map::AddressEnv::default(),
        &options,
    )
    .expect("порождение адаптера APB");
    let text = std::fs::read_to_string(dir.join("ro_apb_apb.sv")).expect("адаптер порождён");
    assert!(
        !text.contains(".reg_wdata("),
        "адаптер не должен подключать вход, которого у ядра нет:\n{text}"
    );
    assert!(
        !text.contains(".reg_wen("),
        "адаптер не должен подключать строб, которого у ядра нет:\n{text}"
    );
    assert!(
        text.contains("_unused_write"),
        "сигналы шины обязаны быть честно поглощены, а не оставлены висеть:\n{text}"
    );
}
