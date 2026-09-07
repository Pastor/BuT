//! Адаптер шины APB для цели `sv-mmio`.
//!
//! ## Что здесь ловится
//!
//! Ядро `sv-mmio` даёт **шинно-агностичный** регистровый файл: `reg_addr` / `reg_wdata`
//! / `reg_wen` / `reg_rdata`. Протокол в него не зашит намеренно - выбор диктует
//! платформа, а цена ошибки высока. назвал первый протокол: **APB**, адресация
//! напрямую.
//!
//! ## Что проверяется здесь, а что - проверкой
//!
//! Здесь - **форма вывода и отказы**: имена сигналов, точные ширины, отсутствие файла
//! без флага, две причины `SV-019`. Валидность RTL и его **работу** доказывают проверка
//! (verilator + yosys) и рукописный тестбенч
//! `examples/generated/sv-mmio/tb/stacker_apb_tb.sv`: линт и синтез принимают обёртку,
//! которая не работает, поэтому цикл APB сверяется прогоном, а не чтением.

use std::path::PathBuf;
use takt_lang::generator::{Bus, GenerateOptions};

/// Каталог теста уникален по имени потока: тесты идут параллельно.
fn tmp(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace("::", "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0169_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Модель с адресованными портами: однобитный вход, восьмибитный вход и выход.
const ADDRESSED: &str = r#"
in flag: bit at 0x300:0;
in level: u8 at 0x102:0;
out ready: bit at 0x600:0;

var n: u8 := 0;

start Run {
    always { n := n + 1; }
    ref Run: n < 100;
}
"#;

/// Модель без единого адреса - вход для отказа `SV-019`.
const NO_ADDRESSES: &str = r#"
var n: u8 := 0;

start Run {
    always { n := n + 1; }
    ref Run: n < 100;
}
"#;

fn options(bus: Option<Bus>) -> GenerateOptions {
    let mut o = GenerateOptions::default();
    o.bus = bus;
    o
}

/// Компилирует целью `sv-mmio`; возвращает каталог вывода.
fn compile_mmio(tag: &str, name: &str, source: &str, bus: Option<Bus>) -> PathBuf {
    let dir = tmp(tag);
    takt_lang::compile_to_sv_mmio(
        &format!("{name}.takt"),
        source,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &Default::default(),
        &options(bus),
    )
    .expect("цель sv-mmio");
    dir
}

/// A1: без флага адаптера нет - вывод прежний.
#[test]
fn without_the_flag_no_adapter_is_emitted() {
    let dir = compile_mmio("plain", "dev", ADDRESSED, None);
    assert!(dir.join("dev.sv").exists(), "ядро обязано быть порождено");
    assert!(
        !dir.join("dev_apb.sv").exists(),
        "без --bus адаптер не порождается: вывод обязан остаться прежним"
    );
}

/// A1: с флагом рядом с ядром появляется обёртка.
#[test]
fn the_flag_emits_the_adapter_next_to_the_core() {
    let dir = compile_mmio("apb", "dev", ADDRESSED, Some(Bus::Apb));
    assert!(dir.join("dev.sv").exists(), "ядро на месте");
    let text = std::fs::read_to_string(dir.join("dev_apb.sv")).expect("адаптер порождён");
    assert!(
        text.contains("module dev_apb ("),
        "модуль обёртки называется <ядро>_apb:\n{text}"
    );
    assert!(
        text.contains("dev u_core ("),
        "обёртка обязана инстанцировать ядро:\n{text}"
    );
}

/// A1/R2: сигналы APB на месте, а `pready`/`pslverr` - константы контракта.
#[test]
fn apb_signals_and_contract_constants() {
    let dir = compile_mmio("signals", "dev", ADDRESSED, Some(Bus::Apb));
    let text = std::fs::read_to_string(dir.join("dev_apb.sv")).expect("адаптер");
    for signal in [
        "pclk", "presetn", "paddr", "psel", "penable", "pwrite", "pwdata", "prdata", "pready",
        "pslverr",
    ] {
        assert!(text.contains(signal), "нет сигнала {signal}:\n{text}");
    }
    assert!(
        text.contains("assign pready    = 1'b1;"),
        "состояний ожидания нет — pready константа:\n{text}"
    );
    assert!(
        text.contains("assign pslverr   = 1'b0;"),
        "pslverr константа:\n{text}"
    );
    // Строб записи - одиночный такт access-фазы; он корректен ровно потому, что pready
    // = 1 (иначе запись повторилась бы).
    assert!(
        text.contains("assign reg_wen   = psel & penable & pwrite;"),
        "строб записи собирается из фаз APB:\n{text}"
    );
}

/// R3: адресация **напрямую**: адрес шины равен адресу из `at`, без сдвига на слово.
#[test]
fn address_maps_straight_through() {
    let dir = compile_mmio("addr", "dev", ADDRESSED, Some(Bus::Apb));
    let text = std::fs::read_to_string(dir.join("dev_apb.sv")).expect("адаптер");
    assert!(
        text.contains("assign reg_addr  = paddr;"),
        "адрес идёт в ядро как есть:\n{text}"
    );
}

/// A1: ширины **точные**, а не канонические 32-битные.
///
/// Это не вкус: проба показала, что 32-битные `paddr`/`pwdata` дают `UNUSEDSIGNAL` и
/// проверка проекта (`verilator -Wall`) отвергает такой модуль. Спецификация APB ширину
/// `PADDR` оставляет реализации, поэтому адаптер, порождаемый под конкретную модель,
/// вправе быть точным.
#[test]
fn widths_are_exact_not_canonical_32() {
    let dir = compile_mmio("widths", "dev", ADDRESSED, Some(Bus::Apb));
    let text = std::fs::read_to_string(dir.join("dev_apb.sv")).expect("адаптер");
    // Максимальный адрес 0x600 -> 11 бит; максимальная ширина порта - 8 бит.
    assert!(
        text.contains("input  logic [10:0] paddr,"),
        "адрес — по максимальному адресу модели:\n{text}"
    );
    assert!(
        text.contains("input  logic [7:0]  pwdata,"),
        "данные — по максимальной ширине порта:\n{text}"
    );
    assert!(
        !text.contains("[31:0]"),
        "32-битные шины дали бы UNUSEDSIGNAL и не прошли бы гейт:\n{text}"
    );
}

/// A5: модель без адресованных портов + `--bus` -> `SV-019` с названной причиной.
#[test]
fn bus_without_registers_is_refused() {
    let dir = tmp("no_regs");
    let err = takt_lang::compile_to_sv_mmio(
        "empty.takt",
        NO_ADDRESSES,
        dir.to_str().expect("путь"),
        &[],
        &[],
        &Default::default(),
        &options(Some(Bus::Apb)),
    )
    .expect_err("шина без регистров бессмысленна");
    assert_eq!(err.code.as_deref(), Some("SV-019"), "код: {:?}", err.code);
    assert!(
        err.message.contains("нет ни одного порта с адресом"),
        "причина обязана быть названа: {}",
        err.message
    );
}

/// A5: та же диагностика у цели `sv` - но с **другим** текстом.
///
/// Один код, два повода: сообщение "у модели нет портов с адресом" было бы ложью - у
/// входа они есть, регистрового файла нет у самой цели.
#[test]
fn bus_on_plain_sv_is_refused_with_its_own_reason() {
    let dir = tmp("plain_sv");
    let err = takt_lang::compile_to_sv(
        "dev.takt",
        ADDRESSED,
        dir.to_str().expect("путь"),
        &[],
        &options(Some(Bus::Apb)),
    )
    .expect_err("у цели sv регистрового файла нет по устройству");
    assert_eq!(err.code.as_deref(), Some("SV-019"), "код: {:?}", err.code);
    assert!(
        err.message
            .contains("цель 'sv' регистрового файла не строит"),
        "причина обязана отличаться от «нет адресов»: {}",
        err.message
    );
}
