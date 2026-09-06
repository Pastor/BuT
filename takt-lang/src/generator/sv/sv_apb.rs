//! Адаптер шины APB для цели `sv-mmio` (фича 0169).
//!
//! # Что это
//!
//! Ядро `sv-mmio` (фича 0062) порождает **шинно-агностичный** регистровый файл:
//! `reg_addr` / `reg_wdata` / `reg_wen` / `reg_rdata`. Протокол в него не зашит
//! намеренно — выбор диктует платформа. Здесь живёт первый адаптер, выбранный
//! заказчиком: **AMBA APB3**, сторона slave.
//!
//! Адаптер — **отдельный модуль-обёртка** `<name>_apb`, инстанцирующий ядро.
//! Ядро при этом не меняется: на нём стоят потактовые сверки регистров и гейт
//! двух инструментов SV (ADR 0169, Option C).
//!
//! # Почему ширины точные, а не 32-битные
//!
//! Канонический APB-slave обычно объявляют с `PADDR[31:0]` и `PWDATA[31:0]`.
//! Проба (2026-08-16) показала, что такой модуль **не проходит гейт проекта**:
//!
//! ```text
//! %Warning-UNUSEDSIGNAL: Bits of signal are not used: 'paddr'[31:11]
//! %Error: Exiting due to 2 warning(s)
//! ```
//!
//! Спецификация APB объявляет ширину `PADDR` implementation-specific, а
//! `PWDATA`/`PRDATA` допускает 8/16/32 бита, поэтому адаптер, порождаемый **под
//! конкретную модель**, вправе быть точным — и это единственная форма, которую
//! принимают оба инструмента.
//!
//! # Почему `pready = 1` — часть контракта, а не мелочь
//!
//! Строб записи — `psel & penable & pwrite`. Он корректен ровно потому, что
//! access-фаза длится **один** такт: при `pready = 1` трансфер завершается
//! сразу, и регистр защёлкивается однажды. Появятся состояния ожидания —
//! правило придётся менять вместе с ними.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::sv::sv_mmio::Mmio;

/// Отказ цели `sv` (без `-mmio`) на флаге `--bus` — `SV-019`.
///
/// Регистрового файла у неё нет **по устройству**: MMIO-адрес для чистого RTL
/// бессмыслен, сигнал приходит на вывод кристалла, а не по адресу (ADR 0062).
///
/// ⚠️ Текст **отдельный**, хотя код тот же: причина иная, и сообщение «у модели
/// нет портов с адресом» было бы ложью — у `stacker` их семнадцать. Один код на
/// два повода, но повод назван словами (образец `CC-023`, фича 0236).
pub(crate) fn refuse_wrong_target() -> Diagnostic {
    sv019(
        "адаптер шины запрошен (--bus), но цель 'sv' регистрового файла не строит: \
         MMIO-адрес для чистого RTL бессмыслен — сигнал приходит на вывод кристалла, \
         а не по адресу. Возьмите цель 'sv-mmio' либо снимите флаг",
    )
}

/// Отказ на модели без адресованных портов — `SV-019`.
///
/// Молчание здесь недопустимо: пользователь просил адаптер и остался бы с
/// ожиданием файла, которого нет.
fn refuse_without_registers() -> Diagnostic {
    sv019(
        "адаптер шины запрошен (--bus), но у модели нет ни одного порта с адресом: \
         транслировать в шину нечего. Задайте адреса портам ('at 0x…', оператор \
         'address' либо внешняя карта) либо снимите флаг",
    )
}

fn sv019(message: &str) -> Diagnostic {
    Diagnostic::error(Location::Codegen, message.to_string()).with_code("SV-019")
}

/// Объявление порта модуля с выравниванием диапазона и комментария.
///
/// Ширины зависят от модели (`[0:0]` у однобитной шины данных, `[10:0]` у
/// адреса), поэтому колонка имени и колонка комментария вычисляются, а не
/// набиваются пробелами вручную: иначе шапка модуля разъезжается на каждой
/// второй модели.
fn port_line(
    dir: &str,
    range: &str,
    name: &str,
    comment: &str,
    range_col: usize,
    name_col: usize,
) -> String {
    let decl = format!(
        "{dir} logic {range:<range_col$}{name},",
        range = range,
        name = name,
        range_col = range_col
    );
    format!(
        "{decl:<name_col$}// {comment}",
        decl = decl,
        name_col = name_col
    )
}

/// Диапазон `[N:0]` для ширины `w`; для однобитного сигнала — пусто.
fn range_of(w: u32) -> String {
    format!("[{}:0] ", w - 1)
}

/// Порождает текст модуля-обёртки `<name>_apb`.
///
/// `core` — имя модуля ядра (оно же имя файла без расширения).
///
/// # Ошибки
///
/// [`SV-019`](refuse_without_registers) — у модели нет адресованных портов.
pub(crate) fn generate_apb(core: &str, mmio: &Mmio) -> Result<String, Diagnostic> {
    if mmio.is_empty() {
        return Err(refuse_without_registers());
    }
    let aw = mmio.addr_width();
    let dw = mmio.data_width();
    let mut out = String::new();
    let mut p = Printer::new(4, &mut out);

    p.ident(&format!(
        "// Порождено компилятором Takt (taktc) — адаптер шины APB для '{core}'."
    ))
    .nl();
    p.ident("// Не редактировать вручную: файл перезаписывается при каждой генерации.")
        .nl();
    p.nl();

    // Колонки: диапазон — по самой широкой шине, комментарий — по самому
    // длинному объявлению. Считаются, а не задаются константой.
    let addr_range = range_of(aw);
    let data_range = range_of(dw);
    let range_col = addr_range.len().max(data_range.len());
    let name_col = "output logic ".len() + range_col + "presetn,".len() + 2;

    p.ident(&format!("module {core}_apb (")).nl();
    p.up();
    for (dir, range, name, comment) in [
        ("input ", "", "pclk", "тактовый сигнал шины (= clk ядра)"),
        (
            "input ",
            "",
            "presetn",
            "сброс шины, активный низкий (= rst_n)",
        ),
        (
            "input ",
            addr_range.as_str(),
            "paddr",
            "адрес APB (равен адресу из 'at')",
        ),
        ("input ", "", "psel", "выбор устройства (от декодера)"),
        ("input ", "", "penable", "access-фаза"),
        ("input ", "", "pwrite", "1 = запись, 0 = чтение"),
        ("input ", data_range.as_str(), "pwdata", "данные записи"),
        ("output", data_range.as_str(), "prdata", "данные чтения"),
        ("output", "", "pready", "готовность: всегда 1"),
        ("output", "", "pslverr", "ошибка: всегда 0"),
        (
            "input ",
            "",
            "en",
            "clock enable ядра (запись регистров им НЕ гейтится)",
        ),
    ] {
        p.ident(&port_line(dir, range, name, comment, range_col, name_col))
            .nl();
    }
    // Последний порт — без запятой, поэтому вне таблицы; выравнивание то же.
    p.ident(&format!(
        "output logic {range:<range_col$}is_done",
        range = "",
        range_col = range_col
    ))
    .nl();
    p.down();
    p.ident(");").nl();
    p.up();

    // Внутренние связи с ядром — с тем же выравниванием, что и шапка.
    //
    // Фича 0214: у ядра без записываемых регистров нет входов `reg_wdata` и
    // `reg_wen` — заводить провода к несуществующим выводам нельзя
    // (`PINNOTFOUND`), да и незачем.
    let writable = mmio.has_writable();
    let wires: Vec<(&str, &str)> = if writable {
        vec![
            (addr_range.as_str(), "reg_addr"),
            (data_range.as_str(), "reg_wdata"),
            ("", "reg_wen"),
            (data_range.as_str(), "reg_rdata"),
        ]
    } else {
        vec![
            (addr_range.as_str(), "reg_addr"),
            (data_range.as_str(), "reg_rdata"),
        ]
    };
    for (range, name) in wires {
        p.ident(&format!(
            "logic {range:<range_col$}{name};",
            range = range,
            name = name,
            range_col = range_col
        ))
        .nl();
    }
    p.nl();

    p.ident("assign reg_addr  = paddr;").nl();
    if writable {
        p.ident("assign reg_wdata = pwdata;").nl();
        p.ident("assign reg_wen   = psel & penable & pwrite;").nl();
    } else {
        // Ядро доступно шине только на чтение: цикл записи APB завершается
        // штатно (`pready` уже 1), но данные никуда не идут. Сигналы шины
        // остаются в интерфейсе — их состав задан протоколом, а не моделью, —
        // и здесь честно поглощаются, чтобы `verilator -Wall` не сообщал о
        // висящем входе (глушить его `lint_off` правило проекта запрещает).
        p.ident("wire _unused_write = &{1'b0, pwdata, pwrite, psel, penable};")
            .nl();
    }
    p.ident("assign prdata    = reg_rdata;").nl();
    p.ident("assign pready    = 1'b1;").nl();
    p.ident("assign pslverr   = 1'b0;").nl();
    p.nl();

    p.ident(&format!("{core} u_core (")).nl();
    p.up();
    p.ident(".clk(pclk),").nl();
    p.ident(".rst_n(presetn),").nl();
    p.ident(".en(en),").nl();
    p.ident(".reg_addr(reg_addr),").nl();
    if writable {
        p.ident(".reg_wdata(reg_wdata),").nl();
        p.ident(".reg_wen(reg_wen),").nl();
    }
    p.ident(".reg_rdata(reg_rdata),").nl();
    p.ident(".is_done(is_done)").nl();
    p.down();
    p.ident(");").nl();
    p.down();
    p.ident("endmodule").nl();

    Ok(out)
}
