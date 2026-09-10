//! Заголовок модуля и порты.
//!
//! ## Порт Takt здесь выражается напрямую - впервые из всех целей
//!
//! | Цель | `out cmd_fork: bit` |
//! |---|---|
//! | `c` | `(*main->write_bit)(STACKER_CMD_FORK, 1, main->userdata)` - косвенный вызов через таблицу колбэков |
//! | `c-hal` | `*(volatile uint8_t*)0x500 = 1` - запись по адресу |
//! | `st` | `VAR_OUTPUT cmd_fork : BOOL;` либо `VAR_GLOBAL ... AT %QX...` |
//! | `rust` | `hal.write_bit(Port::CmdFork, true)` - метод трейта |
//! | **`sv`** | **`output logic cmd_fork`** - физический вывод кристалла |
//!
//! У всех программных целей между портом и миром стоит промежуточный слой, потому что у
//! процессора порта нет - есть адрес или вызов. У RTL порт **и есть** порт.
//!
//! ## Служебные порты `clk`/`rst_n`/`en`: почему их нет в языке
//!
//! Такт Takt == `posedge clk` (вопрос 1, Option A), поэтому модулю нужны сигналы,
//! которых в `.takt` не существует. Они добавляются **генератором**, а имена для цели
//! `sv` **зарезервированы**: коллизия -> [`SV-007`]. Кроме `clk`/`rst_n` это **`en`**
//! (clock enable): вход с умолчанием `1'b1`, управляющий защёлкивание в `always_ff` (`end
//! else if (en)`), но **не** сброс. Неподключённый `en` тождествен `en=1`, поэтому порт
//! необязателен и существующие потребители его не замечают.
//!
//! Отвергнутая альтернатива (Option B - объявлять `clock`/`reset` в самом языке)
//! сломала бы аддитивность фичи и была бы **ложью для четырёх целей из пяти**: у
//! `c`/`st`/`rust` такта как сигнала не существует. Поэтому `clk`/`rst_n` - имена
//! **цели**, а не ключевые слова языка, и диагностика обязана это объяснять: иначе
//! автор решит, что сломан язык.
//!
//! ## Адрес порта не потребляется
//!
//! `GenerateOptions::{hal, address_map}` здесь не читаются. MMIO-адрес для RTL
//! бессмыслен: у RTL-модуля процессора нет, сигнал приходит на вывод кристалла, а не по
//! адресу, - спрашивать адрес порта так же бессмысленно, как адрес ножки микросхемы.
//! Парной цели `sv-at` нет.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::keywords;
use crate::generator::sv::sv_map::SvMap;
use crate::generator::sv::sv_type::{SvType, sv_type};
use crate::semantic::minimap::Name;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode, PortDirection, VariableNode};
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;

/// Имена, которые цель `sv` порождает сама и потому резервирует.
///
/// `clk`/`rst_n`/`is_done` - порты модуля; `state`/`state_next` - регистр автомата и
/// его комбинационная пара. Пользовательское имя, совпавшее с любым из них, дало бы
/// **два объявления одного идентификатора** - то есть невалидный SV, причём в месте, не
/// связанном с исходной строкой `.takt`.
///
/// резервировал только `clk`/`rst_n`. Список расширен по **той же причине**, по которой
/// заведены те двое: генератор объявляет эти имена сам.
pub(crate) const RESERVED_NAMES: &[&str] = &[
    "clk",
    "rst_n",
    "en",
    "is_done",
    "state",
    "state_next",
    // Служебный вход времени: порт пользователя с таким именем разъехался бы со
    // служебным сигналом - отказ `SV-007`, не тишина.
    "time_ms",
];

/// Имя стороны чтения двунаправленного порта.
pub(crate) fn inout_in(name: &str) -> String {
    format!("{}_i", name)
}

/// Имя стороны записи двунаправленного порта.
pub(crate) fn inout_out(name: &str) -> String {
    format!("{}_o", name)
}

/// Имя строба записи двунаправленного порта.
pub(crate) fn inout_we(name: &str) -> String {
    format!("{}_we", name)
}

/// Строит диагностику `SV-007` - коллизия с именем, которое порождает цель.
fn sv007(name: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "имя '{}' зарезервировано целью 'sv': генератор объявляет его сам \
             (clk/rst_n — служебные порты такта и сброса, en — clock enable, \
             is_done — выход терминальности, state/state_next — регистр \
             автомата). Это НЕ \
             ключевое слово языка Takt: модель остаётся полностью валидной для \
             целей 'c', 'c-hal', 'st' и 'rust'. Переименуйте элемент \
             в исходнике .takt, если модель нужна в аппаратуре",
            name
        ),
    )
    .with_code("SV-007")
}

/// Строит диагностику `SV-012` - имя совпало с ключевым словом SystemVerilog.
fn sv012(name: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "имя '{}' является ключевым словом SystemVerilog и идентификатором \
             быть не может. Это НЕ ключевое слово языка Takt: модель остаётся \
             валидной для целей 'c', 'c-hal', 'st' и 'rust'. \
             Переименуйте элемент в исходнике .takt, если модель нужна в \
             аппаратуре",
            name
        ),
    )
    .with_code("SV-012")
}

/// Строит диагностику `SV-020` - имя порта совпало с именем модуля.
fn sv020(name: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "имя порта '{name}' совпадает с именем модуля: `verilator` под \
             `-Wall` отвечает `VARHIDDEN` («Declaration of signal hides \
             declaration in upper scope»), а гейт цели считает предупреждение \
             ошибкой. Имя модуля цель строит из имени модели, а у корневой — из \
             имени файла, поэтому совпадение возникает само собой. Это НЕ \
             ключевое слово языка Takt: модель остаётся валидной для целей 'c', \
             'c-hal', 'st' и 'rust'. Переименуйте порт либо файл, \
             если модель нужна в аппаратуре"
        ),
    )
    .with_code("SV-020")
}

/// Имя порта не должно совпадать с именем модуля - `SV-020`.
///
/// Проверка стоит отдельно от [`check_sv_name`]: та судит имя по спискам, известным
/// заранее, а имя модуля вычисляется позже - из имени модели, а у корневой из имени
/// файла. Ровно поэтому совпадение и возникает само собой: автор пишет `out probe: u8`
/// в файле `probe.takt`.
///
/// Прочие имена цели конфликта не дают, и это замерено: порт с именем типа
/// (`typedef`), с именем функции и переменная с именем модуля `verilator` принимает -
/// отказ на них был бы ложным.
///
/// Проверяются порты модели, а не список портов модуля: в режиме `sv-mmio` адресованный
/// порт портом модуля не становится (он бит регистрового файла), но объявляется
/// сигналом - и `VARHIDDEN` приходит на него же.
pub(crate) fn check_module_name_clash(
    module: &str,
    map: &SvMap,
    blocks: &[(Name, Rc<RefCell<ModelNode>>)],
) -> Result<(), Diagnostic> {
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for var in model.variables.values() {
            let VariableNode::Port { name, loc, .. } = var else {
                continue;
            };
            // Неиспользуемый порт до вывода не доезжает (фильтр `UsageSet`), а значит и
            // конфликтовать ему не с чем: отказ был бы ложным.
            if name != module || !map.usage().ports.contains(name) {
                continue;
            }
            return Err(sv020(name, *loc));
        }
    }
    Ok(())
}

/// Проверяет, что имя пригодно как идентификатор SystemVerilog.
///
/// # Ошибки
/// - [`SV-007`](sv007) - имя порождает сама цель (`clk`, `state`, ...);
/// - [`SV-012`](sv012) - имя является ключевым словом SystemVerilog.
pub(crate) fn check_sv_name(name: &str, loc: Location) -> Result<(), Diagnostic> {
    if RESERVED_NAMES.contains(&name) {
        return Err(sv007(name, loc));
    }
    if keywords::SV.contains(&name) {
        return Err(sv012(name, loc));
    }
    if let Some(ch) = non_ascii_char(name) {
        return Err(sv018(name, ch, loc));
    }
    Ok(())
}

/// Первый символ имени вне алфавита идентификатора цели (или `None`).
///
/// Алфавит SystemVerilog - `[A-Za-z0-9_$]`; всё прочее `verilator` и `yosys` отвергают
/// уже разбором.
pub(crate) fn non_ascii_char(name: &str) -> Option<char> {
    name.chars()
        .find(|c| !(c.is_ascii_alphanumeric() || *c == '_' || *c == '$'))
}

/// Строит диагностику `SV-018` - символ вне алфавита идентификатора SV.
///
/// Отказ принадлежит **цели**, а не языку: `c` и `rust` такие имена переводят, и их
/// проверки (`cc -Wall -Werror`, `clippy -D warnings`) вывод принимают.
fn sv018(name: &str, ch: char, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "имя '{name}' содержит символ '{ch}', недопустимый в идентификаторе              SystemVerilog: алфавит цели — латиница, цифры, '_' и '$'. Это НЕ              ограничение языка Takt — модель остаётся валидной для целей 'c',              'c-hal' и 'rust'. Переименуйте элемент, если модель              нужна в аппаратуре"
        ),
    )
    .with_code("SV-018")
}

/// Порт модуля, подготовленный к эмиссии.
pub(crate) struct SvPort {
    /// Имя порта - **как в исходнике `.takt`**, без нормализации.
    ///
    /// Решение ("порты: имя из `.takt` без изменений"). Приведение регистра завело бы
    /// класс коллизий (`FloorSensor` и `floor_sensor` слиплись бы в одно имя - ср.
    /// `RS-005` цели `rust`) ради чистой косметики; к тому же имя порта читает инженер,
    /// подключающий модуль, и оно обязано совпадать с именем в спецификации автоматики.
    pub(crate) name: String,
    /// Тип порта в SystemVerilog.
    pub(crate) ty: SvType,
    /// Тип порта в языке - для печати литерала сброса.
    ///
    /// [`SvType`] хранит уже готовые части объявления (`logic signed [15:0]`), а
    /// значение сброса печатается по **семантическому** типу: перечисление
    /// восстанавливается по вариантам, широкое число - размерной формой.
    pub(crate) ty_node: TypeNode,
    /// Начальное значение порта (`:=` объявления).
    ///
    /// [`ExpressionNode::None`], если не задано. К этому моменту - литерал: свёртку
    /// делает семантика (`declaration::resolve_port_init`), поэтому контекст печати
    /// роли не играет.
    pub(crate) init: ExpressionNode,
    /// Позиция объявления - для диагностики о непечатаемом значении сброса.
    pub(crate) loc: Location,
    /// Читает ли модель этот порт.
    ///
    /// Значим только у **двунаправленного**: его сторона `_i` есть вход модуля, и если
    /// модель порт лишь пишет, `verilator` под `-Wall` отвечает `UNUSEDSIGNAL`. У `in`
    /// порт по определению читается, у `out` - не читается вовсе (`SE-027`).
    pub(crate) is_read: bool,
}

/// Порты модуля, разложенные по направлениям.
#[derive(Default)]
pub(crate) struct SvPorts {
    /// Входные порты (`in` -> `input logic`).
    pub(crate) inputs: Vec<SvPort>,
    /// Выходные порты (`out` -> `output logic`).
    pub(crate) outputs: Vec<SvPort>,
    /// Двунаправленные порты (`inout`).
    ///
    /// В шапке модуля каждый разворачивается в **три** сигнала: `<имя>_i` (вход),
    /// `<имя>_o` (выход) и `<имя>_we` (строб записи). Форму выбрал 2026-08-23: это
    /// ровно та механика, что у цели `c` (колбэки чтения и записи) и у регистрового
    /// интерфейса `sv-mmio` - плата держит ячейку, модуль её читает и пишет.
    /// Трёхстабильная шина отвергнута: внутри кристалла её нет (yosys: "limited support
    /// for tri-state logic"), а сигнал разрешения пришлось бы выводить из модели,
    /// которая о нём молчит.
    pub(crate) inouts: Vec<SvPort>,
}

/// Собирает порты **всех** моделей файла в единый набор.
///
/// Порты берутся со всех моделей, а не только с корня: в `elevator_mini.takt` они
/// объявлены внутри под-моделей (`out elevator_motor_up: bit;` в `Motor`), а модуль SV -
/// **один на корневую модель** (композиция уплощается, Option A′), поэтому его порты
/// суть объединение портов уровней.
///
/// **Фильтр по [`UsageSet`](crate::semantic::unused::UsageSet) обязателен, а не
/// оптимизация.** Неиспользуемый порт в SV - не просто лишняя строка, а лишний
/// вывод кристалла; и, что решает дело, `verilator --lint-only -Wall` даёт на
/// него `UNUSEDSIGNAL`, то есть без фильтра проверка краснел бы на легальных
/// моделях (открытый вопрос 7 - решён пробой в пользу фильтра).
///
/// # Ошибки
/// [`SV-007`](sv007)/[`SV-012`](sv012) на
/// непригодном имени, `SV-002`...`SV-004` на непереводимом типе.
pub(crate) fn collect_ports(
    map: &SvMap,
    blocks: &[(Name, Rc<RefCell<ModelNode>>)],
    addressed: &BTreeSet<String>,
) -> Result<SvPorts, Diagnostic> {
    let mut ports = SvPorts::default();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    // Чтения считаются по всему дереву модели: порт объявлен в одной модели, а читать
    // его может её ребёнок по вызову. Носитель признака - `semantic::unused`.
    let mut reads = crate::semantic::unused::UsageSet {
        reads_only: true,
        ..Default::default()
    };
    for (_, model_rc) in blocks {
        let of_model = crate::semantic::usage_tree::reads_with_implementations(model_rc);
        reads.ports.extend(of_model.ports);
    }
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for var in model.variables.values() {
            let VariableNode::Port {
                name,
                ty,
                direction,
                loc,
                init,
                ..
            } = var
            else {
                continue;
            };
            // Объявление объявляет своё место: отказ о типе порта рождается вне
            // операторов, и без этого слоя он печатался без координаты вовсе - автор не
            // знал, какой порт назван.
            crate::generator::site::enter_declaration(*loc);
            // Порт **с** адресом (режим `sv-mmio`) портом модуля не становится - он бит
            // регистрового файла (объявляется и обслуживается в `sv_mmio`). В режиме
            // `sv` множество пусто -> фильтр прозрачен.
            if addressed.contains(name) {
                continue;
            }
            if !map.usage().ports.contains(name) || !seen.insert(name.clone()) {
                continue;
            }
            check_sv_name(name, *loc)?;
            // Порт-Массив невыразим в шапке модуля: распакованный массив в списке
            // портов **yosys не принимает вовсе** ("syntax error, unexpected '['"),
            // хотя verilator его допускает - форма выбирается по тому, что принимают
            // оба.
            //
            // `[bit;N <= 64]` под запрет не подпадает: это упакованный скаляр, и в
            // шапке он обычный `logic [N-1:0]`.
            if matches!(ty, TypeNode::Array(_, _))
                && crate::semantic::bit_vector::is_bit_vector(ty).is_none()
            {
                return Err(crate::generator::sv::sv_expr::sv002(&format!(
                    "порт '{name}' типа массива: список портов модуля не \
                     принимает распакованный массив — синтезатор yosys \
                     отвергает такую шапку. Разложите порт на скалярные либо \
                     работайте с переменной модели"
                )));
            }
            let port = SvPort {
                name: name.clone(),
                ty: sv_type(ty, &format!("порт '{}'", name))?,
                ty_node: ty.clone(),
                init: init.clone(),
                loc: *loc,
                // Читается ли порт - признак нужен только двунаправленному: его сторона
                // `_i` есть вход модуля, и без чтения `verilator` под `-Wall` отвечает
                // `UNUSEDSIGNAL`.
                is_read: reads.ports.contains(name),
            };
            match direction {
                PortDirection::In => ports.inputs.push(port),
                PortDirection::Out => ports.outputs.push(port),
                // Двунаправленный порт: три сигнала печатает заголовок, регистры и
                // строб заводит `sv_fsm`.
                PortDirection::InOut => ports.inouts.push(port),
            }
        }
    }
    // Слой объявления снимается парно входу: переживи он сбор портов, координату
    // последнего порта получил бы отказ в теле - то есть сообщение указало бы не туда.
    crate::generator::site::leave_declaration();
    Ok(ports)
}

/// Печатает поглотители непрочитанных полей входного порта-структуры.
pub(crate) fn emit_port_sinks(
    p: &mut Printer,
    structs: &std::collections::BTreeMap<String, Vec<(String, TypeNode)>>,
    ports: &SvPorts,
    blocks: &[(Name, Rc<RefCell<ModelNode>>)],
) {
    let mut printed = false;
    for port in ports.inputs.iter().chain(ports.inouts.iter()) {
        let TypeNode::Struct(struct_name) = &port.ty_node else {
            continue;
        };
        let Some(fields) = structs.get(struct_name.as_str()) else {
            continue;
        };
        let mut read: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        for (_, model_rc) in blocks {
            read.extend(crate::semantic::usage_tree::read_port_fields(
                model_rc, &port.name,
            ));
        }
        if fields.iter().all(|(name, _)| read.contains(name)) {
            continue;
        }
        let is_inout = ports.inouts.iter().any(|p| p.name == port.name);
        // У двунаправленного порта сторона `_i` печатается по факту чтения: если модель
        // порт только пишет, сигнала нет вовсе, и поглотитель ссылался бы на
        // несуществующее имя ("Can't find definition of variable").
        if is_inout && !port.is_read {
            continue;
        }
        let signal = if is_inout {
            inout_in(&port.name)
        } else {
            port.name.clone()
        };
        p.ident(&format!("logic _unused_{signal};")).nl();
        p.ident(&format!("assign _unused_{signal} = &{{1'b0, {signal}}};"))
            .nl();
        printed = true;
    }
    if printed {
        p.nl();
    }
}

/// Печатает заголовок модуля со списком портов.
///
/// Порядок - служебные, затем `in`, затем `out`, затем `is_done` (форма ). Внутри
/// направления порядок задан `BTreeMap` карты модели, то есть детерминирован даром.
pub(crate) fn emit_module_header(
    p: &mut Printer,
    module: &str,
    ports: &SvPorts,
    mmio: Option<&crate::generator::sv::sv_mmio::Mmio>,
    time_ms_bits: Option<u8>,
) {
    p.ident(&format!("module {} (", module)).nl();
    p.up();
    p.ident("input  logic clk,").nl();
    p.ident("input  logic rst_n,").nl();
    // Умолчание `1'b1` (IEEE 1800 §23.2.2.4): неподключённый `en` тождествен `en=1`,
    // поэтому существующие потребители не обязаны его подключать.
    p.ident("input  logic en = 1'b1,").nl();
    // Источник времени (профиль "часы"): внешний вход, как `clk`. Без умолчания -
    // миллисекунду подаёт тот, кто подал такт. Эмитится при использовании.
    if let Some(bits) = time_ms_bits {
        p.ident(&format!(
            "input  logic [{}:0] {},",
            bits.saturating_sub(1),
            crate::generator::sv::sv_time::TIME_MS_PORT
        ))
        .nl();
    }
    // Регистровый интерфейс цели `sv-mmio` - после служебных портов, до
    // пользовательских. В режиме `sv` (`mmio == None`) не эмитится.
    if let Some(m) = mmio {
        crate::generator::sv::sv_mmio::emit_reg_iface_lines(p, m);
    }
    for port in &ports.inputs {
        p.ident(&format!("input  {},", port.ty.declare(&port.name)))
            .nl();
    }
    for port in &ports.outputs {
        p.ident(&format!("output {},", port.ty.declare(&port.name)))
            .nl();
    }
    // Двунаправленный порт - тремя сигналами. Строб `_we` поднят ровно в тот такт,
    // когда модель записала порт: без него внешние изменения ячейки затирались бы
    // каждым тактом, ведь умолчание выхода - "как есть".
    for port in &ports.inouts {
        // Сторона чтения печатается по факту: у порта, который модель только пишет,
        // вход модуля никем не используется, и `verilator` под `-Wall` отвечает
        // `UNUSEDSIGNAL` - то есть вывод отвергает проверка самой цели.
        if port.is_read {
            p.ident(&format!(
                "input  {}, // inout '{}': сторона чтения",
                port.ty.declare(&inout_in(&port.name)),
                port.name
            ))
            .nl();
        }
        p.ident(&format!(
            "output {}, // inout '{}': сторона записи",
            port.ty.declare(&inout_out(&port.name)),
            port.name
        ))
        .nl();
        p.ident(&format!(
            "output logic {}, // inout '{}': строб записи (такт, в котором модель писала)",
            inout_we(&port.name),
            port.name
        ))
        .nl();
    }
    // Последним и без запятой: терминальность модели наблюдаема снаружи.
    p.ident("output logic is_done").nl();
    p.down();
    p.ident(");").nl();
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Location {
        Location::Codegen
    }

    /// Порт без начального значения - форма для проверок заголовка модуля.
    fn test_port(name: &str, ty: TypeNode) -> SvPort {
        SvPort {
            name: name.to_string(),
            ty: sv_type(&ty, "тест").unwrap(),
            ty_node: ty,
            init: ExpressionNode::None,
            loc: loc(),
            // Порт теста считается читаемым: проверки заголовка смотрят на форму
            // объявления, а не на признак чтения.
            is_read: true,
        }
    }

    /// Служебные имена цели отвергаются с `SV-007`.
    #[test]
    fn service_names_are_sv007() {
        for name in ["clk", "rst_n", "en", "is_done", "state", "state_next"] {
            let err = check_sv_name(name, loc()).unwrap_err();
            assert_eq!(err.code.as_deref(), Some("SV-007"), "имя {}", name);
        }
    }

    /// **Диагностика обязана объяснять, что язык не сломан.**
    ///
    /// `clk` - имя цели, а не ключевое слово Takt: та же модель компилируется в
    /// `c`/`st`/`rust`. Без этого пояснения автор решит, что сломан язык.
    #[test]
    fn sv007_explains_that_language_is_intact() {
        let err = check_sv_name("clk", loc()).unwrap_err();
        assert!(
            err.message.contains("НЕ \nключевое слово")
                || err.message.contains("НЕ ключевое слово"),
            "SV-007 обязана отрицать, что это ключевое слово Takt: {}",
            err.message
        );
        assert!(
            err.message.contains("'c'") && err.message.contains("'rust'"),
            "SV-007 обязана назвать цели, где модель по-прежнему валидна: {}",
            err.message
        );
    }

    /// **Контрпример из пробы 2026-07-16:** `fork` и `wire` - ключевые слова SV.
    ///
    /// Takt принимает `in fork: bit;`, цель `c` компилируется, а SV разваливается
    /// синтаксической ошибкой. Ни, ни план задачи этого класса не предусматривали.
    #[test]
    fn sv_keywords_are_sv012() {
        for name in [
            "fork", "wire", "reg", "always", "begin", "end", "time", "edge",
        ] {
            let err = check_sv_name(name, loc()).unwrap_err();
            assert_eq!(err.code.as_deref(), Some("SV-012"), "имя {}", name);
        }
    }

    /// Обиходные имена автоматики ключевыми словами не являются.
    ///
    /// Тест против переусердствования: список не должен отвергать нормальные имена
    /// корпуса.
    #[test]
    fn ordinary_names_are_accepted() {
        for name in [
            "cmd_fork",
            "lift_request",
            "task_valid",
            "elevator_motor_up",
            "sense_loaded",
            "current_floor",
        ] {
            assert!(
                check_sv_name(name, loc()).is_ok(),
                "имя {} отвергнуто",
                name
            );
        }
    }

    /// Заголовок модуля несёт служебные порты, которых в `.takt` нет.
    #[test]
    fn header_carries_service_ports() {
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        emit_module_header(&mut p, "stacker", &SvPorts::default(), None, None);
        assert!(out.contains("input  logic clk,"), "нет clk:\n{out}");
        assert!(out.contains("input  logic rst_n,"), "нет rst_n:\n{out}");
        assert!(out.contains("output logic is_done"), "нет is_done:\n{out}");
        assert!(out.contains("module stacker ("), "нет имени модуля:\n{out}");
    }

    /// Заголовок несёт вход `en` с умолчанием `1'b1`.
    ///
    /// Умолчание (IEEE 1800 §23.2.2.4) делает порт необязательным: неподключённый `en`
    /// тождествен `en = 1`, поэтому сверка, которая `en` не подключает, остаётся зелёной.
    #[test]
    fn header_carries_clock_enable_with_default() {
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        emit_module_header(&mut p, "stacker", &SvPorts::default(), None, None);
        assert!(
            out.contains("input  logic en = 1'b1,"),
            "нет en с умолчанием 1'b1:\n{out}"
        );
    }

    /// `in` даёт `input logic`, `out` - `output logic`; порядок печати - вход, затем
    /// выход.
    #[test]
    fn header_emits_directions_in_order() {
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        let ports = SvPorts {
            inputs: vec![test_port("lift_request", TypeNode::Bit)],
            outputs: vec![test_port("cmd_fork", TypeNode::Bit)],
            inouts: Vec::new(),
        };
        emit_module_header(&mut p, "stacker", &ports, None, None);
        assert!(
            out.contains("input  logic lift_request,"),
            "нет входного порта:\n{out}"
        );
        assert!(
            out.contains("output logic cmd_fork,"),
            "нет выходного порта:\n{out}"
        );
        let inp = out.find("lift_request").unwrap();
        let outp = out.find("cmd_fork").unwrap();
        assert!(inp < outp, "входы обязаны идти раньше выходов:\n{out}");
        // Ни колбэков, ни volatile, ни AT % - порт здесь и есть порт.
        assert!(!out.contains("volatile") && !out.contains("AT %") && !out.contains("write_bit"));
    }

    /// Многобитный порт несёт упакованную ширину.
    #[test]
    fn header_emits_vector_port_width() {
        let mut out = String::new();
        let mut p = Printer::new(4, &mut out);
        let ports = SvPorts {
            inputs: vec![test_port(
                "task_stack_no",
                TypeNode::Integer {
                    bits: 8,
                    signed: false,
                },
            )],
            outputs: Vec::new(),
            inouts: Vec::new(),
        };
        emit_module_header(&mut p, "stacker", &ports, None, None);
        assert!(
            out.contains("input  logic [7:0] task_stack_no,"),
            "нет ширины порта:\n{out}"
        );
    }
}
