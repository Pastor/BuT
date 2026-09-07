//! Регистровый файл цели `sv-mmio`.
//!
//! ## Что делает цель
//!
//! Карта адресов ([0020](../../../../docs/features/0020-port-address-decl.md)) целью
//! `sv` **не потребляется** - MMIO-адрес для RTL бессмыслен. Но осмысленна другая
//! трактовка: породить **регистровый файл** - модуль с синхронным регистровым
//! интерфейсом, где адрес порта задаёт смещение регистра, а бит - позицию внутри слова.
//! Это парная цель, как `c-hal` парная к `c`.
//!
//! ## Форма интерфейса - единственное, что здесь изобретается
//!
//! оставил **протокол** шины (APB/AXI-Lite/Wishbone): выбор
//! произволен, а цена ошибки высока (возражение ). Поэтому интерфейс -
//! **шинно-агностичный** и синхронный: `reg_addr` (адрес), `reg_wdata` (данные
//! записи), `reg_wen` (строб записи), `reg_rdata` (данные чтения, комбинационные).
//! Адаптер под конкретный протокол - тонкий шим поверх, отдельная фича по
//! требованию (A-2 ). BFM тест-плану не нужен: интерфейс дёргается напрямую.
//!
//! ## Направление принадлежит биту, а не слову
//!
//! Одно слово может нести биты обоих направлений - это **факт корпуса**
//! (`extend_complex.takt`: `out :1`, `out :2`, `in :33`), а не гипотеза. Поэтому:
//!
//! - бит **`out`** - регистр автомата (защёлкнут в `always_ff` автомата),
//!   шина его только **читает**; запись шиной **игнорируется** ( -
//!   иначе конфликт драйверов);
//! - бит **`in`** - регистр, **записываемый шиной** и читаемый автоматом;
//!   чтение шиной возвращает записанное ().
//!
//! ## Ширина данных - по старшему занятому биту; предел - 64
//!
//! Ширина слова данных ([`Mmio::data_width`]) - максимум `bit + width` по портам
//! модели, **не** фиксированные 64: лишние старшие биты `reg_wdata` повисли бы как
//! `UNUSEDSIGNAL` у `verilator -Wall` (глушить его `lint_off` правило проекта
//! запрещает). Жёсткий предел - 64 ([`MAX_REG_WIDTH`]): `SE-060` держит бит в `[0,
//! 63]`, то есть регистр не шире `uint64_t` (то же слово, что читает умолчательный HAL
//! стороны `c-hal`). Порт занимает срез `reg_*[bit +: width]`; выход за 64 (`bit +
//! width > 64`) - **отказ** (`SV-013`), а не догадка (R6).

use crate::address_map::ResolvedAddress;
use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::sv::sv_fsm::Block;
use crate::generator::sv::sv_type::{SvType, enum_width, sv_type};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, PortDirection, VariableNode};
use std::collections::{BTreeMap, BTreeSet, HashMap};

/// Жёсткий предел ширины регистра (бит).
///
/// 64 - предел `SE-060` (бит адреса в `[0, 63]`) и слово принятого по умолчаниюо HAL стороны `c-hal`
/// (`uint64_t`). Порт с `bit + width > 64` не помещается в регистр ->
/// [`SV-013`](sv013). **Реальная** ширина шины данных ([`Mmio::data_width`]) - максимум
/// `bit + width` по портам модели: шире, чем нужно, шину не эмитим, иначе `verilator
/// -Wall` даёт `UNUSEDSIGNAL` на старших битах `reg_wdata` (глушить его `lint_off`
/// правило проекта запрещает).
const MAX_REG_WIDTH: u32 = 64;

/// Имена, которые порождает сам регистровый интерфейс цели `sv-mmio`.
///
/// Совпадение пользовательского **неадресованного** порта/переменной с любым из них
/// дало бы два объявления одного идентификатора - [`SV-014`](sv014). (У `sv` этих имён
/// нет, поэтому в общий `RESERVED_NAMES` они не вынесены: там - имена, которые цель
/// порождает **всегда**.)
const REG_IFACE_NAMES: &[&str] = &["reg_addr", "reg_wdata", "reg_wen", "reg_rdata"];

/// Строит диагностику `SV-013` - срез порта не помещается в 64-битный регистр.
fn sv013(name: &str, bit: i64, width: u32) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!(
            "порт '{}' занимает биты [{}..{}] адреса, но регистр цели 'sv-mmio' \
             шириной 64 бита (слово дефолтного HAL): срез bit+width={} выходит за \
             границу. Ширину адресуемого слова язык Takt не выражает, поэтому \
             угадать её нельзя — сузьте тип порта или сместите бит так, чтобы \
             bit+width не превышало 64",
            name,
            bit,
            bit + i64::from(width) - 1,
            bit + i64::from(width)
        ),
    )
    .with_code("SV-013")
}

/// Строит диагностику `SV-014` - имя совпало с сигналом регистрового интерфейса.
fn sv014(name: &str) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!(
            "имя '{}' зарезервировано регистровым интерфейсом цели 'sv-mmio' \
             (reg_addr/reg_wdata/reg_wen/reg_rdata — сигналы шины, которых в .takt \
             нет). Это НЕ ключевое слово языка Takt: модель остаётся валидной для \
             целей 'c', 'c-hal', 'plantuml', 'st', 'rust' и 'sv'. Переименуйте \
             элемент в исходнике .takt, если модель нужна как регистровый файл",
            name
        ),
    )
    .with_code("SV-014")
}

/// Строит диагностику `SV-002` - тип порта не ложится на биты регистра.
///
/// Шаблон **общий** (`sv_expr::sv002`): своя копия давала бы одному коду два разных
/// вида сообщения - /0193/0195.
fn sv002_width(name: &str, ty: &TypeNode) -> Diagnostic {
    crate::generator::sv::sv_expr::sv002(&format!(
        "порт '{name}' с адресом имеет тип '{ty}', ширина которого в битах не \
         определена (регистровый файл цели 'sv-mmio' раскладывает порт по битам \
         слова). Адресуйте порт скалярного типа (bit/целое/q) либо используйте \
         цель 'sv' без адресов"
    ))
}

/// Адресованный порт: занимает срез `[bit +: width]` регистра по адресу `addr`.
pub(crate) struct MmioPort {
    /// Имя порта - как в исходнике `.takt` (совпадает с именем сигнала автомата).
    pub(crate) name: String,
    /// Тип порта (для объявления регистра автомата у `out`-битов).
    pub(crate) ty: TypeNode,
    /// Начальное значение порта (`:=` объявления).
    ///
    /// [`ExpressionNode::None`], если не задано. Берётся из **объявления**, а не из
    /// [`ResolvedAddress`]: начальное значение - свойство порта, а не его размещения, и
    /// в слое адресов (а значит и в выгрузке карты) ему не место.
    pub(crate) init: ExpressionNode,
    /// Позиция объявления - для диагностики о непечатаемом значении сброса.
    pub(crate) loc: Location,
    /// Адрес регистра.
    addr: i64,
    /// Начальный бит внутри слова (умолчание 0, если `0xADDR` без `:bit`).
    bit: i64,
    /// Ширина порта в битах.
    width: u32,
    /// Направление: `in` - пишется шиной; `out` - читается шиной.
    direction: PortDirection,
    /// Значение сброса регистра `in`-порта.
    ///
    /// У перечислимого типа это **первый по тексту вариант**, а не `'0`: ноль может не
    /// принадлежать набору, и `verilator` отвечает `ENUMVALUE`. Считается при
    /// постройке: набор вариантов известен здесь, а печать его уже не видит.
    reset: String,
}

/// Регистровый файл: адресованные порты и ширины шин.
pub(crate) struct Mmio {
    /// Адресованные порты, отсортированы по имени (детерминизм).
    ports: Vec<MmioPort>,
    /// Ширина `reg_addr` в битах - по **максимальному** адресу модели (R3 ).
    addr_width: u32,
    /// Ширина `reg_wdata`/`reg_rdata` в битах - максимум `bit + width` по портам (не
    /// [`MAX_REG_WIDTH`]: лишние старшие биты дали бы `UNUSEDSIGNAL`).
    data_width: u32,
}

impl Mmio {
    /// Строит регистровый файл из разрешённой карты адресов.
    ///
    /// `address_map` уже прошёл `resolve_addresses` (приоритет источников + `SE-060` на
    /// бит вне `[0, 63]`), поэтому здесь остаются лишь проверки, специфичные
    /// регистровому файлу: ширина среза (`SV-013`), тип (`SV-002`), коллизия имени с
    /// интерфейсом (`SV-014`) и невыразимое направление `inout`.
    ///
    /// # Ошибки
    /// [`SV-013`](sv013), [`SV-014`](sv014), [`SV-002`](sv002_width),
    /// `SV-006` (через [`sv_type`] на несовместимом типе - не наступает для
    /// скаляров).
    pub(crate) fn build(
        blocks: &[Block],
        address_map: &HashMap<String, ResolvedAddress>,
        anon_cells: &[crate::semantic::AnonPortAccess],
    ) -> Result<Self, Diagnostic> {
        // Перечисления собираются со всех уровней: ширина enum-порта считается по
        // диапазону его значений (как в `sv_type::enum_width`).
        let mut enums: BTreeMap<String, Vec<(String, i128)>> = BTreeMap::new();
        for (_, model_rc) in blocks {
            for def in model_rc.borrow().enums.values() {
                enums
                    .entry(def.name.clone())
                    .or_insert_with(|| def.variants.clone());
            }
        }

        // Коллизия неадресованного сигнала с именем интерфейса. Проверяются все
        // порты/переменные/константы модели: адресованный порт `reg_addr` стал бы битом
        // регистра, но неадресованный - портом модуля рядом с сигналом интерфейса.
        for (_, model_rc) in blocks {
            for name in model_rc.borrow().variables.keys() {
                if REG_IFACE_NAMES.contains(&name.as_str()) {
                    return Err(sv014(name));
                }
            }
        }

        // Начальные значения портов - из объявлений. Ключ голый, как и `resolved.name`:
        // регистровый файл и без того плоский по имени порта, и одноимённые порты
        // разных под-моделей делят в нём один бит (ограничение цели, не этого сбора).
        // Побеждает **первое** объявление с непустым значением - иначе порядок обхода
        // `variables` (HashMap) решал бы, чем сбрасывается регистр.
        let mut inits: BTreeMap<String, (ExpressionNode, Location)> = BTreeMap::new();
        for (_, model_rc) in blocks {
            for var in model_rc.borrow().variables.values() {
                let VariableNode::Port {
                    name, init, loc, ..
                } = var
                else {
                    continue;
                };
                if matches!(init, ExpressionNode::None) {
                    continue;
                }
                inits
                    .entry(name.clone())
                    .or_insert_with(|| (init.clone(), *loc));
            }
        }

        // Позиция объявления порта: разрешённый адрес её не несёт (она не нужна ни
        // выгрузке карты, ни регистровому файлу), а отказу нужна - иначе он печатается
        // без координаты. Берётся первое объявление: одноимённые порты разных
        // под-моделей делят один бит, и это ограничение цели, а не выбор здесь.
        let mut locs: BTreeMap<String, Location> = BTreeMap::new();
        for (_, model_rc) in blocks {
            for var in model_rc.borrow().variables.values() {
                if let VariableNode::Port { name, loc, .. } = var {
                    locs.entry(name.clone()).or_insert(*loc);
                }
            }
        }

        let mut ports: Vec<MmioPort> = Vec::new();
        for resolved in address_map.values() {
            if let Some(loc) = locs.get(&resolved.name) {
                crate::generator::site::enter_declaration(*loc);
            }
            // ключ карты квалифицирован моделью; имя регистра (пользовательское) -
            // голое `resolved.name`, не ключ.
            let name = &resolved.name;
            let what = format!("порт '{}'", name);
            let width = bit_width(&resolved.ty, &enums, &what)
                .ok_or_else(|| sv002_width(name, &resolved.ty))?;
            let bit = resolved.bit.unwrap_or(0);
            // Бит уже в [0, 63] (SE-060). Здесь - верхняя граница среза.
            if bit + i64::from(width) > i64::from(MAX_REG_WIDTH) {
                return Err(sv013(name, bit, width));
            }
            if matches!(resolved.direction, PortDirection::InOut) {
                return Err(Diagnostic::error(
                    crate::generator::site::at(Location::Codegen),
                    format!(
                        "порт '{}': направление 'inout' целью 'sv-mmio' не \
                         поддерживается — направление принадлежит биту регистра \
                         (бит либо пишется шиной, либо читается ею), а inout не \
                         выражает, когда бит ведёт линию. Разделите порт на \
                         входной и выходной",
                        name
                    ),
                )
                .with_code("SV-006"));
            }
            // Имя порта в модуле уникально (то же правило, что у `collect_ports` цели
            // `sv`): под `--parameters=specialize` копии модели дают один порт по
            // одному адресу дважды, и `verilator` отвечал "Duplicate declaration of
            // signal" при нулевом коде возврата `taktc` (замер 0457).
            if ports.iter().any(|p| p.name == *name) {
                continue;
            }
            let (init, loc) = inits
                .get(name)
                .cloned()
                .unwrap_or((ExpressionNode::None, Location::Codegen));
            ports.push(MmioPort {
                name: name.clone(),
                ty: resolved.ty.clone(),
                init,
                loc,
                addr: resolved.addr,
                bit,
                width,
                direction: resolved.direction,
                reset: reset_literal(&resolved.ty, &enums),
            });
        }
        // Слой объявления снимается парно входу.
        crate::generator::site::leave_declaration();
        // Анонимные ячейки: у них нет объявления, но в регистровом файле они - такой же
        // бит по адресу, как порт. Направления автор не объявлял, поэтому ячейка ведёт
        // себя как **`out`**: регистр внутри модуля, который пишет и читает автомат, а
        // шина только читает ( - запись шиной в такой бит игнорируется, иначе
        // получились бы два драйвера одного разряда).
        //
        // Отсюда ограничение цели, которое обязано быть в документе: в RTL ячейка не
        // может измениться "снаружи" между тактами - внешнего устройства у модуля нет.
        // Эталон (решение 5B) устроен так же, поэтому сверка трасс осмысленна.
        for cell in anon_cells {
            let width = u32::from(cell.width_bits());
            if cell.bit + i64::from(width) > i64::from(MAX_REG_WIDTH) {
                return Err(sv013(&cell.synthetic_name(), cell.bit, width));
            }
            ports.push(MmioPort {
                name: cell.synthetic_name(),
                ty: cell.ty.clone(),
                init: ExpressionNode::None,
                loc: Location::Codegen,
                addr: cell.addr,
                bit: cell.bit,
                width,
                direction: PortDirection::Out,
                reset: reset_literal(&cell.ty, &enums),
            });
        }

        // Детерминизм эмиссии: порядок задаётся именем, а не обходом `HashMap`.
        // Группировка по адресу в эмиттерах - через `BTreeMap`.
        ports.sort_by(|a, b| a.name.cmp(&b.name));

        let max_addr = ports.iter().map(|p| p.addr).max().unwrap_or(0);
        let addr_width = address_bits(max_addr);
        // Ширина данных - по самому старшему занятому биту (min 1). Шире не надо:
        // старшие биты `reg_wdata` иначе повисли бы как `UNUSEDSIGNAL`.
        let data_width = ports
            .iter()
            .map(|p| (p.bit + i64::from(p.width)) as u32)
            .max()
            .unwrap_or(1)
            .max(1);

        Ok(Self {
            ports,
            addr_width,
            data_width,
        })
    }

    /// Имена адресованных портов - их `collect_ports` исключает из портов модуля.
    pub(crate) fn addressed_names(&self) -> BTreeSet<String> {
        self.ports.iter().map(|p| p.name.clone()).collect()
    }

    /// Адресованные `out`-порты: становятся внутренними регистрами автомата.
    pub(crate) fn outputs(&self) -> impl Iterator<Item = &MmioPort> {
        self.ports
            .iter()
            .filter(|p| matches!(p.direction, PortDirection::Out))
    }

    /// Есть ли хоть один адресованный порт (иначе интерфейс не эмитится).
    pub(crate) fn is_empty(&self) -> bool {
        self.ports.is_empty()
    }

    /// Есть ли регистр, который **пишет шина** (входной порт с адресом).
    ///
    /// сигналы записи (`reg_wdata`/`reg_wen`) эмитятся только при `true`. Иначе они
    /// остаются неподключёнными, и `verilator -Wall` отвечает `UNUSEDSIGNAL` - замер на
    /// `examples/regulator.takt` (все порты `out`) давал два таких предупреждения, а с
    /// `-Wall` это ненулевой код возврата.
    ///
    /// Правило не новое, а достроенное: интерфейс уже отражал модель на соседнем случае -
    /// при [`is_empty`](Self::is_empty) не эмитится вовсе ничего. Не хватало различения
    /// по **направлению**.
    pub(crate) fn has_writable(&self) -> bool {
        self.ports
            .iter()
            .any(|p| matches!(p.direction, PortDirection::In))
    }

    /// Ширина `reg_addr` в битах - нужна адаптеру шины.
    pub(crate) fn addr_width(&self) -> u32 {
        self.addr_width
    }

    /// Ширина `reg_wdata`/`reg_rdata` в битах - нужна адаптеру шины.
    pub(crate) fn data_width(&self) -> u32 {
        self.data_width
    }

    /// Диапазоны `reg_wdata`, которые не занимает ни один входной порт.
    ///
    /// Ширина слова считается по **всем** портам, а `reg_wdata` читают лишь
    /// **входные** (`out`-биты запись игнорируют, R5). Поэтому модель, где
    /// входной порт уже выходного, оставляла старшие биты входа висящими:
    /// `verilator -Wall` отвечал `UNUSEDSIGNAL: Bits of signal are not used`
    /// при **нулевом** коде возврата `taktc` (замер 0486; тот же класс, что
    /// 0214, но про биты, а не про сигнал целиком).
    ///
    /// Возвращает пары `(hi, lo)` - в форме среза SystemVerilog.
    fn unused_wdata_ranges(&self) -> Vec<(u32, u32)> {
        let mut used = vec![false; self.data_width as usize];
        for port in &self.ports {
            if !matches!(port.direction, PortDirection::In) {
                continue;
            }
            let from = usize::try_from(port.bit.max(0)).unwrap_or(0);
            let to = (from + port.width as usize).min(used.len());
            for slot in used.iter_mut().take(to).skip(from) {
                *slot = true;
            }
        }
        let mut ranges = Vec::new();
        let mut start: Option<u32> = None;
        for (idx, taken) in used.iter().enumerate() {
            let idx = idx as u32;
            match (taken, start) {
                (false, None) => start = Some(idx),
                (true, Some(lo)) => {
                    ranges.push((idx - 1, lo));
                    start = None;
                }
                _ => {}
            }
        }
        if let Some(lo) = start {
            ranges.push((self.data_width - 1, lo));
        }
        ranges
    }

    /// Группировка портов по адресу (адреса - по возрастанию, детерминизм).
    fn by_address(&self) -> BTreeMap<i64, Vec<&MmioPort>> {
        let mut groups: BTreeMap<i64, Vec<&MmioPort>> = BTreeMap::new();
        for port in &self.ports {
            groups.entry(port.addr).or_default().push(port);
        }
        groups
    }
}

/// Тип `out`-порта в объявлении регистра автомата (для `sv_fsm::Fsm::build`).
pub(crate) fn port_sv_type(port: &MmioPort) -> Result<SvType, Diagnostic> {
    sv_type(&port.ty, &format!("порт '{}'", port.name))
}

/// Имя сигнала `out`-порта - совпадает с именем в `.takt` (как у портов модуля).
pub(crate) fn port_signal_name(port: &MmioPort) -> &str {
    &port.name
}

/// Ширина типа в битах для среза регистра.
///
/// Возвращает `None`, если ширина не определена (массив, структура, `float`,
/// неразрешённый тип) - такой порт битом регистра быть не может.
fn bit_width(
    ty: &TypeNode,
    enums: &BTreeMap<String, Vec<(String, i128)>>,
    what: &str,
) -> Option<u32> {
    match ty {
        // Перечисление ширины не имеет само по себе - её задаёт набор вариантов.
        TypeNode::Enum(name) => {
            let variants = enums.get(name)?;
            enum_width(variants, what).ok().map(|(w, _)| w)
        }
        // Прочие скаляры считает общий носитель `sv_type::scalar_width`, а не вторая
        // копия таблицы: копия отстала на `duration` - цель `sv` порт такого типа
        // печатала, а `sv-mmio` отвечала `SV-002` "ширина не определена", хотя
        // длительность - целое в миллисекундах.
        _ => crate::generator::sv::sv_type::scalar_width(ty).filter(|w| *w > 0),
    }
}

/// Значение сброса регистра: у перечисления - первый по тексту вариант.
///
/// `'0` перечислимому сигналу не годится: ноль может не принадлежать набору, и
/// `verilator` отвечает `ENUMVALUE`. Тот же класс, что, только там он жил в регистрах
/// автомата, а здесь - в регистровом файле.
fn reset_literal(ty: &TypeNode, enums: &BTreeMap<String, Vec<(String, i128)>>) -> String {
    match ty {
        TypeNode::Enum(name) => enums
            .get(name)
            .and_then(|variants| crate::semantic::enum_default(variants))
            .map(|(variant, _)| {
                crate::generator::sv::sv_names::sv_enum_variant_name(name, &variant)
            })
            // Перечисления без вариантов не бывает (`SE-105`, 0172) - ветвь защитная.
            .unwrap_or_else(|| "'0".to_string()),
        _ => "'0".to_string(),
    }
}

/// Число бит, нужное для представления адреса `max_addr` (минимум 1).
fn address_bits(max_addr: i64) -> u32 {
    let m = max_addr.max(0) as u64;
    if m == 0 { 1 } else { 64 - m.leading_zeros() }
}

/// Литерал адреса в формате SV: `<addr_width>'h<hex>`.
fn addr_literal(addr: i64, addr_width: u32) -> String {
    format!("{}'h{:x}", addr_width, addr)
}

/// Печатает строки регистрового интерфейса в заголовок модуля (после `en`).
///
/// Вызывается из
/// [`sv_module::emit_module_header`](super::sv_module::emit_module_header); при
/// отсутствии адресованных портов не печатает ничего (модуль вырождается в обычный
/// `sv`).
pub(crate) fn emit_reg_iface_lines(p: &mut Printer, mmio: &Mmio) {
    if mmio.is_empty() {
        return;
    }
    let aw = mmio.addr_width;
    let dw = mmio.data_width;
    p.ident(&format!("input  logic [{}:0] reg_addr,", aw - 1))
        .nl();
    // сигналы записи - только при наличии записываемого регистра. Модуль без входных
    // портов доступен шине лишь на чтение, и объявлять ей вход данных значило бы
    // обещать запись, которой нет.
    if mmio.has_writable() {
        p.ident(&format!("input  logic [{}:0] reg_wdata,", dw - 1))
            .nl();
        p.ident("input  logic reg_wen,").nl();
    }
    p.ident(&format!("output logic [{}:0] reg_rdata,", dw - 1))
        .nl();
}

/// Печатает поглотитель битов `reg_wdata`, которых не занимает ни один вход.
///
/// Идиома та же, что у обёртки APB и у неиспользуемого параметра функции
/// редукция с константой `1'b0`. Биты честно
/// **используются**, а синтезатор эту логику выбрасывает сам -
/// `lint_off` правило проекта запрещает: прагма гасит теста, а не
/// причину.
///
/// Гасятся **только непокрытые** диапазоны, а не сигнал целиком: покрытие считается по
/// объявленным портам, поэтому сломайся печать среза - бит останется неиспользованным в
/// тексте, и `verilator` об этом скажет.
fn emit_wdata_guard(p: &mut Printer, mmio: &Mmio) {
    if !mmio.has_writable() {
        return;
    }
    let ranges = mmio.unused_wdata_ranges();
    if ranges.is_empty() {
        return;
    }
    let slices: Vec<String> = ranges
        .iter()
        .map(|(hi, lo)| {
            if hi == lo {
                format!("reg_wdata[{hi}]")
            } else {
                format!("reg_wdata[{hi}:{lo}]")
            }
        })
        .collect();
    p.ident(&format!(
        "wire _unused_wdata = &{{1'b0, {}}};",
        slices.join(", ")
    ))
    .nl()
    .nl();
}

/// Печатает регистровый файл: объявление входных регистров, их защёлкивание шиной и
/// комбинационное чтение.
///
/// Выходные адресованные порты - уже регистры автомата (объявлены и защёлкнуты
/// `sv_fsm`), поэтому здесь только читаются мультиплексором `reg_rdata`.
pub(crate) fn emit_register_file(p: &mut Printer, mmio: &Mmio) {
    if mmio.is_empty() {
        return;
    }
    emit_wdata_guard(p, mmio);
    let aw = mmio.addr_width;
    let inputs: Vec<&MmioPort> = mmio
        .ports
        .iter()
        .filter(|p| matches!(p.direction, PortDirection::In))
        .collect();

    // Объявление входных регистров: их пишет шина, а не автомат, поэтому у них нет
    // комбинационной пары `_next` (автомат читает их как значение регистра, ровно как
    // раньше читал входной порт модуля).
    if !inputs.is_empty() {
        for port in &inputs {
            let ty = sv_type(&port.ty, "").unwrap_or(SvType {
                prefix: "logic".to_string(),
                suffix: String::new(),
            });
            p.ident(&format!("{};", ty.declare(&port.name))).nl();
        }
        p.nl();

        // Защёлкивание входов шиной. Отдельный always_ff: каждый входной регистр имеет
        // ровно один драйвер (эту шину); регистры автомата - свой always_ff.
        p.ident("always_ff @(posedge clk) begin").nl();
        p.up();
        p.ident("if (!rst_n) begin").nl();
        p.up();
        for port in &inputs {
            // Перечислимый сигнал сбрасывается своим умолчанием - первым по тексту
            // вариантом: `'0` может не принадлежать набору, и `verilator` отвечает
            // `ENUMVALUE` (тот же класс, что, но в регистровом файле - замер 0452).
            p.ident(&format!("{} <= {};", port.name, port.reset)).nl();
        }
        p.down();
        p.ident("end else if (reg_wen) begin").nl();
        p.up();
        p.ident("unique case (reg_addr)").nl();
        p.up();
        // Группировка по адресу: у слова со смешанными направлениями пишутся только
        // in-биты (out игнорирует запись, R5).
        let mut in_groups: BTreeMap<i64, Vec<&MmioPort>> = BTreeMap::new();
        for port in &inputs {
            in_groups.entry(port.addr).or_default().push(port);
        }
        for (addr, group) in &in_groups {
            p.ident(&format!("{}: begin", addr_literal(*addr, aw))).nl();
            p.up();
            for port in group {
                // Перечислимому сигналу сырые биты не присваиваются: перечисления в SV
                // строго типизированы, и `verilator` отвечает `ENUMVALUE` - "Implicit
                // conversion to enum from 'bit'" (замер 0452). Приведение печатается
                // только ему: у прочих типов оно было бы шумом.
                let slice = format!("reg_wdata[{} +: {}]", port.bit, port.width);
                let value = match &port.ty {
                    TypeNode::Enum(name) => {
                        format!(
                            "{}'({slice})",
                            crate::generator::sv::sv_type::sv_enum_type_name(name)
                        )
                    }
                    _ => slice,
                };
                p.ident(&format!("{} <= {value};", port.name)).nl();
            }
            p.down();
            p.ident("end").nl();
        }
        p.ident("default: ;").nl();
        p.down();
        p.ident("endcase").nl();
        p.down();
        p.ident("end").nl();
        p.down();
        p.ident("end").nl().nl();
    }

    // Чтение регистров шиной (комбинационное): собирает слово из всех портов по адресу -
    // и out (регистр автомата), и in (чтение возвращает записанное, R5).
    p.ident("always_comb begin").nl();
    p.up();
    p.ident("reg_rdata = '0;").nl();
    p.ident("unique case (reg_addr)").nl();
    p.up();
    for (addr, group) in &mmio.by_address() {
        p.ident(&format!("{}: begin", addr_literal(*addr, aw))).nl();
        p.up();
        for port in group {
            p.ident(&format!(
                "reg_rdata[{} +: {}] = {};",
                port.bit, port.width, port.name
            ))
            .nl();
        }
        p.down();
        p.ident("end").nl();
    }
    p.ident("default: reg_rdata = '0;").nl();
    p.down();
    p.ident("endcase").nl();
    p.down();
    p.ident("end").nl().nl();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn address_bits_counts_significant_bits() {
        assert_eq!(address_bits(0), 1);
        assert_eq!(address_bits(1), 1);
        assert_eq!(address_bits(0x601), 11); // stacker: 1537 -> 11 бит
        assert_eq!(address_bits(0x10000000), 29); // elevator
    }

    #[test]
    fn addr_literal_is_sized_hex() {
        assert_eq!(addr_literal(0x100, 11), "11'h100");
    }

    #[test]
    fn bit_width_of_scalars() {
        let enums = BTreeMap::new();
        assert_eq!(bit_width(&TypeNode::Bit, &enums, ""), Some(1));
        assert_eq!(
            bit_width(
                &TypeNode::Integer {
                    bits: 8,
                    signed: false
                },
                &enums,
                ""
            ),
            Some(8)
        );
        assert_eq!(
            bit_width(
                &TypeNode::Fixed {
                    m: 8,
                    n: 8,
                    sat: false
                },
                &enums,
                ""
            ),
            Some(16)
        );
        // Массив/структура/float - ширина не определена.
        assert_eq!(bit_width(&TypeNode::Rational, &enums, ""), None);
    }
}
