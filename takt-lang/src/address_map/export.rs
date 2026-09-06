//! Экспорт разрешённой карты адресов **наружу**.
//!
//! Тема самостоятельна: [`parse`](super::parse) читает внешний файл карты,
//! [`resolve`](super::resolve) сводит адреса модели, а этот модуль **выгружает**
//! разрешённую карту в текст для сторонних инструментов.
//!
//! Формат `map` - тот же `.ld`-подобный синтаксис, что читает
//! [`parse_address_map`](super::parse::parse_address_map): `NAME = 0xADDR[:bit];`. Это
//! даёт **круговой рейс** `export -> parse -> export` = побайтовое равенство: выгрузку
//! можно подать обратно во флаг `--address-map`.
//!
//! Формат `json` - машиночитаемая выгрузка с типом, направлением и источником адреса
//! (для генераторов HAL); версионирована (`format_version`), перечень источников
//! **открыт** (потребитель обязан терпеть незнакомое значение -, R7). JSON печатается
//! **вручную**: имена портов - идентификаторы Takt, экранирование строк тривиально, и
//! это не тянет `serde_json` в умолчательный бинарник `taktc` (тот собирается без фичи
//! `lsp`).

use super::parse::AddressMapEntry;
use super::resolve::{AddressResolution, AddressSource, ResolvedAddress};
use crate::semantic::PortDirection;
use std::fmt::Write;

/// Версия формата выгрузки. Растёт при несовместимом изменении схемы `json`;
/// потребитель обязан её проверять.
pub const EXPORT_FORMAT_VERSION: u32 = 1;

/// Печать имени источника адреса. Перечень **открыт** (R7): потребитель обязан терпеть
/// незнакомое значение, а не падать. Строчные - как в JSON принято.
fn source_name(source: AddressSource) -> &'static str {
    match source {
        AddressSource::Inline => "inline",
        AddressSource::Operator => "operator",
        AddressSource::External => "external",
    }
}

/// Печать направления порта для JSON.
fn direction_name(direction: PortDirection) -> &'static str {
    match direction {
        PortDirection::In => "in",
        PortDirection::Out => "out",
        PortDirection::InOut => "inout",
    }
}

/// Записи разрешённой карты, **схлопнутые по голому имени** и отсортированные.
///
/// Публичные форматы (`.ld` `map`, `json`) - **плоские** по имени порта: одна запись на
/// имя. Ключ карты с 0084 квалифицирован моделью, поэтому одноимённые порты разных
/// под-моделей дают **несколько** значений с одним `name`; для плоской выгрузки они
/// схлопываются, **побеждает последний** (детерминированно - с максимальным
/// квалифицированным ключом, т.е. последняя под-модель в порядке обхода, как было до
/// 0084). Так круговой рейс (R4 0043) остаётся тождеством, а вывод корпуса -
/// байт-в-байт (коллизий в корпусе нет). Полный список обоих портов виден цели `c-hal`
/// (карта не потеряла данных) - это и есть исправление 0084; плоскую выгрузку оно не
/// меняет.
///
/// Порядок **детерминирован** (`map` - `HashMap`, иначе выгрузка "плавала" бы и ломала
/// идемпотентность кругового рейса и `diff` между ревизиями).
fn sorted_entries(resolution: &AddressResolution) -> Vec<&ResolvedAddress> {
    let mut by_name: std::collections::HashMap<&str, (&str, &ResolvedAddress)> =
        std::collections::HashMap::new();
    for (key, ra) in &resolution.map {
        match by_name.get(ra.name.as_str()) {
            // Побеждает запись с максимальным квалиф. ключом (порядок моделей).
            Some((prev_key, _)) if *prev_key >= key.as_str() => {}
            _ => {
                by_name.insert(ra.name.as_str(), (key.as_str(), ra));
            }
        }
    }
    let mut entries: Vec<&ResolvedAddress> = by_name.into_values().map(|(_, ra)| ra).collect();
    entries.sort_by(|a, b| a.name.cmp(&b.name));
    entries
}

/// Печать одной строки `map`: `NAME = 0xADDR[:bit];`.
///
/// Адрес - `0x` + 8 hex-цифр с ведущими нулями (`{:#010x}`): типовая ширина MMIO-адреса
/// и совпадает с примерами формата. Круговой рейс не зависит от ширины (парсер
/// принимает любую), но исправлениеированная ширина делает `diff` осмысленным.
fn write_map_line(out: &mut String, name: &str, addr: i64, bit: Option<i64>) {
    match bit {
        Some(b) => {
            let _ = writeln!(out, "{} = {:#010x}:{};", name, addr, b);
        }
        None => {
            let _ = writeln!(out, "{} = {:#010x};", name, addr);
        }
    }
}

/// Выгрузка разрешённой карты в формат `map` (`.ld`-подобный).
///
/// Печатаются **только** порты, у которых адрес разрешён (те, что в
/// [`AddressResolution::map`]). Порт без адреса **опускается** - категорически не
/// подставляется `0x0` (в выгрузке он неотличим от настоящего адреса; главный способ
/// соврать -, R8). Результат разбирается
/// [`parse_address_map`](super::parse::parse_address_map) без диагностик.
pub fn export_address_map(resolution: &AddressResolution) -> String {
    let mut out = String::new();
    for ra in sorted_entries(resolution) {
        write_map_line(&mut out, &ra.name, ra.addr, ra.bit);
    }
    out
}

/// Повторная выгрузка **разобранной** карты в формат `map` - второе плечо кругового
/// рейса (`export -> parse_address_map -> export`, R4). Печатает те же строки, что
/// [`export_address_map`], поэтому идемпотентность байт-в-байт - свойство общего
/// [`write_map_line`], а не совпадение.
pub fn export_map_entries(entries: &[AddressMapEntry]) -> String {
    let mut sorted: Vec<&AddressMapEntry> = entries.iter().collect();
    sorted.sort_by(|a, b| a.name.cmp(&b.name));
    let mut out = String::new();
    for e in sorted {
        write_map_line(&mut out, &e.name, e.addr, e.bit);
    }
    out
}

/// Экранирование строки для JSON. Имена портов и печать типа (`bit`, `u8`, `q(8, 8)`,
/// `[bit;8]`, имена enum) кавычек/бэкслешей не содержат, но экранируем защитно - на
/// случай будущих типов.
fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            _ => out.push(c),
        }
    }
    out
}

/// Один порт выгрузки `json`: разрешённый адрес **или** явное отсутствие адреса
/// (мёртвый порт). Эмиттер json видит все порты, а не только адресованные.
struct JsonPort<'a> {
    /// Имя порта.
    name: &'a str,
    /// Тип порта (печать Takt: `bit`, `u8`, `q(8, 8)`...).
    ty: String,
    /// Направление (`in`/`out`/`inout`).
    direction: &'a str,
    /// Разрешённый адрес, либо `None` - порт без адреса (мёртвый).
    resolved: Option<&'a ResolvedAddress>,
}

/// Выгрузка разрешённой карты в формат `json`.
///
/// Перечисляет **все** порты - и адресованные ([`AddressResolution::map`]), и мёртвые
/// ([`AddressResolution::address_less`]) - единым массивом, отсортированным по имени
/// (детерминизм). Мёртвый порт эмитится с `"address": null` - явная пометка, не `0x0`
/// (R8).
///
/// Плоский ключ карты (риск Р2): одноимённые порты разных под-моделей делят запись
/// `map` - в выгрузке победит последний. Ограничение задокументировано, не чинится в
/// 0043.
pub fn export_address_map_json(resolution: &AddressResolution) -> String {
    let mut ports: Vec<JsonPort> = Vec::new();
    for ra in sorted_entries(resolution) {
        // ключ карты квалифицирован моделью; в выгрузку идёт
        // **голое** имя порта (`ra.name`), а не ключ, и через тот же плоский
        // дедуп по имени, что и формат `map` - публичный контракт `json`
        // неизменен (плоский ключ при коллизии - граница, см. заметку).
        ports.push(JsonPort {
            name: &ra.name,
            ty: ra.ty.to_string(),
            direction: direction_name(ra.direction),
            resolved: Some(ra),
        });
    }
    for meta in &resolution.address_less {
        ports.push(JsonPort {
            name: &meta.name,
            ty: meta.ty.to_string(),
            direction: direction_name(meta.direction),
            resolved: None,
        });
    }
    ports.sort_by(|a, b| a.name.cmp(b.name));
    export_ports_json(&ports)
}

/// Выгрузка списка портов в формат `json`: объект с версией формата и массивом портов.
/// Печатается вручную (см. модульную заметку).
///
/// Порт **без** адреса (`resolved: None`) эмитится с `"address": null` - явная пометка
/// отсутствия, **не** `0x0` (R8). Адрес - hex-строка (`"0x40000000"`), а не число:
/// адреса читаются человеком в hex и не должны терять ширину/базу.
fn export_ports_json(ports: &[JsonPort]) -> String {
    let mut out = String::new();
    out.push_str("{\n");
    let _ = writeln!(out, "  \"format\": \"lam-address-map\",");
    let _ = writeln!(out, "  \"format_version\": {},", EXPORT_FORMAT_VERSION);
    out.push_str("  \"ports\": [\n");
    for (i, p) in ports.iter().enumerate() {
        out.push_str("    {\n");
        let _ = writeln!(out, "      \"name\": \"{}\",", json_escape(p.name));
        let _ = writeln!(out, "      \"type\": \"{}\",", json_escape(&p.ty));
        let _ = writeln!(out, "      \"direction\": \"{}\",", p.direction);
        match p.resolved {
            Some(ra) => {
                let _ = writeln!(out, "      \"address\": \"{:#010x}\",", ra.addr);
                match ra.bit {
                    Some(b) => {
                        let _ = writeln!(out, "      \"bit\": {},", b);
                    }
                    None => out.push_str("      \"bit\": null,\n"),
                }
                let _ = writeln!(out, "      \"source\": \"{}\"", source_name(ra.source));
            }
            None => {
                // Мёртвый порт: адреса нет ни из одного источника (R8).
                out.push_str("      \"address\": null,\n");
                out.push_str("      \"bit\": null,\n");
                out.push_str("      \"source\": null\n");
            }
        }
        // Запятая между объектами массива, но не после последнего.
        if i + 1 < ports.len() {
            out.push_str("    },\n");
        } else {
            out.push_str("    }\n");
        }
    }
    out.push_str("  ]\n");
    out.push_str("}\n");
    out
}
