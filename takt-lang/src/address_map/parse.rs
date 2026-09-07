//! Парсер `.ld`-подобного формата внешней карты адресов и предупреждения наложения. Тема
//! самостоятельна: читает **текст внешнего файла**, тогда как разрешение адресов
//! ([`resolve`](super::resolve)) ходит по модели.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::{ExpressionNode, ModelNode, VariableNode};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Одна запись внешней карты адресов: `NAME = 0xADDR[:bit];`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AddressMapEntry {
    /// Имя порта, которому назначается адрес.
    pub name: String,
    /// Числовой адрес.
    pub addr: i64,
    /// Битовая позиция (`0xADDR:bit`), если задана.
    pub bit: Option<i64>,
    /// Позиция записи в файле карты.
    pub loc: Location,
}

/// Сканер символов с байтовыми позициями (для `Location`).
struct Scanner {
    chars: Vec<(usize, char)>,
    /// Индекс текущего символа в [`chars`](Scanner::chars).
    pos: usize,
    /// Длина исходного текста в байтах (позиция конца).
    len: usize,
    file_no: u64,
}

impl Scanner {
    fn new(src: &str, file_no: u64) -> Self {
        Scanner {
            chars: src.char_indices().collect(),
            pos: 0,
            len: src.len(),
            file_no,
        }
    }

    /// Байтовое смещение текущего символа (или конец текста).
    fn offset(&self) -> usize {
        self.chars
            .get(self.pos)
            .map(|(o, _)| *o)
            .unwrap_or(self.len)
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).map(|(_, c)| *c)
    }

    fn peek2(&self) -> Option<char> {
        self.chars.get(self.pos + 1).map(|(_, c)| *c)
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    /// Пропускает пробелы и строчные комментарии `//`.
    fn skip_trivia(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => {
                    self.bump();
                }
                Some('/') if self.peek2() == Some('/') => {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.bump();
                    }
                }
                _ => break,
            }
        }
    }

    fn loc(&self, start: usize, end: usize) -> Location {
        Location::source(self.file_no, start, end)
    }
}

pub(super) fn is_name_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

pub(super) fn is_name_cont(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Разбирает содержимое файла внешней карты адресов.
///
/// Возвращает список записей либо непустой список диагностик. Парсер устойчив к
/// ошибкам: при синтаксической ошибке в записи она пропускается до ближайшего `;`,
/// разбор продолжается - чтобы сообщить обо всех проблемах разом.
///
/// # Коды диагностик
///
/// - `AM-001` - ожидалось имя порта;
/// - `AM-002` - ожидался `=`;
/// - `AM-003` - ожидался адрес;
/// - `AM-004` - ожидался `;`;
/// - `AM-005` - некорректный литерал адреса;
/// - `AM-006` - повторная запись для одного порта.
pub fn parse_address_map(src: &str, file_no: u64) -> Result<Vec<AddressMapEntry>, Vec<Diagnostic>> {
    let mut sc = Scanner::new(src, file_no);
    let mut entries: Vec<AddressMapEntry> = Vec::new();
    let mut diags: Vec<Diagnostic> = Vec::new();
    let mut seen: HashMap<String, Location> = HashMap::new();

    loop {
        sc.skip_trivia();
        if sc.peek().is_none() {
            break;
        }
        match parse_entry(&mut sc) {
            Ok(entry) => {
                if let Some(prev) = seen.get(&entry.name) {
                    diags.push(
                        Diagnostic::error(
                            entry.loc,
                            format!(
                                "повторная запись адреса для порта '{}' во внешней карте",
                                entry.name
                            ),
                        )
                        .with_code("AM-006"),
                    );
                    let _ = prev;
                } else {
                    seen.insert(entry.name.clone(), entry.loc);
                    entries.push(entry);
                }
            }
            Err(diag) => {
                diags.push(diag);
                // Восстановление: пропускаем до ближайшего `;` включительно.
                while let Some(c) = sc.peek() {
                    sc.bump();
                    if c == ';' {
                        break;
                    }
                }
            }
        }
    }

    if diags.is_empty() {
        Ok(entries)
    } else {
        Err(diags)
    }
}

/// Разбирает одну запись `NAME = ADDRESS ;`.
fn parse_entry(sc: &mut Scanner) -> Result<AddressMapEntry, Diagnostic> {
    // Имя.
    let name_start = sc.offset();
    let Some(c0) = sc.peek() else {
        return Err(Diagnostic::error(
            sc.loc(name_start, name_start),
            "ожидалось имя порта".to_string(),
        )
        .with_code("AM-001"));
    };
    if !is_name_start(c0) {
        let end = sc.offset() + c0.len_utf8();
        return Err(
            Diagnostic::error(sc.loc(name_start, end), "ожидалось имя порта".to_string())
                .with_code("AM-001"),
        );
    }
    let mut name = String::new();
    while let Some(c) = sc.peek() {
        if is_name_cont(c) {
            name.push(c);
            sc.bump();
        } else {
            break;
        }
    }
    let name_end = sc.offset();

    // `=`.
    sc.skip_trivia();
    if sc.peek() != Some('=') {
        let at = sc.offset();
        return Err(Diagnostic::error(
            sc.loc(at, at),
            format!("ожидался '=' после имени порта '{}'", name),
        )
        .with_code("AM-002"));
    }
    sc.bump();

    // Адрес.
    sc.skip_trivia();
    let addr_start = sc.offset();
    let mut tok = String::new();
    while let Some(c) = sc.peek() {
        if c.is_alphanumeric() || c == ':' {
            tok.push(c);
            sc.bump();
        } else {
            break;
        }
    }
    let addr_end = sc.offset();
    if tok.is_empty() {
        return Err(Diagnostic::error(
            sc.loc(addr_start, addr_start),
            format!("ожидался адрес для порта '{}'", name),
        )
        .with_code("AM-003"));
    }
    let (addr, bit) = parse_address_token(&tok)
        .map_err(|msg| Diagnostic::error(sc.loc(addr_start, addr_end), msg).with_code("AM-005"))?;

    // `;`.
    sc.skip_trivia();
    if sc.peek() != Some(';') {
        let at = sc.offset();
        return Err(Diagnostic::error(
            sc.loc(at, at),
            format!("ожидался ';' после адреса порта '{}'", name),
        )
        .with_code("AM-004"));
    }
    sc.bump();

    Ok(AddressMapEntry {
        name,
        addr,
        bit,
        loc: sc.loc(name_start, name_end),
    })
}

/// Разбирает литерал адреса `0xADDR`, `123` или `0xADDR:bit`.
pub(super) fn parse_address_token(tok: &str) -> Result<(i64, Option<i64>), String> {
    let (addr_part, bit_part) = match tok.split_once(':') {
        Some((a, b)) => (a, Some(b)),
        None => (tok, None),
    };
    let addr = parse_int(addr_part).ok_or_else(|| format!("некорректный адрес '{}'", addr_part))?;
    let bit = match bit_part {
        Some(b) => Some(parse_int(b).ok_or_else(|| format!("некорректный бит '{}'", b))?),
        None => None,
    };
    Ok((addr, bit))
}

/// Разбирает целое: hex (`0x...`/`0X...`) или десятичное.
fn parse_int(s: &str) -> Option<i64> {
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        i64::from_str_radix(hex, 16).ok()
    } else {
        s.parse::<i64>().ok()
    }
}

/// предупреждения при наложении внешней карты на модель.
///
/// Внешняя карта - самый приоритетный источник адреса (наложение). Функция сверяет записи
/// карты с портами модели и возвращает предупреждения:
///
/// - **SE-050** - запись карты переопределяет адрес порта, уже заданный в модели
///   (inline `:=` или оператором `address`); ожидаемое поведение наложения, но
///   заметное.
/// - **SE-051** - запись карты для имени, которого нет среди портов модели.
///
/// Проверяются порты **переданной** модели (её `variables`); понижение адреса и
/// построение итогового `AddressMap` для генерации.
pub fn address_map_overlay_warnings(
    model: Rc<RefCell<ModelNode>>,
    entries: &[AddressMapEntry],
) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    let mut out = Vec::new();
    for e in entries {
        match borrowed.variables.get(&e.name) {
            Some(VariableNode::Port { address, .. }) => {
                let has_inline = !matches!(address, ExpressionNode::None);
                let has_operator = borrowed.address_defs.iter().any(|d| d.port == e.name);
                if has_inline || has_operator {
                    out.push(
                        Diagnostic::warning(
                            e.loc,
                            format!(
                                "внешняя карта переопределяет адрес порта '{}', заданный в модели",
                                e.name
                            ),
                        )
                        .with_code("SE-050"),
                    );
                }
            }
            _ => {
                out.push(
                    Diagnostic::warning(
                        e.loc,
                        format!(
                            "внешняя карта задаёт адрес для несуществующего порта '{}'",
                            e.name
                        ),
                    )
                    .with_code("SE-051"),
                );
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_hex_decimal_and_bit_addressed_entries() {
        let entries = parse_address_map(
            "// карта\nBTN = 0x00200000;\nLED = 0x00200004:3;\nCNT = 42;\n",
            0,
        )
        .expect("карта должна разобраться");
        assert_eq!(entries.len(), 3);
        assert_eq!(entries[0].name, "BTN");
        assert_eq!(entries[0].addr, 0x0020_0000);
        assert_eq!(entries[0].bit, None);
        assert_eq!(entries[1].name, "LED");
        assert_eq!(entries[1].addr, 0x0020_0004);
        assert_eq!(entries[1].bit, Some(3));
        assert_eq!(entries[2].addr, 42);
    }

    #[test]
    fn empty_and_comment_only_map_is_ok() {
        assert!(parse_address_map("", 0).unwrap().is_empty());
        assert!(
            parse_address_map("// только комментарий\n", 0)
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn missing_equals_is_am002() {
        let err = parse_address_map("BTN 0x200000;", 0).unwrap_err();
        assert_eq!(err[0].code.as_deref(), Some("AM-002"));
    }

    #[test]
    fn missing_semicolon_is_am004() {
        let err = parse_address_map("BTN = 0x200000", 0).unwrap_err();
        assert_eq!(err[0].code.as_deref(), Some("AM-004"));
    }

    #[test]
    fn bad_address_literal_is_am005() {
        let err = parse_address_map("BTN = 0xZZ;", 0).unwrap_err();
        assert_eq!(err[0].code.as_deref(), Some("AM-005"));
    }

    #[test]
    fn duplicate_entry_is_am006() {
        let err = parse_address_map("BTN = 0x1; BTN = 0x2;", 0).unwrap_err();
        assert_eq!(err[0].code.as_deref(), Some("AM-006"));
    }

    #[test]
    fn recovers_and_reports_all_errors() {
        // Три битые записи -> три диагностики (восстановление до `;`).
        let err = parse_address_map("A 1; B = ; C = 0xZZ;", 0).unwrap_err();
        assert_eq!(err.len(), 3, "должны сообщаться все три ошибки: {:?}", err);
    }
}
