//! Интеграционные тесты внешней карты адресов.
//!
//! Проверяют наложение внешней `.ld`-подобной карты на семантическую модель:
//! предупреждения об оверлее (SE-050) и висячих записях (SE-051). Разбор самого формата
//! покрыт юнит-тестами модуля `takt_lang::address_map`.

use takt_lang::semantic::tree::construct_model;
use takt_lang::{
    AddressSource, address_map_overlay_warnings, parse, parse_address_map, resolve_addresses,
};

/// Строит семантическую модель из исходника Takt.
fn model_of(src: &str) -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора .takt");
    construct_model(&ast, None, &[]).expect("ошибка построения модели")
}

/// Ищет разрешённый адрес порта по **голому** имени (: ключ карты квалифицирован
/// моделью, поэтому `map.get(имя)` больше не работает - адрес адресуется через
/// `ResolvedAddress::name`).
fn find_addr<'a>(
    r: &'a takt_lang::AddressResolution,
    name: &str,
) -> Option<&'a takt_lang::ResolvedAddress> {
    r.map.values().find(|a| a.name == name)
}

/// Коды всех предупреждений наложения для пары (модель, карта).
fn overlay_codes(model_src: &str, map_src: &str) -> Vec<String> {
    let model = model_of(model_src);
    let entries = parse_address_map(map_src, 0).expect("карта должна разобраться");
    address_map_overlay_warnings(model, &entries)
        .into_iter()
        .map(|d| d.code.unwrap_or_default())
        .collect()
}

/// Карта переопределяет адрес, заданный inline (`:=`) -> SE-050.
#[test]
fn overrides_inline_address_warns_se050() {
    let codes = overlay_codes("in BTN: u8 at 0x00100000; start Idle;", "BTN = 0x00200000;");
    assert_eq!(codes, vec!["SE-050"]);
}

/// Карта переопределяет адрес, заданный оператором `address` -> SE-050.
#[test]
fn overrides_operator_address_warns_se050() {
    let codes = overlay_codes(
        "out LED: bit; address LED = 0x00100004; start Idle;",
        "LED = 0x00200004:3;",
    );
    assert_eq!(codes, vec!["SE-050"]);
}

/// Порт без адреса в модели: карта - единственный источник -> без предупреждений.
#[test]
fn fills_port_without_model_address_is_silent() {
    let codes = overlay_codes("in BTN: u8; start Idle;", "BTN = 0x00200000;");
    assert!(
        codes.is_empty(),
        "не должно быть предупреждений: {:?}",
        codes
    );
}

/// Запись карты для несуществующего порта -> SE-051.
#[test]
fn dangling_map_entry_warns_se051() {
    let codes = overlay_codes("in BTN: u8; start Idle;", "GHOST = 0x00200000;");
    assert_eq!(codes, vec!["SE-051"]);
}

/// Смешанный случай: наложение + висячая запись - оба предупреждения.
#[test]
fn mixed_overlay_and_dangling() {
    let mut codes = overlay_codes(
        "in BTN: u8 at 0x00100000; start Idle;",
        "BTN = 0x00200000; GHOST = 0x00200008;",
    );
    codes.sort();
    assert_eq!(codes, vec!["SE-050", "SE-051"]);
}

// ------------------------- Резолвер AddressMap ---------------------

/// Только inline-адрес -> источник Inline, значение понижено.
#[test]
fn resolve_inline_only() {
    let model = model_of("in BTN: u8 at 0x00200000; start Idle;");
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    let a = find_addr(&r, "BTN").expect("BTN должен быть разрешён");
    assert_eq!(a.addr, 0x0020_0000);
    assert_eq!(a.bit, None);
    assert_eq!(a.source, AddressSource::Inline);
    assert!(r.diagnostics.is_empty());
}

/// Только оператор `address` -> источник Operator; форма `:bit` понижается.
#[test]
fn resolve_operator_with_bit() {
    let model = model_of("out LED: bit; address LED = 0x00200004:3; start Idle;");
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    let a = find_addr(&r, "LED").expect("LED должен быть разрешён");
    assert_eq!(a.addr, 0x0020_0004);
    assert_eq!(a.bit, Some(3));
    assert_eq!(a.source, AddressSource::Operator);
}

/// Внешняя карта перекрывает inline -> источник External + предупреждение SE-050.
#[test]
fn resolve_external_overrides_inline() {
    let model = model_of("in BTN: u8 at 0x00100000; start Idle;");
    let entries = parse_address_map("BTN = 0x00200000;", 0).unwrap();
    let r = resolve_addresses(model, &entries, &takt_lang::AddressEnv::default());
    let a = find_addr(&r, "BTN").unwrap();
    assert_eq!(a.addr, 0x0020_0000);
    assert_eq!(a.source, AddressSource::External);
    assert_eq!(
        r.diagnostics
            .iter()
            .filter(|d| d.code.as_deref() == Some("SE-050"))
            .count(),
        1
    );
}

/// Используемый порт без адреса -> ошибка полноты SE-052.
#[test]
fn resolve_used_port_without_address_is_se052() {
    let model = model_of("in BTN: bit; start S { ref T: BTN; } state T;");
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    assert!(find_addr(&r, "BTN").is_none());
    assert_eq!(
        r.diagnostics
            .iter()
            .filter(|d| d.code.as_deref() == Some("SE-052"))
            .count(),
        1
    );
}

/// Внешняя карта закрывает used-порт без адреса модели -> нет SE-052.
#[test]
fn resolve_external_fills_used_port() {
    let model = model_of("in BTN: bit; start S { ref T: BTN; } state T;");
    let entries = parse_address_map("BTN = 0x00200000;", 0).unwrap();
    let r = resolve_addresses(model, &entries, &takt_lang::AddressEnv::default());
    assert_eq!(
        find_addr(&r, "BTN").unwrap().source,
        AddressSource::External
    );
    assert!(
        r.diagnostics
            .iter()
            .all(|d| d.code.as_deref() != Some("SE-052"))
    );
}

/// Висячая запись карты -> SE-051.
#[test]
fn resolve_dangling_external_is_se051() {
    let model = model_of("in BTN: u8 at 0x1; start Idle;");
    let entries = parse_address_map("GHOST = 0x00200000;", 0).unwrap();
    let r = resolve_addresses(model, &entries, &takt_lang::AddressEnv::default());
    assert_eq!(
        r.diagnostics
            .iter()
            .filter(|d| d.code.as_deref() == Some("SE-051"))
            .count(),
        1
    );
}

// ------------- Вычисление выражений адреса и define'ы ------------
//
// До 0042 выражение адреса понижала `lower_addr_expr`, принимавшая только литералы, а
// ветка `_ => None` молча означала "адреса нет". Поэтому `address BTN = BTN_ADDR;` и
// `address BTN = 0x100000 + 4;` теряли адрес, а пользователь получал `SE-052` "порт не
// имеет адреса" - диагностику о следствии вместо причины. Тесты ниже фиксируют закрытие
// этого тихого пропуска.
//
// Значения (адреса) сняты зондом с реального вывода, а не угаданы (`CLAUDE.md`).

/// Среда символов из пар `имя=значение` - как их даёт `--define`.
fn env_of(args: &[&str]) -> takt_lang::AddressEnv {
    let owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    takt_lang::parse_defines(&owned).expect("аргументы define должны разбираться")
}

fn codes_of(r: &takt_lang::AddressResolution) -> Vec<&str> {
    r.diagnostics
        .iter()
        .filter_map(|d| d.code.as_deref())
        .collect()
}

/// T3: символ из `const` модели - **без** `--define`.
#[test]
fn eval_symbol_from_model_const() {
    let model = model_of(
        "const BTN_ADDR: u32 := 0x00200000; out LED: bit; address LED = BTN_ADDR; start Idle;",
    );
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    let a = find_addr(&r, "LED").expect("адрес обязан вычислиться из const");
    assert_eq!(a.addr, 0x0020_0000);
    assert_eq!(a.source, AddressSource::Operator);
}

/// T2: свёртка арифметики.
#[test]
fn eval_folds_arithmetic() {
    let model = model_of("out LED: bit; address LED = 0x00200000 + 4; start Idle;");
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    assert_eq!(find_addr(&r, "LED").expect("адрес").addr, 0x0020_0004);
}

/// T1: define подставляется в выражение адреса.
#[test]
fn eval_symbol_from_define() {
    let model = model_of("out LED: bit; address LED = BTN_ADDR; start Idle;");
    let env = env_of(&["BTN_ADDR=0x00200000"]);
    let r = resolve_addresses(model, &[], &env);
    assert_eq!(find_addr(&r, "LED").expect("адрес").addr, 0x0020_0000);
}

/// T2: `-D BASE=...` + арифметика в модели - платформа даёт базу, модель раскладку.
#[test]
fn eval_define_plus_arithmetic() {
    let model = model_of("out LED: bit; address LED = BASE + 4; start Idle;");
    let env = env_of(&["BASE=0x00200000"]);
    let r = resolve_addresses(model, &[], &env);
    assert_eq!(find_addr(&r, "LED").expect("адрес").addr, 0x0020_0004);
}

/// T5: форма `адрес:бит` в значении define - та же грамматика, что у карты.
#[test]
fn eval_define_carries_bit() {
    let model = model_of("out LED: bit; address LED = PIN; start Idle;");
    let env = env_of(&["PIN=0x00200000:3"]);
    let r = resolve_addresses(model, &[], &env);
    let a = find_addr(&r, "LED").expect("адрес");
    assert_eq!((a.addr, a.bit), (0x0020_0000, Some(3)));
}

/// T8: define перекрывает одноимённую `const` -> адрес define'а + `SE-053`.
///
/// Симметрия с `SE-050`: платформенный слой главнее модели, но заметен.
#[test]
fn eval_define_overrides_const_with_warning() {
    let model = model_of(
        "const BTN_ADDR: u32 := 0x00200000; out LED: bit; address LED = BTN_ADDR; start Idle;",
    );
    let env = env_of(&["BTN_ADDR=0x00300000"]);
    let r = resolve_addresses(model, &[], &env);
    assert_eq!(
        find_addr(&r, "LED").expect("адрес").addr,
        0x0030_0000,
        "define обязан победить const (решение D2)"
    );
    assert!(
        codes_of(&r).contains(&"SE-053"),
        "перекрытие обязано быть заметным: {:?}",
        codes_of(&r)
    );
}

/// T10: висячий символ -> `SE-054` **с именем**, а не `SE-052` "нет адреса".
///
/// `SE-052` рядом быть не должно: причина названа, вторая диагностика о следствии
/// только запутает - ровно от неё фича и уходит.
#[test]
fn eval_dangling_symbol_names_the_cause() {
    let model =
        model_of("in BTN: bit; address BTN = NOWHERE; start Idle { ref Done: BTN; } state Done;");
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    let se054 = r
        .diagnostics
        .iter()
        .find(|d| d.code.as_deref() == Some("SE-054"))
        .expect("ожидалась SE-054");
    assert!(
        se054.message.contains("NOWHERE"),
        "диагностика обязана назвать символ: {}",
        se054.message
    );
    assert!(
        !codes_of(&r).contains(&"SE-052"),
        "SE-052 говорит «нет адреса» — причина уже названа: {:?}",
        codes_of(&r)
    );
}

/// T11: неконстантное выражение (ссылка на `var`) -> `SE-055`, не молчание.
#[test]
fn eval_non_constant_expression_is_reported() {
    let model = model_of("var x: u32 := 5; out LED: bit; address LED = x; start Idle;");
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    assert!(codes_of(&r).contains(&"SE-055"), "{:?}", codes_of(&r));
}

/// T12: цикл `const A := B; const B := A;` -> `SE-055`, а не зависание.
#[test]
fn eval_const_cycle_terminates_with_diagnostic() {
    let model = model_of(
        "const A: u32 := B; const B: u32 := A; out LED: bit; address LED = A; start Idle;",
    );
    let r = resolve_addresses(model, &[], &takt_lang::AddressEnv::default());
    assert!(codes_of(&r).contains(&"SE-055"), "{:?}", codes_of(&r));
}

/// T14: ** не тронут** - карта бьёт `address` с define'ом.
///
/// Define не источник адреса (решение A2) и приоритет слоя не повышает.
#[test]
fn define_does_not_raise_layer_priority() {
    let model = model_of("out LED: bit; address LED = BTN_ADDR; start Idle;");
    let entries = parse_address_map("LED = 0x00300000;", 0).unwrap();
    let env = env_of(&["BTN_ADDR=0x00200000"]);
    let r = resolve_addresses(model, &entries, &env);
    let a = find_addr(&r, "LED").expect("адрес");
    assert_eq!(
        a.addr, 0x0030_0000,
        "внешняя карта главнее (инвариант 0020)"
    );
    assert_eq!(a.source, AddressSource::External);
    assert!(codes_of(&r).contains(&"SE-050"));
}

/// T16: define сам по себе адреса **не создаёт**.
///
/// Он снабжает значением выражение, а выражения нет -> `SE-052` (как в 0020) плюс
/// `DF-004` (символ никем не спрошен).
#[test]
fn define_alone_is_not_an_address_source() {
    let model = model_of("in BTN: bit; start Idle { ref Done: BTN; } state Done;");
    let env = env_of(&["BTN=0x00200000"]);
    let r = resolve_addresses(model, &[], &env);
    assert!(
        find_addr(&r, "BTN").is_none(),
        "define — не источник адреса"
    );
    let codes = codes_of(&r);
    assert!(codes.contains(&"SE-052"), "{codes:?}");
    assert!(codes.contains(&"DF-004"), "{codes:?}");
}

/// T13: неиспользованный define -> `DF-004` (ловит опечатку в имени).
#[test]
fn unused_define_is_reported() {
    let model = model_of("out LED: bit; address LED = 0x00200000; start Idle;");
    let env = env_of(&["TYPO_ADDR=0x1"]);
    let r = resolve_addresses(model, &[], &env);
    assert!(codes_of(&r).contains(&"DF-004"), "{:?}", codes_of(&r));
}

/// Использованный define о себе не сообщает - `DF-004` не шумит.
#[test]
fn used_define_is_silent() {
    let model = model_of("out LED: bit; address LED = PIN; start Idle;");
    let env = env_of(&["PIN=0x00200000"]);
    let r = resolve_addresses(model, &[], &env);
    assert!(!codes_of(&r).contains(&"DF-004"), "{:?}", codes_of(&r));
}

// ------------------------- Разбор аргумента --define -------------------------

/// T13: `DF-001` - нет `=` либо негодное имя.
#[test]
fn parse_defines_rejects_bad_format() {
    for bad in ["BTN_ADDR", "=0x1", "1BAD=0x1", "a-b=0x1"] {
        let diags = takt_lang::parse_defines(&[bad.to_string()])
            .err()
            .unwrap_or_else(|| panic!("'{bad}' обязан быть отвергнут"));
        assert_eq!(diags[0].code.as_deref(), Some("DF-001"), "вход: {bad}");
    }
}

/// T13: `DF-002` - негодный литерал значения.
#[test]
fn parse_defines_rejects_bad_value() {
    let diags = takt_lang::parse_defines(&["N=0xZZ".to_string()]).expect_err("отказ");
    assert_eq!(diags[0].code.as_deref(), Some("DF-002"));
}

/// T13: `DF-003` - повтор имени.
///
/// Ошибка, а не "побеждает последний": молчаливое затирание сделало бы адрес зависящим
/// от порядка флагов (симметрия с `AM-006`).
#[test]
fn parse_defines_rejects_duplicate() {
    let diags =
        takt_lang::parse_defines(&["N=0x1".to_string(), "N=0x2".to_string()]).expect_err("отказ");
    assert_eq!(diags[0].code.as_deref(), Some("DF-003"));
}

/// T7: флаг повторяем - оба символа доступны.
#[test]
fn parse_defines_accepts_several_symbols() {
    let model = model_of("out A: bit; out B: bit; address A = X; address B = Y; start Idle;");
    let env = env_of(&["X=0x1", "Y=0x2"]);
    let r = resolve_addresses(model, &[], &env);
    assert_eq!(find_addr(&r, "A").expect("A").addr, 1);
    assert_eq!(find_addr(&r, "B").expect("B").addr, 2);
}
