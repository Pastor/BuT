//! Тесты экспорта карты адресов наружу.
//!
//! Два уровня: библиотечный (`resolve_addresses` + эмиттеры `export_*`) и CLI
//! (подкоманда `taktc address-map`, через `CARGO_BIN_EXE_taktc`). Ожидаемые значения -
//! из живой пробы 2026-07-19 (CLAUDE.md: сперва зонд, затем assertions против
//! захваченного), а не из догадок.

use std::process::Command;
use std::rc::Rc;
use takt_lang::address_map::{
    export_address_map, export_address_map_json, export_map_entries, parse_address_map,
    parse_defines, resolve_addresses,
};

const DIR: &str = "tests/data/address_export";

/// Строит модель исправлениетуры и разрешает адреса против внешней карты (текст) и `--define`.
/// Возвращает разрешение для эмиттеров.
fn resolve_fixture(
    lam: &str,
    external_map: Option<&str>,
    defines: &[String],
) -> takt_lang::address_map::AddressResolution {
    let source =
        std::fs::read_to_string(format!("{DIR}/{lam}")).unwrap_or_else(|e| panic!("{lam}: {e}"));
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let external = match external_map {
        Some(text) => parse_address_map(text, 0).expect("разбор карты"),
        None => Vec::new(),
    };
    let env = parse_defines(defines).expect("define");
    resolve_addresses(Rc::clone(&model), &external, &env)
}

// -- Библиотечный уровень: эмиттер map (T2-T5) --------------------------------

/// T2/T3/T5: три источника адреса на модели-пробе. Ожидание - из пробы: inline (`SW`),
/// оператор `address` (`LED`), формат `0x` + 8 цифр, бит сохранён.
///
/// У `LED` (однобитный порт) позиция бита в исходнике не написана, и с слой адресов
/// подставляет ноль **до** выгрузки (`SE-090`), поэтому в карте стоит `:0`.
#[test]
fn map_export_three_sources_probe() {
    let res = resolve_fixture("probe.takt", None, &[]);
    assert_eq!(
        export_address_map(&res),
        "BTN = 0x00200000;\nLED = 0x00200004:0;\nSW = 0x00300000:3;\n"
    );
}

/// T4: внешняя карта бьёт модель - `BTN` берётся из карты (`0x40000000`), не из inline
/// (`0x00200000`). Приоритет источников (R2).
#[test]
fn map_export_external_overrides_model() {
    let res = resolve_fixture("probe.takt", Some("BTN = 0x40000000;\n"), &[]);
    let out = export_address_map(&res);
    assert!(
        out.contains("BTN = 0x40000000;"),
        "карта обязана бить inline: {out}"
    );
    assert!(
        !out.contains("0x00200000"),
        "старый inline-адрес не должен остаться: {out}"
    );
}

// -- Библиотечный уровень: круговой рейс (T6-T8) ------------------------------

/// T6: выгрузка разбирается `parse_address_map` без диагностик `AM-*` (замыкание).
#[test]
fn round_trip_reparses_without_diagnostics() {
    let res = resolve_fixture("probe.takt", Some("BTN = 0x40000000;\n"), &[]);
    let text = export_address_map(&res);
    parse_address_map(&text, 0).expect("выгрузка обязана разбираться без AM-*");
}

/// T7: `export -> parse -> export` - второй текст **побайтово равен** первому.
/// Идемпотентность - свойство общего печатника (`write_map_line`).
#[test]
fn round_trip_is_byte_identical() {
    let res = resolve_fixture("probe.takt", Some("BTN = 0x40000000;\n"), &[]);
    let text1 = export_address_map(&res);
    let entries = parse_address_map(&text1, 0).expect("разбор");
    let text2 = export_map_entries(&entries);
    assert_eq!(
        text1, text2,
        "круговой рейс обязан быть побайтово идемпотентен"
    );
}

// -- Библиотечный уровень: json (T10-T12) -------------------------------------

/// T10/T11/T12: `json` валиден (`serde_json` разбирает), полон (имя/адрес/бит/
/// источник/тип/направление) и версионирован.
#[test]
fn json_export_is_valid_complete_and_versioned() {
    let res = resolve_fixture("probe.takt", Some("BTN = 0x40000000;\n"), &[]);
    let text = export_address_map_json(&res);
    let v: serde_json::Value = serde_json::from_str(&text).expect("json валиден (T10)");

    // T12: версия формата присутствует.
    assert_eq!(
        v["format_version"], 1,
        "версия формата обязана присутствовать"
    );
    assert_eq!(v["format"], "lam-address-map");

    let ports = v["ports"].as_array().expect("массив портов");
    assert_eq!(ports.len(), 3, "три порта пробы");

    // T11: полнота - на каждый порт все поля. Проверяем BTN (из карты) детально.
    let btn = ports.iter().find(|p| p["name"] == "BTN").expect("BTN есть");
    assert_eq!(btn["type"], "[bit;8]");
    assert_eq!(btn["direction"], "in");
    assert_eq!(btn["address"], "0x40000000");
    assert_eq!(btn["bit"], serde_json::Value::Null);
    assert_eq!(btn["source"], "external");

    // SW - inline с битом.
    let sw = ports.iter().find(|p| p["name"] == "SW").expect("SW есть");
    assert_eq!(sw["bit"], 3);
    assert_eq!(sw["source"], "inline");
    assert_eq!(sw["address"], "0x00300000");

    // LED - оператор `address`.
    let led = ports.iter().find(|p| p["name"] == "LED").expect("LED есть");
    assert_eq!(led["source"], "operator");
}

// -- Библиотечный уровень: мёртвый порт (T14) ---------------------------------

/// T14/K2: порт без адреса. В `map` записи нет и `0x0` **не появляется** (главный
/// способ соврать в выгрузке); в `json` - явная пометка `null`.
#[test]
fn dead_port_absent_in_map_and_null_in_json() {
    let res = resolve_fixture("dead_port.takt", None, &[]);

    let map = export_address_map(&res);
    assert_eq!(map, "USED = 0x00100000;\n", "мёртвый DEAD не в map: {map}");
    assert!(
        !map.contains("0x00000000"),
        "0x0 не должен появляться: {map}"
    );

    let json = export_address_map_json(&res);
    let v: serde_json::Value = serde_json::from_str(&json).unwrap();
    let dead = v["ports"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["name"] == "DEAD")
        .expect("DEAD перечислен в json");
    assert_eq!(
        dead["address"],
        serde_json::Value::Null,
        "адрес DEAD — null"
    );
    assert_eq!(dead["source"], serde_json::Value::Null);
}

// -- Библиотечный уровень: плоский ключ (T21) ---------------------------------

/// T21/Р2: одноимённые порты разных под-моделей в **плоской** выгрузке
/// (`.ld`/`json`) схлопываются - побеждает последний. квалифицировала
/// **ключ карты** (c-hal видит оба порта, дефект коллизии исправлен), но
/// **плоская выгрузка осталась прежней**: детерминированный дедуп по голому
/// имени (максимальный квалиф. ключ = последняя под-модель B, 0x02) - иначе
/// круговой рейс `.ld` (R4 0043) перестал бы быть тождеством. Тест исправлениеирует эту
/// границу формата.
#[test]
fn flat_key_collision_last_wins() {
    let res = resolve_fixture("collide.takt", None, &[]);
    // Карта после 0084 содержит оба порта (квалиф. ключи) - это исправление:
    assert_eq!(
        res.map.values().filter(|r| r.name == "SIG").count(),
        2,
        "0084: карта хранит оба одноимённых порта под-моделей (данные не потеряны)"
    );
    // Но плоская выгрузка схлопывает по имени, побеждает последний (B, 0x02):
    let map = export_address_map(&res);
    assert_eq!(
        map, "SIG = 0x00000002:0;\n",
        "плоская выгрузка: последняя под-модель (B, 0x02) побеждает — граница формата: {map}"
    );
}

// -- Библиотечный уровень: сверка адресов с c-hal (T9) ------------------------

/// T9: адреса выгрузки = адреса таблицы `__ADDR[]` цели `c-hal`. Значения сверены живой
/// пробой 2026-07-19 (`-t c-hal --address-map plat.map`): `PROBE_BTN=0x40000000`,
/// `PROBE_LED=0x200004`, `PROBE_SW=0x300000:3`.
#[test]
fn export_addresses_match_chal_table() {
    let res = resolve_fixture("probe.takt", Some("BTN = 0x40000000;\n"), &[]);
    // ключ карты квалифицирован моделью - ищем по голому имени
    // (`ResolvedAddress::name`) среди значений, а не `res.map["BTN"]`.
    let by_name = |n: &str| {
        res.map
            .values()
            .find(|r| r.name == n)
            .unwrap_or_else(|| panic!("порт '{n}' не найден в карте"))
    };
    assert_eq!(by_name("BTN").addr, 0x4000_0000);
    assert_eq!(by_name("LED").addr, 0x0020_0004);
    assert_eq!(by_name("SW").addr, 0x0030_0000);
    assert_eq!(by_name("SW").bit, Some(3));
}

// -- Библиотечный уровень: корпусный круговой рейс (T20) -----------------------

/// T20: для каждого `examples/*.takt`, разрешающегося без ошибок, круговой рейс `export ->
/// parse -> export` = тождество. Примеры с достижимым портом без адреса (SE-052)
/// пропускаются - у них нет полной карты для выгрузки.
#[test]
fn corpus_round_trip_is_identity() {
    let examples = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("examples");
    let mut checked = 0usize;
    for entry in std::fs::read_dir(&examples).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) != Some("takt") {
            continue;
        }
        let source = std::fs::read_to_string(&path).unwrap();
        let (ast, _) = match takt_lang::parse(&source, 0) {
            Ok(p) => p,
            Err(_) => continue,
        };
        let Ok(model) = takt_lang::semantic::tree::construct_model(&ast, None, &[]) else {
            continue;
        };
        let env = parse_defines(&[]).unwrap();
        let res = resolve_addresses(Rc::clone(&model), &[], &env);
        // Пропускаем примеры с достижимым портом без адреса (SE-052).
        if res
            .diagnostics
            .iter()
            .any(|d| matches!(d.level, takt_lang::diagnostics::Level::Error))
        {
            continue;
        }
        let text1 = export_address_map(&res);
        let entries = parse_address_map(&text1, 0).expect("разбор выгрузки корпуса");
        let text2 = export_map_entries(&entries);
        assert_eq!(
            text1,
            text2,
            "{}: круговой рейс не тождественен",
            path.display()
        );
        checked += 1;
    }
    assert!(
        checked > 0,
        "корпус должен дать хотя бы один разрешимый пример"
    );
}

// -- CLI-уровень (подкоманда address-map) -------------------------------------

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// T1: разбор флагов подкоманды; `--emit` по умолчанию `map`. Проверяем прогоном без
/// `--emit` - выгрузка в формате `map`.
#[test]
fn cli_default_emit_is_map() {
    let out = taktc()
        .args(["address-map", &format!("{DIR}/probe.takt")])
        .output()
        .expect("запуск taktc");
    assert!(out.status.success(), "rc=0 ожидался");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "BTN = 0x00200000;\nLED = 0x00200004:0;\nSW = 0x00300000:3;\n"
    );
}

/// T16/T17: предупреждения (SE-050/051) идут в **stderr**, а **stdout** - чистая
/// выгрузка, целиком разбираемая `parse_address_map`.
#[test]
fn cli_warnings_go_to_stderr_not_stdout() {
    let out = taktc()
        .args([
            "address-map",
            "--address-map",
            &format!("{DIR}/plat.map"),
            &format!("{DIR}/probe.takt"),
        ])
        .output()
        .expect("запуск");
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("SE-050"), "оверлей — в stderr: {stderr}");
    assert!(
        !stdout.contains("SE-050"),
        "stdout обязан быть чистым: {stdout}"
    );
    // stdout целиком разбирается как карта.
    parse_address_map(&stdout, 0).expect("stdout — валидная карта");
}

/// T15/K1: достижимый порт без адреса -> `SE-052`, ненулевой код, нет выгрузки.
#[test]
fn cli_reachable_port_without_address_is_se052() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap();
    let out = taktc()
        .args(["address-map"])
        .arg(root.join("examples/elevator_mini.takt"))
        .output()
        .expect("запуск");
    assert!(!out.status.success(), "SE-052 обязан дать ненулевой код");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("SE-052"),
        "ожидался SE-052"
    );
    assert!(out.stdout.is_empty(), "при ошибке выгрузки быть не должно");
}

/// T18/K4: неизвестный формат `--emit svd` -> ошибка, ненулевой код, упоминание что SVD
/// не поставляется (а не пустой файл).
#[test]
fn cli_unknown_format_is_rejected() {
    let out = taktc()
        .args(["address-map", "--emit", "svd", &format!("{DIR}/probe.takt")])
        .output()
        .expect("запуск");
    assert!(!out.status.success(), "неизвестный формат → ненулевой код");
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("SVD"),
        "ошибка обязана назвать SVD"
    );
}

/// T19: вывод в файл (`-o`) - содержимое совпадает со stdout-вариантом.
#[test]
fn cli_output_to_file_matches_stdout() {
    let stdout_run = taktc()
        .args(["address-map", &format!("{DIR}/probe.takt")])
        .output()
        .expect("запуск stdout");
    let out_file = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0043_out.map");
    // Каталог процесса создаётся здесь: файл пишет не тест, а инструмент.
    let _ = std::fs::create_dir_all(out_file.parent().expect("каталог процесса"));
    let file_run = taktc()
        .args([
            "address-map",
            "-o",
            out_file.to_str().unwrap(),
            &format!("{DIR}/probe.takt"),
        ])
        .output()
        .expect("запуск -o");
    assert!(file_run.status.success());
    let file_content = std::fs::read_to_string(&out_file).expect("файл создан");
    assert_eq!(
        file_content,
        String::from_utf8_lossy(&stdout_run.stdout),
        "содержимое файла = stdout-варианту"
    );
}

/// K5: испорченная входная карта (`--address-map` без `=`) -> `AM-002`, ненулевой код,
/// выгрузки нет.
#[test]
fn cli_broken_input_map_is_rejected() {
    let out = taktc()
        .args([
            "address-map",
            "--address-map",
            &format!("{DIR}/broken.map"),
            &format!("{DIR}/probe.takt"),
        ])
        .output()
        .expect("запуск");
    assert!(!out.status.success(), "битая карта → ненулевой код");
    assert!(String::from_utf8_lossy(&out.stderr).contains("AM-002"));
}
