//! Круговой рейс карты адресов и её приоритет.
//!
//! # Что доказывает набор
//!
//! 1. Выгрузка `map` - **тот же** формат, что читает `--address-map`: подать
//!    её обратно и выгрузить снова даёт байт-в-байт то же.
//! 2. Приоритет источников соблюдается и в **выгрузке**, а не только в
//!    порождённом коде: внешняя карта перекрывает inline-адрес объявления.
//! 3. Порт без адреса в `map` **опускается**, а в `json` помечен `null` - `0x0`
//!    там неотличим от настоящего адреса.
//!
//! Перебор целей (`target_matrix_tests`) проверяет адрес в порождённом коде; здесь
//! предмет другой - **выгрузка**, у неё свои потребители (генераторы HAL, отладчики,
//! платформенные скрипты).

use std::path::{Path, PathBuf};
use std::process::Command;

///
/// Мёртвый именно потому, что используемый порт без адреса - ошибка `SE-052`: "нет
/// адреса" и "не нужен" - разные случаи, и выгрузка говорит только о втором.
const PROBE: &str = "\
model Wrap {
    var k: u8 := 0;
    out led: u8 at 0x40000100;
    out relay: u8;
    address relay = 0x40000200;
    out spare: u8;

    start Go {
        always {
            k := k + 1;
            led := k;
            relay := k;
        }
        next Done;
    }
    state Done;
}
start Main = Wrap;
";

/// Внешняя карта: перекрывает inline-адрес `led`.
const MAP: &str = "led = 0x00200004;\n";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0458_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Выгружает карту заданного формата; отдаёт stdout.
fn export(dir: &Path, emit: &str, with_map: bool) -> String {
    let input = dir.join("probe.takt");
    std::fs::write(&input, PROBE).expect("запись пробы");
    let map = dir.join("plat.map");
    std::fs::write(&map, MAP).expect("запись карты");
    let mut cmd = taktc();
    cmd.arg("address-map").args(["--emit", emit]);
    if with_map {
        cmd.arg("--address-map").arg(&map);
    }
    let out = cmd.arg(&input).output().expect("запуск taktc address-map");
    assert!(
        out.status.success(),
        "выгрузка карты не удалась:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// Внешняя карта перекрывает inline - и это видно в выгрузке.
#[test]
fn external_map_wins_in_export() {
    let dir = work_dir("priority");
    let text = export(&dir, "map", true);
    assert!(
        text.contains("led = 0x00200004;"),
        "внешняя карта не перекрыла inline-адрес:\n{text}"
    );
    assert!(
        !text.contains("0x40000100"),
        "в выгрузке остался перекрытый inline-адрес:\n{text}"
    );
    // Оператор `address` - второй источник, он остаётся.
    assert!(
        text.contains("relay = 0x40000200;"),
        "адрес, заданный оператором, потерян:\n{text}"
    );
    // Порт без адреса опускается: `0x0` в выгрузке неотличим от настоящего адреса.
    assert!(
        !text.contains("spare"),
        "порт без адреса попал в выгрузку:\n{text}"
    );
}

/// Круговой рейс: выгрузка читается обратно и даёт то же самое.
#[test]
fn export_import_export_is_stable() {
    let dir = work_dir("roundtrip");
    let first = export(&dir, "map", true);
    // Подаём выгрузку обратно как внешнюю карту.
    let back = dir.join("second.map");
    std::fs::write(&back, &first).expect("запись выгрузки");
    let input = dir.join("probe.takt");
    let out = taktc()
        .arg("address-map")
        .args(["--emit", "map"])
        .arg("--address-map")
        .arg(&back)
        .arg(&input)
        .output()
        .expect("запуск taktc address-map");
    assert!(out.status.success(), "повторная выгрузка не удалась");
    let second = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(second, first, "круговой рейс изменил карту");
}

/// В `json` порт без адреса помечен `null`, а не опущен.
#[test]
fn json_marks_missing_address_explicitly() {
    let dir = work_dir("json");
    let text = export(&dir, "json", true);
    assert!(
        text.contains("\"format\"") && text.contains("\"format_version\""),
        "в выгрузке нет полей формата:\n{text}"
    );
    assert!(
        text.contains("\"spare\""),
        "порт без адреса пропал из json:\n{text}"
    );
    assert!(
        text.contains("\"address\": null"),
        "порт без адреса не помечен null:\n{text}"
    );
}
