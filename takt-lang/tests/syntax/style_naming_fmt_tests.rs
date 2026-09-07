//! Проводка `CS-001` в `taktc fmt`.
//!
//! завела слой стиля и позвала его из `fmt` и LSP, но проводку не пришпилила: у слоя
//! были только юнит-тесты **предикатов формы** (`Case::matches`). То есть проверялось
//! правило, а не то, что оно доезжает до пользователя, - а именно доставка и есть
//! предмет фичи (требование: "ставить предупреждение - в `fmt` и в LSP").
//!
//! # Что здесь ловится
//!
//! 1. **Критерии приёмки ** - 1 (`fmt` и `fmt --check` печатают
//!    `CS-001`), 2 (код возврата от него **не** меняется), 4 (семантически
//!    некорректный файл форматируется, и предупреждение на нём выдаётся),
//!    5 (корпус даёт ноль предупреждений).
//! 2. **Обход АСД слоя стиля.** Юнит-тесты `naming.rs` проверяют предикаты, но
//!    ни одна ветвь `collect_element`/`collect_variable` тестом не покрыта:
//!    объявление, выпавшее из обхода, молча перестало бы проверяться. Здесь
//!    каждый из **четырнадцати** видов объявлений канона наблюдается через
//!    настоящий вывод инструмента.
//! 3. **Имя не исправляется**: `fmt` на месте оставляет имя
//!    как есть, а не переписывает его под канон.
//!
//! Наблюдается так, как это видит пользователь: прогоном бинаря `taktc`
//! (`CARGO_BIN_EXE_taktc`) и перехватом stderr - образец взят у
//! `fmt_diagnostic_tests.rs`.
//!
//! **Тест корпуса стоит здесь, а не в предкоммите.** `precheck.sh` зовёт `fmt --check
//! examples/`, но `CS-001` на код возврата не влияет **по решению ** - то есть
//! неканоничное имя, вернувшееся в корпус, проверка пропустил бы молча. Критерий приёмки 5
//! без теста не имел теста вовсе.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// Каноничный по **формату** файл с неканоничным именем порта.
///
/// Формат проверен пробой (`fmt --check` даёт 0), поэтому единственная причина
/// диагностики - имя.
const CANONICAL_FORMAT_BAD_NAME: &str = "\
model M {
    out BadPort: bit;
    start S {
        always {
            BadPort := 1;
        }
    }
}
start Main = M;
";

/// Файл, неканоничный **и** по формату (отступ ` out`), и по имени.
const BAD_FORMAT_BAD_NAME: &str = "\
model M {
  out BadPort: bit;
    start S { always { BadPort := 1; } }
}
start Main = M;
";

/// Файл, разбираемый успешно, но **семантически некорректный**: тип не найден
/// (`SE-034`). Имя переменной при этом неканонично.
const SEMANTICALLY_BROKEN: &str = "\
model M {
    var BadVar: NoSuchType := 1;
    start S {
        always {
            BadVar := 1;
        }
    }
}
start Main = M;
";

/// Все четырнадцать видов объявлений, которых касается канон, - и каждое нарушает.
///
/// Формат каноничен (проба: отличием была однострочная `struct`, здесь она развёрнута;
/// с исправления то же касается `enum`), поэтому `fmt --check` вернёт 0 и наблюдаемыми
/// останутся только предупреждения.
const EVERY_KIND: &str = "\
model bad_model {
    type badType = u8;
    enum badEnum {
        badVariant
    }
    struct badStruct {
        x: u8
    }
    const bad_const: u8 := 1;
    parameter BadParam: u8 := 1;
    in BadPort: bit;
    var BadVar: u8 := 1;
    cond bad_cond = BadVar > 1;
    invariant bad_invariant = BadVar < 9;
    fn BadFn(BadArg: u8) -> u8 {
        return BadArg;
    }
    start bad_state;
}
start Main = bad_model;
";

/// Тот же состав объявлений, но **каноничный** - контр-пример: инструмент молчит, а не
/// срабатывает на всё подряд.
const EVERY_KIND_CANONICAL: &str = "\
model GoodModel {
    type GoodType = u8;
    enum GoodEnum {
        GoodVariant
    }
    struct GoodStruct {
        x: u8
    }
    const GOOD_CONST: u8 := 1;
    parameter good_param: u8 := 1;
    in good_port: bit;
    var good_var: u8 := 1;
    cond GoodCond = good_var > 1;
    invariant GoodInvariant = good_var < 9;
    fn good_fn(good_arg: u8) -> u8 {
        return good_arg;
    }
    start GoodState;
}
start Main = GoodModel;
";

/// Инвариант **в теле состояния** - вторая позиция объявления `invariant`
/// (`StateElement::Invariant`). Прочие имена файла каноничны, чтобы наблюдаемым
/// осталось ровно одно предупреждение.
const STATE_BODY_INVARIANT: &str = "\
model M {
    var level: u8 := 0;
    start S {
        invariant bad_inv = level < 9;
        always {
            level := 1;
        }
    }
}
start Main = M;
";

/// Тело состояния из элементов, которые имён **не объявляют**: переход, ссылка с
/// условием, именованный блок, периодическое действие.
const STATE_BODY_REFERENCES: &str = "\
model M {
    var level: u8 := 0;
    cond Ready = level > 1;
    start S {
        ref Done: Ready;
        every 100ms {
            level := level + 1;
        }
        always {
            level := 1;
        }
    }
    state Done {
        next S;
    }
}
start Main = M;
";

/// Вложенная модель: обход обязан спускаться в под-модели.
const NESTED_BAD_MODEL: &str = "\
model Outer {
    model inner {
        start S;
    }
    start S = inner;
}
start Main = Outer;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог (: тесты идут параллельно, и каталог, общий для двух
/// тестов, был источником гонок - каждый помощник начинает с `remove_dir_all`).
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0226_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

fn fixture(tag: &str, source: &str) -> PathBuf {
    let path = work_dir(tag).join("probe.takt");
    std::fs::write(&path, source).expect("запись фикстуры");
    path
}

/// stderr и код возврата `taktc fmt --check <файл>`.
fn fmt_check(path: &Path) -> (String, i32) {
    let out = taktc()
        .arg("fmt")
        .arg("--check")
        .arg(path)
        .output()
        .expect("запуск taktc fmt --check");
    (
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

/// stderr и код возврата `taktc fmt <файл>` (форматирование на месте).
fn fmt_in_place(path: &Path) -> (String, i32) {
    let out = taktc()
        .arg("fmt")
        .arg(path)
        .output()
        .expect("запуск taktc fmt");
    (
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

/// Только строки `CS-001` из вывода - прочие диагностики тесту не мешают.
fn style_lines(stderr: &str) -> Vec<String> {
    stderr
        .lines()
        .filter(|l| l.contains("[CS-001]"))
        .map(str::to_string)
        .collect()
}

/// Пары "вид объявления -> имя" из строк `CS-001`.
///
/// Разбор идёт по формату сообщения (`... вид 'имя' не соответствует ...`), то есть
/// тест сломается и от смены формата - это намеренно: формат тоже наблюдаем.
fn kind_and_name(stderr: &str) -> Vec<(String, String)> {
    style_lines(stderr)
        .iter()
        .filter_map(|line| {
            let tail = line.split("[CS-001]: ").nth(1)?;
            let kind = tail.split(" '").next()?.to_string();
            let name = tail.split('\'').nth(1)?.to_string();
            Some((kind, name))
        })
        .collect()
}

/// **Критерий 1 + 2.** `fmt --check` печатает `CS-001` и возвращает **0**.
///
/// Формат файла каноничен, поэтому ноль здесь означает именно "неканоничное имя
/// барьером не становится" (: именование советует, а не запрещает; CI от него не
/// краснеет).
#[test]
fn fmt_check_prints_cs001_and_keeps_zero_exit() {
    let path = fixture("check", CANONICAL_FORMAT_BAD_NAME);
    let (stderr, code) = fmt_check(&path);

    assert!(
        stderr.contains(
            "probe.takt:2:9: Предупреждение [CS-001]: порт 'BadPort' \
                         не соответствует канону именования: ожидается snake_case"
        ),
        "ожидался общий формат `путь:строка:колонка: Предупреждение [код]: текст`, \
         получено: {stderr:?}"
    );
    assert_eq!(
        code, 0,
        "неканоничное имя на код возврата влиять не должно: {stderr:?}"
    );
}

/// **Критерий 1.** `fmt` без `--check` (форматирование на месте) тоже печатает
/// предупреждение - оба режима, как записано в следствиях.
///
/// И **имя не правится**: канон именования форматированием не исправляется (
/// механическая замена испортила бы замысел автора).
#[test]
fn fmt_in_place_warns_and_leaves_name_alone() {
    let path = fixture("inplace", BAD_FORMAT_BAD_NAME);
    let (stderr, code) = fmt_in_place(&path);
    let after = std::fs::read_to_string(&path).expect("файл после fmt");

    assert_eq!(
        style_lines(&stderr).len(),
        1,
        "ожидался один CS-001: {stderr:?}"
    );
    assert_eq!(code, 0, "форматирование на месте прошло: {stderr:?}");
    assert!(
        after.contains("out BadPort: bit;"),
        "имя обязано остаться прежним, файл после fmt:\n{after}"
    );
    assert!(
        !after.contains("bad_port"),
        "форматтер не имеет права переименовывать:\n{after}"
    );
    assert!(
        after.contains("    out BadPort: bit;"),
        "формат при этом обязан быть исправлен (отступ):\n{after}"
    );
}

/// **Критерий 2 (вторая половина).** Неканоничный **формат** по-прежнему даёт
/// код 1, и предупреждение об имени этого не заслоняет.
///
/// Проверка на подмену: если бы `CS-001` считался отказом, различить "упало из-за
/// формата" и "упало из-за имени" стало бы нечем.
#[test]
fn format_verdict_survives_next_to_style_warning() {
    let path = fixture("verdict", BAD_FORMAT_BAD_NAME);
    let (stderr, code) = fmt_check(&path);

    assert_eq!(style_lines(&stderr).len(), 1, "CS-001 на месте: {stderr:?}");
    assert!(
        stderr.contains("требуется форматирование:"),
        "вердикт о формате обязан остаться: {stderr:?}"
    );
    assert_eq!(code, 1, "код возврата даёт формат, а не имя: {stderr:?}");
}

/// **Критерий 4.** Семантически некорректный файл форматируется, и
/// предупреждение на нём выдаётся.
///
/// Это и есть причина, по которой проверка живёт в отдельном слое, а не в семантике:
/// `fmt` семантику не запускает, а имена правят как раз в файлах, которые ещё не
/// собираются.
///
/// Семантическая некорректность **доказана** прогоном `compile` на том же файле: без
/// этого фикстура могла бы незаметно стать корректной, и тест проверял бы не то, что
/// заявлено.
#[test]
fn semantically_broken_file_is_formatted_and_warned() {
    let path = fixture("broken", SEMANTICALLY_BROKEN);
    let (stderr, code) = fmt_check(&path);

    assert!(
        stderr.contains("[CS-001]: переменная 'BadVar'"),
        "на семантически битом файле предупреждение обязано быть: {stderr:?}"
    );
    assert_eq!(code, 0, "формат каноничен — код 0: {stderr:?}");

    let out_dir = path.parent().expect("каталог").join("out");
    let compile = taktc()
        .arg("compile")
        .arg(&path)
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("запуск taktc compile");
    let compile_err = String::from_utf8_lossy(&compile.stderr);
    assert!(
        compile_err.contains("[SE-034]"),
        "фикстура обязана быть семантически некорректной, иначе тест ничего не \
         доказывает: {compile_err:?}"
    );
    assert_eq!(compile.status.code(), Some(1), "compile обязан отказать");
}

/// `--stdin`: предупреждение печатается, пути нет - префикс позиции не выдумывается (то
/// же решение, что у отказа в ).
///
/// Отформатированный текст при этом уходит в stdout: предупреждение не подменяет
/// работу.
#[test]
fn stdin_warns_without_path_prefix_and_prints_formatted_text() {
    let mut child = taktc()
        .arg("fmt")
        .arg("--stdin")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("запуск taktc fmt --stdin");
    child
        .stdin
        .as_mut()
        .expect("stdin")
        .write_all(BAD_FORMAT_BAD_NAME.as_bytes())
        .expect("запись в stdin");
    let out = child.wait_with_output().expect("ожидание taktc");
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stdout = String::from_utf8_lossy(&out.stdout);

    assert!(
        stderr.contains("Предупреждение [CS-001]: порт 'BadPort'"),
        "код и текст обязаны быть и без файла: {stderr:?}"
    );
    assert!(
        !stderr.contains(".takt:"),
        "пути у stdin нет — префикс позиции выдумывать нельзя: {stderr:?}"
    );
    assert!(
        stdout.contains("    out BadPort: bit;"),
        "отформатированный текст обязан уйти в stdout: {stdout:?}"
    );
    assert_eq!(out.status.code(), Some(0));
}

/// **Обход АСД.** Каждый из четырнадцати видов объявлений канона доезжает до
/// пользователя - с верным словом о виде и верной ожидаемой формой.
///
/// Это единственный тест ветвей `collect_element`/`collect_variable`: объявление,
/// выпавшее из обхода, перестало бы проверяться **молча** - как молча терялись `always`
/// уровня модели и реализация модели.
#[test]
fn every_declaration_kind_of_the_canon_is_reported() {
    let path = fixture("kinds", EVERY_KIND);
    let (stderr, code) = fmt_check(&path);

    let expected: &[(&str, &str, &str)] = &[
        ("модель", "bad_model", "UpperCamelCase"),
        ("тип", "badType", "UpperCamelCase"),
        ("перечисление", "badEnum", "UpperCamelCase"),
        ("вариант", "badVariant", "UpperCamelCase"),
        ("структура", "badStruct", "UpperCamelCase"),
        ("константа", "bad_const", "UPPER_SNAKE_CASE"),
        ("параметр", "BadParam", "snake_case"),
        ("порт", "BadPort", "snake_case"),
        ("переменная", "BadVar", "snake_case"),
        ("функция", "BadFn", "snake_case"),
        ("параметр", "BadArg", "snake_case"),
        ("состояние", "bad_state", "UpperCamelCase"),
        // прежде оба падали в `_ => {}` и не проверялись вовсе.
        ("условие", "bad_cond", "UpperCamelCase"),
        ("инвариант", "bad_invariant", "UpperCamelCase"),
    ];

    let found = kind_and_name(&stderr);
    assert_eq!(
        found.len(),
        expected.len(),
        "ожидалось {} предупреждений, получено {}: {stderr}",
        expected.len(),
        found.len()
    );
    for (kind, name, case) in expected {
        assert!(
            found.iter().any(|(k, n)| k == kind && n == name),
            "вид '{kind}' с именем '{name}' не сообщён: {stderr}"
        );
        assert!(
            stderr.contains(&format!(
                "{kind} '{name}' не соответствует канону \
                                      именования: ожидается {case}"
            )),
            "для '{name}' ожидалась форма {case}: {stderr}"
        );
    }
    assert_eq!(code, 0, "формат фикстуры каноничен: {stderr:?}");
}

/// **Контр-пример.** Тот же состав объявлений в каноничной форме - инструмент
/// молчит.
///
/// Без этой проверки предыдущий тест прошёл бы и у слоя, который срабатывает на любое
/// имя.
#[test]
fn canonical_declarations_produce_no_warnings() {
    let path = fixture("canonical", EVERY_KIND_CANONICAL);
    let (stderr, code) = fmt_check(&path);

    assert!(
        style_lines(&stderr).is_empty(),
        "каноничные имена не должны давать предупреждений: {stderr}"
    );
    assert_eq!(code, 0, "и формат каноничен: {stderr:?}");
}

/// Обход спускается в **под-модели**: имя вложенной модели проверяется.
#[test]
fn nested_model_name_is_reported() {
    let path = fixture("nested", NESTED_BAD_MODEL);
    let (stderr, _) = fmt_check(&path);

    assert_eq!(
        kind_and_name(&stderr),
        vec![("модель".to_string(), "inner".to_string())],
        "ожидалось одно предупреждение — о вложенной модели: {stderr}"
    );
}

/// Имя объявляется и в **теле состояния** - `invariant`.
///
/// Единственный тест спуска `collect_state_element`. Без спуска канон действовал бы
/// через раз: `invariant` уровня модели проверялся бы, а тот же `invariant` внутри
/// состояния - нет. Правило, работающее наполовину, хуже отсутствующего: его перестают
/// читать.
#[test]
fn state_body_invariant_is_reported() {
    let path = fixture("state_inv", STATE_BODY_INVARIANT);
    let (stderr, _) = fmt_check(&path);

    assert_eq!(
        kind_and_name(&stderr),
        vec![("инвариант".to_string(), "bad_inv".to_string())],
        "ожидалось одно предупреждение — об инварианте состояния: {stderr}"
    );
}

/// Контр-пример к спуску: элементы тела состояния, которые имён **не объявляют**, а
/// ссылаются на чужие, предупреждений не дают.
///
/// Ловит противоположную ошибку - "проверять всё подряд": переименовывать `ref`, `next`
/// или период `every` канон не вправе, это не объявления.
#[test]
fn state_body_references_are_not_reported() {
    let path = fixture("state_refs", STATE_BODY_REFERENCES);
    let (stderr, _) = fmt_check(&path);

    assert!(
        style_lines(&stderr).is_empty(),
        "ссылки объявлениями не являются, канон о них молчит: {stderr}"
    );
}

/// **Критерий 5.** Корпус даёт **ноль** предупреждений о стиле.
///
/// Тест стоит здесь, а не в предкоммите: `fmt --check examples/` печатает `CS-001`,
/// но код возврата от него не меняется, поэтому имя, вернувшееся в корпус вне канона,
/// проверка пропустил бы молча. Корпус приведён к канону - и обязан таким остаться:
/// предупреждение, которое всегда горит, перестают читать.
#[test]
fn corpus_has_no_style_warnings() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("корень репозитория")
        .join("examples");
    let mut files = Vec::new();
    collect_takt(&root, &mut files);
    assert!(files.len() >= 5, "корпус не найден: {:?}", root);

    let mut offenders = Vec::new();
    for file in &files {
        let source = std::fs::read_to_string(file).expect("исходник читается");
        let (_, warnings) = takt_lang::format::format_source_with_warnings(&source)
            .unwrap_or_else(|e| panic!("{}: корпус обязан форматироваться: {e:?}", file.display()));
        for w in warnings {
            offenders.push(format!("{}: {}", file.display(), w.message));
        }
    }
    assert!(
        offenders.is_empty(),
        "корпус обязан соответствовать канону именования (задача 0226-02):\n{}",
        offenders.join("\n")
    );
}

/// Рекурсивно собирает `.takt` - включая `examples/include/`.
fn collect_takt(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            // `generated/` - вывод целей, исходников там нет.
            if path.file_name().is_some_and(|n| n == "generated") {
                continue;
            }
            collect_takt(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("takt") {
            out.push(path);
        }
    }
}
