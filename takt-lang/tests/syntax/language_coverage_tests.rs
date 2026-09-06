//! Проверка покрытия: примеры обязаны показывать каждую конструкцию языка.
//!
//! ## Что тестытся
//!
//! Две области, каждая со своей ролью:
//!
//! - `examples/` - исполняемая документация по языку. Её гоняют проверки
//!   предкоммита: компиляция восемью целями, потактовые сверки, снимки
//!   порождённого кода. Конструкции, которой здесь нет, эти проверки **не видят**;
//!   отсюда повторяющаяся строка разборов "корпус класс не покрывает".
//! - `book/src/**` - справочник языка. Конструкция, не показанная
//!   ни одним примером документа, читателю не объяснена.
//!
//! ## Откуда берётся перечень
//!
//! Перечень конструкций - **варианты перечислений АСД**, вычитанные из
//! `parser/ast*.rs`, плюс [`takt_lang::parser::coverage::FLAT_CONSTRUCTS`]. Своего
//! списка имён тест не держит: он разошёлся бы с АСД молча. Классификатор сверяется с
//! тем же перечнем - опечатка в имени вида роняет
//! `classifier_speaks_the_names_of_the_ast`, а не создаёт тихо непокрываемую запись.
//!
//! ## Достижимость считается разбором, а не мнением
//!
//! Требовать покрытия можно только от того, что вообще **строится** при разборе
//! исходника. Признак - упоминание имени в `grammar.lalrpop` и модулях лексера;
//! контроль на него стоит рядом: недостижимый вид, встреченный в корпусе, роняет тест -
//! значит признак неверен и его надо чинить, а не обходить.
//!
//! ## Долг
//!
//! Реестр `scripts/language-coverage-baseline.txt` - **узаконенный долг** с ратчетом:
//! непокрытый вид вне реестра роняет тест (`C1`), а запись о виде, который уже покрыт, -
//! протухла и тоже роняет (`C2`). Так покрытие может только расти.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// Область покрытия: у каждой свой список долга.
///
/// Областей три, и роли у них разные:
///
/// - `examples` - исполняемый корпус целиком: есть ли конструкция в проекте
///   вообще. Проверки целей гоняют только его верхний уровень;
/// - `language` - подкаталог примеров языка: показана ли конструкция там, где
///   её ищут глазами. Область `examples` его не заменяет и не заменяется им:
///   львиная доля конструкций живёт **только** в `examples/language`, а
///   проверки целей видят только верхний уровень;
/// - `book` - справочник языка.
const AREAS: [(&str, &str); 3] = [
    ("examples", "examples"),
    ("language", "examples/language"),
    ("book", "book/src"),
];

/// Корень репозитория.
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("у крейта есть родительский каталог")
        .to_path_buf()
}

/// Читает файл репозитория целиком.
fn read(relative: &str) -> String {
    let path = root().join(relative);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

/// Все виды конструкций языка: варианты перечислений АСД и плоские формы.
fn all_constructs() -> BTreeSet<String> {
    let mut kinds: BTreeSet<String> = takt_lang::parser::coverage::FLAT_CONSTRUCTS
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    for file in ["ast.rs", "ast_expr.rs", "ast_cond.rs"] {
        let src = read(&format!("takt-lang/src/parser/{file}"));
        let mut enum_name: Option<String> = None;
        for line in src.lines() {
            if let Some(rest) = line.strip_prefix("pub enum ") {
                enum_name = rest.split_whitespace().next().map(str::to_string);
                continue;
            }
            if line == "}" {
                enum_name = None;
                continue;
            }
            let Some(name) = &enum_name else { continue };
            // Вариант перечисления - строка ровно с четырьмя пробелами отступа,
            // начинающаяся с заглавной буквы: ` Match(...)`, ` Bit,`.
            let Some(body) = line.strip_prefix("    ") else {
                continue;
            };
            if body.starts_with(' ') || !body.starts_with(char::is_uppercase) {
                continue;
            }
            let variant: String = body
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            if !variant.is_empty() {
                kinds.insert(format!("{name}::{variant}"));
            }
        }
    }
    kinds
}

/// Виды, которые разбор действительно строит.
///
/// Признак - упоминание имени в грамматике либо в лексере: там и только там узлы АСД
/// рождаются. Всё прочее - узлы без правила.
fn reachable(all: &BTreeSet<String>) -> BTreeSet<String> {
    let mut builders = without_comments(&read("takt-lang/src/grammar.lalrpop"));
    for file in ["lexer.rs", "lexer_time.rs", "time_literal.rs"] {
        builders.push_str(&without_comments(&read(&format!(
            "takt-lang/src/parser/{file}"
        ))));
    }
    all.iter()
        .filter(|kind| {
            takt_lang::parser::coverage::FLAT_CONSTRUCTS.contains(&kind.as_str())
                || mentions(&builders, kind)
        })
        .cloned()
        .collect()
}

/// Убирает строчные комментарии: узел строит код, а не пояснение к нему.
///
/// Без этого изъятая из языка конструкция остаётся "достижимой" через упоминание в
/// комментарии - и проверка требует примера для того, чего грамматика уже не строит.
fn without_comments(source: &str) -> String {
    source
        .lines()
        .map(|line| match line.find("//") {
            Some(at) => &line[..at],
            None => line,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Упомянуто ли имя целиком, а не как приставка чужого.
///
/// Наивная подстрока объявляет достижимым `Expression::Array` из-за
/// `Expression::ArraySubscript` - то есть снимает требование покрытия с конструкции,
/// которой разбор не строит вовсе, и требует его от той, которую показать нечем.
/// Граница проверяется по следующему символу.
fn mentions(haystack: &str, name: &str) -> bool {
    haystack.match_indices(name).any(|(at, _)| {
        haystack[at + name.len()..]
            .chars()
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_')
    })
}

/// Виды, встреченные в примерах области.
fn covered_by(area_dir: &str) -> BTreeSet<&'static str> {
    let mut found = BTreeSet::new();
    let mut stack = vec![root().join(area_dir)];
    while let Some(dir) = stack.pop() {
        let entries = std::fs::read_dir(&dir).unwrap_or_else(|e| panic!("{}: {e}", dir.display()));
        for entry in entries {
            let path = entry.expect("запись каталога").path();
            if path.is_dir() {
                // `examples/generated/` - вывод целей, не исходники языка.
                if path.file_name().is_some_and(|name| name == "generated") {
                    continue;
                }
                stack.push(path);
                continue;
            }
            if !path.extension().is_some_and(|ext| ext == "takt") {
                continue;
            }
            let src = std::fs::read_to_string(&path).expect("пример читается");
            // Неразобранный пример - предмет других проверок (0133, компиляция корпуса);
            // здесь он просто не даёт покрытия.
            if let Ok((model, comments)) = takt_lang::parse(&src, 0) {
                found.extend(takt_lang::parser::coverage::constructs_of(
                    &model, &comments,
                ));
            }
        }
    }
    found
}

/// Область `never`: конструкция не может появиться в каноничном примере.
///
/// Не долг, а граница: узел восстановления после ошибки живёт только на ошибочном
/// входе, а лишнюю `;` стирает `taktc fmt` (проба 2026-08-23), то есть в `examples/`
/// она не удержится - там канон тестыт `fmt --check`.
const NEVER: &str = "never";

/// Реестр: область -> виды, покрытия которых не требуют. Причина обязательна.
fn debt() -> BTreeMap<String, BTreeSet<String>> {
    let mut debt: BTreeMap<String, BTreeSet<String>> = AREAS
        .iter()
        .map(|(area, _)| ((*area).to_string(), BTreeSet::new()))
        .collect();
    debt.insert(NEVER.to_string(), BTreeSet::new());
    for line in read("scripts/language-coverage-baseline.txt").lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let (record, reason) = line
            .split_once('#')
            .unwrap_or_else(|| panic!("строка реестра без причины: {line}"));
        assert!(
            !reason.trim().is_empty(),
            "причина в строке реестра пуста: {line}"
        );
        let mut parts = record.split_whitespace();
        let area = parts.next().expect("непустая строка несёт область");
        let kind = parts
            .next()
            .unwrap_or_else(|| panic!("строка реестра без вида: {line}"));
        let entry = debt
            .get_mut(area)
            .unwrap_or_else(|| panic!("неизвестная область '{area}' в строке: {line}"));
        entry.insert(kind.to_string());
    }
    debt
}

/// Классификатор называет виды теми же именами, что объявлены в АСД.
///
/// Без этой сверки опечатка (`Statement::Mach`) дала бы вид, которого нет в перечне: он
/// никогда не считался бы покрытым и никогда не был бы затребован - то есть проверка молча
/// перестал бы тестыть конструкцию.
#[test]
fn classifier_speaks_the_names_of_the_ast() {
    let known = all_constructs();
    let source = read("takt-lang/src/parser/coverage/construct.rs");
    let mut unknown = Vec::new();
    for literal in source.split('"').skip(1).step_by(2) {
        if !literal.contains("::") || literal.contains(' ') {
            continue;
        }
        if !known.contains(literal) {
            unknown.push(literal.to_string());
        }
    }
    assert!(
        unknown.is_empty(),
        "классификатор называет виды, которых нет в АСД: {unknown:?}"
    );
}

/// Плоская форма - не выдумка теста: каждая обязана выдаваться классификатором.
#[test]
fn flat_constructs_are_emitted_by_the_classifier() {
    let source = read("takt-lang/src/parser/coverage/construct.rs");
    let orphans: Vec<_> = takt_lang::parser::coverage::FLAT_CONSTRUCTS
        .iter()
        .filter(|kind| !source.contains(&format!("\"{kind}\"")))
        .collect();
    assert!(
        orphans.is_empty(),
        "плоские формы объявлены, но не выдаются классификатором: {orphans:?}"
    );
}

/// Контроль признака достижимости: недостижимый вид не может встретиться.
///
/// Если встретился - признак неверен, и требование покрытия было снято с
/// конструкции, которую автор пишет. Ошибка тут тихая, поэтому проверка своя.
#[test]
fn unreachable_constructs_never_appear_in_examples() {
    let all = all_constructs();
    let reachable = reachable(&all);
    let mut seen = BTreeSet::new();
    for (_, dir) in AREAS {
        seen.extend(covered_by(dir));
    }
    let wrong: Vec<_> = all
        .iter()
        .filter(|kind| !reachable.contains(*kind) && seen.contains(kind.as_str()))
        .collect();
    assert!(
        wrong.is_empty(),
        "признак достижимости неверен — эти виды встречены в примерах: {wrong:?}"
    );
}

/// Реестр долга не залёживается: покрытая конструкция обязана быть удалена (`C2`).
#[test]
fn debt_registry_has_no_stale_entries() {
    let debt = debt();
    let mut stale = Vec::new();
    for (area, dir) in AREAS {
        let covered = covered_by(dir);
        for kind in debt[area].iter().chain(debt[NEVER].iter()) {
            if covered.contains(kind.as_str()) {
                stale.push(format!("{area} {kind}"));
            }
        }
    }
    assert!(
        stale.is_empty(),
        "записи долга протухли — конструкция покрыта, строку надо удалить \
         из scripts/language-coverage-baseline.txt:\n  {}",
        stale.join("\n  ")
    );
}

/// Реестр не перечисляет того, чего нет: вид обязан существовать и быть достижим.
#[test]
fn debt_registry_names_reachable_constructs() {
    let all = all_constructs();
    let reachable = reachable(&all);
    let mut wrong = Vec::new();
    let registry = debt();
    for area in AREAS.iter().map(|(area, _)| *area).chain([NEVER]) {
        for kind in &registry[area] {
            if !all.contains(kind) {
                wrong.push(format!("{area} {kind} — вида нет в АСД"));
            } else if !reachable.contains(kind) {
                wrong.push(format!("{area} {kind} — вид недостижим, требования нет"));
            }
        }
    }
    assert!(
        wrong.is_empty(),
        "реестр долга разошёлся с языком:\n  {}",
        wrong.join("\n  ")
    );
}

/// Главное условие (`C1`): каждая достижимая конструкция показана примером.
///
/// Падает **списком** - иначе покрытие пришлось бы догонять по одной находке за
/// прогон. Непокрытое либо получает пример, либо вносится в реестр долга с
/// явной строкой: молчаливого пропуска у проверки нет.
#[test]
fn every_construct_of_the_language_has_an_example() {
    let all = all_constructs();
    let reachable = reachable(&all);
    let debt = debt();
    let mut missing = Vec::new();
    for (area, dir) in AREAS {
        let covered = covered_by(dir);
        for kind in &reachable {
            if !covered.contains(kind.as_str())
                && !debt[area].contains(kind)
                && !debt[NEVER].contains(kind)
            {
                missing.push(format!("{area} {kind}"));
            }
        }
    }
    assert!(
        missing.is_empty(),
        "конструкции языка без примера ({} шт.). Добавьте пример либо запись \
         в scripts/language-coverage-baseline.txt:\n  {}",
        missing.len(),
        missing.join("\n  ")
    );
}

/// Отчёт о покрытии: печатается всегда, читается человеком при разборе долга.
#[test]
fn coverage_report() {
    let all = all_constructs();
    let reachable = reachable(&all);
    println!(
        "конструкций в АСД: {}, достижимо разбором: {}",
        all.len(),
        reachable.len()
    );
    for (area, dir) in AREAS {
        let covered = covered_by(dir);
        let hit = reachable
            .iter()
            .filter(|kind| covered.contains(kind.as_str()))
            .count();
        let percent = hit * 100 / reachable.len();
        println!(
            "  {area}: покрыто {hit} из {} ({percent}%)",
            reachable.len()
        );
    }
}
