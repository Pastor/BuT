//! Именованное целое в таблице расширения типов.
//!
//! # Что здесь проверяется
//!
//! Тип объявления без аннотации выводится из инициализатора, и у бинарной операции его
//! выбирает `wider_type`. Таблица знала `Bit`, `Bool`, `Rational`, `Duration`, `Fixed`,
//! `Enum` и `Array(n, T)`, но **не знала** `Integer` - представления `u8`...`i64`.
//! Целочисленный литерал при этом получает `Array(n, Bit)`, поэтому **тип литерала
//! побеждал объявленный тип источника**, а пара из двух именованных целых давала
//! `Unsupported`.
//!
//!
//! | Запись | Эталон | `c` | `rust` | `st` | `sv` |
//! |---|---|---|---|---|---|
//! | `A: u16 := 300; B := A - 100;` + `seen := B + 100;` | 300 | 300 | `E0308` | отказ `iec2c` | `WIDTHEXPAND` |
//! | `A: u16 := 300; C: u8 := 5; D := A + C;` | 305 | 305 | `RS-014` | `ST-002` | `SV-002` |
//! | `A: i16 := -300; D := A + 1;` | **213** | 213 | 213 | 213 | 213 |
//!
//! Третья строка - самый дорогой исход: знак терялся, `−299` заворачивалось в `213 =
//! −299 mod 2⁸`, и все девять потребителей были **согласованно неправы**.
//!
//! Проверяется **тип**, а не факт разбора: с типом `[bit;8]` вместо `u16` файл
//! разбирается, компилируется целью `c` и даже даёт верное значение - а три другие цели
//! порождают вывод, который отвергает чужой инструмент.
//!
//! Границы соседних правил проверяются здесь же: фича не имеет права трогать
//! `Rational`, перечисления и массивы данных.

use takt_lang::generator::GenerateOptions;
use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

/// Тип объявления после построения дерева - как его видит потребитель.
fn ty_of(src: &str, name: &str) -> String {
    let (ast, _) = parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("построение дерева");
    let model = model.borrow();
    let var = model
        .variables
        .get(name)
        .unwrap_or_else(|| panic!("объявление '{name}' не найдено"));
    format!("{}", var.ty())
}

/// Модель-обёртка: объявление должно **использоваться**, иначе штатный фильтр выбросит
/// его из вывода целей и проверка ничего не докажет.
fn model_with(decl: &str, expr: &str) -> String {
    format!("{decl}\nvar seen: u32 := 0;\nstart Run {{ always {{ seen := {expr}; }} ref Run; }}\n")
}

/// Объявленный тип источника побеждает тип литерала.
#[test]
fn declared_type_beats_literal_width() {
    let src = model_with("const A: u16 := 300;\nconst B := A - 100;", "B + 100");
    assert_eq!(ty_of(&src, "B"), "u16", "тип взят у литерала, а не у `A`");
}

/// Знак источника не теряется: было `213`, стало `−299`.
#[test]
fn declared_sign_is_not_lost() {
    let src = model_with("const A: i16 := -300;\nconst D := A + 1;", "D");
    assert_eq!(ty_of(&src, "D"), "i16", "знак потерян при расширении");
}

/// Два именованных целых дают именованное целое, а не `Unsupported`.
#[test]
fn two_named_integers_yield_named_integer() {
    let src = model_with(
        "const A: u16 := 300;\nconst C: u8 := 5;\nconst D := A + C;",
        "D",
    );
    assert_eq!(
        ty_of(&src, "D"),
        "u16",
        "пара именованных целых не разобрана"
    );
}

/// Бит уточняет тип целого, не подменяя его.
#[test]
fn bit_operand_keeps_integer_type() {
    let src = model_with(
        "const F: u8 := 3;\nvar flag: bit := 1;\nvar g := F + flag;",
        "g",
    );
    assert_eq!(ty_of(&src, "g"), "u8", "бит подменил тип целого");
}

/// **Контроль: явная аннотация действует как прежде.**
///
/// Без этой проверки нельзя отличить "стало брать тип источника" от "стало брать что
/// попало": с явным типом ответ обязан остаться прежним.
#[test]
fn explicit_annotation_is_untouched() {
    let src = model_with("const A: u16 := 300;\nconst B: u16 := A - 100;", "B + 100");
    assert_eq!(ty_of(&src, "B"), "u16");
    let plain = model_with("const K := 5;", "K");
    assert_eq!(
        ty_of(&plain, "K"),
        "[bit;8]",
        "литерал сменил представление"
    );
}

/// **Граница: соседние правила таблицы сильнее.**
///
/// `Rational` побеждает любое целое, перечисление с целым несовместимо (`Unsupported` -
/// вход для `SE-059`/`SE-043`). Мутация "поставить ветви `Integer` выше" валит этот
/// тест.
#[test]
fn neighbour_rules_still_win() {
    let rational = model_with("const A: u8 := 3;\nconst R := A + 1.5;", "R");
    assert_eq!(ty_of(&rational, "R"), "float", "целое перебило дробное");
}

/// **Граница: вектор шире целого решают прежние ветви `Array`.**
///
/// Правило фичи **уточняет** тип источника литералом, а не подменяет: когда вектор шире
/// объявленного целого, результат остаётся вектором.
///
/// Вход подобран так, чтобы граница была **наблюдаема**: значение `103` влезает в оба
/// типа, поэтому расширение по результату в спор не вмешивается. Первая редакция теста
/// брала `A + 300`, где результат не влезает в восемь бит, - и мутация "снять границу"
/// её проходила.
#[test]
fn wider_literal_keeps_vector_type() {
    let src = model_with("const A: u8 := 3;\nconst W := 400 - 300 + A;", "W");
    assert_eq!(ty_of(&src, "W"), "[bit;16]");
}

/// Точка входа цели: сигнатура у всех четырёх одна.
type Compile =
    fn(
        &str,
        &str,
        &str,
        &[String],
        &GenerateOptions,
    ) -> Result<Vec<takt_lang::diagnostics::Diagnostic>, takt_lang::diagnostics::Diagnostic>;

/// Порождает код всеми целями; возвращает список отказов.
fn translate_all(tag: &str, src: &str) -> Vec<String> {
    let thread = std::thread::current()
        .name()
        .unwrap_or("x")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0287_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    let path = dir.to_str().expect("путь в UTF-8");
    let o = GenerateOptions::default();
    let mut failures = Vec::new();
    let targets: [(&str, Compile); 4] = [
        ("c", takt_lang::compile_to_c),
        ("rust", takt_lang::compile_to_rust),
        ("st", takt_lang::compile_to_st),
        ("sv", takt_lang::compile_to_sv),
    ];
    for (name, compile) in targets {
        if let Err(d) = compile(tag, src, path, &[], &o) {
            failures.push(format!("{name}: [{:?}] {}", d.code, d.message));
        }
    }
    failures
}

/// Пара именованных целых переводится **всеми** целями.
///
/// До фичи отказывали четверо: `RS-014`, `ST-002`, `SV-002` (и `st-at`/`sv-mmio` вслед
/// за ними) - на записи, которую эталон считает без затруднений.
#[test]
fn named_integer_pair_translates_by_every_target() {
    let src = model_with(
        "const A: u16 := 300;\nconst C: u8 := 5;\nconst D := A + C;",
        "D",
    );
    let failures = translate_all("pair", &src);
    assert!(failures.is_empty(), "цели отказали: {failures:?}");
}

/// Тип доезжает до объявления в порождённом Rust - проверяется текст.
///
/// Факт успешной генерации здесь ничего не доказывает: прежний вывод тоже "удавался", а
/// отвергал его `rustc` (`E0308`: `u8` в поле `u16`).
#[test]
fn declared_width_reaches_generated_rust() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0287_rust_decl");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    let src = model_with("const A: u16 := 300;\nconst B := A - 100;", "B + 100");
    takt_lang::compile_to_rust(
        "widen",
        &src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join("widen.rs")).expect("чтение модуля");
    assert!(
        text.contains("const WIDEN_B: u16 = 200;"),
        "константа объявлена не тем типом:\n{text}"
    );
}
