//! Вывод типа через ссылку на другое объявление.
//!
//! # Что здесь проверяется
//!
//! Документ обещает вывод типа "из **выражения**", а не "из литерала". До фичи обещание
//! исполнялось только для литералов: ссылка на объявление без явного типа не давала
//! типа вовсе, а `2s + 1s` давало `Unsupported`.
//!
//! Проверяется **тип**, а не факт разбора: прежний тест этой формы
//! (`semantic_tests::example_ce6_type_inference_chain_valid`) проверял, что файл
//! разбирается без ошибок, - и проходил всё время, пока обещанный им вывод не работал.

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
    format!("{:?}", var.ty())
}

/// **T1.** Ссылка на константу-длительность протягивает тип.
#[test]
fn reference_to_duration_const_carries_its_type() {
    let ty = ty_of(
        "const BASE := 2s;\nconst DWELL := BASE;\nstart Run { ref Run; }\n",
        "DWELL",
    );
    assert_eq!(ty, "Duration", "ссылка не протянула тип длительности");
}

/// **T2.** Порядок имён не важен: источник может стоять **позже** по алфавиту.
///
/// Таблица объявлений - `BTreeMap`, и обход идёт по алфавиту: `A` разбирается раньше
/// `Z`, на который ссылается. Однопроходный вывод такую цепочку не сходится - отсюда
/// неподвижная точка.
#[test]
fn reference_resolves_regardless_of_alphabetical_order() {
    let ty = ty_of(
        "const Z := 5;\nconst A := Z;\nstart Run { ref Run; }\n",
        "A",
    );
    assert_eq!(ty, "Array(8, Bit)", "обратный порядок имён не сошёлся");
}

/// **T3.** Цепочка длины три сходится целиком.
#[test]
fn three_link_chain_resolves() {
    let src = "const A := 5;\nconst B := A;\nconst C := B;\nstart Run { ref Run; }\n";
    assert_eq!(ty_of(src, "B"), "Array(8, Bit)");
    assert_eq!(ty_of(src, "C"), "Array(8, Bit)", "третье звено без типа");
}

/// **T4.** Обычная переменная - тот же случай, что константа.
#[test]
fn simple_variable_reference_carries_type() {
    let ty = ty_of(
        "var a := 5;\nvar b := a;\nstart Run { always { b := b; } }\n",
        "b",
    );
    assert_eq!(ty, "Array(8, Bit)");
}

/// **T5.** Арифметика длительностей - вторая причина, независимая от ссылок.
///
/// Правила `(Duration, Duration)` в `wider_type` не было вовсе, поэтому `2s + 1s` -
/// запись **без единой ссылки** - давала `Unsupported`.
#[test]
fn duration_arithmetic_yields_duration() {
    let ty = ty_of("const S := 2s + 1s;\nstart Run { ref Run; }\n", "S");
    assert_eq!(ty, "Duration");
}

/// **T6.** Таблица доезжает до **рекурсивных** ветвей вывода.
///
/// Ссылка спрятана под скобками и двумя операциями: если хоть один рекурсивный вызов
/// забудет протащить таблицу, тип снова станет `Unsupported` - и молча.
#[test]
fn table_reaches_nested_expression_branches() {
    let ty = ty_of(
        "const A := 2s;\nconst B := (A + 1s) - 500ms;\nstart Run { ref Run; }\n",
        "B",
    );
    assert_eq!(ty, "Duration", "таблица не дошла до вложенных ветвей");
}

/// **T7.** Взаимная ссылка не зацикливает вывод.
///
/// Проход без прогресса завершает цикл; оба объявления остаются без типа - о нём скажут
/// цели. Падение теста по таймауту здесь и означало бы незавершение.
#[test]
fn mutual_reference_terminates_without_type() {
    let src = "const A := B;\nconst B := A;\nstart Run { ref Run; }\n";
    assert_eq!(ty_of(src, "A"), "Inference");
    assert_eq!(ty_of(src, "B"), "Inference");
}

/// **T8.** Порядок трёх шагов цел: свёртка по-прежнему после вывода типов.
///
/// Тест чужой фичи, стоящий здесь намеренно: соблазн "свернуть инициализаторы раньше" -
/// первое, что приходит в голову для этой задачи, и именно он даёт `a` тип `bool`
/// вместо `bit`. Значение при этом верное, а тип нет - то есть ошибка молчаливая.
#[test]
fn folding_still_happens_after_inference() {
    let ty = ty_of(
        "var b: bit := false;\nvar a := b;\nstart Run { always { a := a; } }\n",
        "a",
    );
    assert_eq!(ty, "Bit", "свёртка обогнала вывод типов");
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
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0204_{tag}"));
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

/// **T9.** Форма из фикстуры переводится **всеми** целями.
///
/// До фичи отказывали все четыре: тип `_` не представим ни в C, ни в Rust, ни в ST, ни
/// в SV.
#[test]
fn inferred_chain_translates_by_every_target() {
    let src = "var x := 42;\nvar y := x;\nstart S { always { y := y + 1; } }\n";
    let failures = translate_all("chain", src);
    assert!(failures.is_empty(), "цели отказали: {failures:?}");
}

/// **T10.** Тип доезжает до объявления в порождённом C.
///
/// Проверяется **текст** вывода, а не факт успеха: тип мог бы стать иным (например,
/// шире), и сборка это скрыла бы.
#[test]
fn inferred_type_reaches_generated_c_declaration() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0204_decl");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    takt_lang::compile_to_c(
        "decl",
        "var x := 42;\nvar y := x;\nstart S { always { y := y + 1; } }\n",
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let header = std::fs::read_to_string(dir.join("decl.h")).expect("чтение заголовка");
    assert!(
        header.contains("uint8_t y;"),
        "в структуре нет поля выведенного типа:\n{header}"
    );
}
