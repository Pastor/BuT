//! Ширина выведенного типа берётся у результата.
//!
//! # Что здесь проверяется
//!
//! Тип объявления без явного типа выводится **до** свёртки, поэтому ширину выбирал
//! левый операнд: `const K := 1 + 255;` получал `[bit;8]`, а нормирование заворачивало
//! вычисленные 256 в **ноль**. Молча и одинаково у всех девяти потребителей - то есть
//! согласованно неверно.
//!
//!
//! | Запись | Было | Ожидается |
//! |---|---|---|
//! | `1 << 8` | 0 | 256 |
//! | `1 + 255` | 0 | 256 |
//! | `2 * 128` | 0 | 256 |
//! | `200 + 200` | 144 | 400 |
//! | `300 - 44` | 256 | 256 |
//!
//! Последняя строка - ключ к причине: там левый операнд `300` уже не влезает в восемь
//! бит, поэтому выведенная ширина оказывалась достаточной **случайно**.
//!
//! # Границы, каждая из которых уже ловила перебор
//!
//! **Явный тип не трогается**: `var u: u8 := 200 + 100;` обязан остаться `44` - там
//! ширину выбрал автор, и обёртка совпадает с телом. **Тип из сигнатуры функции не
//! трогается**: первая редакция правки переопределяла тип всегда и сломала вывод Ce6.
//! **Отрицательное значение не расширяется**: `var u := ~0;` даёт `255` - это домен, а
//! не ширины.

use takt_lang::generator::GenerateOptions;
use takt_sim::{TickResult, Value};

fn model(decl: &str) -> String {
    format!(
        "{decl}\nvar seen: u32 := 0;\nvar ticks: u8 := 0;\nstart Run {{ always {{ ticks := ticks + 1; seen := K; }} ref Run: ticks < 2; }}\n"
    )
}

fn reference_value(src: &str, name: &str) -> Value {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение юнита");
    let result = unit.tick();
    assert!(
        !matches!(result, TickResult::Failed(_)),
        "эталон упал: {result:?}"
    );
    unit.variable(name).expect("значение")
}

/// Сверяется с целью, а не с числом (образец 0205/0300).
fn generated_c_const(tag: &str, src: &str) -> String {
    let thread = std::thread::current()
        .name()
        .unwrap_or("x")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0285_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    takt_lang::compile_to_c(
        tag,
        src,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join(format!("{tag}.c")))
        .expect("чтение .c")
        .lines()
        .find(|l| l.contains("#define") && l.contains("_K "))
        .expect("определение константы K")
        .trim()
        .to_string()
}

fn check(tag: &str, expr: &str, expected: i128) {
    let src = model(&format!("const K := {expr};"));
    assert_eq!(
        reference_value(&src, "seen"),
        Value::Number(expected),
        "эталон разошёлся с ожиданием на `{expr}`"
    );
    let line = generated_c_const(tag, &src);
    assert!(
        line.ends_with(&expected.to_string()),
        "цель `c` печатает иное на `{expr}`: {line}"
    );
}

/// Сложение, дающее переполнение левого операнда.
#[test]
fn sum_widens_to_result() {
    check("sum", "1 + 255", 256);
}

/// Порядок операндов роли не играет.
#[test]
fn operand_order_does_not_matter() {
    check("sum_rev", "255 + 1", 256);
}

/// Умножение и сдвиг - тот же случай (сдвиг называл кандидат).
#[test]
fn product_and_shift_widen_too() {
    check("mul", "2 * 128", 256);
    check("shift", "1 << 8", 256);
}

/// Значение шире одного разряда: было `144` вместо `400`.
#[test]
fn wrap_no_longer_eats_the_value() {
    check("wrap", "200 + 200", 400);
}

/// **Контроль: там, где ширины хватало, ничего не изменилось.**
///
/// `300` не влезает в восемь бит, поэтому выведенная ширина была достаточной и до фичи.
/// Без этой проверки нельзя отличить "починили" от "поменяли всё".
#[test]
fn already_wide_enough_is_untouched() {
    check("wide", "300 - 44", 256);
    check("plain", "250", 250);
}

/// **Граница: Явный тип не трогается** - обёртка остаётся.
#[test]
fn declared_type_still_wraps() {
    let src = "var u: u8 := 200 + 100;\nvar ticks: u8 := 0;\n\
               start Run { always { ticks := ticks + 1; u := u; } ref Run: ticks < 2; }\n";
    assert_eq!(reference_value(src, "u"), Value::Number(44));
}

/// **Граница: отрицательное значение - домен.**
///
/// `~0` даёт `-1`; беззнаковый выведенный тип обязан завернуть его в `255`, а не
/// "расшириться". Мутация "расширять и по нижней границе" валит этот тест.
#[test]
fn negative_value_is_not_widened() {
    let src = "var u := ~0;\nvar ticks: u8 := 0;\n\
               start Run { always { ticks := ticks + 1; u := u; } ref Run: ticks < 2; }\n";
    assert_eq!(reference_value(src, "u"), Value::Number(255));
}

/// **Граница: тип из сигнатуры функции не трогается.**
///
/// Вычислитель сворачивает вызов в `0`, и первая редакция правки давала `[bit;8]`
/// вместо объявленного `[bit;32]`. Ширину там выбрал не операнд.
#[test]
fn return_type_of_function_is_kept() {
    use takt_lang::semantic::type_node::TypeNode;
    let src = "fn get32() -> [bit;32] { return 0; }\nvar val := get32();\nstart S;\n";
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let borrowed = model.borrow();
    let var = borrowed.search_var("val").expect("val");
    assert_eq!(
        var.ty().clone(),
        TypeNode::Array(32, Box::new(TypeNode::Bit))
    );
}
