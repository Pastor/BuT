//! Бенчмарки верификации LTL - фича.
//!
//! Верификация - самое затратное, что делает `taktc`: конвейер
//! Крипке -> Бюхи -> произведение -> проверка пустоты. Именно здесь дважды менялся
//! класс сложности: 0052 переводила обходы с рекурсии на итерацию (падение на
//! 2800 состояниях), 0068 вводила потолок вершин `1_000_000`, потому что
//! абстракция по данным умножает число состояний на произведение доменов.
//!
//! Поэтому меряются **две** оси отдельно:
//!
//! - **управляющие свойства** - растут по числу состояний;
//! - **свойства над данными** - растут по произведению доменов отслеживаемых
//!   переменных, и рост там куда быстрее.
//!
//! Бенч ловит замедление, а не неверный вердикт. Правильность вердикта
//! проверяют тесты верификации (`verify_tests.rs`), и подменять их бенчем
//! нельзя.

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use takt_lang::semantic::tree::construct_model;
use takt_lang::{parse_ltl_property, verify_model};

/// Цепочка из `states` состояний - вход для управляющих свойств.
///
/// Состояния объявляются на **верхнем уровне**, без обёртки
/// `model Big { ... } start Root = Big;`. `verify_model` проверяет ровно ту
/// модель, которую ей дали, а у анонимного корня с одной лишь ссылкой на
/// под-модель **своих состояний нет**: атом `S199` там неизвестен, и проверка
/// честно отвечает `Unsupported`. Первая редакция этого бенча так и мерила -
/// ранний отказ за 376 нс, одинаковый для 200 и 2000 состояний. Отсюда контроль
/// ниже.
fn chain_model(states: usize) -> String {
    let mut src = String::from("out flag: bit;\n");
    src.push_str("start S0 { always { flag := 1; } ref S1; }\n");
    for i in 1..states - 1 {
        src.push_str(&format!("state S{i} {{ ref S{}; }}\n", i + 1));
    }
    src.push_str(&format!("state S{};\n", states - 1));
    src
}

/// Модель с `vars` отслеживаемыми переменными типа `u8`.
///
/// Число вершин Крипке - состояния x 256^vars, поэтому уже две
/// переменные дают 65 536 оценок: рост по этой оси несопоставим с ростом по
/// числу состояний.
fn data_model(vars: usize) -> String {
    let mut src = String::from("out flag: bit;\n");
    for i in 0..vars {
        src.push_str(&format!("var v{i}: u8 := 0;\n"));
        // Отдельное условие на каждую переменную: отслеживаются только те, что
        // встречаются в формуле. Без этого вторая переменная в граф
        // не попадала бы, и `vars = 2` мерило бы ровно то же, что `vars = 1` -
        // первая редакция бенча так и показывала: 87.5 мс против 87.7 мс.
        src.push_str(&format!("cond Hot{i} = v{i} >= 200;\n"));
    }
    src.push_str("start Idle {\n    always { flag := 1; }\n    ref Work: v0 > 10;\n}\n");
    src.push_str("state Work { ref Idle: v0 < 5; }\n");
    src
}

/// Убеждается, что мерить есть что: `Unsupported` возвращается мгновенно, и
/// бенч на нём показывал бы наносекунды независимо от размера модели.
///
/// Контроль, не проверяющий взведённость ловушки, проверяет собственную удачу -
/// здесь ловушка сработала на первой же редакции бенча.
fn assert_verifiable(
    model: &std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>>,
    phi: &takt_lang::verification::ltl::Ltl,
    what: &str,
) {
    use takt_lang::verification::verify::Verdict;
    match verify_model(model.clone(), phi) {
        Verdict::Holds | Verdict::Violated(_) => {}
        other => panic!("{what}: проверка не выполняется ({other:?}) — бенч мерил бы отказ"),
    }
}

/// Модель с `bits` отслеживаемыми переменными типа `bit`.
///
/// Число оценок - `2^bits`, то есть сетка вдвое (а не в 256 раз) мельче, чем у
/// [`data_model`]. Управляющий граф тот же, что там: два состояния, четыре
/// ребра (по guard'у и самопетле у каждого), - поэтому рёбер Крипке с данными
/// ровно `4 x (2^bits)²`, и точка бенча сравнима с потолком фичи прямым
/// счётом.
fn data_model_bits(bits: usize) -> String {
    let mut src = String::from("out flag: bit;\n");
    for i in 0..bits {
        src.push_str(&format!("var v{i}: bit := 0;\n"));
        src.push_str(&format!("cond Hot{i} = v{i} = 1;\n"));
    }
    src.push_str("start Idle {\n    always { flag := 1; }\n    ref Work: v0 = 1;\n}\n");
    src.push_str("state Work { ref Idle: v0 = 0; }\n");
    src
}

fn model_of(src: &str) -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect("семантика")
}

fn bench_control_properties(c: &mut Criterion) {
    let mut group = c.benchmark_group("verify_control");
    for states in [200usize, 2000] {
        let model = model_of(&chain_model(states));
        // `F S{n-1}` - достижимость последнего состояния: обход идёт по всей
        // цепочке, то есть время должно расти линейно по числу состояний.
        let phi = parse_ltl_property(&format!("F S{}", states - 1)).expect("формула");
        assert_verifiable(&model, &phi, &format!("chain/{states}"));
        group.bench_with_input(
            BenchmarkId::new("reachability/chain", states),
            &(model, phi),
            |b, (model, phi)| {
                b.iter(|| verify_model(black_box(model.clone()), black_box(phi)));
            },
        );
    }
    group.finish();
}

fn bench_data_properties(c: &mut Criterion) {
    let mut group = c.benchmark_group("verify_data");
    // Ось роста - число оценок (произведение доменов отслеживаемых переменных),
    // и меряется она `bit`-переменными: `k` штук дают `D = 2^k`, то есть сетку
    // вдвое мельче, чем шаг "ещё один `u8`" (x256).
    //
    // Точки подобраны под потолок фичи: он считается по рёбрам
    // (`рёбра управляющего графа x D²`, здесь рёбер 4), поэтому `k = 8` даёт
    // 262 144 рёбер и проходит, а `k = 9` - 1 048 576 и отвергается
    // `Unsupported`. Контроль `assert_verifiable` ниже это и ловит: бенч на
    // отказе мерил бы наносекунды.
    //
    // До 0145 здесь стояла одна точка, и в комментарии объяснялось, почему
    // вторую поставить нельзя: при двух `u8` (65 536 оценок) прогон не
    // заканчивался за 90 секунд, хотя вершин было 131 072 - вшестеро ниже
    // тогдашнего потолка по вершинам. Ось, ради которой бенч заведён, не
    // мерилась вовсе.
    for bits in [6usize, 7, 8] {
        let model = model_of(&data_model_bits(bits));
        // Формула упоминает все переменные - иначе неупомянутые не отслеживаются.
        // Конъюнкция в LTL пишется `&` (не `&&`): грамматика формул своя.
        let atoms: Vec<String> = (0..bits).map(|i| format!("Hot{i}")).collect();
        let phi = parse_ltl_property(&format!("G ({})", atoms.join(" & "))).expect("формула");
        assert_verifiable(&model, &phi, &format!("data/bits={bits}"));
        group.bench_with_input(
            BenchmarkId::new("globally/valuations", 1usize << bits),
            &(model, phi),
            |b, (model, phi)| {
                b.iter(|| verify_model(black_box(model.clone()), black_box(phi)));
            },
        );
    }

    // Реалистичный вход того же класса: один `u8` (256 оценок) - так свойство
    // над данными выглядит в модели, а не в бенче.
    {
        let model = model_of(&data_model(1));
        let phi = parse_ltl_property("G (Hot0)").expect("формула");
        assert_verifiable(&model, &phi, "data/u8");
        group.bench_with_input(
            BenchmarkId::new("globally/u8", 1usize),
            &(model, phi),
            |b, (model, phi)| {
                b.iter(|| verify_model(black_box(model.clone()), black_box(phi)));
            },
        );
    }
    group.finish();
}

criterion_group!(benches, bench_control_properties, bench_data_properties);
criterion_main!(benches);
