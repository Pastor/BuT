//! Бенчмарки симуляции - фича.
//!
//! Симулятор - **эталон поведения** для всех целей генерации: потактовые сверки
//! (`conformance_*_tests`) гоняют его на каждом прогоне тестов, поэтому его
//! стоимость видна не только пользователю, но и предкоммиту.
//!
//! Меряются два разных дела:
//!
//! - **построение** дерева `Unit` из семантической модели - цена "холодного
//!   старта" симуляции;
//! - **такт** - цена шага, то есть то, что умножается на длину прогона.
//!
//! Быстро != верно: бенч не проверяет ни значений, ни переходов. За это
//! отвечают `eval_tests.rs` и сверки с порождённым кодом.

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use takt_lang::semantic::tree::construct_model;
use takt_sim::build_unit;

/// Цепочка из `states` состояний: каждый такт делает ровно один переход.
fn chain_model(states: usize) -> String {
    let mut src = String::from("model Big {\n    out flag: bit;\n");
    src.push_str("    start S0 { always { flag := 1; } ref S1; }\n");
    for i in 1..states - 1 {
        src.push_str(&format!("    state S{i} {{ ref S{}; }}\n", i + 1));
    }
    src.push_str(&format!(
        "    state S{};\n}}\nstart Root = Big;\n",
        states - 1
    ));
    src
}

/// Модель со счётной работой в теле: такт делает арифметику, а не только
/// переход, - ближе к реальным моделям корпуса.
const BUSY: &str = r#"
model Busy {
    in sensor: u8;
    out level: u8;
    var acc: u8 := 0;
    var ticks: u8 := 0;
    start Run {
        always {
            acc := acc + sensor;
            ticks := ticks + 1;
            level := acc / 2;
        }
    }
}
start Root = Busy;
"#;

fn model_of(src: &str) -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect("семантика")
}

fn bench_build(c: &mut Criterion) {
    let mut group = c.benchmark_group("sim_build");
    for states in [200usize, 2000] {
        let model = model_of(&chain_model(states));
        group.bench_with_input(BenchmarkId::new("chain", states), &model, |b, model| {
            b.iter(|| build_unit(black_box(model.clone())).expect("построение Unit"));
        });
    }
    group.finish();
}

fn bench_tick(c: &mut Criterion) {
    let mut group = c.benchmark_group("sim_tick");

    // Такт с переходом: цена диспетчеризации состояния.
    let model = model_of(&chain_model(2000));
    group.bench_function("chain/2000/100_ticks", |b| {
        b.iter(|| {
            let mut unit = build_unit(model.clone()).expect("построение Unit");
            for _ in 0..100 {
                unit.tick();
            }
            black_box(unit.active_states())
        });
    });

    // Такт со счётной работой: цена вычислений в теле.
    let busy = model_of(BUSY);
    group.bench_function("busy/1000_ticks", |b| {
        b.iter(|| {
            let mut unit = build_unit(busy.clone()).expect("построение Unit");
            for _ in 0..1000 {
                unit.tick();
            }
            black_box(unit.active_states())
        });
    });
    group.finish();
}

criterion_group!(benches, bench_build, bench_tick);
criterion_main!(benches);
