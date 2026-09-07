//! Бенчмарки конвейера компиляции - фича.
//!
//! ## Что меряется и почему именно это
//!
//! Меряются **функции**, а не время процесса: правки идут в разборе, обходах и
//! печатниках, а запуск `taktc` добавляет к ним старт процесса и ввод-вывод, на
//! фоне которых полезный сигнал теряется.
//!
//! Вход - **синтетика известного размера** плюс пример корпуса. Синтетика даёт
//! сигнал (видно, как время зависит от N), корпус - узнаваемость: он же гоняется
//! проверками.
//!
//! **Быстро != верно.** Бенч ловит замедление, а не ошибку; тот же класс
//! оговорки, что у покрытия и у проверок целевых языков.

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use takt_lang::semantic::index::SemanticIndex;
use takt_lang::semantic::tree::construct_model;
use takt_lang::semantic::usages::collect_usages;

/// Цепочка из `states` состояний: `S0 -> S1 -> ... -> S{n-1}`.
///
/// Линейная модель выбрана намеренно: у неё известен размер и предсказуема
/// сложность обходов, поэтому смена класса (линия -> квадрат) становится видна
/// сразу. Именно этот класс дефекта и лечит замер на устоявшейся системе.
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

/// Пример корпуса - "реальная" точка отсчёта.
fn corpus_example() -> String {
    std::fs::read_to_string("../examples/stacker.takt").expect("пример корпуса stacker.takt")
}

fn bench_parse(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse");
    for states in [200usize, 2000] {
        let src = chain_model(states);
        group.bench_with_input(BenchmarkId::new("chain", states), &src, |b, src| {
            b.iter(|| takt_lang::parse(black_box(src), 0).expect("разбор"));
        });
    }
    let corpus = corpus_example();
    group.bench_function("corpus/stacker", |b| {
        b.iter(|| takt_lang::parse(black_box(&corpus), 0).expect("разбор"));
    });
    group.finish();
}

fn bench_semantics(c: &mut Criterion) {
    let mut group = c.benchmark_group("semantics");
    for states in [200usize, 2000] {
        let src = chain_model(states);
        let (ast, _) = takt_lang::parse(&src, 0).expect("разбор");
        group.bench_with_input(BenchmarkId::new("chain", states), &ast, |b, ast| {
            b.iter(|| construct_model(black_box(ast), None, &[]).expect("семантика"));
        });
    }
    let corpus = corpus_example();
    let (ast, _) = takt_lang::parse(&corpus, 0).expect("разбор");
    group.bench_function("corpus/stacker", |b| {
        b.iter(|| construct_model(black_box(&ast), None, &[]).expect("семантика"));
    });
    group.finish();
}

fn bench_full_pipeline(c: &mut Criterion) {
    let dir = std::env::temp_dir().join("takt_bench_c");
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    let out = dir.to_str().expect("путь в UTF-8").to_string();

    let mut group = c.benchmark_group("compile_to_c");
    // Полный конвейер: разбор -> семантика -> генерация. Пишет на диск, поэтому
    // сравнивать эти числа с `parse`/`semantics` напрямую нельзя - только с
    // самими собой между прогонами.
    let corpus = corpus_example();
    group.bench_function("corpus/stacker", |b| {
        b.iter(|| {
            takt_lang::compile_to_c(
                "Bench",
                black_box(&corpus),
                &out,
                &[],
                &takt_lang::GenerateOptions::default(),
            )
            .expect("генерация C");
        });
    });
    group.finish();
}

fn bench_lsp_layers(c: &mut Criterion) {
    let mut group = c.benchmark_group("lsp_layers");
    for states in [200usize, 2000] {
        let src = chain_model(states);
        let (ast, _) = takt_lang::parse(&src, 0).expect("разбор");
        let model = construct_model(&ast, None, &[]).expect("семантика");

        // Индекс LSP: строится на каждый переход к декларации.
        group.bench_with_input(BenchmarkId::new("index/chain", states), &model, |b, m| {
            b.iter(|| SemanticIndex::build(black_box(m)));
        });
        // Слой использований: строится на каждый `references`/`rename`.
        group.bench_with_input(BenchmarkId::new("usages/chain", states), &ast, |b, ast| {
            b.iter(|| collect_usages(black_box(ast)));
        });
    }
    group.finish();
}

fn bench_format(c: &mut Criterion) {
    let mut group = c.benchmark_group("format");
    // Форматтер гоняется по всему корпусу на каждом предкоммите и в редакторе на
    // каждое сохранение - его стоимость видна пользователю напрямую.
    let corpus = corpus_example();
    group.bench_function("corpus/stacker", |b| {
        b.iter(|| takt_lang::format::format_source(black_box(&corpus)).expect("форматирование"));
    });
    group.finish();
}

criterion_group!(
    benches,
    bench_parse,
    bench_semantics,
    bench_full_pipeline,
    bench_lsp_layers,
    bench_format
);
criterion_main!(benches);
