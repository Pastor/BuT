//! Свёртка инициализатора объявления в литерал.
//!
//! ## Что доказывается
//!
//! До фичи одно объявление давало **пять разных** результатов (замер 2026-08-02, `var
//! a: u8 := 1 + 2;`): эталон `0`, цели `c`/`rust` - `3`, цель `st` теряла инициализатор
//! молча, цель `sv` отказывала `SV-002`. Теперь выражения за границей семантики не
//! существует - в дереве стоит литерал, и расходиться потребителям не по чему.
//!
//! Тесты смотрят на **вывод целей**, а не на факт компиляции: молча неверная трансляция
//! компилируется тоже. Значенческую сверку с эталоном ведёт `takt-sim`
//! (`conformance_*`), здесь - текст вывода.

use std::sync::atomic::{AtomicUsize, Ordering};

use takt_lang::generator::GenerateOptions;

/// Порождает код цели во временном каталоге и возвращает его текст.
fn compile(target: &str, src: &str) -> String {
    // Каталог уникален по вызову, а не по (цель, процесс): тесты идут параллельно, и
    // два теста с одной целью делили бы каталог - первый удалял бы вывод второго.
    // Поймал именно параллельный прогон предкоммита: поодиночке тест был зелёным.
    static SEQ: AtomicUsize = AtomicUsize::new(0);
    let dir = std::env::temp_dir().join(format!(
        "takt_const_fold_{}_{}_{}",
        target.replace('-', "_"),
        std::process::id(),
        SEQ.fetch_add(1, Ordering::Relaxed)
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let out = dir.to_str().expect("путь в UTF-8");
    let opts = GenerateOptions::default();
    let result = match target {
        "c" => takt_lang::compile_to_c("fold.takt", src, out, &[], &opts),
        "rust" => takt_lang::compile_to_rust("fold.takt", src, out, &[], &opts),
        "st" => takt_lang::compile_to_st("fold.takt", src, out, &[], &opts),
        other => panic!("цель '{other}' в этом тесте не предусмотрена"),
    };
    result.unwrap_or_else(|e| panic!("цель '{target}' отказала: {e:?}"));
    let text = std::fs::read_dir(&dir)
        .expect("чтение каталога")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file())
        .map(|e| std::fs::read_to_string(e.path()).unwrap_or_default())
        .collect::<Vec<_>>()
        .join("\n");
    let _ = std::fs::remove_dir_all(&dir);
    text
}

/// Модель с одним объявлением-предметом; `sink` нужен, чтобы значение использовалось
/// (неиспользуемое объявление цели не эмитят).
fn model(decls: &str) -> String {
    format!(
        "model M {{\n    out lamp: u8 at 0x600;\n{decls}\n    \
         start S {{ always {{ lamp := probe; }} }}\n}}\nstart Main = M;\n"
    )
}

/// Арифметика в инициализаторе вычисляется - и одинаково у всех целей.
#[test]
fn arithmetic_initializer_is_folded_for_every_target() {
    let src = model("    var probe: u8 := 1 + 2;");
    for target in ["c", "rust", "st"] {
        let out = compile(target, &src);
        assert!(
            out.contains('3'),
            "цель '{target}': ожидалось свёрнутое значение 3\n{out}"
        );
        assert!(
            !out.contains("1 + 2"),
            "цель '{target}': выражение доехало до вывода — свёртки не было\n{out}"
        );
    }
}

/// Цель `st` больше не теряет инициализатор молча.
///
/// Это отдельная проверка, а не частный случай предыдущей: до фичи `st` печатала
/// объявление **без** значения, то есть расходилась с эталоном беззвучно - ни
/// диагностики, ни отказа `iec2c`.
#[test]
fn st_no_longer_drops_the_initializer() {
    let out = compile("st", &model("    var probe: u8 := 1 + 2;"));
    assert!(
        out.contains("probe : USINT := 3;"),
        "цель st обязана объявить probe со значением:\n{out}"
    );
}

/// Имя переменной в инициализаторе - её **начальное значение**: `var a := 5; var b := a
/// + 1;` -> `b = 6`.
#[test]
fn initializer_may_reference_a_variable_declared_above() {
    let src = model("    var base: u8 := 5;\n    var probe: u8 := base + 1;");
    for target in ["c", "rust", "st"] {
        let out = compile(target, &src);
        assert!(
            out.contains('6'),
            "цель '{target}': ссылка на объявленную выше переменную обязана \
             вычисляться\n{out}"
        );
    }
}

/// Свёртка идёт **после** вывода типов, и тип не искажается.
///
/// Тест против регресса, который уже случался при разработке: свёртка до вывода типов
/// делала `probe` типом `bool` вместо `bit` - значение верное, тип нет. В цели `st` это
/// видно прямо: `bit` и `bool` печатаются как `BOOL`, а вот в цели `c` типы разные.
#[test]
fn folding_runs_after_type_inference() {
    let src = model("    var flag: bit := false;\n    var probe := flag;");
    let out = compile("c", &src);
    assert!(
        out.contains("uint8_t probe"),
        "тип probe обязан выводиться из flag (bit → uint8_t), а не из литерала\n{out}"
    );
}
