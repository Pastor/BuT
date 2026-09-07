//! Предикат над упакованным бит-вектором.
//!
//! # Что проверяется
//!
//! - предикат над `[bit; N<=64]` проверяется наравне с целым той же ширины;
//! - домен берётся по **объявленной** ширине `N`, а не по машинной: `[bit; 3]`
//!   даёт восемь значений, и `mask < 8` при нём **держится**, тогда как у `u8`
//!   тот же предикат нарушается. Это и есть мутационный тест ширины -
//!   проверка на потолок задачи была бы хрупкой (домен `[bit; 9]` лежит у самой
//!   границы `EDGE_LIMIT`), а вердикт от потолка не зависит;
//! - контроль: `[bit; 96]` - массив слов, и он остаётся вне охвата.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;
use takt_lang::verification::verify::{Verdict, verify_model};

/// Вердикт свойства у модели `Probe`.
///
/// Спрашивается именно она, а не корень: область формулы - по месту объявления, а
/// `cond` объявлен внутри `Probe`.
fn verdict(src: &str, phi_src: &str) -> Verdict {
    let (ast, _) = parse(src, 0).expect("разбор модели");
    let root = construct_model(&ast, None, &[]).expect("построение дерева");
    let phi = takt_lang::parse_ltl_property(phi_src).expect("разбор формулы");
    let probe = root
        .borrow()
        .models
        .get("Probe")
        .cloned()
        .expect("модель Probe");
    let m = probe.borrow();
    verify_model(&m, &phi)
}

/// Модель с переменной типа `ty` и предикатом `pred` над ней.
fn source(ty: &str, init: &str, pred: &str) -> String {
    format!(
        "model Probe {{\n\
         \x20   var mask: {ty} := {init};\n\
         \x20   var ticks: u8 := 0;\n\
         \x20   out ticks_out: u8 at 0x300;\n\
         \x20   cond Ready = {pred};\n\
         \x20   start Cycle {{\n\
         \x20       always {{ ticks := ticks + 1; ticks_out := ticks; }}\n\
         \x20       ref Cycle: ticks < 200;\n\
         \x20   }}\n\
         }}\n\
         start Main = Probe;\n"
    )
}

/// Предикат над бит-вектором проверяется - как над целым той же ширины.
///
/// Тавтология `G (!Ready | Ready)` взята намеренно: она держится при любом домене,
/// поэтому тест говорит ровно об одном - предикат в охвате.
#[test]
fn packed_bit_vector_predicate_is_verified() {
    for ty in ["[bit; 8]", "[bit; 3]", "[bit; 1]"] {
        let verdict = verdict(&source(ty, "0", "mask > 0"), "G (!Ready | Ready)");
        assert!(
            matches!(verdict, Verdict::Holds),
            "{ty}: предикат обязан быть в охвате, получено {verdict:?}"
        );
    }
}

/// Вердикт содержателен: `G Ready` над тем же входом нарушается.
///
/// Без этой проверки "держится" ничего не доказывало бы: вердикт `Holds` пришёл бы и от
/// пустого перебора.
#[test]
fn bit_vector_predicate_can_be_violated() {
    let verdict = verdict(&source("[bit; 8]", "0", "mask > 0"), "G Ready");
    assert!(
        matches!(verdict, Verdict::Violated { .. }),
        "начальное значение 0 обязано давать контрпример, получено {verdict:?}"
    );
}

/// Домен - по объявленной ширине: у `[bit; 3]` значений восемь.
///
/// Мутационный тест: возьми носитель машинную ширину (`[bit; 3]` есть `u8`), и
/// `mask < 8` перестало бы держаться - ровно как у контрольного `u8` ниже. Ширина здесь
/// контракт, а не деталь хранения.
#[test]
fn domain_follows_declared_width() {
    let narrow = verdict(&source("[bit; 3]", "0", "mask < 8"), "G Ready");
    assert!(
        matches!(narrow, Verdict::Holds),
        "у `[bit; 3]` значений 0..7, и `mask < 8` истинно всегда, получено {narrow:?}"
    );

    let machine = verdict(&source("u8", "0", "mask < 8"), "G Ready");
    assert!(
        matches!(machine, Verdict::Violated { .. }),
        "контроль: у `u8` значения доходят до 255, получено {machine:?}"
    );
}

/// **Контроль:** бит-вектор шире слова - массив слов, и он вне охвата.
#[test]
fn wide_bit_vector_stays_outside_subset() {
    let verdict = verdict(&source("[bit; 96]", "0", "mask > 0"), "G (!Ready | Ready)");
    assert!(
        matches!(verdict, Verdict::Unsupported { .. }),
        "`[bit; 96]` — массив слов, перебирать там нечего, получено {verdict:?}"
    );
}
