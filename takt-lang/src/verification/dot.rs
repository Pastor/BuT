//! Экспорт графов верификации в формат **Graphviz DOT**.
//!
//! Структуры Крипке (`kripke.rs`), автомат Бюхи (`buchi.rs`) и их произведение
//! (`product.rs`) - те же, что использует проверка, - печатаются графом переходов для
//! рендера `dot -Tsvg` и встраивания в документ (`book/`). Диаграмма = вывод движка:
//! правка верификации отражается в картинке.
//!
//! Соглашения нотации:
//! - **старт** - точка-источник (`shape=point`) со стрелкой в начальную вершину;
//! - **принимающее** состояние автомата/произведения - двойной кружок
//!   (`shape=doublecircle`);
//! - идентификатор узла - **индекс** вершины (в структуре Крипке по данным одно
//!   имя состояния делят несколько вершин - уникально только смещение), метка -
//!   человекочитаемая (имя состояния, для пути данных плюс набор истинных атомов).
//!
//! Порядок вывода детерминирован (обход по возрастанию индекса, множества -
//! `BTreeSet`), поэтому DOT воспроизводим побайтно (согласуется с проверкой 0048).

use crate::semantic::ModelNode;
use crate::verification::buchi::BuchiAutomaton;
use crate::verification::kripke::Kripke;
use crate::verification::ltl::Ltl;
use crate::verification::product::Product;
use crate::verification::verify::{self, Verdict};

/// Экранирует метку узла для DOT (кавычки и обратный слэш).
fn escape(label: &str) -> String {
    label.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Шрифт всех подписей графа - **ГОСТ тип А, наклонный** (чертёжный, ГОСТ 2.304-81).
///
/// Значение - имя семейства свободного шрифта `GOST2.304-81TypeA-Slanted.ttf` (проект
/// Metrolog/Font.GOST2.304-81, стиль Italic/Наклонный - стандартное чертёжное
/// начертание). Ставит его `book/Makefile` (цель `fonts`) - **только наклонный**
/// вариант, поэтому семейство `GOST 2.304-81` резолвится именно в него. Дефис в имени
/// экранировать не нужно: Graphviz задаёт семейство напрямую (в отличие от `fc-match`,
/// чей парсер шаблонов принял бы `-81` за размер).
///
/// Имя используется на **генерации** SVG (Graphviz - метрики раскладки; скрипт
/// `book/scripts/svg_flatten_text.py` - чтение контуров глифов). Итоговый SVG раздела
/// **самодостаточен**: подписи переведены в векторные `<path>`, поэтому при просмотре и
/// сборке PDF шрифт уже не нужен.
const GRAPH_FONT: &str = "GOST 2.304-81";

/// Заголовок орграфа с единым стилем:
/// - **все вершины одного радиуса** - `fixedsize=true` + исправлениеированные
///   `width`/`height`; метка вписывается подбором `fontsize`, не растягивая круг;
/// - **подписи - шрифтом ГОСТ тип А** ([`GRAPH_FONT`]).
///
/// Служебные точки-источники (`shape=point`) переопределяют `width` у себя, так что на
/// равенство радиусов состояний-кругов не влияют.
fn graph_header(name: &str) -> String {
    format!(
        "digraph {name} {{\n\
         \x20 rankdir=LR;\n\
         \x20 fontname=\"{GRAPH_FONT}\";\n\
         \x20 fontsize=10;\n\
         \x20 node [shape=circle, fixedsize=true, width=0.5, height=0.5, \
         fontsize=12, fontname=\"{GRAPH_FONT}\"];\n\
         \x20 edge [arrowsize=0.5, fontname=\"{GRAPH_FONT}\"];\n"
    )
}

/// Подстрочный номер (Unicode ₀-₉, U+2080...2089) для числа `n`. Глифы есть в шрифте
/// ГОСТ тип А, поэтому подпись флаттерится в контуры без потерь.
fn subscript(n: usize) -> String {
    n.to_string()
        .chars()
        .map(|c| char::from_u32(0x2080 + c.to_digit(10).unwrap()).unwrap())
        .collect()
}

/// Компактная подпись вершины: `S` с подстрочным номером (1-based, `S₁`, `S₂`, ...).
/// Расшифровка полного имени - в легенде графа (см.
fn node_label(i: usize) -> String {
    format!("S{}", subscript(i + 1))
}

/// Легенда графа (метка снизу, `labelloc=b`): по строке на вершину - `Sᵢ - <полная
/// подпись>`, левым краем (`\l`). `entries[i]` - исходная (полная) подпись `i`-й
/// вершины. Возвращает три строки атрибутов графа.
fn legend_label(entries: &[String]) -> String {
    let mut lines = String::new();
    for (i, full) in entries.iter().enumerate() {
        // `\l` - директива Graphviz (левое выравнивание), не экранируется; сам текст
        // подписи уже экранирован вызывающим.
        lines.push_str(&format!("{} — {}\\l", node_label(i), full));
    }
    format!("  label=\"{lines}\";\n  labelloc=\"b\";\n  labeljust=\"l\";\n")
}

/// DOT структуры Крипке.
///
/// Вершины - состояния FSM; тотальность (самопетля у вершины без безусловного выхода,
/// `may_stutter`) видна ребром-петлёй. Для пути данных (0068, `labels` непусто) метка
/// вершины несёт набор истинных в ней атомов.
pub fn kripke_to_dot(kripke: &Kripke) -> String {
    let mut out = graph_header("Kripke");
    // Точка-источник -> начальная вершина.
    out.push_str("  __start [shape=point, width=0.12];\n");
    out.push_str(&format!("  __start -> k{};\n", kripke.initial));

    // Полные подписи вершин (для легенды); в кругах - компактные Sᵢ.
    let entries: Vec<String> = kripke
        .states
        .iter()
        .enumerate()
        .map(|(k, name)| {
            if kripke.labels.is_empty() {
                escape(name)
            } else {
                // Путь данных: имя состояния + истинные атомы (кроме самого имени).
                let atoms: Vec<String> = kripke.labels[k]
                    .iter()
                    .filter(|a| a.as_str() != name)
                    .map(|a| escape(a))
                    .collect();
                if atoms.is_empty() {
                    escape(name)
                } else {
                    format!("{} {{{}}}", escape(name), atoms.join(", "))
                }
            }
        })
        .collect();

    for k in 0..kripke.states.len() {
        out.push_str(&format!("  k{k} [label=\"{}\"];\n", node_label(k)));
    }
    for (&from, tos) in &kripke.transitions {
        for &to in tos {
            out.push_str(&format!("  k{from} -> k{to};\n"));
        }
    }
    out.push_str(&legend_label(&entries));
    out.push_str("}\n");
    out
}

/// Полная подпись состояния автомата Бюхи для легенды - набор его литералов
/// (ограничений на текущую букву; `Atom`/`Not`, как в [`BuchiAutomaton::dump`]). Прочие
/// формулы состояния - темпоральные обязательства, а не разметка узла. Пустой набор ->
/// `{}` (нет ограничений на букву).
fn buchi_legend_entry(formulas: &std::collections::BTreeSet<std::rc::Rc<Ltl>>) -> String {
    let literals: Vec<String> = formulas
        .iter()
        .filter(|f| matches!(f.as_ref(), Ltl::Atom(_) | Ltl::Not(_)))
        .map(|f| escape(&f.to_string()))
        .collect();
    format!("{{{}}}", literals.join(", "))
}

/// DOT автомата Бюхи для `¬φ`.
///
/// Начальные состояния получают стрелку из точки-источника, **принимающие** - двойной
/// кружок. Язык автомата - нарушающие свойство прогоны. В кругах - компактные `Sᵢ`,
/// расшифровка (набор литералов) - в легенде.
pub fn buchi_to_dot(automaton: &BuchiAutomaton) -> String {
    let mut out = graph_header("Buchi");

    let entries: Vec<String> = automaton.states.iter().map(buchi_legend_entry).collect();

    for i in 0..automaton.states.len() {
        let shape = if automaton.accepting.contains(&i) {
            "doublecircle"
        } else {
            "circle"
        };
        out.push_str(&format!(
            "  s{i} [shape={shape}, label=\"{}\"];\n",
            node_label(i)
        ));
    }
    // Точки-источники начальных состояний.
    for &init in &automaton.initial_states {
        out.push_str(&format!("  __start{init} [shape=point, width=0.12];\n"));
        out.push_str(&format!("  __start{init} -> s{init};\n"));
    }
    for (&from, tos) in &automaton.transitions {
        for &to in tos {
            out.push_str(&format!("  s{from} -> s{to};\n"));
        }
    }
    out.push_str(&legend_label(&entries));
    out.push_str("}\n");
    out
}

/// DOT произведения `K x A_¬φ`.
///
/// Узел - пара `(состояние FSM, состояние автомата)`; **принимающие** пары - двойной
/// кружок, начальные - стрелка из точки-источника. Непустой цикл через принимающую пару
/// = контрпример (проверяется [`super::check::emptiness`]).
pub fn product_to_dot(product: &Product, kripke: &Kripke) -> String {
    let mut out = graph_header("Product");

    // Полные подписи пар `(состояние, qN)` - в легенду; в кругах - Sᵢ.
    let entries: Vec<String> = product
        .states
        .iter()
        .map(|&(k, q)| format!("{}, q{q}", escape(&kripke.states[k])))
        .collect();

    for s in 0..product.states.len() {
        let shape = if product.accepting.contains(&s) {
            "doublecircle"
        } else {
            "circle"
        };
        out.push_str(&format!(
            "  p{s} [shape={shape}, label=\"{}\"];\n",
            node_label(s)
        ));
    }
    for &init in &product.initial {
        out.push_str(&format!("  __start{init} [shape=point, width=0.12];\n"));
        out.push_str(&format!("  __start{init} -> p{init};\n"));
    }
    for (&from, tos) in &product.transitions {
        for &to in tos {
            out.push_str(&format!("  p{from} -> p{to};\n"));
        }
    }
    out.push_str(&legend_label(&entries));
    out.push_str("}\n");
    out
}

/// Какой граф верификации выгрузить в DOT. Тип общий для CLI (`taktc verify
/// --emit-graph`) и библиотеки, чтобы разбор значения и построение жили в одном месте,
/// а бинарник оставался тонким (лимит размера `taktc.rs`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphKind {
    /// Структура Крипке модели (без `--property` - управляющая абстракция 0049).
    Kripke,
    /// Автомат Бюхи для `¬φ` (требует свойства).
    Buchi,
    /// Произведение `K x A_¬φ` (требует свойства).
    Product,
}

impl GraphKind {
    /// Имя для сообщений/диагностики.
    pub fn name(self) -> &'static str {
        match self {
            GraphKind::Kripke => "kripke",
            GraphKind::Buchi => "buchi",
            GraphKind::Product => "product",
        }
    }
}

/// Разбирает значение флага `--emit-graph`.
///
/// Негодное значение - отказ (тот же принцип, что у `--scope`): иначе `--emit-graph
/// kripk` тихо ушёл бы в проверку.
pub fn parse_graph_kind(value: &str) -> Result<GraphKind, String> {
    match value {
        "kripke" => Ok(GraphKind::Kripke),
        "buchi" => Ok(GraphKind::Buchi),
        "product" => Ok(GraphKind::Product),
        other => Err(format!(
            "неизвестный граф '{other}'; допустимо: kripke (структура Крипке), \
             buchi (автомат ¬φ), product (произведение)"
        )),
    }
}

/// Диагностика вердикта-отказа при экспорте графа.
fn refusal_message(verdict: &Verdict) -> String {
    match verdict {
        Verdict::NoStartState => {
            "Экспорт графа невозможен: у модели нет стартового состояния.".to_string()
        }
        // Причина берётся у вердикта, а не пересказывается здесь: прежний текст
        // утверждал "не имя состояния и не отслеживаемый предикат" и на входе за
        // потолком был прямо ложным - атомы там как раз отслеживаемые.
        Verdict::Unsupported { atoms, reason } => format!(
            "Экспорт графа невозможен: атом(ы) {} — {}.",
            atoms.join(", "),
            reason.text()
        ),
        // Holds/Violated здесь не возникают: build_graphs не проверяет пустоту.
        _ => "Экспорт графа невозможен.".to_string(),
    }
}

/// Строит запрошенный граф верификации и возвращает его DOT.
///
/// `kripke` без свойства - управляющая структура Крипке; `buchi`/`product` требуют
/// свойства (строятся по `¬φ`). `Err` несёт готовое сообщение для пользователя (CLI
/// печатает его в stderr).
pub fn emit_graph_dot(
    model: &ModelNode,
    kind: GraphKind,
    property: Option<&str>,
) -> Result<String, String> {
    // Крипке без свойства - единственный граф, не требующий формулы.
    if kind == GraphKind::Kripke && property.is_none() {
        let kripke = verify::build_control_kripke(model).map_err(|v| refusal_message(&v))?;
        return Ok(kripke_to_dot(&kripke));
    }

    let Some(text) = property else {
        return Err(format!(
            "граф '{}' строится по свойству — задайте его флагом --property \"φ\".",
            kind.name()
        ));
    };
    let phi = crate::parse_ltl_property(text)
        .map_err(|d| format!("Ошибка разбора свойства: {}", d.message))?;
    let graphs = verify::build_graphs(model, &phi).map_err(|v| refusal_message(&v))?;
    Ok(match kind {
        GraphKind::Kripke => kripke_to_dot(&graphs.kripke),
        GraphKind::Buchi => buchi_to_dot(&graphs.automaton),
        GraphKind::Product => product_to_dot(&graphs.product, &graphs.kripke),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;
    use crate::verification::verify::{build_control_kripke, build_graphs};
    use crate::{parse, parse_ltl_property};

    /// Строит модель из исходника (для тестов эмиттеров).
    fn model(src: &str) -> std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>> {
        let (ast, _) = parse(src, 0).expect("разбор");
        construct_model(&ast, None, &[]).expect("семантика")
    }

    const DISPENSER: &str = "\
        var level: u8 := 0;\n\
        start Idle { next Filling; }\n\
        state Filling { always { level := level + 20; } ref Ready: level >= 100; }\n\
        state Ready { next Idle; }";

    #[test]
    fn kripke_dot_marks_start_and_all_states() {
        let m = model(DISPENSER);
        let k = build_control_kripke(&m.borrow()).unwrap();
        let dot = kripke_to_dot(&k);
        assert!(dot.starts_with("digraph Kripke {"), "заголовок: {dot}");
        // Точка-источник ведёт в начальную вершину.
        assert!(
            dot.contains(&format!("__start -> k{};\n", k.initial)),
            "старт: {dot}"
        );
        // В кругах - компактные Sᵢ; полные имена - в легенде графа.
        for i in 0..k.states.len() {
            assert!(
                dot.contains(&format!("k{i} [label=\"{}\"]", node_label(i))),
                "вершина k{i} = {}: {dot}",
                node_label(i)
            );
        }
        // Легенда (label графа) расшифровывает каждое имя состояния.
        assert!(dot.contains("labelloc=\"b\""), "легенда снизу: {dot}");
        for (i, name) in k.states.iter().enumerate() {
            assert!(
                dot.contains(&format!("{} — {name}", node_label(i))),
                "легенда {}: {name}: {dot}",
                node_label(i)
            );
        }
        assert!(dot.trim_end().ends_with('}'));
    }

    #[test]
    fn buchi_dot_accepting_is_doublecircle() {
        let m = model(DISPENSER);
        let phi = parse_ltl_property("F Filling").unwrap();
        let g = build_graphs(&m.borrow(), &phi).unwrap();
        let dot = buchi_to_dot(&g.automaton);
        assert!(dot.starts_with("digraph Buchi {"));
        // Хотя бы одно принимающее состояние - двойной кружок.
        assert!(
            !g.automaton.accepting.is_empty(),
            "автомат ¬(F Filling) принимает"
        );
        for &acc in &g.automaton.accepting {
            assert!(
                dot.contains(&format!("s{acc} [shape=doublecircle")),
                "принимающее s{acc}: {dot}"
            );
        }
        // Начальные состояния получают точку-источник.
        for &init in &g.automaton.initial_states {
            assert!(
                dot.contains(&format!("__start{init} -> s{init};")),
                "нач s{init}"
            );
        }
    }

    #[test]
    fn product_dot_labels_are_state_automaton_pairs() {
        let m = model(DISPENSER);
        let phi = parse_ltl_property("F Ready").unwrap();
        let g = build_graphs(&m.borrow(), &phi).unwrap();
        let dot = product_to_dot(&g.product, &g.kripke);
        assert!(dot.starts_with("digraph Product {"));
        // В кругах - Sᵢ; пара (имя состояния, qN) - в легенде.
        for (s, &(k, q)) in g.product.states.iter().enumerate() {
            let name = &g.kripke.states[k];
            assert!(
                dot.contains(&format!("p{s} [shape=")) && dot.contains(&format!("{name}, q{q}")),
                "пара p{s}=({name}, q{q}) в легенде: {dot}"
            );
        }
    }

    #[test]
    fn data_path_kripke_label_carries_atoms() {
        // Предикат над отслеживаемым bit-var -> метка несёт атом.
        let src = "\
            var flag: bit := 0;\n\
            cond On = flag = 1;\n\
            start A { always { flag := 1; } ref B: flag = 1; }\n\
            state B;";
        let m = model(src);
        let phi = parse_ltl_property("F On").unwrap();
        let g = build_graphs(&m.borrow(), &phi).unwrap();
        let dot = kripke_to_dot(&g.kripke);
        // Есть вершина в состоянии A, где предикат On истинен -> в легенде "A {On}".
        assert!(dot.contains("A {On}"), "подпись данных в легенде: {dot}");
    }

    #[test]
    fn legend_separator_not_escaped() {
        // Разделитель строк легенды - `\l` (одиночный слэш = левое выравнивание DOT), а
        // не `\\l` (был бы литеральный текст) - регресс экранирования.
        let src = "var flag: bit := 0;\n cond On = flag = 1;\n \
                   start A { always { flag := 1; } ref B: flag = 1; }\n state B;";
        let m = model(src);
        let phi = parse_ltl_property("F On").unwrap();
        let g = build_graphs(&m.borrow(), &phi).unwrap();
        let dot = kripke_to_dot(&g.kripke);
        assert!(dot.contains("\\l"), "легенда с `\\l`: {dot}");
        assert!(!dot.contains("\\\\l"), "двойное экранирование `\\l`: {dot}");
    }

    #[test]
    fn escape_quotes_in_label() {
        assert_eq!(escape("a\"b"), "a\\\"b");
        assert_eq!(escape("a\\b"), "a\\\\b");
    }
}
