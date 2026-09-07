//! `textDocument/documentSymbol`.
//!
//! # Что проверяется
//!
//! Три свойства ответа, и каждое ломается по-своему:
//!
//! 1. **состав** - какие объявления вообще становятся символами (и какие
//!    намеренно не становятся: `import`, `address`, `clock`, формулы);
//! 2. **вложенность** - модель -> её элементы, состояние -> именованные блоки,
//!    перечисление -> варианты, структура -> поля. Плоский список вместо дерева
//!    компилируется и выглядит "работающим", но панель структуры бесполезна;
//! 3. **диапазоны** - `selection_range` обязан покрывать **имя** (по нему
//!    редактор переходит из панели), а `range` - всё объявление и содержать
//!    `selection_range` внутри себя. Перепутанные диапазоны уводят курсор не
//!    туда, и никакой тест на "состав" этого не заметит.
//!
//! Ожидания сняты **зондом** с фактического вывода, а не выведены из чтения кода:
//! позиции, номера строк и виды символов угадывать нельзя.
//!
//! Тесты LSP живут под `#[cfg(feature = "lsp")]`: обычная `cargo test` их не видит.
//! Гоняет `cargo test --all-features` в `precheck.sh`.

#[cfg(feature = "lsp")]
mod symbols {
    use lsp_types::{DocumentSymbol, Position, Range, SymbolKind};
    use takt_lang::lsp::document_symbols;

    /// Файл со **всеми** видами объявлений верхнего уровня, какие модуль умеет
    /// показывать, плюс те, что он намеренно пропускает (`import`).
    const SRC: &str = r#"import { Thermostat } from "t.takt";
type Speed = u8;
enum Mode { Idle, Run }
struct Point { x: u8, y: u8 }
const LIMIT: u8 := 7;
out flag: bit;
var speed: Speed := 0;
cond Fast = speed > 5;
invariant Safe = speed < 100;
fn bump(x: u8) -> u8 { return x + 1; }
always { speed := speed; }
model Inner {
    var t: u8 := 0;
    start S;
}
start Idle {
    enter { speed := 0; }
    always { speed := bump(speed); }
    invariant Sane = speed < 50;
    ref Done: Fast;
}
state Done;
"#;

    /// Символ верхнего уровня по имени.
    fn top<'a>(syms: &'a [DocumentSymbol], name: &str) -> &'a DocumentSymbol {
        syms.iter()
            .find(|s| s.name == name)
            .unwrap_or_else(|| panic!("нет символа '{name}'; есть: {:?}", names(syms)))
    }

    fn names(syms: &[DocumentSymbol]) -> Vec<&str> {
        syms.iter().map(|s| s.name.as_str()).collect()
    }

    fn children(sym: &DocumentSymbol) -> &[DocumentSymbol] {
        sym.children.as_deref().unwrap_or(&[])
    }

    /// Текст, покрытый диапазоном (в координатах строк/столбцов).
    fn slice(source: &str, range: Range) -> String {
        let line_of = |p: Position| {
            source
                .lines()
                .nth(p.line as usize)
                .unwrap_or_else(|| panic!("нет строки {}", p.line))
        };
        if range.start.line == range.end.line {
            let line: Vec<char> = line_of(range.start).chars().collect();
            return line[range.start.character as usize..range.end.character as usize]
                .iter()
                .collect();
        }
        // Многострочный диапазон: для проверок ниже достаточно первой строки.
        let line: Vec<char> = line_of(range.start).chars().collect();
        line[range.start.character as usize..].iter().collect()
    }

    fn contains(outer: Range, inner: Range) -> bool {
        (outer.start.line, outer.start.character) <= (inner.start.line, inner.start.character)
            && (inner.end.line, inner.end.character) <= (outer.end.line, outer.end.character)
    }

    // -- Состав ---------------------------------------------------------------

    /// Каждое именованное объявление верхнего уровня становится символом - и получает
    /// **свой** вид.
    ///
    /// Вид (`SymbolKind`) - не косметика: по нему редактор рисует значок и группирует
    /// панель. Перепутанные виды дают "работающий" список, в котором порт неотличим от
    /// переменной.
    #[test]
    fn every_top_level_declaration_becomes_a_symbol() {
        let syms = document_symbols(SRC);
        let expected: &[(&str, SymbolKind)] = &[
            ("Speed", SymbolKind::TYPE_PARAMETER),
            ("Mode", SymbolKind::ENUM),
            ("Point", SymbolKind::STRUCT),
            ("LIMIT", SymbolKind::CONSTANT),
            ("flag", SymbolKind::PROPERTY),
            ("speed", SymbolKind::VARIABLE),
            ("Fast", SymbolKind::CONSTANT),
            ("Safe", SymbolKind::CONSTANT),
            ("bump", SymbolKind::FUNCTION),
            ("always", SymbolKind::EVENT),
            ("Inner", SymbolKind::MODULE),
            ("Idle", SymbolKind::CLASS),
            ("Done", SymbolKind::CLASS),
        ];
        for (name, kind) in expected {
            let sym = top(&syms, name);
            assert_eq!(
                sym.kind, *kind,
                "символ '{name}': вид определяет значок и группировку в панели \
                 структуры — перепутанный вид даёт «работающий» бесполезный список"
            );
        }
        assert_eq!(
            syms.len(),
            expected.len(),
            "состав символов верхнего уровня изменился: {:?}",
            names(&syms)
        );
    }

    /// `import` символом **не** становится - и это замысел.
    ///
    /// Он не объявляет имени в этом файле: показать его в панели структуры значило бы
    /// предложить переход к объявлению, которого здесь нет.
    #[test]
    fn import_is_not_a_symbol() {
        let syms = document_symbols(SRC);
        assert!(
            !names(&syms).contains(&"Thermostat"),
            "импортированное имя не объявлено в этом файле и символом быть не \
             должно: {:?}",
            names(&syms)
        );
    }

    // -- Вложенность ----------------------------------------------------------

    /// Под-модель отдаёт свои элементы **детьми**, а не в общий плоский список.
    #[test]
    fn nested_model_owns_its_elements() {
        let syms = document_symbols(SRC);
        let inner = top(&syms, "Inner");
        assert_eq!(
            names(children(inner)),
            vec!["t", "S"],
            "элементы под-модели обязаны быть её детьми"
        );
        assert!(
            !names(&syms).contains(&"t"),
            "элемент под-модели не должен дублироваться на верхнем уровне: {:?}",
            names(&syms)
        );
    }

    /// Состояние отдаёт детьми свои именованные блоки и инварианты.
    ///
    /// Порядок - как в исходнике: панель повторяет текст, а не сортирует его. `Sane`
    /// здесь появился (прежде инвариант состояния символом не становился); блоки при
    /// этом остались событиями.
    #[test]
    fn state_owns_its_named_blocks() {
        let syms = document_symbols(SRC);
        let idle = top(&syms, "Idle");
        assert_eq!(
            names(children(idle)),
            vec!["enter", "always", "Sane"],
            "именованные блоки и инварианты состояния обязаны быть его детьми"
        );
        for block in children(idle).iter().filter(|c| c.name != "Sane") {
            assert_eq!(block.kind, SymbolKind::EVENT, "блок — событие");
        }
    }

    /// Перечисление отдаёт детьми свои варианты, структура - свои поля.
    #[test]
    fn enum_and_struct_own_their_members() {
        let syms = document_symbols(SRC);
        assert_eq!(names(children(top(&syms, "Mode"))), vec!["Idle", "Run"]);
        assert_eq!(names(children(top(&syms, "Point"))), vec!["x", "y"]);
        assert!(
            children(top(&syms, "Mode"))
                .iter()
                .all(|v| v.kind == SymbolKind::ENUM_MEMBER)
        );
        assert!(
            children(top(&syms, "Point"))
                .iter()
                .all(|f| f.kind == SymbolKind::FIELD)
        );
    }

    /// Состояние `Idle` и вариант `Mode::Idle` - **разные** символы.
    ///
    /// Одноимённость законна (разные пространства), и плоский поиск по имени склеил бы
    /// их. Проверяется, что вариант лежит внутри перечисления, а состояние - на верхнем
    /// уровне.
    #[test]
    fn same_name_in_different_scopes_stays_separate() {
        let syms = document_symbols(SRC);
        assert_eq!(top(&syms, "Idle").kind, SymbolKind::CLASS);
        let variant = children(top(&syms, "Mode"))
            .iter()
            .find(|v| v.name == "Idle")
            .expect("вариант Mode::Idle");
        assert_eq!(variant.kind, SymbolKind::ENUM_MEMBER);
        assert_ne!(
            top(&syms, "Idle").range,
            variant.range,
            "одноимённые символы из разных областей обязаны различаться диапазоном"
        );
    }

    // -- Диапазоны ------------------------------------------------------------

    /// `selection_range` покрывает **имя**, `range` - всё объявление и содержит
    /// `selection_range`.
    ///
    /// По `selection_range` редактор переходит из панели структуры: сдвиг уводит курсор
    /// не туда, и ни один тест на состав этого не заметит.
    #[test]
    fn selection_range_covers_the_name() {
        let syms = document_symbols(SRC);
        for name in [
            "Speed", "Mode", "Point", "LIMIT", "flag", "speed", "Fast", "Safe", "bump", "Inner",
            "Idle", "Done",
        ] {
            let sym = top(&syms, name);
            assert_eq!(
                slice(SRC, sym.selection_range),
                name,
                "'{name}': selection_range обязан покрывать имя — по нему \
                 редактор переходит из панели структуры"
            );
            assert!(
                contains(sym.range, sym.selection_range),
                "'{name}': range обязан содержать selection_range"
            );
        }
    }

    /// Тот же контракт - у детей.
    #[test]
    fn selection_range_covers_the_name_for_children() {
        let syms = document_symbols(SRC);
        for (parent, child) in [
            ("Point", "x"),
            ("Point", "y"),
            ("Inner", "t"),
            ("Inner", "S"),
        ] {
            let sym = children(top(&syms, parent))
                .iter()
                .find(|c| c.name == child)
                .unwrap_or_else(|| panic!("нет '{parent}::{child}'"));
            assert_eq!(
                slice(SRC, sym.selection_range),
                child,
                "'{parent}::{child}': selection_range обязан покрывать имя"
            );
            assert!(contains(sym.range, sym.selection_range));
        }
    }

    /// `range` объявления начинается с его ключевого слова.
    ///
    /// Пиннинг границы: `range` - всё объявление, а не только имя.
    #[test]
    fn range_starts_at_the_declaration_keyword() {
        let syms = document_symbols(SRC);
        for (name, head) in [
            ("Speed", "type"),
            ("Mode", "enum"),
            ("Point", "struct"),
            ("LIMIT", "const"),
            ("flag", "out"),
            ("speed", "var"),
            ("Fast", "cond"),
            ("Safe", "invariant"),
            ("bump", "fn"),
            ("Inner", "model"),
            ("Idle", "start"),
            ("Done", "state"),
        ] {
            let text = slice(SRC, top(&syms, name).range);
            assert!(
                text.starts_with(head),
                "'{name}': range обязан начинаться с '{head}', а начинается с {text:?}"
            );
        }
    }

    // -- Границы и отказы -----------------------------------------------------

    /// **Контрпример:** неразбираемый файл даёт пустой список, а не панику.
    ///
    /// Сервер языка получает файл в каждом промежуточном состоянии набора - то есть
    /// чаще неразбираемый, чем разбираемый. Паника здесь роняет сервер у пользователя
    /// под руками.
    #[test]
    fn broken_source_yields_empty_list_not_panic() {
        for src in [
            "",
            "model {{{",
            "struct { x: u8 }",
            "model M { start",
            "enum { A, B }",
            "фыва",
        ] {
            let syms = document_symbols(src);
            assert!(
                syms.is_empty(),
                "неразбираемый вход {src:?} обязан давать пустой список: {:?}",
                names(&syms)
            );
        }
    }

    /// Пустая, но валидная модель даёт пустой список без ошибок.
    #[test]
    fn empty_model_yields_no_symbols() {
        assert!(document_symbols("").is_empty());
    }

    /// Инвариант виден в панели независимо от места объявления.
    ///
    /// До неё `invariant` уровня модели становился символом, а такой же внутри
    /// состояния - нет: дети состояния собирались только из именованных блоков. Одна
    /// конструкция языка была видна или невидима в зависимости от места. Асимметрию
    /// пришпиливал тест-заглушка 0147, прямо требовавший снять его при осознанной смене
    /// поведения, - это она и есть.
    ///
    /// Вид символа у обоих один (`CONSTANT`): разные виды показали бы одну конструкцию
    /// двумя разными.
    #[test]
    fn invariant_is_a_symbol_at_both_levels() {
        let syms = document_symbols(SRC);
        let sane = children(top(&syms, "Idle"))
            .iter()
            .find(|c| c.name == "Sane")
            .unwrap_or_else(|| {
                panic!(
                    "инвариант состояния обязан быть символом; дети Idle: {:?}",
                    names(children(top(&syms, "Idle")))
                )
            });
        assert_eq!(sane.kind, SymbolKind::CONSTANT);
        assert_eq!(
            top(&syms, "Safe").kind,
            SymbolKind::CONSTANT,
            "инвариант МОДЕЛИ остаётся символом того же вида — симметрия, а не перестановка"
        );
    }

    /// Диапазоны нового символа подчиняются общему контракту.
    ///
    /// Проверяется отдельно от `selection_range_covers_the_name_for_children`: тот
    /// перечисляет пары "родитель - ребёнок" списком, и запись, забытая в списке,
    /// промолчала бы.
    #[test]
    fn state_invariant_ranges_follow_the_contract() {
        let syms = document_symbols(SRC);
        let sane = children(top(&syms, "Idle"))
            .iter()
            .find(|c| c.name == "Sane")
            .expect("нет 'Idle::Sane'");
        assert_eq!(
            slice(SRC, sane.selection_range),
            "Sane",
            "selection_range обязан покрывать имя — по нему редактор переходит из панели"
        );
        assert!(
            slice(SRC, sane.range).starts_with("invariant"),
            "range — всё объявление, начиная с ключевого слова: {:?}",
            slice(SRC, sane.range)
        );
        assert!(contains(sane.range, sane.selection_range));
    }

    /// Состав панели больше ничем не изменился.
    ///
    /// `every` и встроенные формулы имени не объявляют, `ref`/`next` -
    /// **ссылки** на чужое имя; символ здесь врал бы о месте объявления. Это
    /// решение (Option C отвергнут), а не забывчивость, - потому оно и
    /// закреплено тестом.
    #[test]
    fn nameless_and_referencing_state_elements_are_not_symbols() {
        const SRC_STATE: &str = r#"var speed: u8 := 0;
start Idle {
    enter { speed := 0; }
    every 100ms { speed := speed + 1; }
    invariant Sane = speed < 50;
    : [Guard] speed < 100;
    ref Done: speed > 10;
}
state Done;
"#;
        let syms = document_symbols(SRC_STATE);
        let kids = names(children(top(&syms, "Idle")));
        assert_eq!(
            kids,
            vec!["enter", "Sane"],
            "детьми состояния становятся только объявления с именем"
        );
    }

    /// Возможность объявлена клиенту - иначе редактор её не вызовет.
    #[test]
    fn capability_is_advertised() {
        let caps = takt_lang::lsp::server_capabilities();
        assert!(
            caps.document_symbol_provider.is_some(),
            "без объявления в ServerCapabilities реализация мертва: редактор \
             просто не пришлёт запрос"
        );
    }
}
