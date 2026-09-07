//! Кросс-файловый переход к декларации.
//!
//! Вынесен из `lsp_tests.rs`: тот файл сверх лимита размера, и храповик
//! `scripts/check-module-size.sh` не дал ему вырасти. Ровно то, что советует сообщение
//! отказа: "вынесите новое в отдельный модуль".

/// Кросс-файловый переход к декларации.
///
/// Фикстуры - `tests/data/goto56/`, пара "угадывание сработало бы / не сработало бы".
/// Проверяется **поведение** по нажатию "перейти к декларации": какой файл откроется и
/// на каком месте, - а не наличие `file_no` в индексе: ровно из-за отсутствия
/// потребителя ветка угадывания и прожила непроверенной.
#[cfg(feature = "lsp")]
mod goto_exact_file {
    use lsp_types::Position;

    // -- 0056: кросс-файловый переход к декларации ----------------------------
    //
    // Фикстуры - `tests/data/goto56/`, пара "угадывание сработало бы / не сработало
    // бы". Проверяется поведение по нажатию "перейти к декларации": какой файл
    // откроется и на каком месте, - а не наличие `file_no` в индексе: ровно из-за
    // отсутствия потребителя ветка угадывания и прожила непроверенной.

    /// Каталог фикстур 0056.
    fn goto56_dir() -> String {
        "tests/data/goto56".to_string()
    }

    fn goto56_source(file: &str) -> String {
        std::fs::read_to_string(format!("tests/data/goto56/{file}"))
            .unwrap_or_else(|e| panic!("не прочитать фикстуру {file}: {e}"))
    }

    /// Позиция курсора на первом использовании `needle` **после** `= ` - то есть на
    /// имени модели в `start Main = X;`, а не на алиасе в строке `import`.
    fn cursor_on_use(source: &str, needle: &str) -> Position {
        let offset = source
            .find(&format!("= {needle}"))
            .unwrap_or_else(|| panic!("в фикстуре нет использования `= {needle}`"))
            + 2;
        let head = &source[..offset];
        let line = head.matches('\n').count() as u32;
        let col = head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32;
        Position::new(line, col)
    }

    /// T3 (тест зонда): узел под курсором - из **текущего** файла.
    ///
    /// Зонд: курсор на `Helper` возвращал переменную `speed` **из `helper.takt`** - её
    /// диапазон (19..37) там накрыл смещение курсора (35) здесь. Не "не тот файл", а
    /// **не тот узел**.
    ///
    /// Тест сначала доказывает, что **ловушка взведена**: в чужом файле есть узел, чей
    /// диапазон накрывает смещение курсора. Без этой проверки правка фикстуры
    /// (например, добавленный комментарий сдвинет смещения) молча разоружила бы
    /// теста, и он зеленел бы впустую.
    #[test]
    fn t3_node_under_cursor_belongs_to_current_file() {
        use takt_lang::diagnostics::{FileTable, Location};
        use takt_lang::semantic::index::SemanticIndex;
        use takt_lang::semantic::tree::construct_model_with_files;

        let source = goto56_source("uses_helper.takt");
        let offset = source.find("= Helper").expect("нет использования") + 2;

        let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
        let mut files = FileTable::new("uses_helper.takt");
        let model = construct_model_with_files(&ast, None, &[goto56_dir()], &mut files, false)
            .expect("семантика");
        let index = SemanticIndex::build(&model);

        // Шаг 1. Ловушка взведена? В чужом файле обязан быть узел, накрывающий смещение
        // курсора, - иначе путать нечего и тест зеленеет впустую.
        let foreign = index.node_at_offset_in_file(1, offset).expect(
            "ЛОВУШКА РАЗОРУЖЕНА: в helper.takt нет узла, накрывающего смещение курсора. \
             Верните фикстуры к виду, где смещения пересекаются",
        );
        assert!(
            matches!(foreign.loc, Location::Source(1, _, _)),
            "поиск по файлу 1 вернул узел не из файла 1 — ФИЛЬТР ПО ФАЙЛУ СЛОМАН: {:?}",
            foreign.loc
        );
        assert_eq!(
            foreign.name, "speed",
            "ЛОВУШКА РАЗОРУЖЕНА: ожидался узел `speed` из зонда 0056 (его диапазон \
             19..37 в helper.takt накрывает смещение курсора здесь)"
        );

        // Шаг 2. То, ради чего тест: под курсором - узел своего файла.
        let node = index.node_at_offset(offset).expect("узел под курсором");
        assert!(
            matches!(node.loc, Location::Source(0, _, _)),
            "узел под курсором обязан принадлежать корневому файлу: {:?}",
            node.loc
        );
        assert_eq!(
            node.name, "Helper",
            "под курсором обязана быть ссылка на модель текущего файла"
        );
    }

    /// T5/A1: переход открывает импортированный файл на объявлении модели.
    #[test]
    fn t5_goto_opens_imported_file() {
        use takt_lang::lsp::goto_declaration_at;

        let source = goto56_source("uses_helper.takt");
        let pos = cursor_on_use(&source, "Helper");
        let loc = goto_declaration_at("uses_helper.takt", &source, pos, &[goto56_dir()])
            .expect("переход на имени импортированной модели обязан находиться");

        assert!(
            loc.uri.ends_with("goto56/helper.takt"),
            "обязан открыться helper.takt, получено: {}",
            loc.uri
        );
        assert_eq!(
            loc.range.start.line, 0,
            "объявление модели — с первой строки"
        );
    }

    /// T6/A2: **контрпример угадыванию** - алиас.
    ///
    /// `import "engine.takt" as Motor;` связывает имя `Motor` с файлом `engine.takt`.
    /// Прежний код строил кандидатов из имени модели (`to_snake_case("Motor")` ->
    /// `motor.takt`) и не нашёл бы цель **никогда**.
    #[test]
    fn t6_goto_follows_alias_not_model_name() {
        use takt_lang::lsp::goto_declaration_at;

        let source = goto56_source("uses_alias.takt");
        let pos = cursor_on_use(&source, "Motor");
        let loc = goto_declaration_at("uses_alias.takt", &source, pos, &[goto56_dir()])
            .expect("переход по алиасу обязан находиться");

        assert!(
            loc.uri.ends_with("goto56/engine.takt"),
            "обязан открыться engine.takt (а не выдуманный motor.takt), получено: {}",
            loc.uri
        );
        assert!(
            !loc.uri.contains("motor"),
            "имя файла угадывалось из имени модели — этого больше быть не должно: {}",
            loc.uri
        );
    }

    /// T8/R4: диапазон в чужом файле считается по **его** тексту.
    ///
    /// Смещения чужого файла к своему тексту не относятся: наложи их на текущий
    /// документ - получишь мусорный диапазон (так и было до 0056).
    #[test]
    fn t8_range_in_foreign_file_uses_its_own_text() {
        use takt_lang::lsp::goto_declaration_at;

        let source = goto56_source("uses_alias.takt");
        let target = goto56_source("engine.takt");
        let pos = cursor_on_use(&source, "Motor");
        let loc = goto_declaration_at("uses_alias.takt", &source, pos, &[goto56_dir()])
            .expect("переход обязан находиться");

        // `model Engine` объявлена после шапки-комментария - на 4-й строке.
        let declaration_line = target
            .lines()
            .position(|l| l.starts_with("model Engine"))
            .expect("в engine.takt нет объявления модели") as u32;
        assert_eq!(
            loc.range.start.line, declaration_line,
            "диапазон обязан указывать на объявление в engine.takt (строка {}), \
             а не на случайное место: {:?}",
            declaration_line, loc.range
        );
    }

    /// T7/A4/R6: переход внутри своего файла не изменился - URI пуст.
    ///
    /// Пустой `uri` - контракт "это текущий файл": его подставляет вызывающий.
    #[test]
    fn t7_goto_within_own_file_keeps_contract() {
        use takt_lang::lsp::goto_declaration_at;

        let src = "var counter: [bit;8] := 0;\nstart S;";
        let loc = goto_declaration_at("main.takt", src, Position::new(0, 4), &[])
            .expect("переменная своего файла обязана находиться");
        assert!(
            loc.uri.is_empty(),
            "для своего файла URI подставляет вызывающий: {}",
            loc.uri
        );
        assert_eq!(loc.range.start.line, 0);
    }

    /// T9/A5: угадывание удалено. Каталог целиком - 0027 разделила `lsp.rs`, и проверка
    /// одного пути после переезда функции молча зеленела бы.
    #[test]
    fn t9_guessing_is_gone() {
        let src: String = std::fs::read_dir("src/lsp")
            .expect("нет каталога src/lsp")
            .filter_map(|e| std::fs::read_to_string(e.ok()?.path()).ok())
            .collect();
        assert!(!src.is_empty(), "каталог src/lsp пуст — проверять нечего");
        assert!(
            !src.contains("fn to_snake_case"),
            "`to_snake_case` (угадывание файла по имени модели) обязана быть \
             удалена: путь берётся из реестра файлов (0053)"
        );
    }

    /// T10/A6: сервер зовёт кросс-файловый вариант.
    ///
    /// Иначе проверять нечего: код без потребителя молча гниёт, а вся работа 0056
    /// осталась бы невидимой в редакторе.
    #[test]
    fn t10_server_calls_cross_file_goto() {
        let server = std::fs::read_to_string("src/bin/takt_lsp.rs")
            .expect("не прочитать src/bin/takt_lsp.rs");
        assert!(
            server.contains("goto_declaration_at("),
            "сервер обязан звать кросс-файловый вариант с путём документа"
        );
        assert!(
            !server.contains("lsp::goto_declaration(text"),
            "однофайловый вариант в обработчике declaration больше не место: \
             URI ответа был бы всегда текущим документом"
        );
    }
}

/// Переход на имя состояния в условии.
///
/// Продолжение 0056: там позицию использования получил `ConditionNode::Model` (`Ping`),
/// здесь - имя **состояния** в условии. До 0071 goto на нём возвращал `None` (узел
/// индексировался как рядовая `ReferenceCondition` без ссылки на декларацию либо не
/// индексировался вовсе).
///
/// **Два разных механизма** (выяснено пробой при разработке - предпосылка
/// была неполной):
/// - **кросс-модельный** `S(Ping) = Done` (headline): `Done` - состояние
///   модели-аргумента `Ping`, текущая модель `Pong` его не видит, резолвер
///   оставляет `ConditionNode::Unresolved(Variable)`. Разбор `S(Модель) =
///   Состояние` - на уровне `ConditionNode::Equal` (`try_collect_state_of_model`),
///   имя резолвится в области `Ping`.
/// - **внутримодельный** `x = Done`, где `Done` - состояние **той же** модели:
///   резолвер даёт `ConditionNode::State(Rc, use-site)`; его индексирует ветка
///   `ConditionNode::State`.
///
/// Зонд: строка декларации вычисляется из исходника, а не угадывается.
#[cfg(feature = "lsp")]
mod goto_state_in_condition {
    use lsp_types::Position;
    use takt_lang::lsp::goto_declaration;

    // `Done` - состояние-сестра (в Ping), приходит `Unresolved(Variable)` и резолвится
    // через `S(Ping)`; узел индексируется как `ReferenceState`.
    const SRC: &str = "model Ping {\n    start Run { ref Done: true; }\n    state Done;\n}\nmodel Pong {\n    start Go { ref Stop: S(Ping) = Done; }\n    state Stop;\n}\nstart Entry = (Ping | Pong);\n";

    /// Курсор на `needle` в первой позиции `hay` (все - ASCII).
    fn cursor_on(src: &str, hay: &str, needle: &str) -> Position {
        let anchor = src.find(hay).expect("нет фрагмента") + hay.find(needle).unwrap();
        let head = &src[..anchor];
        let line = head.matches('\n').count() as u32;
        let col = head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32;
        Position::new(line, col)
    }

    /// Строка объявления `state <name>;` в исходнике.
    fn decl_line(src: &str, decl: &str) -> u32 {
        src[..src.find(decl).unwrap()].matches('\n').count() as u32
    }

    /// T2: кросс-модельный `S(Ping) = Done`, курсор на `Done` -> декларация в Ping.
    #[test]
    fn goto_state_name_in_condition_resolves_to_declaration() {
        let range = goto_declaration(SRC, cursor_on(SRC, "S(Ping) = Done", "Done"));
        assert!(
            range.is_some(),
            "goto на имени состояния в условии должен вернуть декларацию (фича 0071)"
        );
        assert_eq!(
            range.unwrap().start.line,
            decl_line(SRC, "state Done;"),
            "переход должен открыть декларацию `state Done;`"
        );
    }

    /// T4 (без регресса 0056): курсор на `Ping` в `S(Ping)` - это ссылка на
    /// **модель** (`ReferenceModel`), переход ведёт к её объявлению, а не к
    /// состоянию. Спецразбор `S(Модель) = Состояние` не должен перехватывать имя
    /// модели.
    #[test]
    fn goto_model_name_in_state_of_still_resolves_to_model() {
        let range = goto_declaration(SRC, cursor_on(SRC, "S(Ping)", "Ping"));
        assert!(
            range.is_some(),
            "goto на имени модели в S(...) не должен ломаться"
        );
        let model_line = SRC[..SRC.find("model Ping").unwrap()].matches('\n').count() as u32;
        assert_eq!(
            range.unwrap().start.line,
            model_line,
            "переход должен открыть объявление `model Ping`"
        );
    }

    // Имя состояния в паттерне проверки состояния под-модели.
    //
    // Прежде здесь стояла фикстура `ref B: x = Done;` - сравнение переменной с именем
    // состояния той же модели. такой вход отвергает (`SE-110`: имя состояния само по
    // себе условием не является), и вместе с ним из валидной программы исчезла
    // возможность построить голый узел `ConditionNode::State`. Навигация от этого не
    // потерялась: имя состояния законно стоит в правой части паттерна `S(Модель) =
    // Состояние`, и переход по нему проверяется ниже.
    const SRC_SAME: &str = "model Feeder {\n    start Run { always { } ref Idle; }\n    state Idle;\n}\nmodel Work {\n    var n: u8 := 0;\n    start Busy { always { n := n + 1; } ref Done: S(Feeder) = Idle; }\n    state Done;\n}\nstart Main = Work | Feeder;\n";

    /// T2b: курсор на имени состояния в `S(Feeder) = Idle` -> его декларация.
    #[test]
    fn state_name_in_state_of_pattern_resolves_to_declaration() {
        let range = goto_declaration(SRC_SAME, cursor_on(SRC_SAME, "= Idle;", "Idle"));
        assert!(
            range.is_some(),
            "goto на имени состояния в паттерне S(Модель) = Состояние должен вернуть декларацию"
        );
        assert_eq!(
            range.unwrap().start.line,
            decl_line(SRC_SAME, "state Idle;"),
            "переход должен открыть декларацию `state Idle;` в модели Feeder"
        );
    }
}

/// T7: `ConditionNode::State` игнорирует use-site позицию в равенстве.
///
/// Две ссылки на **одно** состояние из разных мест текста обязаны быть равны. Тест
/// обязателен: `ConditionNode` сравнивается транзитивно через `ModelNode::PartialEq`, и
/// автовыведённое равенство расщепило бы такие ссылки в разные узлы -> поехал бы
/// детерминированный кодоген. Ловушка взведена **разными** `Location` - мутация ручного
/// `PartialEq` на сравнение позиций провалит тест. Тест чисто семантический (без LSP) -
/// потому вне `#[cfg(feature = "lsp")]`-модулей выше.
#[test]
fn condition_state_equality_ignores_use_site() {
    use std::cell::RefCell;
    use std::rc::Rc;
    use takt_lang::diagnostics::Location;
    use takt_lang::semantic::{ConditionNode, StateNode, StateNodeKind};

    let state = Rc::new(RefCell::new(StateNode::Simple {
        upper: None,
        name: "End".to_string(),
        named_blocks: vec![],
        references: vec![],
        kind: StateNodeKind::Simple,
        loc: Location::Source(0, 100, 103),
        formulas: vec![],
    }));
    let a = ConditionNode::State(state.clone(), Location::Source(0, 10, 13));
    let b = ConditionNode::State(state.clone(), Location::Source(0, 40, 43));
    assert_eq!(a, b, "use-site позиция не должна влиять на равенство");
}
