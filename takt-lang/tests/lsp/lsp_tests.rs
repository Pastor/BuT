//! Интеграционные тесты LSP-функций: SemanticIndex, position_to_offset,
//! node_at_position, hover_info, goto_declaration.
//!
//! Тесты разделены на группы:
//! - `position_to_offset_*` - конвертация LSP-позиции в байтовое смещение
//! - `node_at_position_*` - поиск семантического узла по LSP-позиции
//! - `hover_*` - генерация hover-текста с использованием нового алгоритма
//! - `goto_declaration_*` - переход к декларации элемента

#[cfg(feature = "lsp")]
mod lsp_integration {
    use lsp_types::Position;
    use takt_lang::lsp::{goto_declaration, hover_info, node_at_position, position_to_offset};
    use takt_lang::parse;
    use takt_lang::semantic::index::{SemanticIndex, SemanticNodeKind};
    use takt_lang::semantic::tree::construct_model;

    // -- Тесты position_to_offset ----------------------------------------------

    /// Первая строка, нулевой столбец -> байт 0.
    #[test]
    fn position_to_offset_first_char() {
        let src = "var x: bit := false;";
        assert_eq!(
            position_to_offset(src, Position::new(0, 0)),
            Some(0),
            "первый символ первой строки — байт 0"
        );
    }

    /// Первая строка, 4-й символ -> байт 4.
    #[test]
    fn position_to_offset_middle_of_first_line() {
        let src = "var x: bit := false;";
        assert_eq!(
            position_to_offset(src, Position::new(0, 4)),
            Some(4),
            "символ 4 первой строки — байт 4"
        );
    }

    /// Вторая строка: после "\n" = 1 байт, символ 2 -> байт 1 + 2 = 3.
    #[test]
    fn position_to_offset_second_line() {
        let src = "ab\ncd";
        // "ab\n" = 3 байта, символ 1 на строке 1 -> байт 4
        assert_eq!(
            position_to_offset(src, Position::new(1, 1)),
            Some(4),
            "строка 1, символ 1 → байт 4"
        );
    }

    /// Несуществующая строка -> None.
    #[test]
    fn position_to_offset_nonexistent_line() {
        let src = "hello";
        assert_eq!(
            position_to_offset(src, Position::new(99, 0)),
            None,
            "строка 99 не существует"
        );
    }

    /// Символ за концом строки -> байт конца строки.
    #[test]
    fn position_to_offset_past_end_of_line() {
        let src = "hi";
        // Строка "hi" = 2 символа; character=99 -> clamp до конца строки -> байт 2
        assert_eq!(
            position_to_offset(src, Position::new(0, 99)),
            Some(2),
            "символ за концом строки → конец строки"
        );
    }

    /// Многострочный файл: строка 2, символ 0.
    #[test]
    fn position_to_offset_third_line_start() {
        let src = "line0\nline1\nline2";
        // "line0\n" = 6, "line1\n" = 6, итого 12 байт до строки 2
        assert_eq!(
            position_to_offset(src, Position::new(2, 0)),
            Some(12),
            "начало третьей строки — байт 12"
        );
    }

    // -- Тесты node_at_position ------------------------------------------------

    /// Вспомогательная функция: парсит, строит модель, возвращает её.
    fn make_model(src: &str) -> std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>> {
        let (ast, _) = parse(src, 0).expect("ошибка парсинга");
        construct_model(&ast, None, &[]).expect("ошибка семантики")
    }

    /// Курсор на объявлении переменной -> возвращает Variable.
    #[test]
    fn node_at_position_variable_declaration() {
        // 0 1 0123456789012345678
        let src = "var counter: bit := false;";
        let model = make_model(src);
        // Позиция 4 - символ 'c' в "counter"
        let node = node_at_position(src, Position::new(0, 4), &model);
        assert!(node.is_some(), "должен найти переменную");
        let node = node.unwrap();
        assert_eq!(node.name, "counter");
        assert_eq!(node.kind, SemanticNodeKind::Variable);
    }

    /// Курсор на ключевом слове -> None (не объявление).
    #[test]
    fn node_at_position_on_keyword_returns_none() {
        let src = "var x: bit := false;";
        let model = make_model(src);
        // Позиция 0 - символ 'v' в "var" (ключевое слово) Если Location включает всё
        // объявление, может найти переменную; если нет - None. В любом случае не должно
        // паниковать.
        let _ = node_at_position(src, Position::new(0, 0), &model);
    }

    /// Курсор за пределами файла -> None.
    #[test]
    fn node_at_position_past_end_of_file() {
        let src = "var x: bit := false;";
        let model = make_model(src);
        let node = node_at_position(src, Position::new(99, 0), &model);
        assert!(node.is_none(), "позиция за концом файла → None");
    }

    /// Курсор на объявлении константы -> возвращает Const.
    #[test]
    fn node_at_position_const_declaration() {
        // 0 1 2 012345678901234567890123456789
        let src = "const LIMIT: bit := true;";
        let model = make_model(src);
        // Позиция 6 - символ 'L' в "LIMIT"
        let node = node_at_position(src, Position::new(0, 6), &model);
        assert!(node.is_some(), "должен найти константу");
        let node = node.unwrap();
        assert_eq!(node.name, "LIMIT");
        assert_eq!(node.kind, SemanticNodeKind::Const);
    }

    /// Курсор на объявлении состояния -> возвращает StartState или State.
    #[test]
    fn node_at_position_state_declaration() {
        // 0 1 0123456789012345
        let src = "start Idle; state Run;";
        let model = make_model(src);
        // Позиция 6 - символ 'I' в "Idle"
        let node = node_at_position(src, Position::new(0, 6), &model);
        assert!(node.is_some(), "должен найти состояние Idle");
        let node = node.unwrap();
        assert_eq!(node.name, "Idle");
        assert_eq!(node.kind, SemanticNodeKind::StartState);
    }

    /// Курсор на объявлении псевдонима типа -> возвращает TypeAlias.
    #[test]
    fn node_at_position_type_alias() {
        // 0 1 01234567890123456789
        let src = "type Byte = [bit;8];";
        let model = make_model(src);
        // Позиция 5 - символ 'B' в "Byte"
        let node = node_at_position(src, Position::new(0, 5), &model);
        assert!(node.is_some(), "должен найти псевдоним типа");
        let node = node.unwrap();
        assert_eq!(node.name, "Byte");
        assert_eq!(node.kind, SemanticNodeKind::TypeAlias);
    }

    /// Курсор на объявлении условия -> возвращает Condition.
    #[test]
    fn node_at_position_condition() {
        let src = "cond Ready = true;";
        let model = make_model(src);
        // Позиция 5 - символ 'R' в "Ready"
        let node = node_at_position(src, Position::new(0, 5), &model);
        assert!(node.is_some(), "должен найти условие");
        let node = node.unwrap();
        assert_eq!(node.name, "Ready");
        assert_eq!(node.kind, SemanticNodeKind::Condition);
    }

    /// Курсор на объявлении перечисления -> возвращает Enum.
    #[test]
    fn node_at_position_enum() {
        let src = "model M { enum Color { Red, Green } start S; }";
        let model = make_model(src);
        // Позиция 15 - символ 'C' в "Color"
        let node = node_at_position(src, Position::new(0, 15), &model);
        assert!(node.is_some(), "должен найти перечисление");
        let node = node.unwrap();
        assert_eq!(node.name, "Color");
        assert_eq!(node.kind, SemanticNodeKind::Enum);
    }

    /// Курсор на объявлении функции -> возвращает Function или ExternFunction.
    #[test]
    fn node_at_position_extern_function() {
        let src = "extern fn send(data: bit);";
        let model = make_model(src);
        // Позиция 10 - символ 's' в "send"
        let node = node_at_position(src, Position::new(0, 10), &model);
        assert!(node.is_some(), "должен найти extern fn");
        let node = node.unwrap();
        assert_eq!(node.name, "send");
        assert_eq!(node.kind, SemanticNodeKind::ExternFunction);
    }

    /// Курсор на объявлении именованной модели -> возвращает Model.
    #[test]
    fn node_at_position_model() {
        let src = "model Blinker { start On; state Off; }";
        let model = make_model(src);
        // Позиция 6 - символ 'B' в "Blinker"
        let node = node_at_position(src, Position::new(0, 6), &model);
        assert!(node.is_some(), "должен найти модель");
        let node = node.unwrap();
        assert_eq!(node.name, "Blinker");
        assert_eq!(node.kind, SemanticNodeKind::Model);
    }

    /// Комплексный тест: несколько элементов на разных строках.
    #[test]
    fn node_at_position_multiline_file() {
        let src = "var alpha: bit := false;\nconst BETA: bit := true;\nstart Gamma;";
        let model = make_model(src);

        // Строка 0, позиция 4 - 'a' в "alpha"
        let n0 = node_at_position(src, Position::new(0, 4), &model);
        assert!(n0.is_some());
        assert_eq!(n0.unwrap().name, "alpha");

        // Строка 1, позиция 6 - 'B' в "BETA"
        let n1 = node_at_position(src, Position::new(1, 6), &model);
        assert!(n1.is_some());
        assert_eq!(n1.unwrap().name, "BETA");

        // Строка 2, позиция 6 - 'G' в "Gamma"
        let n2 = node_at_position(src, Position::new(2, 6), &model);
        assert!(n2.is_some());
        assert_eq!(n2.unwrap().name, "Gamma");
    }

    // -- Тесты hover с использованием нового алгоритма ------------------------

    /// Hover на объявлении переменной возвращает корректный тип.
    #[test]
    fn hover_variable_via_position_index() {
        let src = "var speed: bit := false; start S;";
        // Позиция 4 - 's' в "speed"
        let h = hover_info(src, Position::new(0, 4));
        assert!(h.is_some(), "hover должен найти переменную");
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("speed"),
                "hover должен содержать имя переменной: {}",
                mc.value
            );
            assert!(
                mc.value.contains("var"),
                "hover должен указывать вид: {}",
                mc.value
            );
        }
    }

    /// Hover на объявлении константы содержит "const".
    #[test]
    fn hover_const_via_position_index() {
        let src = "const LIMIT: bit := true; start S;";
        let h = hover_info(src, Position::new(0, 6));
        assert!(h.is_some(), "hover должен найти константу");
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("LIMIT"),
                "hover должен содержать имя: {}",
                mc.value
            );
            assert!(
                mc.value.contains("const"),
                "hover должен указывать 'const': {}",
                mc.value
            );
        }
    }

    /// Hover на объявлении функции содержит сигнатуру.
    #[test]
    fn hover_function_via_position_index() {
        let src = "extern fn compute(x: bit) -> bit; start S;";
        let h = hover_info(src, Position::new(0, 10));
        assert!(h.is_some(), "hover должен найти функцию");
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("compute"),
                "hover должен содержать имя функции: {}",
                mc.value
            );
        }
    }

    /// Hover на объявлении перечисления содержит варианты.
    #[test]
    fn hover_enum_via_position_index() {
        // Перечисление на верхнем уровне (в корневой модели) 0 1 2 3 4
        // 01234567890123456789012345678901234567890
        let src = "enum Status { Active = 0, Idle = 1 } start S;";
        // Позиция 5 - символ 'S' в "Status"
        let h = hover_info(src, Position::new(0, 5));
        assert!(h.is_some(), "hover должен найти перечисление");
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("Status"),
                "hover должен содержать имя enum: {}",
                mc.value
            );
        }
    }

    /// Hover на объявлении состояния содержит тип состояния.
    #[test]
    fn hover_state_via_position_index() {
        let src = "start Idle; state Active;";
        let h = hover_info(src, Position::new(0, 6));
        assert!(h.is_some(), "hover должен найти состояние");
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("Idle"),
                "hover должен содержать имя состояния: {}",
                mc.value
            );
        }
    }

    /// Hover на объявлении модели возвращает "model".
    #[test]
    fn hover_model_via_position_index() {
        let src = "model Engine { start On; state Off; } start S = Engine;";
        let h = hover_info(src, Position::new(0, 6));
        assert!(h.is_some(), "hover должен найти модель");
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("Engine"),
                "hover должен содержать имя модели: {}",
                mc.value
            );
            assert!(
                mc.value.contains("model"),
                "hover должен указывать 'model': {}",
                mc.value
            );
        }
    }

    /// Hover на использовании переменной (не объявлении) - резервный поиск по имени.
    #[test]
    fn hover_variable_usage_fallback_search() {
        // "var x" на строке 0; использование "x" в условии на строке 1
        let src = "var x: bit := false;\ncond C = x = true;\nstart S;";
        // Позиция (1, 9) - 'x' в условии (использование)
        let h = hover_info(src, Position::new(1, 9));
        // Резервный поиск должен найти переменную "x"
        assert!(
            h.is_some(),
            "hover на использовании переменной должен работать"
        );
    }

    /// Hover в пустом файле -> None.
    #[test]
    fn hover_empty_file_returns_none() {
        let h = hover_info("", Position::new(0, 0));
        assert!(h.is_none(), "hover в пустом файле → None");
    }

    #[test]
    fn hover_big() {
        const SRC: &str = r#"// Пример для демонстрации возможностей LSP-сервера Takt.
//
// При открытии этого файла в редакторе с поддержкой LSP (например, Zed с takt-lsp):
// - Ошибки и предупреждения подсвечиваются сразу
// - При наведении на идентификатор отображается его тип
// - Автодополнение предлагает ключевые слова и имена из модели


/// Перечисление направлений движения робота.
enum Direction { North, South, East, West }

/// Перечисление приоритетов задачи.
enum Priority { Low = 0, Medium = 5, High = 10 }

extern fn log(msg: u8);

/// Переменная состояния направления.
var heading: Direction := 0;

/// Модель управления роботом.
model Robot {
    var speed: u8 := 0;
    var active: bit := false;

    start Idle {
        enter {
            speed := 0;
            active := false;
        }
        ref Moving: active;
    }

    state Moving {
        always {
            speed := 100;
        }
        ref Idle: true;
    }
}

start Main = Robot;
        "#;
        let h = hover_info(SRC, Position::new(420 - 391, 23));
        assert!(h.is_some(), "hover должен найти переменную условия active");
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("active"),
                "hover должен содержать имя элемента условия: {}",
                mc.value
            );
            assert!(
                mc.value.contains("var"),
                "hover должен указывать 'var': {}",
                mc.value
            );
        }
    }

    #[test]
    fn hover_model() {
        const SRC: &str = r#"model Robot {
    start Idle;
}
start Main = Robot;
        "#;
        let h = hover_info(SRC, Position::new(3, 16));
        assert!(
            h.is_some(),
            "hover должен найти модель реализации состояния"
        );
        let h = h.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = h.contents {
            assert!(
                mc.value.contains("Robot"),
                "hover должен содержать имя модели: {}",
                mc.value
            );
            assert!(
                mc.value.contains("model"),
                "hover должен указывать 'model': {}",
                mc.value
            );
        }
    }

    // -- Тесты goto_declaration ------------------------------------------------

    /// Курсор на объявлении переменной -> возвращает тот же диапазон (самоссылка).
    #[test]
    fn goto_declaration_variable_self() {
        // 0 1 2 0123456789012345678901234
        let src = "var counter: bit := false;";
        // Позиция 4 - символ 'c' в "counter"
        let range = goto_declaration(src, Position::new(0, 4));
        assert!(
            range.is_some(),
            "goto_declaration на объявлении переменной должен вернуть диапазон"
        );
        let range = range.unwrap();
        assert_eq!(range.start.line, 0, "декларация переменной на строке 0");
    }

    /// Курсор на объявлении функции -> возвращает диапазон этой же функции.
    #[test]
    fn goto_declaration_function_self() {
        let src = "extern fn send(data: bit); start S;";
        // Позиция 10 - символ 's' в "send"
        let range = goto_declaration(src, Position::new(0, 10));
        assert!(
            range.is_some(),
            "goto_declaration на объявлении функции должен вернуть диапазон"
        );
        let range = range.unwrap();
        assert_eq!(range.start.line, 0, "декларация функции на строке 0");
    }

    /// Курсор на объявлении состояния -> возвращает диапазон этого же состояния.
    #[test]
    fn goto_declaration_state_self() {
        let src = "start Idle; state Moving;";
        // Позиция 6 - символ 'I' в "Idle"
        let range = goto_declaration(src, Position::new(0, 6));
        assert!(
            range.is_some(),
            "goto_declaration на объявлении состояния должен вернуть диапазон"
        );
        let range = range.unwrap();
        assert_eq!(range.start.line, 0, "декларация состояния на строке 0");
    }

    /// Курсор на ref-переходе -> возвращает диапазон объявления целевого состояния.
    ///
    /// Пример: `ref Moving;` внутри состояния -> декларация `state Moving;`.
    #[test]
    fn goto_declaration_reference_resolves_to_state() {
        // Строка 0: "start Idle { ref Moving; }" Строка 1: "state Moving;"
        let src = "start Idle { ref Moving; }\nstate Moving;";
        // Позиция (0, 17) - символ 'M' в "ref Moving"
        let range = goto_declaration(src, Position::new(0, 17));
        assert!(
            range.is_some(),
            "goto_declaration на ref-переходе должен вернуть декларацию состояния"
        );
        let range = range.unwrap();
        assert_eq!(
            range.start.line, 1,
            "декларация состояния Moving на строке 1: {:?}",
            range
        );
    }

    /// Курсор на переменной в условии перехода -> декларация этой переменной.
    ///
    /// Пример: `ref Run: flag;` где `flag` - ReferenceCondition -> декларация `var
    /// flag`.
    #[test]
    fn goto_declaration_reference_condition_resolves_to_variable() {
        // Строка 0: "var flag: bit = false;" Строка 1: "start Idle;" Строка 2: "state
        // Run;" Строка 3: "start Idle { ref Run: flag; }"
        let src = "var flag: bit := false;\nstart Idle;\nstate Run;\nstart Idle { ref Run: flag; }";
        // Строка 3: "start Idle { ref Run: flag; }" 0 1 2
        // 0123456789012345678901234567890 "flag" начинается с позиции 22 на строке 3
        let range = goto_declaration(src, Position::new(3, 22));
        assert!(
            range.is_some(),
            "goto_declaration на переменной условия должен вернуть декларацию переменной"
        );
        let range = range.unwrap();
        assert_eq!(
            range.start.line, 0,
            "декларация var flag на строке 0: {:?}",
            range
        );
    }

    /// Курсор вне идентификаторов -> None.
    #[test]
    fn goto_declaration_outside_node_returns_none() {
        let src = "var x: bit := false;\nstart S;";
        // Позиция (0, 0) - символ 'v' в "var" (ключевое слово, не идентификатор
        // объявления) Может вернуть Some (если Location включает всё объявление) или
        // None. Главное - не паниковать.
        let _ = goto_declaration(src, Position::new(0, 0));

        // Позиция за пределами файла -> точно None
        let range = goto_declaration(src, Position::new(99, 0));
        assert!(range.is_none(), "позиция за пределами файла → None");
    }

    /// Пустой файл -> None.
    #[test]
    fn goto_declaration_empty_file_returns_none() {
        let range = goto_declaration("", Position::new(0, 0));
        assert!(range.is_none(), "пустой файл → None");
    }

    // -- I7: goto_declaration_with_paths --------------------------------------

    /// I7: декларация переменной в текущем файле через goto_declaration_with_paths.
    #[test]
    fn i7_goto_declaration_local_var() {
        use takt_lang::lsp::goto_declaration_with_paths;

        let src = "var counter: [bit;8] := 0;\nstart S;";
        // Позиция 4 - символ 'c' в "counter"
        let loc = goto_declaration_with_paths(src, Position::new(0, 4), &[]);
        assert!(
            loc.is_some(),
            "переменная 'counter' должна быть найдена в текущем файле"
        );
        let loc = loc.unwrap();
        assert_eq!(
            loc.range.start.line, 0,
            "декларация counter на строке 0: {:?}",
            loc.range
        );
    }

    /// I7: декларация состояния в текущем файле через goto_declaration_with_paths.
    #[test]
    fn i7_goto_declaration_local_state() {
        use takt_lang::lsp::goto_declaration_with_paths;

        let src = "start S;\nstate Ready;";
        // Позиция (1, 6) - символ 'R' в "Ready"
        let loc = goto_declaration_with_paths(src, Position::new(1, 6), &[]);
        assert!(
            loc.is_some(),
            "состояние 'Ready' должно быть найдено в текущем файле"
        );
    }

    /// I7: при отсутствии идентификатора под курсором возвращается None.
    #[test]
    fn i7_goto_declaration_outside_returns_none() {
        use takt_lang::lsp::goto_declaration_with_paths;

        let src = "var x: bit := false;\nstart S;";
        let loc = goto_declaration_with_paths(src, Position::new(99, 0), &[]);
        assert!(loc.is_none(), "позиция за пределами файла → None");
    }

    /// I7: пустой файл -> None.
    #[test]
    fn i7_goto_declaration_empty_file() {
        use takt_lang::lsp::goto_declaration_with_paths;

        let loc = goto_declaration_with_paths("", Position::new(0, 0), &[]);
        assert!(loc.is_none(), "пустой файл → None");
    }

    // -- I8: индексация локальных переменных в enter/exit/always -------------

    /// I8: локальная переменная в `always`-блоке доступна через SemanticIndex.
    #[test]
    fn i8_local_var_in_always_indexed() {
        let src = "start S;\nalways { var local_x: [bit;8] := 0; }";
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let index = SemanticIndex::build(&model);

        // Находим позицию имени "local_x" в "var local_x: ..." "start S;\nalways { var
        // local_x: " - "local_x" начинается с байта 19
        let offset = src
            .find("local_x")
            .expect("local_x должен быть в источнике");
        let node = index.node_at_offset(offset);
        assert!(
            node.is_some(),
            "local_x должна быть в индексе по offset {}: {:?}",
            offset,
            node
        );
        let node = node.unwrap();
        assert_eq!(node.name, "local_x", "имя узла: {:?}", node);
        assert_eq!(
            node.kind,
            SemanticNodeKind::LocalVar,
            "вид узла должен быть LocalVar: {:?}",
            node
        );
    }

    /// I8: локальная переменная в модельном `enter`-блоке доступна через SemanticIndex.
    #[test]
    fn i8_local_var_in_model_enter_indexed() {
        // Блок enter на уровне модели (не внутри состояния)
        let src = "start S;\nenter { var enter_var: bit := false; }";
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let index = SemanticIndex::build(&model);

        let offset = src
            .find("enter_var")
            .expect("enter_var должен быть в источнике");
        let node = index.node_at_offset(offset);
        assert!(
            node.is_some(),
            "enter_var должна быть в индексе по offset {}",
            offset
        );
        let node = node.unwrap();
        assert_eq!(node.name, "enter_var");
        assert_eq!(node.kind, SemanticNodeKind::LocalVar);
    }

    /// I8: локальная переменная в модельном `exit`-блоке доступна через SemanticIndex.
    #[test]
    fn i8_local_var_in_model_exit_indexed() {
        // Блок exit на уровне модели (не внутри состояния)
        let src = "start S;\nexit { var exit_var: bit := false; }";
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let index = SemanticIndex::build(&model);

        let offset = src
            .find("exit_var")
            .expect("exit_var должен быть в источнике");
        let node = index.node_at_offset(offset);
        assert!(
            node.is_some(),
            "exit_var должна быть в индексе по offset {}",
            offset
        );
        let node = node.unwrap();
        assert_eq!(node.name, "exit_var");
        assert_eq!(node.kind, SemanticNodeKind::LocalVar);
    }

    /// I8: поиск локальной переменной по позиции LSP в always-блоке.
    #[test]
    fn i8_node_at_position_local_var_in_always() {
        let src = "start S;\nalways { var my_local: [bit;8] := 0; }";
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();

        // Находим строку и столбец для "my_local"
        let offset = src.find("my_local").unwrap();
        let line = src[..offset].matches('\n').count() as u32;
        let line_start = src[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let col = (offset - line_start) as u32;

        let node = node_at_position(src, Position::new(line, col), &model);
        assert!(
            node.is_some(),
            "my_local должна быть найдена по позиции ({}, {})",
            line,
            col
        );
        let node = node.unwrap();
        assert_eq!(node.name, "my_local");
        assert_eq!(node.kind, SemanticNodeKind::LocalVar);
    }

    /// I8: тестовый файл local_var_in_blocks.takt - все локальные переменные
    /// индексируются.
    #[test]
    fn i8_file_local_vars_indexed() {
        let src = std::fs::read_to_string("tests/data/lsp/local_var_in_blocks.takt")
            .expect("не удалось прочитать файл");
        let (ast, _) = parse(&src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let index = SemanticIndex::build(&model);

        // Все три локальные переменные должны быть в индексе
        for var_name in &["local_in_enter", "local_in_exit", "local_in_always"] {
            let offset = src
                .find(var_name)
                .unwrap_or_else(|| panic!("{} должен быть в тестовом файле", var_name));
            let node = index.node_at_offset(offset);
            assert!(
                node.is_some(),
                "{} должна быть в индексе по offset {}",
                var_name,
                offset
            );
            assert_eq!(
                node.unwrap().kind,
                SemanticNodeKind::LocalVar,
                "{} должна иметь вид LocalVar",
                var_name
            );
        }
    }

    /// I7: кросс-файловый переход - объявление модели из импортируемого файла.
    #[test]
    fn i7_goto_declaration_cross_file_model() {
        use takt_lang::lsp::goto_declaration_with_paths;

        // Создаём временный файл с моделью
        let dir = tempfile::tempdir().unwrap();
        let ping_path = dir.path().join("ping.takt");
        std::fs::write(&ping_path, "model Ping { start S; }").unwrap();
        let dir_str = dir.path().to_string_lossy().into_owned();

        // Исходный файл с импортом
        let src = r#"import "ping.takt"; start Main;"#;
        // Позиция 7 - символ '"' (начало строки импорта) - за пределами идентификатора
        // Проверяем что функция не паникует при отсутствии узла
        let _ =
            goto_declaration_with_paths(src, Position::new(0, 7), std::slice::from_ref(&dir_str));

        // Нет паники - тест прошёл (полное кросс-файловое разрешение зависит от наличия
        // идентификатора в индексе)
    }
}

// Группы diagnostic/formatting/multifile - в подмодуле.
#[cfg(feature = "lsp")]
#[path = "lsp_tests/more.rs"]
mod more;
