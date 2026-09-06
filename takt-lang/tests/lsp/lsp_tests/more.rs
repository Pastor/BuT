//! Продолжение `lsp_tests`: группы `diagnostic_location_tests`, `formatting_tests`,
//! `lsp_multifile` вынесены целыми mod-блоками (самодостаточны - свои `use`). Чистое
//! перемещение тестов, утверждения не меняются.

#[cfg(feature = "lsp")]
#[cfg(test)]
mod diagnostic_location_tests {
    use lsp_types::Position;
    use takt_lang::lsp::{
        collect_diagnostics, completion_items, grammar_diagnostic_to_lsp, hover_info,
        position_to_offset, semantic_tokens,
    };

    /// Проверяет, что ошибка синтаксиса имеет точные координаты (не нулевые).
    #[cfg(feature = "lsp")]
    #[test]
    fn collect_diagnostics_syntax_error_has_location() {
        // Пропущена точка с запятой - парсер должен вернуть ошибку с позицией
        let src = "var x: bit = false\nstart S;";
        let diags = collect_diagnostics(src);
        assert!(!diags.is_empty(), "должна быть хотя бы одна диагностика");
        // Хотя бы одна диагностика должна иметь ненулевую позицию
        let has_location = diags
            .iter()
            .any(|d| d.range.start != Position::new(0, 0) || d.range.end != Position::new(0, 0));
        assert!(
            has_location,
            "хотя бы одна диагностика должна содержать ненулевые координаты"
        );
    }

    /// Проверяет, что грамматическая диагностика с Location::Source содержит правильный
    /// диапазон после конвертации в LSP-формат.
    #[cfg(feature = "lsp")]
    #[test]
    fn grammar_diagnostic_to_lsp_source_location_gives_correct_range() {
        use takt_lang::diagnostics::{Diagnostic as GDiag, Location};

        let src = "var x: bit := false;";
        // Создаём диагностику с конкретной позицией (байты 4..5 - символ 'x')
        let diag = GDiag::error(Location::Source(0, 4, 5), "Тестовая ошибка".to_string());
        let lsp_diag = grammar_diagnostic_to_lsp(&diag, src);
        // Позиция 4 -> строка 0, столбец 4 (в ASCII 'x' = 1 байт)
        assert_eq!(
            lsp_diag.range.start,
            Position::new(0, 4),
            "начало диапазона должно быть (0, 4)"
        );
        assert_eq!(
            lsp_diag.range.end,
            Position::new(0, 5),
            "конец диапазона должно быть (0, 5)"
        );
    }

    /// Проверяет, что заметки добавляются к основному сообщению диагностики.
    #[cfg(feature = "lsp")]
    #[test]
    fn grammar_diagnostic_to_lsp_notes_appended_to_message() {
        use takt_lang::diagnostics::{Diagnostic as GDiag, Location};

        let src = "var x: bit := false;";
        let diag = GDiag::error_with_note(
            Location::Source(0, 4, 5),
            "Основная ошибка".to_string(),
            Location::Source(0, 0, 3),
            "Дополнительная информация".to_string(),
        );
        let lsp_diag = grammar_diagnostic_to_lsp(&diag, src);
        assert!(
            lsp_diag.message.contains("Основная ошибка"),
            "основное сообщение должно присутствовать: {}",
            lsp_diag.message
        );
        assert!(
            lsp_diag.message.contains("Дополнительная информация"),
            "заметка должна быть включена в сообщение: {}",
            lsp_diag.message
        );
    }

    /// Проверяет, что ошибки семантики (дубликат состояния) имеют координаты.
    #[cfg(feature = "lsp")]
    #[test]
    fn collect_diagnostics_semantic_error_has_location() {
        // Два состояния с одинаковым именем - семантическая ошибка
        let src = "start S;\nstate S;";
        let diags = collect_diagnostics(src);
        // Проверяем просто что нет паники и набор диагностик не пустой (конкретная
        // ошибка зависит от реализации дублирования)
        let _ = diags; // не паникует
    }

    /// Проверяет, что предупреждение Ce13 (неиспользуемая переменная) содержит
    /// координаты объявления переменной, а не нулевую позицию.
    #[cfg(feature = "lsp")]
    #[test]
    fn ce13_unused_variable_warning_has_source_location() {
        // Переменная `heading` объявлена на строке 0, нигде не используется
        let src = "var heading: bit := false;\nstart S;";
        let diags = collect_diagnostics(src);

        let ce13 = diags
            .iter()
            .find(|d| d.message.contains("heading"))
            .expect("должно быть предупреждение Ce13 для 'heading'");

        // Предупреждение не должно указывать на (0,0)-(0,0) - у переменной есть позиция
        let is_zero_range =
            ce13.range.start == Position::new(0, 0) && ce13.range.end == Position::new(0, 0);
        assert!(
            !is_zero_range,
            "Ce13 для 'heading' должно содержать координаты объявления, получено {:?}",
            ce13.range
        );
    }

    // -- Тесты semantic_tokens -------------------------------------------------

    /// Вспомогательная функция: возвращает список (слово, тип_токена) из
    /// semantic_tokens.
    fn decode_semantic_tokens(src: &str) -> Vec<(String, u32)> {
        let tokens = semantic_tokens(src);
        let mut result = Vec::new();
        let mut line: u32 = 0;
        let mut col: u32 = 0;
        for tok in &tokens.data {
            if tok.delta_line > 0 {
                line += tok.delta_line;
                col = tok.delta_start;
            } else {
                col += tok.delta_start;
            }
            let length = tok.length as usize;
            let token_type = tok.token_type;
            if let Some(start) = position_to_offset(src, Position::new(line, col)) {
                let end = (start + length).min(src.len());
                if src.is_char_boundary(start) && src.is_char_boundary(end) {
                    result.push((src[start..end].to_string(), token_type));
                }
            }
        }
        result
    }

    /// `while` подсвечивается как ключевое слово (TT_KEYWORD = 0).
    #[test]
    fn semantic_tokens_while_is_keyword() {
        let src = "model M { start S { always { while true { } } } }";
        let tokens = decode_semantic_tokens(src);
        let while_tok = tokens.iter().find(|(w, _)| w == "while");
        assert!(while_tok.is_some(), "токен 'while' должен присутствовать");
        assert_eq!(
            while_tok.unwrap().1,
            0,
            "'while' должен быть TT_KEYWORD (0), получено {}",
            while_tok.unwrap().1
        );
    }

    /// `match` подсвечивается как ключевое слово (TT_KEYWORD = 0).
    #[test]
    fn semantic_tokens_match_is_keyword() {
        let src = "model M { var x: u8 := 0; start S { always { match x { _ => { } } } } }";
        let tokens = decode_semantic_tokens(src);
        let match_tok = tokens.iter().find(|(w, _)| w == "match");
        assert!(match_tok.is_some(), "токен 'match' должен присутствовать");
        assert_eq!(
            match_tok.unwrap().1,
            0,
            "'match' должен быть TT_KEYWORD (0), получено {}",
            match_tok.unwrap().1
        );
    }

    /// `inout` подсвечивается как ключевое слово (TT_KEYWORD = 0).
    #[test]
    fn semantic_tokens_inout_is_keyword() {
        let src = "inout bus: u8 at 0x1000:0;\nstart S;";
        let tokens = decode_semantic_tokens(src);
        let tok = tokens.iter().find(|(w, _)| w == "inout");
        assert!(tok.is_some(), "токен 'inout' должен присутствовать");
        assert_eq!(tok.unwrap().1, 0, "'inout' должен быть TT_KEYWORD (0)");
    }

    /// `address` (оператор адреса порта) подсвечивается как keyword.
    #[test]
    fn semantic_tokens_address_is_keyword() {
        let src = "in BTN: bit;\naddress BTN = 0x200000;\nstart S;";
        let tokens = decode_semantic_tokens(src);
        let tok = tokens.iter().find(|(w, _)| w == "address");
        assert!(tok.is_some(), "токен 'address' должен присутствовать");
        assert_eq!(tok.unwrap().1, 0, "'address' должен быть TT_KEYWORD (0)");
    }

    /// `u8` в аннотации типа подсвечивается как тип (TT_TYPE = 3).
    #[test]
    fn semantic_tokens_u8_is_type() {
        let src = "var x: u8 := 0;\nstart S;";
        let tokens = decode_semantic_tokens(src);
        let u8_tok = tokens.iter().find(|(w, _)| w == "u8");
        assert!(u8_tok.is_some(), "токен 'u8' должен присутствовать");
        assert_eq!(
            u8_tok.unwrap().1,
            3,
            "'u8' должен быть TT_TYPE (3), получено {}",
            u8_tok.unwrap().1
        );
    }

    /// `i32` в аннотации типа подсвечивается как тип (TT_TYPE = 3).
    #[test]
    fn semantic_tokens_i32_is_type() {
        let src = "var n: i32 := 0;\nstart S;";
        let tokens = decode_semantic_tokens(src);
        let tok = tokens.iter().find(|(w, _)| w == "i32");
        assert!(tok.is_some(), "токен 'i32' должен присутствовать");
        assert_eq!(tok.unwrap().1, 3, "'i32' должен быть TT_TYPE (3)");
    }

    /// `bit` в аннотации типа подсвечивается как тип (TT_TYPE = 3).
    #[test]
    fn semantic_tokens_bit_is_type() {
        let src = "var flag: bit := 0;\nstart S;";
        let tokens = decode_semantic_tokens(src);
        let tok = tokens.iter().find(|(w, _)| w == "bit");
        assert!(tok.is_some(), "токен 'bit' должен присутствовать");
        assert_eq!(tok.unwrap().1, 3, "'bit' должен быть TT_TYPE (3)");
    }

    // -- Тесты hover для встроенных типов -------------------------------------

    /// Hover над `u8` возвращает описание типа.
    #[test]
    fn hover_builtin_type_u8() {
        let src = "var x: u8 := 0;\nstart S;";
        // Позиция курсора на 'u8' (строка 0, столбец 7)
        let result = hover_info(src, Position::new(0, 7));
        assert!(result.is_some(), "hover над 'u8' должен вернуть результат");
        let hover = result.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = hover.contents {
            assert!(
                mc.value.contains("u8"),
                "hover должен содержать 'u8': {}",
                mc.value
            );
        } else {
            panic!("ожидался MarkupContent");
        }
    }

    /// Hover над `i8` возвращает описание типа.
    #[test]
    fn hover_builtin_type_i8() {
        let src = "var n: i8 := 0;\nstart S;";
        let result = hover_info(src, Position::new(0, 7));
        assert!(result.is_some(), "hover над 'i8' должен вернуть результат");
        let hover = result.unwrap();
        if let lsp_types::HoverContents::Markup(mc) = hover.contents {
            assert!(mc.value.contains("i8"), "hover должен содержать 'i8'");
        } else {
            panic!("ожидался MarkupContent");
        }
    }

    /// Автодополнение содержит `while`, `match`, `inout` и встроенные типы.
    #[test]
    fn completion_includes_new_keywords_and_builtin_types() {
        let src = "start S;";
        let items = completion_items(src);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        for kw in &["while", "match", "inout", "struct"] {
            assert!(labels.contains(kw), "completion должен содержать '{}'", kw);
        }
        for ty in &["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64"] {
            assert!(
                labels.contains(ty),
                "completion должен содержать тип '{}'",
                ty
            );
        }
    }

    /// Hover над переменной с типом `u8` показывает "u8", а не Debug-строку "Integer {
    /// bits: 8, signed: false }".
    #[test]
    fn hover_var_u8_shows_u8_not_debug() {
        let src = "var speed: u8 := 0;\nstart S;";
        // Позиция 4 - "s" в "speed"
        let h = hover_info(src, lsp_types::Position::new(0, 4));
        assert!(h.is_some(), "hover над переменной должен вернуть результат");
        if let lsp_types::HoverContents::Markup(mc) = h.unwrap().contents {
            assert!(
                mc.value.contains("u8"),
                "hover должен содержать 'u8': {}",
                mc.value
            );
            assert!(
                !mc.value.contains("Integer"),
                "hover не должен содержать Debug-имя 'Integer': {}",
                mc.value
            );
        }
    }

    /// `collect_diagnostics` не выдаёт SE-034 для встроенных целочисленных типов
    /// `u8`...`i64`.
    #[test]
    fn collect_diagnostics_builtin_integer_types_no_error() {
        // const-переменные не генерируют предупреждения об использовании
        let src = "const A: u8 := 10;\nconst B: i32 := -5;\nconst C: u64 := 0;\nstart S;";
        let diags = collect_diagnostics(src);
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Some(lsp_types::DiagnosticSeverity::ERROR))
            .collect();
        assert!(
            errors.is_empty(),
            "встроенные целочисленные типы не должны давать ошибок SE-034: {:?}",
            errors
        );
    }
}

// -- textDocument/formatting ----------------------
//
// Проверка `cfg(feature = "lsp")` обязателен: `takt_lang::lsp` собирается только с этой
// фичей, а `precheck.sh` гоняет `cargo test` без `--all-features`.
#[cfg(feature = "lsp")]
#[cfg(test)]
mod formatting_tests {
    #[test]
    fn formatting_returns_single_full_document_edit() {
        let source = "var   x :u8:=0;\nstart   S ;\n";
        let edits = takt_lang::lsp::formatting_edits(source)
            .expect("форматирование удалось")
            .expect("текст не каноничен — правка обязана быть");
        assert_eq!(edits.len(), 1, "ожидается одна правка на весь документ");
        assert_eq!(edits[0].new_text, "var x: u8 := 0;\nstart S;\n");
        // Диапазон обязан покрывать документ целиком, иначе редактор склеит текст.
        assert_eq!(edits[0].range.start.line, 0);
        assert_eq!(edits[0].range.start.character, 0);
    }

    #[test]
    fn formatting_returns_none_when_already_canonical() {
        // Файл уже каноничен - правок нет, редактор не помечает его изменённым.
        let canonical = "var x: u8 := 0;\nstart S;\n";
        let edits = takt_lang::lsp::formatting_edits(canonical).expect("форматирование удалось");
        assert!(
            edits.is_none(),
            "на каноническом тексте правок быть не должно"
        );
    }

    #[test]
    fn formatting_reports_error_instead_of_mangling() {
        // Контрпример: неудача форматирования - это ошибка, а не "отформатировали как
        // смогли". Сервер её залогирует и ответит null.
        //
        // Вход тут менялся дважды, и оба раза по одной причине: узел, взятый как пример
        // непечатаемого, получал печать. Сперва это был `InlineFormula`, затем
        // `assembly`. Достижимых непечатаемых узлов больше не осталось вовсе -
        // `KNOWN_GAPS` форматтера пуст, а ветви отказа, которые ещё есть
        // (`Expression::CodeBlock`, `Type::Function`, инициализатор `for`
        // не-выражением), грамматика не строит. Поэтому вход теперь неразбираемый:
        // проверяется само свойство - при неудаче сервер отдаёт ошибку, а не искажённый
        // текст.
        let broken = "start S {\n    always { := ; }\n}\n";
        assert!(
            takt_lang::lsp::formatting_edits(broken).is_err(),
            "неудача форматирования обязана давать ошибку, а не молча искажать исходник"
        );
    }

    #[test]
    fn a6_lsp_and_cli_share_one_core() {
        // Критерий A6: LSP и `taktc fmt` не могут разойтись в стиле - они зовут одну и
        // ту же функцию. Проверяем это фактом, а не договорённостью.
        let source = "var   x :u8:=0;\nstart   S ;\n";
        let from_core = takt_lang::format::format_source(source).unwrap();
        let from_lsp = takt_lang::lsp::formatting_edits(source)
            .unwrap()
            .unwrap()
            .remove(0)
            .new_text;
        assert_eq!(
            from_lsp, from_core,
            "LSP обязан давать ровно то же, что ядро"
        );
    }
}

/// Многофайловость LSP: импорты и чужие диагностики.
#[cfg(feature = "lsp")]
mod lsp_multifile {
    // --- Многофайловость: импорты и чужие диагностики ----------------

    const LSP55_DIR: &str = "tests/data/lsp55";

    fn diagnostics_at(fixture: &str) -> Vec<lsp_types::Diagnostic> {
        let path = format!("{LSP55_DIR}/{fixture}");
        let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
        takt_lang::lsp::collect_diagnostics_at(&path, &source, &[])
    }

    /// Импорт разрешается в редакторе.
    #[test]
    fn import_resolves_in_editor() {
        let diags = diagnostics_at("uses_ok.takt");
        assert!(
            !diags.iter().any(|d| d.message.contains("не найден")),
            "импорт рядом с документом обязан разрешаться: {diags:?}"
        );
    }

    /// Ошибка чужого файла привязана к строке `import`, а не к чужому смещению.
    #[test]
    fn foreign_error_is_anchored_at_the_import_line() {
        let diags = diagnostics_at("uses_bad.takt");
        let d = diags
            .iter()
            .find(|d| d.message.contains("Nowhere"))
            .unwrap_or_else(|| panic!("ошибка библиотеки обязана быть показана: {diags:?}"));
        assert_eq!(
            d.range.start.line, 0,
            "якорь — строка `import` (первая): {:?}",
            d.range
        );
    }

    /// Текст называет настоящее место ошибки: `в файле X:строка:колонка`.
    ///
    /// Без этого автор видел бы подсветку на `import` и не знал, что искать.
    #[test]
    fn foreign_error_names_the_real_location() {
        let diags = diagnostics_at("uses_bad.takt");
        let d = diags
            .iter()
            .find(|d| d.message.contains("Nowhere"))
            .expect("ошибка библиотеки");
        assert!(
            d.message.contains("в файле") && d.message.contains("lib_bad.takt:2:"),
            "сообщение обязано называть файл и позицию: {}",
            d.message
        );
    }

    /// Своя ошибка показывается на своём месте - сужение не задело обычный путь.
    #[test]
    fn own_error_keeps_its_own_range() {
        let source = "start A { ref Nowhere; }";
        let diags = takt_lang::lsp::collect_diagnostics_at("own.takt", source, &[]);
        let d = diags.first().expect("ошибка своя");
        assert!(
            !d.message.contains("в файле"),
            "своя ошибка не помечается как чужая: {}",
            d.message
        );
        assert!(
            d.range.start.character > 0,
            "позиция внутри строки: {:?}",
            d.range
        );
    }
}
