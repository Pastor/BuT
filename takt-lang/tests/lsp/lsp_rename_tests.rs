//! `textDocument/rename` и `prepareRename` -.
//!
//! Главный тест здесь - не "правки вернулись", а **сверка порождённого C**: частичное
//! переименование часто оставляет текст компилируемым (затенение), и проверка
//! "собралось" его пропустила бы. Поэтому применённые правки компилируются целью `c` и
//! сравниваются с эталоном - исходным текстом, в котором имя заменено сплошняком.

#[cfg(feature = "lsp")]
mod rename {
    use lsp_types::Position;
    use takt_lang::GenerateOptions;
    use takt_lang::compile_to_c;
    use takt_lang::lsp::{
        RenameRefusal, position_to_offset, prepare_rename_at, rename_at, server_capabilities,
    };

    /// Модель, где переменная используется всюду, где это возможно.
    const SRC: &str = r#"model Machine {
    out flag: bit;
    var speed: u8 := 0;
    var mirror: u8 := speed;
    cond Fast = speed > 3;
    fn bump(x: u8) -> u8 { return x + speed; }
    start Idle {
        enter { speed := bump(speed); }
        always { speed := speed + 1; }
        ref Done: speed > 10;
    }
    state Done { always { flag := 1; } }
}

start Root = Machine;
"#;

    /// Позиция курсора на первом вхождении подстроки.
    fn cursor_on(source: &str, needle: &str) -> Position {
        let offset = source
            .find(needle)
            .unwrap_or_else(|| panic!("в тексте нет `{needle}`"));
        let head = &source[..offset];
        let line = head.matches('\n').count() as u32;
        let col = head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32;
        Position::new(line, col)
    }

    /// Применяет правки переименования к тексту.
    ///
    /// Правки применяются **с конца**, чтобы ранее применённая не сдвинула диапазоны
    /// следующих.
    fn apply(source: &str, position: Position, new_name: &str) -> String {
        let mut edits = rename_at(source, position, new_name).expect("правки переименования");
        edits.sort_by_key(|e| std::cmp::Reverse((e.range.start.line, e.range.start.character)));
        let mut text = source.to_string();
        for edit in edits {
            let start = position_to_offset(&text, edit.range.start).expect("начало правки");
            let end = position_to_offset(&text, edit.range.end).expect("конец правки");
            text.replace_range(start..end, &edit.new_text);
        }
        text
    }

    /// Порождает C и склеивает содержимое всех файлов вывода.
    fn generated_c(source: &str, dir_name: &str) -> String {
        let dir = std::env::temp_dir()
            .join(format!("takt_pid{}", std::process::id()))
            .join(dir_name);
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("каталог вывода");
        // Имя корневой (анонимной) модели берётся из имени файла - оно не должно
        // совпадать с именем вложенной модели, иначе кодоген не найдёт её состояний
        // (`CC-005`).
        compile_to_c(
            "Plant",
            source,
            dir.to_str().expect("путь в UTF-8"),
            &[],
            &GenerateOptions::default(),
        )
        .expect("порождение C");
        let mut files: Vec<_> = std::fs::read_dir(&dir)
            .expect("чтение каталога")
            .flatten()
            .map(|e| e.path())
            .collect();
        files.sort();
        files
            .iter()
            .map(|p| std::fs::read_to_string(p).expect("чтение файла"))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Возможность объявлена, и `prepareRename` тоже.
    #[test]
    fn rename_provider_with_prepare_is_advertised() {
        let caps = server_capabilities();
        let provider = caps.rename_provider.expect("renameProvider не объявлен");
        match provider {
            lsp_types::OneOf::Right(options) => assert_eq!(
                options.prepare_provider,
                Some(true),
                "prepareProvider обязателен: отказ должен приходить до ввода имени"
            ),
            lsp_types::OneOf::Left(_) => panic!("ожидались RenameOptions с prepareProvider"),
        }
    }

    /// `prepareRename` отдаёт диапазон **ровно имени** под курсором.
    #[test]
    fn prepare_returns_exact_identifier_range() {
        let position = cursor_on(SRC, "speed");
        let range = prepare_rename_at(SRC, position).expect("переименование разрешено");
        let start = position_to_offset(SRC, range.start).expect("начало");
        let end = position_to_offset(SRC, range.end).expect("конец");
        assert_eq!(
            &SRC[start..end],
            "speed",
            "диапазон обязан покрывать ровно идентификатор, а не оператор целиком"
        );
    }

    /// A6 - главный тест: применённые правки дают код, совпадающий с эталоном (тот же
    /// текст со сплошной заменой имени).
    #[test]
    fn rename_is_complete_generated_c_matches_reference() {
        let position = cursor_on(SRC, "speed");
        let renamed = apply(SRC, position, "velocity");

        // Все вхождения `speed` в исправлениетуре принадлежат одному символу, поэтому сплошная
        // текстовая замена - законный эталон.
        let reference = SRC.replace("speed", "velocity");
        assert_eq!(
            renamed, reference,
            "переименование обязано затронуть все вхождения и только их"
        );

        // И - главное - порождённый код совпадает. Проверка на тексте могла бы
        // пропустить случай, где вхождение осталось старым, но текст всё ещё
        // компилируется.
        assert_eq!(
            generated_c(&renamed, "takt_0131_renamed"),
            generated_c(&reference, "takt_0131_reference"),
            "код цели `c` после переименования обязан совпасть с эталонным"
        );
    }

    /// Локальная переменная переименовывается отдельно от одноимённой переменной модели -
    /// и код остаётся тем же по смыслу.
    #[test]
    fn renaming_shadowing_local_leaves_model_variable_intact() {
        const SHADOW: &str = r#"model M {
    var x: u8 := 1;
    out y: u8;
    start S {
        always {
            var x: u8 := 2;
            x := x + 1;
            y := x;
        }
    }
}
"#;
        let position = cursor_on(SHADOW, "x: u8 := 2");
        let renamed = apply(SHADOW, position, "tmp");
        assert!(
            renamed.contains("var x: u8 := 1;"),
            "объявление переменной модели не должно измениться:\n{renamed}"
        );
        assert!(
            renamed.contains("var tmp: u8 := 2;") && renamed.contains("tmp := tmp + 1;"),
            "локальная и её вхождения обязаны смениться:\n{renamed}"
        );
    }

    /// A7: имя модели переименовать нельзя - оно видно за пределами файла.
    #[test]
    fn model_name_is_refused() {
        let position = cursor_on(SRC, "Machine");
        assert_eq!(
            prepare_rename_at(SRC, position),
            Err(RenameRefusal::ModelName)
        );
        assert_eq!(
            rename_at(SRC, position, "Device"),
            Err(RenameRefusal::ModelName)
        );
    }

    /// A7: символ, объявленный вне открытого документа, переименовать нельзя.
    #[test]
    fn foreign_symbol_is_refused() {
        const IMPORTING: &str = r#"import "helper.takt" as Helper;

model M {
    start S { ref S: outside > 0; }
}
"#;
        let position = cursor_on(IMPORTING, "outside");
        assert_eq!(
            prepare_rename_at(IMPORTING, position),
            Err(RenameRefusal::ForeignDeclaration),
            "имя из чужого файла: полнота недостижима"
        );
    }

    /// A7: новое имя обязано быть идентификатором.
    #[test]
    fn non_identifier_new_name_is_refused() {
        let position = cursor_on(SRC, "speed");
        for bad in ["2speed", "with space", "", "имя-с-дефисом"] {
            assert_eq!(
                rename_at(SRC, position, bad),
                Err(RenameRefusal::NotAnIdentifier),
                "имя `{bad}` не идентификатор"
            );
        }
    }

    /// A7: ключевое слово новым именем быть не может.
    #[test]
    fn keyword_new_name_is_refused() {
        let position = cursor_on(SRC, "speed");
        for keyword in ["state", "model", "var", "fn"] {
            assert_eq!(
                rename_at(SRC, position, keyword),
                Err(RenameRefusal::Keyword),
                "ключевое слово `{keyword}`"
            );
        }
    }

    /// Кириллица в имени допустима - лексер принимает XID_Start.
    #[test]
    fn cyrillic_new_name_is_accepted() {
        let position = cursor_on(SRC, "speed");
        let renamed = apply(SRC, position, "скорость");
        assert!(renamed.contains("var скорость: u8 := 0;"), "{renamed}");
    }

    /// Неразбираемый текст: отказ, а не паника и не частичная правка.
    #[test]
    fn unparsable_source_is_refused() {
        assert_eq!(
            prepare_rename_at("model Broken {", Position::new(0, 7)),
            Err(RenameRefusal::Unparsable)
        );
    }

    /// Курсор не на имени - отказ с внятной причиной.
    #[test]
    fn cursor_without_symbol_is_refused() {
        assert_eq!(
            prepare_rename_at(SRC, Position::new(999, 0)),
            Err(RenameRefusal::NoSymbol)
        );
    }

    /// У каждой причины отказа есть сообщение для пользователя.
    #[test]
    fn every_refusal_has_a_message() {
        for refusal in [
            RenameRefusal::Unparsable,
            RenameRefusal::NoSymbol,
            RenameRefusal::ForeignDeclaration,
            RenameRefusal::ModelName,
            RenameRefusal::Incomplete,
            RenameRefusal::NotAnIdentifier,
            RenameRefusal::Keyword,
        ] {
            assert!(
                !refusal.message().is_empty(),
                "у отказа {refusal:?} нет сообщения"
            );
        }
    }
}
