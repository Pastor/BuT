//! `textDocument/references` -.
//!
//! Проверяется вход LSP: позиция курсора -> диапазоны вхождений в координатах редактора
//! (строка/столбец). Логика областей видимости покрыта юнит-тестами слоя
//! (`semantic::usages`); здесь - что вход отдаёт ровно то, что нашёл слой, и в
//! правильных координатах.

#[cfg(feature = "lsp")]
mod references {
    use lsp_types::{Position, Range};
    use takt_lang::lsp::{references_at, server_capabilities};

    /// Модель, где переменная используется во всех интересных местах.
    const SRC: &str = r#"model M {
    out flag: bit;
    var speed: u8 := 0;
    fn bump(x: u8) -> u8 { return x + speed; }
    start Idle {
        enter { speed := bump(speed); }
        always { speed := speed + 1; }
        ref Done: speed > 10;
    }
    state Done { always { flag := 1; } }
}
"#;

    /// Позиция курсора на `n`-м (с нуля) вхождении подстроки.
    fn cursor_on_nth(source: &str, needle: &str, n: usize) -> Position {
        let offset = source
            .match_indices(needle)
            .nth(n)
            .unwrap_or_else(|| panic!("нет {}-го вхождения `{needle}`", n + 1))
            .0;
        let head = &source[..offset];
        let line = head.matches('\n').count() as u32;
        let col = head.rsplit('\n').next().map_or(0, |l| l.chars().count()) as u32;
        Position::new(line, col)
    }

    /// Текст, покрытый диапазоном (в координатах строк/столбцов).
    fn text_of(source: &str, range: Range) -> String {
        let line = source
            .lines()
            .nth(range.start.line as usize)
            .unwrap_or_default();
        let chars: Vec<char> = line.chars().collect();
        chars[range.start.character as usize..range.end.character as usize]
            .iter()
            .collect()
    }

    /// Возможность объявлена клиенту.
    #[test]
    fn references_provider_is_advertised() {
        assert!(
            server_capabilities().references_provider.is_some(),
            "referencesProvider не объявлен — «найти использования» не появится"
        );
    }

    /// A3: ответ содержит вхождения из тел блоков и функции - то, чего `SemanticIndex`
    /// не видел.
    #[test]
    fn references_include_block_and_function_bodies() {
        let position = cursor_on_nth(SRC, "speed", 0);
        let found = references_at(SRC, position, true).expect("вхождения должны находиться");
        assert_eq!(
            found.len(),
            7,
            "объявление + fn + enter (2) + always (2) + условие ребра, найдено {}",
            found.len()
        );
        // Каждое вхождение - ровно имя.
        for range in &found {
            assert_eq!(
                text_of(SRC, *range),
                "speed",
                "диапазон {range:?} не на имени"
            );
        }
        // Вхождение в теле `enter` присутствует.
        let enter_line = SRC
            .lines()
            .position(|l| l.contains("enter {"))
            .expect("строка enter") as u32;
        assert!(
            found.iter().any(|r| r.start.line == enter_line),
            "вхождение в теле enter не найдено: {found:?}"
        );
    }

    /// `includeDeclaration: false` убирает объявление и только его.
    #[test]
    fn declaration_is_excluded_on_demand() {
        let position = cursor_on_nth(SRC, "speed", 0);
        let with = references_at(SRC, position, true).expect("с объявлением");
        let without = references_at(SRC, position, false).expect("без объявления");
        assert_eq!(without.len() + 1, with.len());
        let decl_line = SRC
            .lines()
            .position(|l| l.contains("var speed"))
            .expect("строка объявления") as u32;
        assert!(
            !without.iter().any(|r| r.start.line == decl_line),
            "объявление не должно попасть в ответ: {without:?}"
        );
    }

    /// A4: одноимённая переменная другой модели в ответ не попадает.
    #[test]
    fn same_name_in_other_model_is_not_returned() {
        const TWO: &str = r#"model A {
    var speed: u8 := 0;
    start S { always { speed := speed + 1; } }
}

model B {
    var speed: u8 := 0;
    start S { always { speed := speed + 2; } }
}
"#;
        let position = cursor_on_nth(TWO, "speed: u8 := 0", 0);
        let found = references_at(TWO, position, true).expect("вхождения модели A");
        assert_eq!(found.len(), 3, "только модель A: {found:?}");
        let border = TWO
            .lines()
            .position(|l| l.contains("model B"))
            .expect("строка model B") as u32;
        assert!(
            found.iter().all(|r| r.start.line < border),
            "ответ заехал в модель B: {found:?}"
        );
    }

    /// A4: локальная переменная, затеняющая переменную модели, - отдельный символ
    /// (проба F4 анализа: цель `c` печатает именно локальную).
    #[test]
    fn shadowing_local_is_a_separate_symbol() {
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
        let on_model_var = cursor_on_nth(SHADOW, "x: u8 := 1", 0);
        let model_refs = references_at(SHADOW, on_model_var, true).expect("переменная модели");
        assert_eq!(
            model_refs.len(),
            1,
            "у затенённой переменной модели остаётся только объявление: {model_refs:?}"
        );

        let on_local = cursor_on_nth(SHADOW, "x: u8 := 2", 0);
        let local_refs = references_at(SHADOW, on_local, true).expect("локальная переменная");
        assert_eq!(
            local_refs.len(),
            4,
            "объявление + три вхождения в теле: {local_refs:?}"
        );
    }

    /// Курсор не на имени - ответа нет (и сервер не падает).
    #[test]
    fn cursor_outside_any_name_yields_nothing() {
        assert!(references_at(SRC, Position::new(999, 0), true).is_none());
    }

    /// Неразбираемый текст - ответа нет, а не паника.
    #[test]
    fn broken_source_yields_nothing() {
        assert!(references_at("model Broken {", Position::new(0, 7), true).is_none());
    }
}
