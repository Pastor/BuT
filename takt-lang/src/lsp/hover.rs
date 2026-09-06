//! Подсказка при наведении.
//!
//! Часть модуля `lsp`.

use super::*;

/// Возвращает слово (идентификатор) под заданной позицией курсора.
///
/// Позиция `position.character` задаётся в **кодовых единицах UTF-16** согласно
/// спецификации LSP. Функция корректно обрабатывает многобайтовые символы UTF-8
/// (кириллица, CJK, эмодзи).
///
/// Символами слова считаются буквенно-цифровые символы (`is_alphanumeric()`), знак
/// подчёркивания `_` и знак `$`.
///
/// # Примеры (примеры / контр-примеры)
///
/// ```
/// # #[cfg(feature = "lsp")]
/// # {
/// use takt_lang::lsp::word_at_position;
/// use lsp_types::Position;
///
/// // Курсор внутри слова "hello" -> "hello"
/// assert_eq!(word_at_position("hello world", Position::new(0, 2)), Some("hello".to_string()));
///
/// // Курсор на границе слова (позиция прямо после "hello") -> возвращает "hello"
/// assert_eq!(word_at_position("hello world", Position::new(0, 5)), Some("hello".to_string()));
///
/// // Курсор строго внутри двойного пробела -> None
/// assert_eq!(word_at_position("hello  world", Position::new(0, 6)), None);
///
/// // Несуществующая строка -> None
/// assert_eq!(word_at_position("hello", Position::new(99, 0)), None);
/// # }
/// ```
pub fn word_at_position(source: &str, position: Position) -> Option<String> {
    let line_text = source.lines().nth(position.line as usize)?;
    // Конвертируем UTF-16 смещение символа в байтовое смещение
    let col =
        utf16_to_byte_offset(line_text, position.character as usize).unwrap_or(line_text.len());

    // Ищем начало слова (идём влево от курсора)
    let start = line_text[..col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != '$')
        .map(|i| {
            // rfind возвращает байтовый индекс начала символа-разделителя; шагаем
            // вперёд на длину этого символа
            let ch = line_text[i..].chars().next().unwrap();
            i + ch.len_utf8()
        })
        .unwrap_or(0);

    // Ищем конец слова (идём вправо от курсора)
    let end = line_text[col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '$')
        .map(|i| i + col)
        .unwrap_or(line_text.len());

    if start < end {
        Some(line_text[start..end].to_string())
    } else {
        None
    }
}

/// Возвращает информацию о типе идентификатора под курсором.
///
/// Алгоритм поиска работает в два этапа:
///
/// 1. **Поиск по позиции** (через [`node_at_position`]): находит семантический узел,
///    объявление которого точно покрывает позицию курсора. Устраняет неоднозначность
///    между элементами с одинаковым именем в разных областях видимости.
///
/// 2. **Поиск по имени** (через [`word_at_position`]): резервный метод - извлекает
///    идентификатор под курсором и ищет его в семантической модели. Применяется,
///    если курсор находится на *использовании* элемента (не на объявлении), или
///    если поиск по позиции не дал результата.
pub fn hover_info(source: &str, position: Position) -> Option<Hover> {
    // Строим семантическую модель с привязкой doc-комментариев
    let (ast, comments) = crate::parse(source, 0).ok()?;
    let model = semantic::tree::construct_model_with_docs(&ast, None, &[], &comments).ok()?;
    let borrowed = model.borrow();

    // Шаг 1: пытаемся найти узел по точной позиции в объявлении
    let position_node = node_at_position(source, position, &model);

    // Шаг 2: всегда берём слово непосредственно под курсором как основу поиска.
    // Направленный поиск (position_node) задействуется только если имя узла совпадает с
    // тем, что стоит под курсором. Это устраняет ситуацию, когда курсор стоит на ссылке
    // (например `Robot` в `start Main = Robot;`), а position_node указывает на
    // объявление `Main`, чей диапазон покрывает всю строку.
    let word = word_at_position(source, position)?;
    if word.is_empty() {
        return None;
    }

    // Вспомогательные функции для формирования hover-текста
    let make_var_hover = |var: &VariableNode, word: &str, doc: &[String]| {
        let (type_str, kind_str) = match var {
            VariableNode::Simple { ty, .. } => (format!("{}", ty), "var"),
            VariableNode::Const { ty, .. } => (format!("{}", ty), "const"),
            VariableNode::Port { ty, .. } => (format!("{}", ty), "port"),
            VariableNode::Unresolved => ("?".to_string(), "var"),
        };
        let mut text = format!("```but\n{} {}: {}\n```", kind_str, word, type_str);
        if !doc.is_empty() {
            text.push_str("\n\n");
            text.push_str(&doc.join("\n"));
        }
        text
    };

    let make_func_hover = |func: &FunctionDefinitionNode, word: &str, doc: &[String]| {
        let sig = match func {
            FunctionDefinitionNode::Local { params, ret, .. } => {
                let ps: Vec<String> = params
                    .iter()
                    .map(|(n, t)| format!("{}: {}", n, t))
                    .collect();
                format!("fn {}({}) -> {}", word, ps.join(", "), ret)
            }
            FunctionDefinitionNode::External { params, ret, .. } => {
                let ps: Vec<String> = params
                    .iter()
                    .map(|(n, t)| format!("{}: {}", n, t))
                    .collect();
                format!("extern fn {}({}) -> {}", word, ps.join(", "), ret)
            }
            FunctionDefinitionNode::Builtin(name, params, ret) => {
                format!("builtin fn {}({} params) -> {}", name, params.len(), ret)
            }
            _ => format!("fn {}", word),
        };
        let mut text = format!("```but\n{}\n```", sig);
        if !doc.is_empty() {
            text.push_str("\n\n");
            text.push_str(&doc.join("\n"));
        }
        text
    };

    let mut hover_text = String::new();

    // Шаг 1 (направленный поиск): если известен вид узла - ищем только в нужной
    // категории. Это устраняет ложные совпадения при одинаковых именах в разных
    // категориях.
    //
    // Ключевое правило: клон Rc должен храниться в именованном биндинге до вызова
    // borrow(), иначе временная переменная освобождается раньше, чем завершается
    // заимствование.
    //
    // Направленный поиск активируется только если имя узла совпадает с курсорным
    // словом. Если cursor_word = "Robot", а position_node.name = "Main" (объявление
    // покрывает всю строку), поиск пропускается и уступает место резервному поиску по
    // имени.
    use crate::semantic::index::SemanticNodeKind;
    if let Some(node_ref) = position_node.as_ref().filter(|n| n.name == word) {
        // Клонируем Rc в именованный биндинг, чтобы продлить его жизнь на весь блок
        let node_model_rc = node_ref.model.clone().unwrap_or_else(|| model.clone());
        let node_model = node_model_rc.borrow();
        // Документация берётся из модели, содержащей элемент (не из корневой)
        let doc = node_model.element_doc(&word);

        match node_ref.kind {
            SemanticNodeKind::Variable | SemanticNodeKind::Const | SemanticNodeKind::Port => {
                if let Some(var) = node_model.search_var(&word) {
                    hover_text = make_var_hover(&var, &word, doc);
                }
            }
            SemanticNodeKind::Function | SemanticNodeKind::ExternFunction => {
                if let Some(func_rc) = node_model.search_func(&word) {
                    let func = func_rc.borrow();
                    hover_text = make_func_hover(&func, &word, doc);
                }
            }
            SemanticNodeKind::TypeAlias => {
                // types.get() возвращает &TypeNode напрямую, что правильно для
                // форматирования
                if let Some(ty) = node_model.types.get(&word) {
                    let mut text = format!("```but\ntype {} = {}\n```", word, ty);
                    if !doc.is_empty() {
                        text.push_str("\n\n");
                        text.push_str(&doc.join("\n"));
                    }
                    hover_text = text;
                }
            }
            SemanticNodeKind::Condition => {
                if node_model.search_cond(&word).is_some() {
                    let mut text = format!("```but\ncond {}\n```", word);
                    if !doc.is_empty() {
                        text.push_str("\n\n");
                        text.push_str(&doc.join("\n"));
                    }
                    hover_text = text;
                }
            }
            SemanticNodeKind::Enum => {
                if let Some(enum_node) = node_model.search_enum(&word) {
                    let variants: Vec<String> = enum_node
                        .variants
                        .iter()
                        .map(|(n, v)| format!("  {} = {}", n, v))
                        .collect();
                    let mut text = format!(
                        "```but\nenum {} {{\n{}\n}}\n```",
                        word,
                        variants.join(",\n")
                    );
                    if !doc.is_empty() {
                        text.push_str("\n\n");
                        text.push_str(&doc.join("\n"));
                    }
                    hover_text = text;
                }
            }
            SemanticNodeKind::State | SemanticNodeKind::StartState | SemanticNodeKind::EndState => {
                let kind_label = match node_ref.kind {
                    SemanticNodeKind::StartState => "start state",
                    SemanticNodeKind::EndState => "end state",
                    _ => "state",
                };
                let mut text = format!("```but\n{} {}\n```", kind_label, word);
                if !doc.is_empty() {
                    text.push_str("\n\n");
                    text.push_str(&doc.join("\n"));
                }
                hover_text = text;
            }
            SemanticNodeKind::Model => {
                let mut text = format!("```but\nmodel {}\n```", word);
                if !doc.is_empty() {
                    text.push_str("\n\n");
                    text.push_str(&doc.join("\n"));
                }
                hover_text = text;
            }
            // Цель `ref`-перехода или имя состояния в условии (`S(Ping) = End`) -
            // показываем как объявление состояния.
            SemanticNodeKind::Reference | SemanticNodeKind::ReferenceState => {
                let mut text = format!("```but\nstate {}\n```", word);
                if !doc.is_empty() {
                    text.push_str("\n\n");
                    text.push_str(&doc.join("\n"));
                }
                hover_text = text;
            }
            // Использование имени модели (`= Helper`, `S(Helper)`) - показываем то же,
            // что и на её объявлении.
            SemanticNodeKind::ReferenceModel => {
                let mut text = format!("```but\nmodel {}\n```", word);
                if !doc.is_empty() {
                    text.push_str("\n\n");
                    text.push_str(&doc.join("\n"));
                }
                hover_text = text;
            }
            SemanticNodeKind::ReferenceCondition => {
                // Ищем переменную или функцию в модели, содержащей условие
                if let Some(var) = node_model.search_var(&word) {
                    hover_text = make_var_hover(&var, &word, doc);
                } else if let Some(func_rc) = node_model.search_func(&word) {
                    let func = func_rc.borrow();
                    hover_text = make_func_hover(&func, &word, doc);
                } else {
                    // Запасной вариант: показываем имя как есть
                    hover_text = format!("```but\n{}\n```", word);
                }
            }
            SemanticNodeKind::LocalVar => {
                // Локальная переменная в блоке (enter/exit/always/fn): показываем имя
                hover_text = format!("```but\nvar {} (локальная)\n```", word);
            }
        }
    }

    // Шаг 2 (резервный поиск по имени): применяется если:
    // - курсор на использовании, а не на объявлении (position_node = None)
    // - направленный поиск не дал результата (hover_text пустой)
    if hover_text.is_empty() {
        let doc = borrowed.element_doc(&word);
        // Ищем переменную
        if let Some(var) = borrowed.search_var(&word) {
            hover_text = make_var_hover(&var, &word, doc);
        }
        // Ищем функцию
        else if let Some(func_rc) = borrowed.search_func(&word) {
            let func = func_rc.borrow();
            hover_text = make_func_hover(&func, &word, doc);
        }
        // Ищем псевдоним типа
        else if let Some(ty) = borrowed.types.get(&word) {
            let mut text = format!("```but\ntype {} = {}\n```", word, ty);
            if !doc.is_empty() {
                text.push_str("\n\n");
                text.push_str(&doc.join("\n"));
            }
            hover_text = text;
        }
        // Ищем именованное условие
        else if borrowed.search_cond(&word).is_some() {
            let mut text = format!("```but\ncond {}\n```", word);
            if !doc.is_empty() {
                text.push_str("\n\n");
                text.push_str(&doc.join("\n"));
            }
            hover_text = text;
        }
        // Ищем перечисление
        else if let Some(enum_node) = borrowed.search_enum(&word) {
            let variants: Vec<String> = enum_node
                .variants
                .iter()
                .map(|(n, v)| format!("  {} = {}", n, v))
                .collect();
            let mut text = format!(
                "```but\nenum {} {{\n{}\n}}\n```",
                word,
                variants.join(",\n")
            );
            if !doc.is_empty() {
                text.push_str("\n\n");
                text.push_str(&doc.join("\n"));
            }
            hover_text = text;
        }
        // Ищем состояние
        else if borrowed.search_state(&word).is_some() {
            let mut text = format!("```but\nstate {}\n```", word);
            if !doc.is_empty() {
                text.push_str("\n\n");
                text.push_str(&doc.join("\n"));
            }
            hover_text = text;
        }
        // Ищем вариант перечисления
        else if let Some((enum_node, value)) = borrowed.search_enum_variant(&word) {
            hover_text = format!("```but\n{}::{} = {}\n```", enum_node.name, word, value);
        }
        // Ищем модель
        else if borrowed.search_model(&word).is_some() {
            let mut text = format!("```but\nmodel {}\n```", word);
            if !doc.is_empty() {
                text.push_str("\n\n");
                text.push_str(&doc.join("\n"));
            }
            hover_text = text;
        }
        // Встроенный тип (u8, i32, bit, bool и т.д.)
        else if let Some((_, description)) =
            TAKT_BUILTIN_TYPES.iter().find(|(t, _)| *t == word.as_str())
        {
            hover_text = format!("```but\n{}\n```\n\n{}", word, description);
        }
    }

    if hover_text.is_empty() {
        return None;
    }

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover_text,
        }),
        range: None,
    })
}
