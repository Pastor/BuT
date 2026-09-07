//! Позиции: перевод между смещениями Takt и координатами LSP (UTF-16).
//!
//! Часть модуля `lsp`.

use super::*;

/// Конвертирует байтовое смещение в LSP `Range`.
pub fn offset_to_range(source: &str, start: usize, end: usize) -> Range {
    Range {
        start: offset_to_position(source, start),
        end: offset_to_position(source, end),
    }
}

/// Конвертирует байтовое смещение в LSP `Position` (строка + столбец в кодовых единицах
/// UTF-16).
///
/// Протокол LSP  требует, чтобы поле `character` позиции
/// выражалось в **кодовых единицах UTF-16**, а не в байтах или кодовых точках Unicode.
/// Для ASCII-символов все три единицы совпадают; различие возникает при наличии
/// многобайтовых UTF-8 символов (кириллица, CJK, эмодзи, ...).
///
/// Если `offset` указывает на середину многобайтового символа (невалидная
/// char-граница), функция безопасно отступает до ближайшей предшествующей границы
/// символа.
///
/// # Примеры
///
/// ```
/// # #[cfg(feature = "lsp")]
/// # {
/// use takt_lang::lsp::offset_to_position;
/// use lsp_types::Position;
///
/// // ASCII: байтовое смещение == UTF-16-столбец
/// assert_eq!(offset_to_position("hello", 3), Position::new(0, 3));
///
/// // Многострочный текст: смещение 7 - второй байт второй строки
/// assert_eq!(offset_to_position("line1\nab", 7), Position::new(1, 1));
///
/// // Кириллица: 'А' занимает 2 байта в UTF-8, но 1 кодовую единицу в UTF-16
/// // "аб" = [0xD0,0x90, 0xD0,0x91] - 4 байта, 2 символа
/// let src = "аб";
/// assert_eq!(offset_to_position(src, 4), Position::new(0, 2)); // конец строки
/// assert_eq!(offset_to_position(src, 2), Position::new(0, 1)); // после 'А'
/// # }
/// ```
pub fn offset_to_position(source: &str, offset: usize) -> Position {
    // Зажимаем до валидной границы символа UTF-8
    let offset = {
        let clamped = offset.min(source.len());
        // Если попали в середину многобайтового символа - откатываемся назад
        (0..=clamped)
            .rev()
            .find(|&i| source.is_char_boundary(i))
            .unwrap_or(0)
    };
    let prefix = &source[..offset];
    let line = prefix.matches('\n').count() as u32;
    // Находим начало текущей строки (байт сразу после последнего '\n')
    let line_start = prefix.rfind('\n').map(|nl| nl + 1).unwrap_or(0);
    // LSP требует столбец в кодовых единицах UTF-16
    let col_utf16: u32 = prefix[line_start..]
        .chars()
        .map(|c| c.len_utf16() as u32)
        .sum();
    Position::new(line, col_utf16)
}

/// Конвертирует UTF-16 смещение символа в байтовое смещение внутри строки `s`.
///
/// Возвращает `Some(byte_offset)`, если `utf16_offset` не выходит за пределы строки,
/// иначе `None`.
///
/// # Примеры
///
/// ```
/// // ASCII: 1 байт = 1 кодовая единица UTF-16
/// // utf16_offset 3 -> байт 3
///
/// // "абв": каждый символ - 2 байта UTF-8, 1 кодовая единица UTF-16
/// // utf16_offset 2 -> байт 4
/// ```
pub(super) fn utf16_to_byte_offset(s: &str, utf16_offset: usize) -> Option<usize> {
    let mut utf16_count = 0usize;
    for (byte_i, ch) in s.char_indices() {
        if utf16_count >= utf16_offset {
            return Some(byte_i);
        }
        utf16_count += ch.len_utf16();
    }
    // Если точно достигли конца строки
    if utf16_count >= utf16_offset {
        Some(s.len())
    } else {
        None
    }
}

/// Конвертирует LSP-позицию (строка + UTF-16 символ) в байтовое смещение в исходном
/// тексте.
///
/// Протокол LSP использует `Position { line, character }`, где `character` - смещение в
/// кодовых единицах UTF-16 от начала строки. Функция переводит эту позицию в байтовое
/// смещение от начала файла, пригодное для работы с [`Location::Source`].
///
/// Возвращает `None`, если строка с номером `position.line` не существует в `source`.
///
/// # Примеры
///
/// ```
/// # #[cfg(feature = "lsp")]
/// # {
/// use takt_lang::lsp::position_to_offset;
/// use lsp_types::Position;
///
/// let src = "hello\nworld";
/// // Строка 0, символ 3 -> байт 3
/// assert_eq!(position_to_offset(src, Position::new(0, 3)), Some(3));
/// // Строка 1 начинается с байта 6 ("hello\n"), символ 2 -> байт 8
/// assert_eq!(position_to_offset(src, Position::new(1, 2)), Some(8));
/// // Несуществующая строка -> None
/// assert_eq!(position_to_offset(src, Position::new(99, 0)), None);
/// # }
/// ```
pub fn position_to_offset(source: &str, position: Position) -> Option<usize> {
    let target_line = position.line as usize;
    let mut line_start = 0usize;
    let mut current_line = 0usize;

    for (i, c) in source.char_indices() {
        if current_line == target_line {
            // Нашли начало нужной строки - определяем столбец
            let line_text = source[line_start..].lines().next().unwrap_or("");
            let col_byte = utf16_to_byte_offset(line_text, position.character as usize)
                .unwrap_or(line_text.len());
            return Some(line_start + col_byte);
        }
        if c == '\n' {
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Обрабатываем последнюю строку (без завершающего '\n')
    if current_line == target_line {
        let line_text = source[line_start..].lines().next().unwrap_or("");
        let col_byte =
            utf16_to_byte_offset(line_text, position.character as usize).unwrap_or(line_text.len());
        return Some(line_start + col_byte);
    }

    None
}

/// Возвращает семантический узел по LSP-позиции курсора.
///
/// Строит [`SemanticIndex`](semantic::index::SemanticIndex) из переданной семантической
/// модели и выполняет поиск наиболее конкретного узла, объявление которого покрывает
/// позицию курсора. Более точен, чем поиск по имени слова под курсором: учитывает
/// точные диапазоны объявлений и избегает неоднозначностей при совпадении имён разных
/// элементов (например, переменная и состояние с одним именем в разных областях
/// видимости).
///
/// Возвращает `None`, если:
/// - `position` выходит за пределы исходного текста.
/// - Ни один семантический узел не покрывает данную позицию (например, курсор
///   стоит на ключевом слове или пробеле).
///
/// # Пример
///
/// ```
/// # #[cfg(feature = "lsp")]
/// # {
/// use takt_lang::parse;
/// use takt_lang::semantic::tree::construct_model;
/// use takt_lang::lsp::node_at_position;
/// use takt_lang::semantic::index::SemanticNodeKind;
/// use lsp_types::Position;
///
/// let src = "var counter: bit := false; start S;";
/// let (ast, _) = parse(src, 0).unwrap();
/// let model = construct_model(&ast, None, &[]).unwrap();
///
/// // Позиция 4 - символ 'c' в "counter"
/// let node = node_at_position(src, Position::new(0, 4), &model);
/// assert!(node.is_some());
/// let node = node.unwrap();
/// assert_eq!(node.name, "counter");
/// assert_eq!(node.kind, SemanticNodeKind::Variable);
/// # }
/// ```
pub fn node_at_position(
    source: &str,
    position: Position,
    model: &std::rc::Rc<RefCell<ModelNode>>,
) -> Option<SemanticNodeRef> {
    use crate::semantic::index::SemanticIndex;
    let offset = position_to_offset(source, position)?;
    let index = SemanticIndex::build(model);
    index.node_at_offset(offset).cloned()
}
