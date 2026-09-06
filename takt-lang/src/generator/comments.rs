//! Перенос комментариев автора модели в вывод.
//!
//! # Что переносится
//!
//! **Ведущие** комментарии оператора - те, что стоят на своих строках
//! непосредственно перед ним, и **хвостовой** - тот, что стоит на строке
//! оператора после него. Классификация та же, что у форматтера
//! ([`crate::format`]): она уже описана словами канона и
//! проверена на всём корпусе, и заводить вторую разошедшуюся бы молча.
//!
//! # Форма принадлежит цели, а текст - автору
//!
//! Комментарий Takt записан как `// ...` или `/* ... */`, а в выводе он обязан
//! выглядеть комментарием **целевого** языка: `(* ... *)` у Structured Text, `'` у
//! PlantUML, `//` у прочих. Поэтому носитель снимает авторские маркеры, оставляет текст
//! и обрамляет его [`CommentStyle`](super::header::CommentStyle).
//!
//! **"Как есть" невозможно буквально.** В `examples/stacker.takt:203` авторский
//! комментарий содержит `(cmd_target_*)`: перенесённый дословно в ST, он закроется на
//! `*)`, а хвост строки уедет в код - `iec2c` отвергнет файл при нулевом коде возврата
//! `taktc`. Поэтому закрывающая последовательность целевого языка **обезвреживается**
//! ([`defuse`]).
//!
//! # Один узел - несколько мест
//!
//! Комментарий повторяется в **каждом** месте, куда попал размноженный узел:
//! подстановка функции, разворот цикла у `sv`, специализация параметров. Поэтому
//! носитель не ведёт "выдан/не выдан": на один и тот же оператор он отвечает одинаково
//! сколько угодно раз.

use crate::diagnostics::Location;
use crate::generator::header::CommentStyle;
use crate::parser::ast::Comment;

/// Комментарии исходника, сшитые с позициями (одна запись на файл).
#[derive(Debug, Default, Clone)]
pub struct SourceComments {
    /// Номер файла, которому принадлежит текст.
    ///
    /// Обязателен: позиция узла приходит с номером файла, и у импортированного
    /// объявления она указывает в чужой текст. Приложи такое смещение к своему
    /// исходнику - и срез попадёт внутрь многобайтового символа: инструмент падает
    /// паникой.
    file_no: u32,
    /// Текст исходника - по нему определяется, что отделяет комментарий от узла.
    source: String,
    /// Комментарии в порядке появления: начало, конец, сырой текст.
    items: Vec<(usize, usize, String)>,
}

impl SourceComments {
    /// Собирает носитель из исходника и комментариев, отданных разбором.
    pub fn new(source: &str, comments: &[Comment]) -> Self {
        Self::for_file(0, source, comments)
    }

    /// То же, но для названного файла реестра (корневой - `0`).
    pub fn for_file(file_no: u32, source: &str, comments: &[Comment]) -> Self {
        let mut items: Vec<(usize, usize, String)> = comments
            .iter()
            .filter_map(|c| {
                let loc = match c {
                    Comment::Line(loc, _) | Comment::DocLine(loc, _) | Comment::Block(loc, _) => {
                        loc
                    }
                };
                match loc {
                    Location::Source(_, start, end) => {
                        Some((*start as usize, *end as usize, c.value().to_string()))
                    }
                    // Комментарий без координаты перенести некуда: его место в выводе
                    // определяется только позицией.
                    _ => None,
                }
            })
            .collect();
        items.sort_by_key(|(start, _, _)| *start);
        Self {
            file_no,
            source: source.to_string(),
            items,
        }
    }

    /// Смещения этой позиции применимы к нашему тексту?
    ///
    /// Чужой файл - не отказ и не пустой комментарий, а честное "переносить нечего":
    /// текста импортированного файла у носителя нет.
    fn ours(&self, loc: Location) -> Option<(usize, usize)> {
        match loc {
            Location::Source(file_no, start, end) if file_no == self.file_no => {
                Some((start as usize, end as usize))
            }
            _ => None,
        }
    }

    /// Пуст ли носитель - тогда печатники не тратят время на поиск.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Ведущие комментарии оператора: те, что стоят прямо перед ним.
    ///
    /// "Прямо перед" - значит между концом комментария и началом узла нет ничего, кроме
    /// пробелов и переводов строк. Идём назад, пока такие комментарии находятся,
    /// поэтому блок из нескольких строк переносится целиком и в исходном порядке.
    pub fn leading(&self, loc: Location, style: CommentStyle) -> Vec<String> {
        let Some((start, _)) = self.ours(loc) else {
            return Vec::new();
        };
        // Отсчёт идёт от начала строки, а не от самой позиции: у объявления координата
        // указывает на имя (`struct St` - на `St`), и поиск назад упёрся бы в слово
        // `struct`.
        let mut at = self.line_start(start);
        let mut picked: Vec<&str> = Vec::new();
        loop {
            let gap_start = self.skip_space_back(at);
            let Some((s, _, text)) = self
                .items
                .iter()
                .find(|(_, end, _)| *end == gap_start && gap_start < at)
            else {
                break;
            };
            // Комментарий обязан начинаться на своей строке. Иначе хвостовой
            // комментарий соседа (`x := 1; // счётчик`) стал бы ведущим для следующего
            // оператора - то есть уехал бы к чужому коду.
            let head = self.line_start(*s);
            if head != *s
                && !self
                    .source
                    .get(head..*s)
                    .is_none_or(|gap| gap.trim().is_empty())
            {
                break;
            }
            picked.push(text);
            at = self.line_start(*s);
        }
        picked.reverse();
        wrap_all(&picked, style)
    }

    /// Смещение начала строки, в которой лежит `at`.
    fn line_start(&self, at: usize) -> usize {
        // `get` вместо индексирования: смещение, пришедшее не с границы символа, роняло
        // бы инструмент паникой. Своего файла это не касается (лексер границы
        // соблюдает), но вторая линия обороны здесь дешева, а цена ошибки - падение
        // `taktc` вместо вывода.
        self.source
            .get(..at.min(self.source.len()))
            .and_then(|head| head.rfind('\n'))
            .map_or(0, |i| i + 1)
    }

    /// Хвостовой комментарий оператора: на его строке, после него.
    ///
    /// Ищется по **строке**, а не по расстоянию: комментарий за концом оператора, но с
    /// переводом строки между ними, принадлежит следующему оператору - так его читает и
    /// автор, и форматтер.
    pub fn trailing(&self, loc: Location, style: CommentStyle) -> Option<String> {
        let (_, end) = self.ours(loc)?;
        let (_, _, text) = self.items.iter().find(|(s, _, _)| {
            *s >= end
                && !self
                    .source
                    .get(end..*s)
                    .is_some_and(|gap| gap.contains('\n'))
        })?;
        // Хвостовой комментарий обязан помещаться В строку кода: многострочная блочная
        // форма оставила бы в ней открывающее `(*` без закрытия.
        let body: Vec<String> = strip_markers(text)
            .into_iter()
            .map(|line| defuse(&line, style))
            .collect();
        if body.is_empty() {
            return None;
        }
        Some(style.wrap_inline(&body.join(" ")))
    }

    /// Смещение начала пробельного участка, кончающегося в `at`.
    fn skip_space_back(&self, at: usize) -> usize {
        let bytes = self.source.as_bytes();
        let mut i = at.min(bytes.len());
        while i > 0 && bytes[i - 1].is_ascii_whitespace() {
            i -= 1;
        }
        i
    }
}

/// Снимает авторские маркеры и обрамляет текст комментарием целевого языка.
fn wrap_all(raw: &[&str], style: CommentStyle) -> Vec<String> {
    let lines: Vec<String> = raw
        .iter()
        .flat_map(|text| strip_markers(text))
        .map(|line| defuse(&line, style))
        .collect();
    if lines.is_empty() {
        return Vec::new();
    }
    style.wrap(&lines)
}

/// Снимает `//`, `///` и `/* */`, оставляя строки содержания.
///
/// Пустые строки внутри блочного комментария сохраняются: автор поставил их намеренно.
/// А комментарий, состоящий **только** из маркеров, не даёт ни одной строки - печатать
/// пустой комментарий проверка запрещает (`G3`).
fn strip_markers(raw: &str) -> Vec<String> {
    let text = raw.trim();
    let body = if let Some(rest) = text.strip_prefix("///") {
        rest.to_string()
    } else if let Some(rest) = text.strip_prefix("//") {
        rest.to_string()
    } else if let Some(rest) = text.strip_prefix("/*") {
        rest.strip_suffix("*/").unwrap_or(rest).to_string()
    } else {
        text.to_string()
    };
    let lines: Vec<String> = body
        .lines()
        .map(|l| l.trim().trim_start_matches('*').trim().to_string())
        .collect();
    if lines.iter().all(String::is_empty) {
        return Vec::new();
    }
    lines
}

/// Обезвреживает последовательность, закрывающую комментарий целевого языка.
///
/// Это не косметика: `*)` внутри текста закрывает комментарий ST досрочно, и остаток
/// строки становится кодом.
pub fn defuse(text: &str, style: CommentStyle) -> String {
    match style {
        // В ST закрывает `*)`; в C - `*/`. Разрываем пробелом: текст остаётся читаемым,
        // а последовательности больше нет.
        CommentStyle::IecBlock => text.replace("*)", "* )"),
        CommentStyle::Slashes => text.replace("*/", "* /"),
        // Построчный комментарий PlantUML закрывается переводом строки - его в строке
        // текста не бывает по построению `strip_markers`.
        CommentStyle::Quote => text.to_string(),
    }
}

thread_local! {
    /// Комментарии исходника, действующие в текущей генерации.
    ///
    /// Носитель потоковый - тот же приём, что у позиции оператора
    /// ([`crate::generator::site`]) и по той же причине: печатники операторов у четырёх
    /// целей принимают три разных контекста (`ModelNode`, `Scope`, карту), и
    /// протаскивание комментариев через все сигнатуры стоило бы правки десятков функций
    /// ради данных, которые нужны в двух местах на цель.
    ///
    /// Сброс обязателен: состояние переживает вызов, и следующая генерация получила бы
    /// комментарии чужого исходника. Ставит и снимает его диспетчер `generate_texts` -
    /// там же, где `site::reset`.
    static ACTIVE: std::cell::RefCell<Option<std::rc::Rc<SourceComments>>> =
        const { std::cell::RefCell::new(None) };
}

/// Делает комментарии исходника действующими до [`reset`].
pub(crate) fn activate(comments: Option<std::rc::Rc<SourceComments>>) {
    ACTIVE.with(|cell| *cell.borrow_mut() = comments);
}

/// Снимает носитель. Обязателен на всех путях выхода из генерации.
pub(crate) fn reset() {
    ACTIVE.with(|cell| *cell.borrow_mut() = None);
}

/// Ведущие комментарии оператора или объявления на активном носителе.
pub(crate) fn leading(loc: Location, style: CommentStyle) -> Vec<String> {
    ACTIVE.with(|cell| {
        cell.borrow()
            .as_ref()
            .map_or_else(Vec::new, |c| c.leading(loc, style))
    })
}

/// Хвостовой комментарий оператора на активном носителе.
pub(crate) fn trailing(loc: Location, style: CommentStyle) -> Option<String> {
    ACTIVE.with(|cell| cell.borrow().as_ref().and_then(|c| c.trailing(loc, style)))
}

/// Печатает оператор, обрамив его комментариями автора.
///
/// Ведущие идут своими строками перед оператором, хвостовой приклеивается к
/// **последней** его строке - там, где автор его и написал.
///
/// Хвостовой требует печати в буфер (`Printer::fork`): дотянуться до конца уже
/// выведенной строки нельзя, а печатать его следующей строкой значило бы отдать
/// комментарий следующему оператору. Приём тот же, которым цели собирают тело перед
/// решением "печатать ли" (0473, 0509).
///
/// Виден только внутри крейта: он принимает `Printer`, а тот - деталь печати, и в
/// открытом API ей не место (иначе `clippy` требует у неё `Debug`).
///
/// Помощник один на все цели: копии обрамления разошлись бы молча, а место печати
/// комментария - это место в выводе, то есть наблюдаемое поведение.
pub(crate) fn emit_around<F>(
    printer: &mut crate::generator::indent::Printer,
    loc: Location,
    style: CommentStyle,
    body: F,
) -> Result<(), crate::diagnostics::Diagnostic>
where
    F: FnOnce(&mut crate::generator::indent::Printer) -> Result<(), crate::diagnostics::Diagnostic>,
{
    for line in leading(loc, style) {
        printer.ident(&line).nl();
    }
    let Some(tail) = trailing(loc, style) else {
        return body(printer);
    };
    let mut buffer = String::new();
    {
        let mut forked = printer.fork(&mut buffer);
        body(&mut forked)?;
    }
    printer.print(&attach_trailing(&buffer, &tail));
    Ok(())
}

/// Приклеивает хвостовой комментарий к последней непустой строке текста.
///
/// Если печатать нечего (оператор не дал вывода - так бывает у пропускаемых целью
/// узлов), комментарий тоже не печатается: висящий комментарий без кода читается как
/// потерянный оператор.
pub(crate) fn attach_trailing(text: &str, comment: &str) -> String {
    let Some(cut) = text.rfind(|c: char| !c.is_whitespace()) else {
        return text.to_string();
    };
    let end = text[cut..].find('\n').map_or(text.len(), |i| cut + i);
    let mut out = String::with_capacity(text.len() + comment.len() + 1);
    out.push_str(&text[..end]);
    out.push(' ');
    out.push_str(comment);
    out.push_str(&text[end..]);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn comments(src: &str) -> SourceComments {
        let (_, list) = crate::parse(src, 0).expect("разбор пробы");
        SourceComments::new(src, &list)
    }

    const SRC: &str = "\
model M {
    var x: u8 := 0;
    start S {
        always {
            // ведущий комментарий
            x := x + 1; // хвостовой
        }
    }
}
";

    /// Ведущий комментарий находится по позиции оператора и обрамляется целью.
    #[test]
    fn leading_comment_is_found_and_wrapped() {
        let c = comments(SRC);
        let at = SRC.find("x := x + 1;").expect("оператор в пробе") as u32;
        let loc = Location::Source(0, at, at + 11);
        assert_eq!(
            c.leading(loc, CommentStyle::Slashes),
            vec!["// ведущий комментарий"]
        );
        assert_eq!(
            c.leading(loc, CommentStyle::IecBlock),
            vec!["(*", " * ведущий комментарий", " *)"]
        );
    }

    /// Хвостовой комментарий берётся со строки оператора, а не со следующей.
    #[test]
    fn trailing_comment_belongs_to_its_line() {
        let c = comments(SRC);
        let at = SRC.find("x := x + 1;").expect("оператор в пробе") as u32;
        let loc = Location::Source(0, at, at + 11);
        assert_eq!(
            c.trailing(loc, CommentStyle::Slashes).as_deref(),
            Some("// хвостовой")
        );
    }

    /// Оператор без комментария их не получает - и это не отказ.
    #[test]
    fn statement_without_comments_gets_none() {
        let c = comments(SRC);
        let at = SRC.find("var x: u8").expect("объявление в пробе") as u32;
        let loc = Location::Source(0, at, at + 9);
        assert!(c.leading(loc, CommentStyle::Slashes).is_empty());
    }

    /// Блок из нескольких строк переносится целиком и в исходном порядке.
    #[test]
    fn several_leading_lines_keep_their_order() {
        let src = "\
model M {
    start S {
        always {
            // первая
            // вторая
            probe := 1;
        }
    }
    out probe: u8;
}
";
        let c = comments(src);
        let at = src.find("probe := 1;").expect("оператор") as u32;
        let loc = Location::Source(0, at, at + 11);
        assert_eq!(
            c.leading(loc, CommentStyle::Slashes),
            vec!["// первая", "// вторая"]
        );
    }

    /// Закрывающая последовательность обезвреживается - иначе вывод невалиден.
    ///
    /// Вход взят с живого корпуса (`examples/stacker.takt:203`), где автор написал
    /// `(cmd_target_*)`.
    #[test]
    fn closing_sequence_is_defused() {
        assert_eq!(
            defuse("координаты (cmd_target_*)", CommentStyle::IecBlock),
            "координаты (cmd_target_* )"
        );
        assert_eq!(defuse("маска a*/b", CommentStyle::Slashes), "маска a* /b");
    }

    /// Хвостовой комментарий соседа не становится ведущим для следующего.
    ///
    /// Класс появился вместе с отсчётом от начала строки: между концом комментария `//
    /// счётчик` и началом следующей строки - только перевод строки, и наивный поиск
    /// назад подхватил бы его. Комментарий уехал бы к чужому коду, где ничего не
    /// объясняет.
    #[test]
    fn trailing_comment_of_a_neighbour_is_not_leading() {
        let src = "\
model M {
    out probe: u8;
    var n: u8 := 0;
    start S {
        always {
            n := n + 1; // счётчик тактов
            probe := n;
        }
    }
}
";
        let c = comments(src);
        let at = src.find("probe := n;").expect("второй оператор") as u32;
        let loc = Location::Source(0, at, at + 11);
        assert!(
            c.leading(loc, CommentStyle::Slashes).is_empty(),
            "хвостовой комментарий предыдущей строки не принадлежит этому оператору"
        );
    }

    /// Комментарий перед объявлением находится, хотя позиция указывает на имя.
    #[test]
    fn leading_comment_of_a_declaration_is_found() {
        let src = "\
// Состояние контура регулятора.
struct St { a: u8 }

model M { start S; }
";
        let c = comments(src);
        // позиция объявления - это позиция имени (`St`), как её ставит семантика
        let at = src.find("St {").expect("имя структуры") as u32;
        let loc = Location::Source(0, at, at + 2);
        assert_eq!(
            c.leading(loc, CommentStyle::Slashes),
            vec!["// Состояние контура регулятора."]
        );
    }

    /// Хвостовой комментарий помещается В строку, а не открывает блок.
    ///
    /// Класс пойман проверкой цели: блочная форма IEC многострочна, и её первая строка -
    /// голое `(*`. Приклеенная к строке кода, она открывала комментарий и не закрывала
    /// его, а `iec2c` отвечал "invalid statement in ST statement" - невалидный вывод
    /// при нулевом коде возврата `taktc`.
    #[test]
    fn a_trailing_comment_fits_one_line() {
        let c = comments(SRC);
        let at = SRC.find("x := x + 1;").expect("оператор в пробе") as u32;
        let loc = Location::Source(0, at, at + 11);
        let tail = c
            .trailing(loc, CommentStyle::IecBlock)
            .expect("хвостовой комментарий");
        assert_eq!(tail, "(* хвостовой *)");
        assert!(
            !tail.contains('\n'),
            "перевода строки быть не должно: {tail}"
        );
    }

    /// Позиция чужого файла не роняет инструмент и ничего не переносит.
    ///
    /// Класс пойман прогоном: у импортированного объявления координата указывает в
    /// текст другого файла, и приложенное к своему исходнику смещение попадало внутрь
    /// многобайтового символа - `taktc` падал паникой (`is not a char boundary`) на
    /// живом примере документа. Кириллица в исходниках проекта повсеместна, так что
    /// случай не экзотический.
    #[test]
    fn a_position_from_another_file_is_not_ours() {
        let c = comments(SRC);
        // тот же диапазон, но файл иной - переносить нечего
        let at = SRC.find("x := x + 1;").expect("оператор в пробе") as u32;
        let foreign = Location::Source(7, at, at + 11);
        assert!(c.leading(foreign, CommentStyle::Slashes).is_empty());
        assert!(c.trailing(foreign, CommentStyle::Slashes).is_none());
    }

    /// Смещение не с границы символа не роняет носитель.
    ///
    /// Вторая линия обороны к предыдущему тесту: лексер границы соблюдает, но цена
    /// ошибки - падение инструмента вместо вывода, и проверка дешева.
    #[test]
    fn an_offset_inside_a_character_does_not_panic() {
        let src = "model M { start S; } // хвост по-русски\n";
        let c = comments(src);
        let inside = src.find("русски").expect("кириллица") as u32 + 1;
        let _ = c.leading(
            Location::Source(0, inside, inside + 1),
            CommentStyle::Slashes,
        );
        let _ = c.trailing(
            Location::Source(0, inside, inside + 1),
            CommentStyle::Slashes,
        );
    }

    /// Пустой комментарий не даёт ни одной строки: проверка `G3` их запрещает.
    #[test]
    fn empty_comment_produces_nothing() {
        assert!(strip_markers("//").is_empty());
        assert!(strip_markers("/* */").is_empty());
        assert!(wrap_all(&["//"], CommentStyle::Slashes).is_empty());
    }

    /// Ответ носителя не зависит от числа обращений: размноженный узел получает
    /// комментарий в каждом месте.
    #[test]
    fn repeated_queries_answer_the_same() {
        let c = comments(SRC);
        let at = SRC.find("x := x + 1;").expect("оператор в пробе") as u32;
        let loc = Location::Source(0, at, at + 11);
        let first = c.leading(loc, CommentStyle::Slashes);
        let second = c.leading(loc, CommentStyle::Slashes);
        assert_eq!(first, second);
        assert!(!first.is_empty());
    }
}
