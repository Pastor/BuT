//! Канонический форматтер `.takt` - ядро печати.
//!
//! # Откуда печатаем
//!
//! Печать идёт **от АСД** (`ast::Model`), а не от семантического дерева: `ModelNode`
//! нормализует имена (`Root`, snake_case) и теряет исходную раскладку - для форматтера
//! непригоден. Семантический проход не запускается вовсе, поэтому форматтер работает и
//! на семантически некорректных, но синтаксически валидных файлах (требование R1).
//!
//! # Ошибка вместо тихой потери
//!
//! Непокрытый узел даёт [`FormatError`], а не пустую строку. Урок: молчаливый пропуск
//! неотличим от корректной работы. Там, где перечисление помечено `#[non_exhaustive]`
//! (например, `ast::Statement`), ветка `_` вынужденная - она **отказывает**, а не
//! печатает пустоту.

mod comments;
mod expr;
/// Печать блока `formula` - своего языка внутри Takt.
mod formula;
mod literal;
mod stmt;

use crate::parser::ast;

/// Единица отступа канонического стиля.
pub(crate) const INDENT: &str = "    ";

/// Ошибка форматирования.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatError {
    /// Исходник не разобран: диагностика разбора **как есть**.
    ///
    /// Именно `Vec<Diagnostic>`, а не строка. Формат диагностики - её собственное
    /// свойство, а позицию `строка:колонка` умеет проставить только тот, кто знает путь
    /// к файлу, - то есть вызывающий.
    ///
    /// Диагностик может быть несколько: разбор их накапливает.
    Parse(Vec<crate::diagnostics::Diagnostic>),
    /// Узел АСД пока не поддержан печатью - диагностика **`FM-001`**.
    ///
    /// Явный отказ: печатать "что-нибудь" вместо неизвестного узла означало бы молча
    /// портить исходник пользователя.
    ///
    /// Несёт [`Diagnostic`](crate::diagnostics::Diagnostic) - позицию, код и
    /// текст. Прежде здесь была голая строка: пользователь узнавал
    /// **вид** узла, но не место, и в большом файле искал его грепом. Позиция у
    /// узла есть всегда - терялась она на границе типа ошибки.
    Unsupported(crate::diagnostics::Diagnostic),
}

impl std::fmt::Display for FormatError {
    /// Ветвь `Parse` печатает диагностики **без преисправления позиции**: пути к файлу на
    /// этом уровне нет, а выдумывать координаты нельзя (докблок
    /// [`crate::diagnostics::position_prefix`] - пользователь пошёл бы искать ошибку в
    /// первой строке).
    ///
    /// Вызывающий, который путь знает, обязан идти **структурным** путём: проставить
    /// файл (`with_file_if_unset`) и напечатать каждую диагностику через
    /// [`crate::diagnostics::format_compile_error`]. Так делает `taktc fmt`. Печать
    /// через `Display` теряет позицию - молча.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatError::Parse(diagnostics) => {
                let text = diagnostics
                    .iter()
                    .map(crate::diagnostics::format_compile_error)
                    .collect::<Vec<_>>()
                    .join("\n");
                write!(f, "{text}")
            }
            FormatError::Unsupported(diagnostic) => {
                write!(
                    f,
                    "{}",
                    crate::diagnostics::format_compile_error(diagnostic)
                )
            }
        }
    }
}

impl std::error::Error for FormatError {}

/// Текст условия "как написал автор" - для диагностик о нём.
///
/// Печатает тем же принтером, что и `taktc fmt`, поэтому сообщение показывает
/// **исходную запись**, а не внутреннее представление. `None` - узел печатать
/// нечем; тогда вызывающий обязан обойтись без цитаты, а **не** подставлять
/// `Debug`-дамп.
pub(crate) fn condition_text(cond: &ast::Condition) -> Option<String> {
    expr::condition(cond).ok()
}

/// Строит отказ печати узла - диагностику **`FM-001`** с позицией.
///
/// Единственная точка, где рождается [`FormatError::Unsupported`]: код и текст пишутся
/// **один раз**. Шесть мест отказа, собиравших сообщение порознь, разошлись бы
/// формулировками - а сообщение о непокрытом узле пользователь видит вместо своего
/// файла и должен по нему понять, что именно не печатается.
pub(crate) fn unsupported(loc: crate::diagnostics::Location, node: &str) -> FormatError {
    FormatError::Unsupported(
        crate::diagnostics::Diagnostic::error(
            loc,
            format!("печать узла '{node}' пока не поддерживается форматтером"),
        )
        .with_code("FM-001"),
    )
}

/// Накопитель канонического текста с отступами и курсором комментариев.
pub(crate) struct Out<'a> {
    buf: String,
    depth: usize,
    comments: comments::Comments<'a>,
    /// Исходник - нужен для восстановления осмысленных пустых строк.
    source: &'a str,
    /// Следующая строка продолжает текущую - печатается без отступа.
    ///
    /// Взводится [`Out::join`] ровно на одну строку; снимает его первая же
    /// [`Out::line`]. Живёт полем, а не параметром печати блока, потому что продолжение
    /// приходит от рекурсивного вызова: заголовок `if cond {` в цепочке `} else if cond
    /// {` печатает тот же `stmt::print`, который не знает, что его позвали из ветки
    /// `else`.
    glued: bool,
}

impl<'a> Out<'a> {
    fn new(source: &'a str, items: &[ast::Comment]) -> Self {
        Self {
            buf: String::with_capacity(source.len() + 64),
            depth: 0,
            comments: comments::Comments::new(source, items),
            source,
            glued: false,
        }
    }

    /// Печатает строку, **привязанную к узлу**: сперва ведущие комментарии узла, затем
    /// сама строка, затем хвостовой комментарий той же строки исходника.
    ///
    /// Это единственный путь печати содержательных строк - так комментарий не может
    /// "потеряться" из-за того, что о нём забыли в конкретной ветке.
    pub(crate) fn node_line(&mut self, loc: &crate::diagnostics::Location, text: &str) {
        let Some((start, end)) = comments::span(loc) else {
            self.line(text);
            return;
        };
        for (offset, text) in self.comments.leading(start) {
            self.blank_at(offset);
            self.line(&text);
        }
        match self.comments.trailing(end) {
            Some(trailing) => self.line(&format!("{text} {trailing}")),
            None => self.line(text),
        }
    }

    /// То же, что [`Out::node_line`], но хвостовой комментарий берётся лишь
    /// **внутри** границы `limit` (см. `comments::trailing_within`).
    ///
    /// Нужен для частей записи, чей `Location` вложен в чужой: вариант перечисления в
    /// однострочной записи стоит на той же строке, что и комментарий обо всём
    /// перечислении, и без границы забрал бы его себе.
    pub(crate) fn node_line_within(
        &mut self,
        loc: &crate::diagnostics::Location,
        text: &str,
        limit: usize,
    ) {
        let Some((start, end)) = comments::span(loc) else {
            self.line(text);
            return;
        };
        for (offset, text) in self.comments.leading(start) {
            self.blank_at(offset);
            self.line(&text);
        }
        match self.comments.trailing_within(end, limit) {
            Some(trailing) => self.line(&format!("{text} {trailing}")),
            None => self.line(text),
        }
    }

    /// Печатает комментарии, начинающиеся **до** `offset`, на текущем отступе.
    ///
    /// Нужен для комментария, стоящего **последней строкой тела**: ведущих комментариев
    /// ему никто не выдаст (следующего оператора нет), а `leading()` следующего
    /// Элемента выдал бы его уже за закрывающей скобкой - то есть привязал бы к чужому
    /// узлу. Вызывается перед печатью `}` блока, с концом блока в качестве границы.
    pub(crate) fn comments_before(&mut self, offset: usize) {
        for (start, text) in self.comments.leading(offset) {
            self.blank_at(start);
            self.line(&text);
        }
    }

    /// Пустая строка - не более одной подряд (правило канона).
    fn blank(&mut self) {
        // Строка, ждущая продолжения (`} else `), пустой строкой не разрывается: иначе
        // восстановление зазора из исходника разорвало бы K&R-раскладку.
        if self.glued {
            return;
        }
        if !self.buf.is_empty() && !self.buf.ends_with("\n\n") {
            self.buf.push('\n');
        }
    }

    /// Восстанавливает **осмысленную** пустую строку перед узлом.
    ///
    /// Пустые строки не хранятся ни в АСД, ни в `Vec<Comment>`, поэтому берём их из
    /// исходника (решение ).
    pub(crate) fn blank_before(&mut self, loc: &crate::diagnostics::Location) {
        let Some((start, _)) = comments::span(loc) else {
            return;
        };
        let anchor = self
            .comments
            .peek_start()
            .filter(|s| *s < start)
            .unwrap_or(start);
        self.blank_at(anchor);
    }

    /// Пустая строка перед позицией `offset`, если она была в исходнике.
    ///
    /// Вызывается и перед узлом, и перед **каждым** ведущим комментарием - иначе пустая
    /// строка между группами комментариев (`/// шапка` ⏎⏎ `/// про enum`) терялась бы.
    fn blank_at(&mut self, offset: usize) {
        let Some(prefix) = self.source.get(..offset) else {
            return;
        };
        let trimmed = prefix.trim_end_matches(|c: char| c.is_whitespace());
        // Два перевода строки в пробельном хвосте = автор оставил пустую строку.
        if prefix[trimmed.len()..].matches('\n').count() >= 2 {
            self.blank();
        }
    }

    /// Ведущие комментарии перед узлом (для блоков, где строка печатается сама).
    pub(crate) fn leading_for(&mut self, loc: &crate::diagnostics::Location) {
        let Some((start, _)) = comments::span(loc) else {
            return;
        };
        for (offset, text) in self.comments.leading(start) {
            self.blank_at(offset);
            self.line(&text);
        }
    }

    pub(crate) fn up(&mut self) {
        self.depth += 1;
    }

    pub(crate) fn down(&mut self) {
        self.depth = self.depth.saturating_sub(1);
    }

    /// Продолжает **уже напечатанную** строку: снимает её перевод строки и дописывает
    /// `tail`; следующая [`Out::line`] встанет без отступа.
    ///
    /// Нужен ровно для K&R-раскладки канона - `} else {` и `} else if c {`. Закрывающую
    /// скобку печатает `stmt::block_with_head`, слово `else` приходит от печати `if`, а
    /// заголовок следующей ветки - от печати блока либо от рекурсивного `stmt::print`.
    /// Три разных места, одна строка вывода: склеить их иначе, чем сняв уже
    /// поставленный `'\n'`, значит размазать знание о раскладке по всем трём.
    ///
    /// Между `join` и следующей `line` печатать нечего: флаг снимает первая же строка,
    /// а пустая строка при взведённом флаге подавлена ([`Out::blank`]).
    pub(crate) fn join(&mut self, tail: &str) {
        debug_assert!(
            self.buf.ends_with('\n'),
            "join продолжает строку, а её ещё нет"
        );
        debug_assert!(!self.glued, "join поверх join — перевод строки уже снят");
        if self.buf.ends_with('\n') {
            self.buf.pop();
        }
        self.buf.push_str(tail);
        self.glued = true;
    }

    /// Печатает строку с текущим отступом и переводом строки.
    ///
    /// Отступ **не ставится**, если строка продолжает предыдущую ([`Out::join`]).
    pub(crate) fn line(&mut self, text: &str) {
        let glued = std::mem::take(&mut self.glued);
        debug_assert!(
            !(glued && text.is_empty()),
            "продолжение строки пустым текстом оставило бы висеть `}} else `"
        );
        if !text.is_empty() {
            if !glued {
                for _ in 0..self.depth {
                    self.buf.push_str(INDENT);
                }
            }
            self.buf.push_str(text);
        }
        self.buf.push('\n');
    }

    fn finish(mut self) -> String {
        debug_assert!(!self.glued, "склейка строк осталась незакрытой");
        // Канон: ровно один перевод строки в конце файла.
        while self.buf.ends_with("\n\n") {
            self.buf.pop();
        }
        if !self.buf.is_empty() && !self.buf.ends_with('\n') {
            self.buf.push('\n');
        }
        self.buf
    }
}

/// Форматирует исходник `.takt` в канонический вид.
///
/// Комментарии сохраняются: они переассоциируются по `Location` (см. [`comments`]) и
/// печатаются как ведущие/хвостовые. Ни один не теряется - требование R2.
pub fn format_source(source: &str) -> Result<String, FormatError> {
    format_source_with_warnings(source).map(|(text, _)| text)
}

/// То же, что [`format_source`], плюс **предупреждения о стиле**.
///
/// Сегодня это канон именования (`CS-001`). Проверка идёт по тому же АСД, что уже
/// разобран для печати, - семантика по-прежнему **не запускается**, и контракт
/// форматтера (работать на семантически некорректном файле) цел.
///
/// Предупреждения **не влияют** на результат форматирования и на код возврата `fmt
/// --check`: имя - решение автора, механическая замена испортила бы замысел.
pub fn format_source_with_warnings(
    source: &str,
) -> Result<(String, Vec<crate::diagnostics::Diagnostic>), FormatError> {
    let (model, items) = crate::parse(source, 0).map_err(FormatError::Parse)?;
    let warnings = crate::style::naming_warnings(&model);
    // Форма записи чисел берётся из исходника: носитель потоковый, и сбросить его
    // обязаны все пути выхода - включая ранний возврат ошибки, оттого печать вынесена в
    // замыкание.
    literal::set(source);
    let printed = (|| {
        let mut out = Out::new(source, &items);
        print_model_body(&mut out, &model)?;
        // Хвост файла: комментарии после последнего узла.
        for c in out.comments.rest() {
            out.line(&c);
        }
        // Страховка R2: ни один комментарий не должен остаться непогашенным.
        debug_assert!(
            out.comments.is_exhausted(),
            "комментарий потерян при печати — нарушено требование R2"
        );
        Ok::<String, FormatError>(out.finish())
    })();
    literal::reset();
    Ok((printed?, warnings))
}

/// Позиция элемента модели.
///
/// У `ModelElement` нет `loc()` (в отличие от `Expression`/`Condition`), поэтому
/// извлекаем из вложенного узла - иначе комментарии не к чему привязать.
fn element_loc(element: &ast::ModelElement) -> crate::diagnostics::Location {
    use ast::ModelElement as M;
    match element {
        M::StraySemicolon(loc) => *loc,
        // Вставка уровня модели: позиция - у самого оператора.
        M::Assembly(a) => a.loc(),
        M::Variable(v) => variable_loc(v),
        M::Type(t) => t.loc,
        M::State(s) => s.loc,
        M::Model(m) => m.loc,
        M::NamedBlockCode(b) => b.loc,
        M::Enum(e) => e.loc,
        M::Struct(s) => s.loc,
        M::Function(f) => f.loc,
        M::Import(i) => import_loc(i),
        M::Formula(f) => f.loc,
        M::Condition(c) => c.loc,
        M::Invariant(i) => i.loc,
        M::InlineFormula(f) => inline_formula_loc(f),
        M::Address(a) => a.loc,
        M::Clock(c) => c.loc,
    }
}

fn variable_loc(v: &ast::VariableDefine) -> crate::diagnostics::Location {
    use ast::VariableDefine as V;
    match v {
        V::Variable { loc, .. }
        | V::Port { loc, .. }
        | V::Constant { loc, .. }
        | V::Parameter { loc, .. } => *loc,
    }
}

fn import_loc(i: &ast::ImportDefine) -> crate::diagnostics::Location {
    use ast::ImportDefine as I;
    match i {
        I::Plain(_, loc) | I::GlobalSymbol(_, _, loc) | I::Rename(_, _, loc) => *loc,
    }
}

#[allow(clippy::wildcard_enum_match_arm)]
fn inline_formula_loc(f: &ast::InlineFormulaDefine) -> crate::diagnostics::Location {
    expr::inline_formula_loc(f)
}

/// Печатает элементы модели верхнего уровня (без обёртки `model ... { }`).
fn print_model_body(out: &mut Out, model: &ast::Model) -> Result<(), FormatError> {
    for element in &model.elements {
        print_element(out, element)?;
    }
    if let Some(implements) = &model.implements {
        out.line(&format!("= {};", expr::expression(implements)?));
    }
    Ok(())
}

fn print_element(out: &mut Out, element: &ast::ModelElement) -> Result<(), FormatError> {
    let loc = element_loc(element);
    // Единая точка для любого элемента: сперва осмысленная пустая строка из зазора
    // исходника, затем ведущие комментарии. Так ни одна ветка печати не может о них
    // забыть (требования R2, R5).
    out.blank_before(&loc);
    out.leading_for(&loc);
    print_element_inner(out, element, &loc)
}

fn print_element_inner(
    out: &mut Out,
    element: &ast::ModelElement,
    loc: &crate::diagnostics::Location,
) -> Result<(), FormatError> {
    let loc = *loc;
    match element {
        ast::ModelElement::StraySemicolon(_) => {
            // Одиночная `;` - синтаксический шум; канон её опускает.
            Ok(())
        }
        ast::ModelElement::Variable(v) => {
            out.node_line(&loc, &format!("{};", expr::variable_define(v)?));
            Ok(())
        }
        ast::ModelElement::Type(t) => {
            out.node_line(
                &loc,
                &format!("type {} = {};", t.name.name, expr::ty(&t.ty)?),
            );
            Ok(())
        }
        ast::ModelElement::State(s) => print_state(out, s, &loc),
        ast::ModelElement::Model(m) => print_nested_model(out, m, &loc),
        ast::ModelElement::NamedBlockCode(b) => print_named_block(out, b),
        ast::ModelElement::Enum(e) => print_enum(out, e, &loc),
        ast::ModelElement::Struct(s) => print_struct(out, s, &loc),
        ast::ModelElement::Import(i) => {
            out.node_line(&loc, &expr::import(i)?);
            Ok(())
        }
        ast::ModelElement::Function(f) => print_function(out, f),
        ast::ModelElement::Formula(f) => formula::print_block(out, f.dialect.as_ref(), &f.formula),
        // Вставка уровня модели печатается тем же печатником, что оператор тела: форма
        // записи у них одна.
        ast::ModelElement::Assembly(a) => stmt::print(out, a),
        ast::ModelElement::Condition(c) => {
            // `cond Имя = условие;` - печатается печатью условий, а не выражений: `=`
            // здесь равенство.
            let name = c.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
            out.node_line(
                &loc,
                &format!("cond {name} = {};", expr::condition(&c.value)?),
            );
            Ok(())
        }
        ast::ModelElement::Invariant(i) => {
            // `invariant Имя = условие;` - печатается печатью условий (как `cond`): `=`
            // здесь равенство. Форма автора сохраняется - Не разворачиваем в `cond` +
            // `: [Guard]`.
            let name = i.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
            out.node_line(
                &loc,
                &format!("invariant {name} = {};", expr::condition(&i.value)?),
            );
            Ok(())
        }
        ast::ModelElement::InlineFormula(f) => {
            out.node_line(&loc, &expr::inline_formula(f)?);
            Ok(())
        }
        ast::ModelElement::Clock(c) => {
            // `clock 1kHz;`: печатается авторская запись частоты.
            out.node_line(&loc, &format!("clock {};", c.text));
            Ok(())
        }
        ast::ModelElement::Address(a) => {
            // `address ИМЯ = <выражение>;`.
            let name = a.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
            out.node_line(
                &loc,
                &format!("address {name} = {};", expr::expression(&a.value)?),
            );
            Ok(())
        }
    }
}

/// Печатает перечисление - **по варианту на строке** (канон стиля).
fn print_enum(
    out: &mut Out,
    e: &ast::EnumDefine,
    loc: &crate::diagnostics::Location,
) -> Result<(), FormatError> {
    let name = e.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");

    // Перечисление без вариантов сегодня отвергает парсер (`enum E {}` уходит в
    // восстановление после ошибки), поэтому сюда не доезжает; чем такая запись должна
    // быть - вопрос, он не закрыт. Печатаем одной строкой: это единственная раскладка,
    // которая переживёт круговой рейс, если запись однажды станет законной, и она
    // ничего не обещает о семантике.
    if e.variants.is_empty() {
        out.node_line(loc, &format!("enum {name} {{}}"));
        return Ok(());
    }

    // Граница хвостовых комментариев вариантов - байт закрывающей скобки. В
    // Однострочной записи `enum E { A, B } // вид` комментарий стоит на той же строке,
    // что и каждый вариант, и без границы его забрал бы первый же - а написан он о
    // перечислении целиком и обязан остаться при `}`.
    let brace = comments::span(loc).map_or(usize::MAX, |(_, end)| end.saturating_sub(1));
    out.line(&format!("enum {name} {{"));
    out.up();
    for (i, v) in e.variants.iter().enumerate() {
        let comma = if i + 1 < e.variants.len() { "," } else { "" };
        let text = match v.value {
            Some(value) => format!("{} = {value}{comma}", v.name.name),
            None => format!("{}{comma}", v.name.name),
        };
        // `node_line_within`, а не `line`: у варианта свой `loc`, значит комментарий к
        // нему сохраняет место (тем же приёмом печатает поля `print_struct`).
        out.node_line_within(&v.loc, &text, brace);
    }
    // Комментарий последней строкой тела: следующего варианта нет, и без явной выдачи
    // его подхватил бы `leading()` следующего элемента - уже за закрывающей скобкой,
    // привязанным к чужому узлу (тот же случай, что `close_body` у блоков).
    //
    // Граница - байт самой `}` (`end - 1`), а не `end`: `loc` кончается сразу за
    // скобкой, и по `end` сюда попал бы хвостовой комментарий, который обязан остаться
    // на строке `}`.
    if let Some((_, end)) = comments::span(loc) {
        out.comments_before(end.saturating_sub(1));
    }
    out.down();
    out.node_line(loc, "}");
    Ok(())
}

/// Печатает структуру - **по полю на строке**, с привязкой комментариев.
///
/// Устройство в точности повторяет [`print_enum`], и не по вкусовым соображениям:
/// `struct` печатался полностью через `line`, минуя привязку комментариев, - и
/// комментарий обо всей записи (`struct Trip { ... } // структура`) уезжал на свою
/// строку **за** `}`, где его на следующем прогоне подбирал `leading()` следующего
/// элемента. То есть комментарий менял хозяина.
///
/// # Границы
///
/// - Заголовок печатает `line`, закрывающую скобку - `node_line`: `loc`
///   структуры покрывает запись целиком, значит её единственный хвостовой
///   комментарий стоит за `}` и обязан остаться при ней. Приклеенный к
///   заголовку, он уехал бы вверх и на втором прогоне уже не нашёлся бы -
///   печать перестала бы быть идемпотентной (инвариант A1).
/// - Поля печатает `node_line_within` с границей по байту самой `}`
///   (`end - 1`): в однострочной записи комментарий обо всей структуре стоит на
///   той же строке, что и каждое поле, и без границы его забрало бы первое.
///
/// # Чем структура отличается от перечисления
///
/// Пустая запись `struct S {}` **законна**: поля - `Comma<StructField>`, а варианты -
/// `CommaOne<EnumVariant>`. Поэтому отдельной ветки, как у перечисления, здесь нет:
/// общий путь печатает `struct S {` и `}` двумя строками - так было и до исправления, и
/// менять раскладку без требования незачем; комментарий при этом привязан к `}` наравне
/// с непустой записью.
///
/// Висячей запятой нет по той же причине, что у перечисления: `Comma<T>` её не
/// принимает, и напечатанная запятая сделала бы файл неразбираемым - класс исправления
/// [](../../../docs/fixes/-fmt-drops-model-implement.md), где `taktc fmt` уничтожал
/// исходник на месте.
fn print_struct(
    out: &mut Out,
    s: &ast::StructDefine,
    loc: &crate::diagnostics::Location,
) -> Result<(), FormatError> {
    let name = s.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
    let brace = comments::span(loc).map_or(usize::MAX, |(_, end)| end.saturating_sub(1));

    out.line(&format!("struct {name} {{"));
    out.up();
    for (i, field) in s.fields.iter().enumerate() {
        let comma = if i + 1 < s.fields.len() { "," } else { "" };
        let text = format!("{}: {}{comma}", field.name.name, expr::ty(&field.ty)?);
        out.node_line_within(&field.loc, &text, brace);
    }
    // Комментарий последней строкой тела: следующего поля нет, и без явной выдачи его
    // подхватил бы `leading()` следующего элемента - уже за закрывающей скобкой,
    // привязанным к чужому узлу.
    if let Some((_, end)) = comments::span(loc) {
        out.comments_before(end.saturating_sub(1));
    }
    out.down();
    out.node_line(loc, "}");
    Ok(())
}

/// Печатает вложенную модель - с реализацией (`= выражение`) или без.
///
/// **, два симптома одной ветки.** Прежде печать выбирала между двумя формами, и обе
/// врали:
///
/// - `model M = A | B { ... }` (реализация **и** тело, форма ) печаталась
///   как `model M { ... }` - реализация **терялась**, композиция исчезала, и
///   форматтер молча менял смысл программы;
/// - `model M = A { }` (реализация с пустым телом) печаталась как
///   `model M = A;` - а такой записи **грамматика не принимает**: получившийся
///   файл переставал разбираться, то есть `taktc fmt` уничтожал исходник на
///   месте.
///
/// Форма одна: `model Имя = выражение {` ... `}`; без реализации - `model Имя {`.
/// Дефект дожил потому, что единственная такая запись корпуса лежит в исправлениетурах
/// `takt-sim/tests/data/`, куда обход теста форматтера не доходил.
fn print_nested_model(
    out: &mut Out,
    model: &ast::Model,
    loc: &crate::diagnostics::Location,
) -> Result<(), FormatError> {
    let name = model.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
    let head = match &model.implements {
        Some(implements) => format!("model {name} = {} {{", expr::expression(implements)?),
        None => format!("model {name} {{"),
    };
    out.line(&head);
    out.up();
    for element in &model.elements {
        print_element(out, element)?;
    }
    close_body(out, loc);
    Ok(())
}

fn print_state(
    out: &mut Out,
    state: &ast::StateDefine,
    loc: &crate::diagnostics::Location,
) -> Result<(), FormatError> {
    let kind = match state.kind {
        Some(ast::StateKind::Start) => "start ",
        Some(ast::StateKind::End) => "end ",
        Some(ast::StateKind::Next) => "next ",
        None => "state ",
    };
    let name = state.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");

    // `start S = Модель;` / `start S;` - состояние без тела.
    if state.elements.is_empty() {
        return match &state.implements {
            Some(implements) => {
                out.line(&format!(
                    "{kind}{name} = {};",
                    expr::expression(implements)?
                ));
                Ok(())
            }
            None => {
                out.line(&format!("{kind}{name};"));
                Ok(())
            }
        };
    }

    let head = match &state.implements {
        Some(implements) => format!("{kind}{name} = {} {{", expr::expression(implements)?),
        None => format!("{kind}{name} {{"),
    };
    out.line(&head);
    out.up();
    for element in &state.elements {
        print_state_element(out, element)?;
    }
    close_body(out, loc);
    Ok(())
}

/// Закрывает тело конструкции: хвостовые комментарии - затем `}`.
fn close_body(out: &mut Out, loc: &crate::diagnostics::Location) {
    if let Some((_, end)) = comments::span(loc) {
        out.comments_before(end.saturating_sub(1));
    }
    out.down();
    out.node_line(loc, "}");
}

fn print_state_element(out: &mut Out, element: &ast::StateElement) -> Result<(), FormatError> {
    let loc = state_element_loc(element);
    out.blank_before(&loc);
    out.leading_for(&loc);
    print_state_element_inner(out, element)
}

/// Позиция элемента состояния - для пустых строк и комментариев.
fn state_element_loc(element: &ast::StateElement) -> crate::diagnostics::Location {
    use ast::StateElement as S;
    match element {
        S::StraySemicolon(loc) | S::Reference(loc, _, _) => *loc,
        S::Next(id) => id.loc,
        S::NamedBlockCode(b) => b.loc,
        S::InlineFormula(f) => expr::inline_formula_loc(f),
        S::Invariant(i) => i.loc,
        S::Every(e) => e.loc,
        // Обязательство и вставка уровня состояния.
        S::Formula(f) => f.loc,
        S::Assembly(a) => a.loc(),
    }
}

fn print_state_element_inner(
    out: &mut Out,
    element: &ast::StateElement,
) -> Result<(), FormatError> {
    match element {
        ast::StateElement::StraySemicolon(_) => Ok(()),
        ast::StateElement::Next(id) => {
            out.line(&format!("next {};", id.name));
            Ok(())
        }
        ast::StateElement::Reference(_, id, cond) => {
            match cond {
                Some(cond) => out.line(&format!("ref {}: {};", id.name, expr::condition(cond)?)),
                None => out.line(&format!("ref {};", id.name)),
            }
            Ok(())
        }
        ast::StateElement::NamedBlockCode(b) => print_named_block(out, b),
        // `every 100ms { ... }` - голова блока с авторским литералом.
        ast::StateElement::Every(e) => {
            stmt::block_with_head(out, &format!("every {} ", e.text), &e.body)
        }
        // Обязательство и вставка уровня состояния печатаются теми же печатниками, что
        // и на уровне модели: форма записи у них одна.
        ast::StateElement::Formula(f) => formula::print_block(out, f.dialect.as_ref(), &f.formula),
        ast::StateElement::Assembly(a) => stmt::print(out, a),
        ast::StateElement::InlineFormula(f) => {
            let loc = expr::inline_formula_loc(f);
            out.node_line(&loc, &expr::inline_formula(f)?);
            Ok(())
        }
        ast::StateElement::Invariant(i) => {
            // `invariant Имя = условие;` в теле состояния.
            let name = i.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
            out.node_line(
                &i.loc,
                &format!("invariant {name} = {};", expr::condition(&i.value)?),
            );
            Ok(())
        }
    }
}

fn print_named_block(out: &mut Out, block: &ast::NamedBlockCodeDefine) -> Result<(), FormatError> {
    let name = block.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
    stmt::block_with_head(out, &format!("{name} "), &block.statement)
}

fn print_function(out: &mut Out, func: &ast::FunctionDefine) -> Result<(), FormatError> {
    let name = func.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
    let params = expr::parameter_list(&func.params)?;
    let ret = match &func.return_type {
        Some(ty) => format!(" -> {}", expr::ty(ty)?),
        None => String::new(),
    };
    // Атрибут печатается перед `fn`. Молча потерять его нельзя: `taktc fmt` пишет на
    // месте, и потеря сменила бы форму порождённого кода у уже отформатированной
    // модели.
    let attribute = match &func.attribute {
        Some(name) => format!("[{}] ", name.name),
        None => String::new(),
    };
    match &func.body {
        // `extern fn f(...);` - объявление без тела.
        None => {
            out.line(&format!("{attribute}extern fn {name}({params}){ret};"));
            Ok(())
        }
        Some(body) => {
            stmt::block_with_head(out, &format!("{attribute}fn {name}({params}){ret} "), body)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Отказ печати несёт код, позицию узла и текст без внутреннего дампа.
    ///
    /// Проверка живёт здесь, а не в интеграционном наборе, и это следствие замера:
    /// достижимых непечатаемых узлов в языке не осталось - все оставшиеся ветви отказа
    /// принадлежат узлам, которых грамматика не строит. Построить вход снаружи нечем, а
    /// свойство отказа проверять надо: сообщение видит пользователь.
    #[test]
    fn unsupported_refusal_has_code_position_and_plain_text() {
        let loc = crate::diagnostics::Location::Source(0, 42, 50);
        let FormatError::Unsupported(diagnostic) = unsupported(loc, "проба") else {
            panic!("конструктор обязан давать ветвь Unsupported");
        };
        assert_eq!(diagnostic.code.as_deref(), Some("FM-001"));
        assert_eq!(diagnostic.loc, loc, "позиция берётся у узла");
        assert!(
            diagnostic.message.contains("'проба'"),
            "текст называет ВИД узла: {}",
            diagnostic.message
        );
        for forbidden in ["Source(", "loc:", "Statement::"] {
            assert!(
                !diagnostic.message.contains(forbidden),
                "внутреннее представление наружу не выходит ({forbidden}): {}",
                diagnostic.message
            );
        }
    }

    #[test]
    fn formats_minimal_model() {
        let out = format_source("start   S;").unwrap();
        assert_eq!(out, "start S;\n");
    }

    #[test]
    fn normalises_variable_declaration() {
        let out = format_source("var   x :u8:=0;").unwrap();
        assert_eq!(out, "var x: u8 := 0;\n");
    }

    #[test]
    fn indents_state_body() {
        let out = format_source("state A { ref B: x > 1; }").unwrap();
        assert_eq!(out, "state A {\n    ref B: x > 1;\n}\n");
    }

    #[test]
    fn leading_comment_is_printed_before_node() {
        // комментарии сохраняются.
        let out = format_source("// заметка\nstart S;").unwrap();
        assert_eq!(out, "// заметка\nstart S;\n");
    }

    #[test]
    fn trailing_comment_stays_on_its_line() {
        let out = format_source("var x: u8 := 0; // счётчик").unwrap();
        assert_eq!(out, "var x: u8 := 0; // счётчик\n");
    }

    #[test]
    fn file_ends_with_single_newline() {
        let out = format_source("start S;").unwrap();
        assert!(out.ends_with(";\n"));
        assert!(!out.ends_with("\n\n"));
    }
}
