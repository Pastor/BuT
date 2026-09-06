//! Слой использований: где в тексте написано каждое имя.
//!
//! ## Идентичность символа
//!
//! [`SymbolId`] - позиция **объявляющего идентификатора** (`Identifier.loc`). Имя для
//! этого не годится: одноимённые переменные разных моделей - норма, а локальная `var x`
//! в блоке **затеняет** переменную модели (проверено: цель `c` печатает локальную, поля
//! структуры не трогая). Семантический узел тоже не годится: его `loc` покрывает **весь
//! оператор** объявления, а не имя.
//!
//! ## Полнота
//!
//! Таблица помечает то, чего не смогла разобрать или разрешить
//! ([`UsageTable::is_complete`]). Потребитель, которому нужна полнота (переименование),
//! обязан отказаться от работы, а не молча пропустить вхождение.

use crate::diagnostics::Location;
use crate::parser::ast;

mod scope;
mod walk;

#[cfg(test)]
mod tests;

pub use scope::SymbolKind;

/// Идентичность символа - позиция его объявляющего идентификатора.
///
/// Пара `(файл, смещение начала имени)`. Смещение осмысленно только внутри своего
/// файла, поэтому номер файла - часть ключа (тот же довод, что у индекса ).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId {
    /// Номер файла в реестре компиляции.
    pub file_no: u32,
    /// Смещение первого байта имени в объявлении.
    pub start: u32,
}

/// Роль вхождения: объявление символа или его использование.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsageKind {
    /// Имя в объявлении (`var x`, `state S`, `fn f`, параметр).
    Declaration,
    /// Имя-ссылка (в выражении, условии, формуле, `ref`, `address`).
    Reference,
}

/// Одно вхождение имени в текст.
///
/// Диапазон `[start, end)` покрывает **ровно идентификатор** - его можно без правки
/// отдать в `TextEdit`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Usage {
    /// Имя, как оно написано.
    pub name: String,
    /// Начало имени (байтовое смещение).
    pub start: u32,
    /// Конец имени (байтовое смещение, эксклюзивно).
    pub end: u32,
    /// Символ, к которому вхождение относится.
    pub symbol: SymbolId,
    /// Вид символа - нужен потребителю, чтобы отказать (например, на модели).
    pub symbol_kind: SymbolKind,
    /// Объявление это или использование.
    pub kind: UsageKind,
}

/// Имя, которое обход встретил, но **не смог связать** ни с одним объявлением текущего
/// файла (импортированный символ, вариант перечисления, встроенное имя).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedName {
    /// Имя, как оно написано.
    pub name: String,
    /// Начало имени.
    pub start: u32,
    /// Конец имени (эксклюзивно).
    pub end: u32,
}

/// Таблица вхождений имён одного файла.
#[derive(Debug, Default)]
pub struct UsageTable {
    usages: Vec<Usage>,
    unresolved: Vec<UnresolvedName>,
    unsupported: Vec<Location>,
    /// Диапазоны имён в позиции типа - см.
    type_refs: Vec<(u32, u32)>,
}

impl UsageTable {
    /// Вхождение, покрывающее смещение `offset`.
    ///
    /// Диапазоны имён не вложены друг в друга, поэтому "наиболее конкретного" выбора
    /// здесь нет - подходит первое совпавшее. Конец **эксклюзивен**, но курсор,
    /// поставленный сразу за именем, редакторы считают "на имени", и потому граница
    /// включена.
    pub fn usage_at(&self, offset: usize) -> Option<&Usage> {
        let offset = offset as u32;
        self.usages
            .iter()
            .find(|u| u.start <= offset && offset <= u.end)
    }

    /// Все вхождения символа - объявление и ссылки - в порядке появления.
    pub fn occurrences_of(&self, symbol: SymbolId) -> Vec<&Usage> {
        let mut found: Vec<&Usage> = self.usages.iter().filter(|u| u.symbol == symbol).collect();
        found.sort_by_key(|u| u.start);
        found
    }

    /// Объявление символа.
    pub fn declaration_of(&self, symbol: SymbolId) -> Option<&Usage> {
        self.usages
            .iter()
            .find(|u| u.symbol == symbol && u.kind == UsageKind::Declaration)
    }

    /// Неразрешённое имя, покрывающее смещение `offset`.
    ///
    /// Нужно, чтобы отличить "курсор не на имени" от "курсор на имени, которое
    /// объявлено вне этого файла": у отказа переименовать это разные причины.
    pub fn unresolved_at(&self, offset: usize) -> Option<&UnresolvedName> {
        let offset = offset as u32;
        self.unresolved
            .iter()
            .find(|u| u.start <= offset && offset <= u.end)
    }

    /// Есть ли неразрешённое имя, текстуально совпадающее с `name`.
    ///
    /// Это **тест полноты**, а не диагностика. Если имя где-то встретилось, но
    /// связать его с объявлением не удалось, переименование обязано отказаться:
    /// пропущенное вхождение - испорченный исходник, а "наверное, это что-то другое" -
    /// не довод.
    pub fn has_unresolved_named(&self, name: &str) -> bool {
        self.unresolved.iter().any(|u| u.name == name)
    }

    /// Разобран ли файл целиком.
    ///
    /// `false` - встретился узел АСД, которого обход не знает. Такое возможно только
    /// после добавления в язык новой конструкции без обновления обхода; потребителю,
    /// которому нужна полнота, работать нельзя.
    pub fn is_complete(&self) -> bool {
        self.unsupported.is_empty()
    }

    /// Места, которые обход не разобрал (для диагностики и тестов).
    pub fn unsupported(&self) -> &[Location] {
        &self.unsupported
    }

    /// Все вхождения (для тестов и будущих потребителей - подсветка вхождений).
    pub fn usages(&self) -> &[Usage] {
        &self.usages
    }

    /// Неразрешённые имена (для тестов).
    pub fn unresolved(&self) -> &[UnresolvedName] {
        &self.unresolved
    }

    /// Диапазоны имён, стоящих **в позиции типа** - `(начало, конец)`, конец
    /// эксклюзивен.
    ///
    /// Отдельный список, а не разновидность [`Usage`], **намеренно**: имя типа в
    /// позиции типа далеко не всегда разрешается в символ этого файла (`u8`, `bit`, `q`
    /// объявлений не имеют), а класть их в `unresolved` значило бы менять поведение
    /// `rename`, который по неразрешённым именам принимает решение "полнота или отказ".
    pub fn type_refs(&self) -> &[(u32, u32)] {
        &self.type_refs
    }

    /// Стоит ли имя, занимающее `[start, end)`, в позиции типа.
    pub fn is_type_position(&self, start: u32, end: u32) -> bool {
        self.type_refs.iter().any(|&(s, e)| s == start && e == end)
    }

    pub(crate) fn push(&mut self, usage: Usage) {
        self.usages.push(usage);
    }

    pub(crate) fn push_unresolved(&mut self, name: UnresolvedName) {
        self.unresolved.push(name);
    }

    pub(crate) fn push_type_ref(&mut self, start: u32, end: u32) {
        self.type_refs.push((start, end));
    }

    pub(crate) fn push_unsupported(&mut self, loc: Location) {
        self.unsupported.push(loc);
    }
}

/// Собирает таблицу вхождений имён по АСД одного файла.
///
/// Импортированные файлы **не обходятся**: их текст серверу не принадлежит, а видимость
/// в языке направлена вниз по импорту - символ, объявленный здесь, за пределами файла
/// не виден. Имена, пришедшие из импорта, попадают в [`UsageTable::unresolved`] - этого
/// достаточно, чтобы потребитель отказался от переименования чужого символа.
pub fn collect_usages(root: &ast::Model) -> UsageTable {
    let mut table = UsageTable::default();
    walk::walk_root(root, &mut table);
    table
}

/// Диапазон имени по позиции его идентификатора.
///
/// Возвращает `None` для позиций без файла (`Codegen`/`Implicit`/`Builtin`) - у таких
/// имён в тексте нет, и вхождением они быть не могут.
pub(crate) fn name_range(loc: Location) -> Option<(u32, u32, u32)> {
    match loc {
        Location::Source(file_no, start, end) => Some((file_no, start, end)),
        _ => None,
    }
}
