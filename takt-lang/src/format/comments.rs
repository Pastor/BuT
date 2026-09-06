//! Переассоциация комментариев по байтовым смещениям.
//!
//! # Классификация
//!
//! - **Ведущий** - начинается раньше узла: печатается на своих строках перед ним.
//! - **Хвостовой** - начинается после конца узла и на **той же** строке
//!   исходника: приклеивается к строке узла (`var x: u8 := 0; // счётчик`).
//! - **Остаточные** - всё, что осталось после последнего узла (хвост файла).

use crate::diagnostics::Location;
use crate::parser::ast;

/// Комментарий, приведённый к смещению начала и сырому тексту.
struct Item {
    start: usize,
    text: String,
}

/// Курсор по комментариям исходника.
///
/// Комментарии выдаются **строго по одному разу**: каждый либо напечатан как
/// ведущий/хвостовой, либо попадёт в [`Comments::rest`]. Потерять комментарий
/// пользователя нельзя - это главное требование R2.
pub(crate) struct Comments<'a> {
    source: &'a str,
    items: Vec<Item>,
    next: usize,
}

impl<'a> Comments<'a> {
    pub(crate) fn new(source: &'a str, comments: &[ast::Comment]) -> Self {
        let mut items: Vec<Item> = comments
            .iter()
            .filter_map(|c| {
                let Location::Source(_, start, _) = *location(c) else {
                    return None;
                };
                Some(Item {
                    start: start as usize,
                    text: c.value().trim_end().to_string(),
                })
            })
            .collect();
        // Лексер выдаёт комментарии по порядку, но полагаться на это не будем.
        items.sort_by_key(|i| i.start);
        Self {
            source,
            items,
            next: 0,
        }
    }

    /// Ведущие комментарии: все, что начинаются раньше `offset`.
    ///
    /// Возвращает пары "начало, текст": начало нужно, чтобы восстановить пустые строки
    /// **между** комментариями. Без него блок `/// шапка` + пустая строка + `/// про
    /// enum` слипался бы в один - потеря авторского замысла.
    pub(crate) fn leading(&mut self, offset: usize) -> Vec<(usize, String)> {
        let mut out = Vec::new();
        while self.next < self.items.len() && self.items[self.next].start < offset {
            let item = &self.items[self.next];
            out.push((item.start, item.text.clone()));
            self.next += 1;
        }
        out
    }

    /// Хвостовой комментарий: начинается после `end` и на той же строке.
    pub(crate) fn trailing(&mut self, end: usize) -> Option<String> {
        let item = self.items.get(self.next)?;
        if item.start < end {
            return None;
        }
        // Между концом узла и началом комментария не должно быть перевода строки.
        let gap = self.source.get(end..item.start)?;
        if gap.contains('\n') {
            return None;
        }
        let text = item.text.clone();
        self.next += 1;
        Some(text)
    }

    /// Хвостовой комментарий, но **только внутри** границы `limit`.
    ///
    /// Нужен там, где узел печатается по частям, а `Location` внешней записи покрывает
    /// их все: у однострочного `enum E { A, B } // вид` комментарий стоит на той же
    /// строке, что и каждый вариант, и без границы его забрал бы первый же из них -
    /// хотя автор написал его о перечислении целиком. `limit` - байт закрывающей
    /// скобки: всё, что начинается за ней, принадлежит внешней записи, а не её части.
    pub(crate) fn trailing_within(&mut self, end: usize, limit: usize) -> Option<String> {
        if self.items.get(self.next)?.start >= limit {
            return None;
        }
        self.trailing(end)
    }

    /// Начало ближайшего непогашенного комментария (без потребления).
    pub(crate) fn peek_start(&self) -> Option<usize> {
        self.items.get(self.next).map(|i| i.start)
    }

    /// Всё, что осталось (хвост файла).
    pub(crate) fn rest(&mut self) -> Vec<String> {
        let out: Vec<String> = self.items[self.next..]
            .iter()
            .map(|i| i.text.clone())
            .collect();
        self.next = self.items.len();
        out
    }

    /// Остались ли непогашенные комментарии.
    ///
    /// Страховка требования R2: если после печати что-то осталось необработанным, это
    /// дефект привязки, а не норма.
    pub(crate) fn is_exhausted(&self) -> bool {
        self.next >= self.items.len()
    }
}

fn location(c: &ast::Comment) -> &Location {
    match c {
        ast::Comment::Line(loc, _)
        | ast::Comment::DocLine(loc, _)
        | ast::Comment::Block(loc, _) => loc,
    }
}

/// Достаёт байтовые смещения из `Location`.
pub(crate) fn span(loc: &Location) -> Option<(usize, usize)> {
    match loc {
        Location::Source(_, start, end) => Some((*start as usize, *end as usize)),
        Location::Builtin | Location::CommandLine | Location::Implicit | Location::Codegen => None,
    }
}
