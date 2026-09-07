//! Утилита форматированного вывода с управлением отступами.
//!
//! [`Printer`] оборачивает любой `&mut dyn Write` и предоставляет методы-строители для
//! управления уровнем вложенности и записи текста.

use std::cell::RefCell;
use std::fmt::Write;

/// Построитель форматированного вывода с поддержкой отступов.
///
/// Хранит текущий уровень вложенности (`indent`) и размер одного уровня (`indent_size`
/// пробелов). Метод [`ident`](Printer::ident) автоматически добавляет накопленный
/// отступ перед выводимой строкой.
///
/// # Пример
///
/// ```ignore
/// let mut output = String::new();
/// let mut printer = Printer::new(4, &mut output);
/// printer
///     .print("struct A {").nl()
///     .up()
///     .ident("value: u8;").nl()
///     .down()
///     .print("}");
///
/// assert_eq!(output, "struct A {\n    value: u8;\n}");
/// ```
pub struct Printer<'a> {
    indent_size: usize,
    indent: usize,
    writer: &'a mut dyn Write,
    padding: RefCell<String>,
}

impl<'a> Printer<'a> {
    /// Создаёт новый `Printer` с заданным размером отступа и целевым `writer`.
    ///
    /// Начальный уровень вложенности равен `0`.
    pub fn new(indent_size: usize, writer: &'a mut dyn Write) -> Self {
        Self {
            indent: 0,
            indent_size,
            padding: RefCell::new(String::new()),
            writer,
        }
    }

    /// Печатник **в другой приёмник**, с тем же уровнем вложенности.
    ///
    /// Нужен, когда текст надо получить до решения, что печатать перед ним:
    /// цель `c` собирает так тело функции и по нему решает, нужна ли заглушка
    /// неиспользуемого параметра. Уровень вложенности наследуется, поэтому отступы в
    /// буфере совпадают с отступами прямой печати - иначе вывод сместился бы.
    ///
    /// Время жизни приёмника независимо от времени жизни исходного печатника: буфер
    /// живёт в вызывающем и переживает копию. Свяжи их - и буфер станет нечитаемым.
    pub fn fork<'b>(&self, writer: &'b mut dyn Write) -> Printer<'b> {
        Printer {
            indent: self.indent,
            indent_size: self.indent_size,
            padding: RefCell::new(self.padding.borrow().clone()),
            writer,
        }
    }

    /// Увеличивает уровень вложенности на один.
    pub fn up(&mut self) -> &mut Self {
        self.indent += 1;
        self.calculate_padding();
        self
    }

    /// Уменьшает уровень вложенности на один.
    pub fn down(&mut self) -> &mut Self {
        self.indent -= 1;
        self.calculate_padding();
        self
    }

    /// Записывает символ новой строки `\n`.
    pub fn nl(&mut self) -> &mut Self {
        self.writer.write_char('\n').unwrap();
        self
    }

    fn calculate_padding(&mut self) {
        // Длина отступа известна точно (уровень x размер) - строим строку нужной
        // ёмкости за одну аллокацию вместо посимвольного push в двойном цикле.
        let padding = " ".repeat(self.indent * self.indent_size);
        self.padding.replace(padding);
    }

    /// Записывает строку `message` без отступа.
    pub fn print(&mut self, message: &str) -> &mut Self {
        self.writer.write_fmt(format_args!("{}", message)).unwrap();
        self
    }

    /// Записывает строку `message` с текущим отступом.
    pub fn ident(&mut self, message: &str) -> &mut Self {
        self.writer
            .write_fmt(format_args!("{}{}", *self.padding.borrow(), message))
            .unwrap();
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn print_padding() {
        let mut output = String::new();
        let mut printer = Printer::new(4, &mut output);
        printer
            .print("struct A {")
            .nl()
            .up()
            .ident("value: u8;")
            .nl()
            .down()
            .print("}");
        assert_eq!(
            output,
            r#"struct A {
    value: u8;
}"#
        );
    }
}
