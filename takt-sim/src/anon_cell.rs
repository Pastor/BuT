//! Анонимная ячейка памяти в эталоне - `#0x346619:0 as u64`.
//!
//! ## Ячейка - синтетический порт
//!
//! Имени у обращения нет, но эталон обязан **показывать** значение: сверка с целью
//! сравнивает трассы, а не факт исполнения. Поэтому ячейка живёт в контексте под
//! именем, которое строит сам компилятор ([`AnonPortAccess::synthetic_name`]) - одно и
//! то же у эталона, у `st-at` и у `sv-mmio`. Строить имя здесь второй раз нельзя:
//! разойдясь на один символ, сверка сравнивала бы разные величины.
//!
//! ## Чего эталон не моделирует
//!
//! **Перекрытие полей разной ширины по одному адресу.** `#0x100 as u8` и
//! `#0x100 as u32` - две разные ячейки эталона, тогда как `c-hal` читает одну и
//! ту же память. Ограничение осознанное и записано в
//! документе: моделировать байтовую память значило бы завести в симуляторе
//! вторую модель памяти ради случая, которого в примерах нет.
//!
//! ## Непрочитанная ячейка читается нулём
//!
//! Физическая память после сброса содержит что угодно, но эталон обязан быть
//! **детерминирован** (иначе сверка с целью недоказуема). Ноль - то же
//! умолчание, что у необъявленного значения порта в сценарии.

use crate::context::Context;
use crate::eval::value::Value;
use takt_lang::semantic::AnonPortAccess;
use takt_lang::semantic::type_node::TypeNode;

/// Читает значение ячейки.
pub(crate) fn read(access: &AnonPortAccess, ctx: &dyn Context) -> Value {
    ctx.get_value(&access.synthetic_name())
        .unwrap_or_else(|| default_of(access))
}

/// Пишет значение в ячейку.
pub(crate) fn write(access: &AnonPortAccess, value: Value, ctx: &mut dyn Context) {
    ctx.set_value(&access.synthetic_name(), value);
}

/// Значение ячейки, в которую ещё не писали.
fn default_of(access: &AnonPortAccess) -> Value {
    match access.ty {
        TypeNode::Bit | TypeNode::Bool => Value::Boolean(false),
        _ => Value::Number(0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct MapContext {
        vars: HashMap<String, Value>,
    }

    impl Context for MapContext {
        fn get_value(&self, name: &str) -> Option<Value> {
            self.vars.get(name).cloned()
        }
        fn set_value(&mut self, name: &str, value: Value) {
            self.vars.insert(name.to_string(), value);
        }
    }

    fn access(bit: i64, ty: TypeNode) -> AnonPortAccess {
        AnonPortAccess {
            addr: 0x100,
            bit,
            ty,
            loc: takt_lang::diagnostics::Location::Codegen,
        }
    }

    /// Записанное значение читается обратно - тем же именем ячейки.
    #[test]
    fn write_then_read_round_trip() {
        let mut ctx = MapContext {
            vars: HashMap::new(),
        };
        let cell = access(
            0,
            TypeNode::Integer {
                bits: 8,
                signed: false,
            },
        );
        write(&cell, Value::Number(67), &mut ctx);
        assert_eq!(read(&cell, &ctx), Value::Number(67));
    }

    /// Ячейка, в которую не писали, читается нулём, а не "значения нет".
    #[test]
    fn unwritten_cell_reads_zero() {
        let ctx = MapContext {
            vars: HashMap::new(),
        };
        assert_eq!(
            read(
                &access(
                    0,
                    TypeNode::Integer {
                        bits: 32,
                        signed: false
                    }
                ),
                &ctx
            ),
            Value::Number(0)
        );
    }

    /// Битовая ячейка читается булевым, а не числом: иначе сравнение с `1` в условии
    /// пошло бы другим правилом, чем у именованного бит-порта.
    #[test]
    fn unwritten_bit_cell_reads_false() {
        let ctx = MapContext {
            vars: HashMap::new(),
        };
        assert_eq!(read(&access(4, TypeNode::Bit), &ctx), Value::Boolean(false));
    }

    /// Поля разной ширины по одному адресу - Разные ячейки эталона.
    ///
    /// Пришпилено намеренно: ограничение решения 5B видно в тесте, а не только в
    /// документе, и его смена будет осознанной.
    #[test]
    fn different_widths_are_different_cells() {
        let mut ctx = MapContext {
            vars: HashMap::new(),
        };
        let byte = access(
            0,
            TypeNode::Integer {
                bits: 8,
                signed: false,
            },
        );
        let word = access(
            0,
            TypeNode::Integer {
                bits: 32,
                signed: false,
            },
        );
        write(&byte, Value::Number(5), &mut ctx);
        assert_eq!(read(&word, &ctx), Value::Number(0));
    }
}
