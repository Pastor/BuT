//! Крейт симуляции моделей языка Takt.
//!
//! Предоставляет инструменты для выполнения (симуляции) моделей, построенных
//! семантическим анализатором крейта [`takt-lang`].
//!
//! # Использование
//!
//! ```rust,ignore
//! use takt_lang::parse;
//! use takt_lang::semantic::{minimap::Map, tree::construct_model};
//!
//! let (ast, _) = parse("start S;", 0).unwrap();
//! let model = construct_model(&ast, None, &[]).unwrap();
//! let map = Map::create(model).unwrap();
//! ```
//!
//! Видимость публичного API согласована: типы, достижимые из публичных элементов,
//! обязаны быть публичными. Линт держит это механически - утечка `pub(crate)`-типа
//! наружу валит сборку. Именно точечный `deny` одного правила, а **не**
//! `#![deny(warnings)]` (запрещён `docs/CODE.md`: ломает сборку при обновлении
//! компилятора).
#![deny(private_interfaces)]

pub(crate) mod anon_cell;
mod context;
pub(crate) mod eval;
/// Экспорт проекта: картинки листов и видео прогона.
#[cfg(feature = "graphics")]
pub mod export;
pub(crate) mod expression;
/// Кадры прогона: такт эталона - цветной вид листа по файлу раскладки.
#[cfg(feature = "graphics")]
pub mod film;
pub mod json_input;
/// Реестр имён портов и переменных модели.
pub mod port_names;
mod predicate;
pub mod runner;
pub mod state_io;
/// Трасса прогона строками: шаг и сводка.
pub mod trace;
mod unit;

/// Значение переменной или порта - для наблюдения извне ([`Unit::variable`]).
pub use eval::value::Value;
/// Человекочитаемая запись длительности для трасс.
pub use trace::format_duration;
/// Дерево симуляции и результат шага.
///
/// Реэкспорт: сам модуль `unit` внутренний, но его типы возвращаются публичным
/// [`build_unit`] и обязаны быть именуемыми снаружи (иначе `private_interfaces` и
/// невозможность написать интеграционный тест - пункт бэклога, закрыт ).
pub use unit::instance::{ActiveState, Segment};
pub use unit::{TickResult, Unit};

/// Строит дерево симуляции из семантической модели.
pub fn build_unit(
    model: std::rc::Rc<std::cell::RefCell<takt_lang::semantic::ModelNode>>,
) -> Result<unit::Unit, takt_lang::diagnostics::Diagnostic> {
    unit::builder::build(model)
}

#[cfg(test)]
mod tests {
    use takt_lang::parse;
    use takt_lang::semantic::minimap::Map;
    use takt_lang::semantic::tree::construct_model;

    #[test]
    fn test_access() {
        let (model, _) = parse("start E;", 0).unwrap();
        let model = construct_model(&model, None, &[]).unwrap();
        let map = Map::create(model.clone()).unwrap();
        let raw = &*model.borrow();
        assert!(raw.search_state("E").is_some());
        map.states().iter().for_each(|state| {
            assert!(raw.search_state(state.local()).is_some());
        });
        assert!(map.used_models().is_empty());
    }
}
