//! Занятые имена полей структуры модели цели `rust`.
//!
//! Переменную автора цель печатает полем структуры, приводя имя к snake_case, и рядом
//! ставит **свои** поля: `state`, `shared`, `hal`, механизм времени, аккумуляторы
//! `every`, шаги цепочек, экземпляры под-моделей. Столкновение давало `E0124: field ...
//! is already declared` при **нулевом** коде возврата `taktc` - рапорт об успехе на
//! файле, который отвергает `rustc`. Хуже дубля объявления был смысл вывода: `var
//! State` печаталась тем же `self.state`, что и состояние автомата, то есть операторы
//! автора писали в поле автомата.
//!
//! **Набор строится из носителей печати, а не из списка строк.** Имена берут те же
//! функции и константы, которыми поля печатаются (`rust_time`, `rust_every`,
//! `rust_chain::seq_field_name`, `Instance::field`); собственных литералов у набора три -
//! `state`, `shared`, `hal`, и каждое стоит рядом с местом печати. Второй список
//! разошёлся бы с печатью молча.
//!
//! **Служебные имена занимаются до печати переменных.** Порядок печати полей менять
//! нельзя (снимки `examples/generated/` сверяются побайтно), а отказ обязан нести
//! координату **объявления** - у служебного поля позиции нет.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::rust::rust_chain::Chain;
use crate::generator::rust::rust_chain::seq_field_name;
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_model::Instance;
use crate::generator::rust::rust_name::name_collision;
use crate::generator::rust::{rust_every, rust_time};
use crate::semantic::ModelNode;

/// Занятое имя поля: чем занято и под каким исходным именем.
struct Taken {
    /// Имя в исходнике `.takt`; у служебного поля совпадает с `produced`.
    original: String,
    /// Идентификатор, который печатается в `struct`.
    produced: String,
    /// Поле печатает цель сама - автор такого объявления не писал.
    service: bool,
}

/// Имена полей структуры модели: занятые целью и занимаемые автором.
pub(super) struct Fields {
    taken: Vec<Taken>,
}

impl Fields {
    /// Набор служебных полей, которые цель напечатает в эту структуру.
    ///
    /// Условия эмиссии здесь **не повторяются**: список времени отдаёт
    /// `rust_time::field_names`, список `every` - `rust_every::field_names`, шаги
    /// цепочек и экземпляры приходят готовыми. Три собственных имени - `state`,
    /// `shared`, `hal` - печатаются в `rust_model` рядом с их условиями, и тест
    /// сверяет набор с напечатанной структурой.
    pub(super) fn service(
        map: &RustMap,
        model: &ModelNode,
        instances: &[&Instance],
        concats: &[Chain],
        has_shared: bool,
        uses_hal: bool,
    ) -> Result<Self, Diagnostic> {
        let mut names: Vec<String> = vec!["state".to_string()];
        if has_shared {
            names.push("shared".to_string());
        }
        names.extend(
            rust_time::field_names(map, model)
                .into_iter()
                .map(str::to_string),
        );
        names.extend(rust_every::field_names(model));
        for chain in concats {
            names.push(seq_field_name(&chain.state, &chain.path)?);
        }
        for instance in instances {
            names.push(instance.field.clone());
        }
        if uses_hal {
            names.push("hal".to_string());
        }
        Ok(Self {
            taken: names
                .into_iter()
                .map(|name| Taken {
                    original: name.clone(),
                    produced: name,
                    service: true,
                })
                .collect(),
        })
    }

    /// Занимает имя поля под объявление автора.
    ///
    /// # Ошибки
    /// [`RS-026`], если имя занято полем, которое печатает сама цель;
    /// [`RS-005`], если два **авторских** имени слиплись после приведения
    /// регистра (`fooBar` и `foo_bar` дают одно `foo_bar`).
    pub(super) fn claim(
        &mut self,
        original: &str,
        produced: &str,
        loc: Location,
    ) -> Result<(), Diagnostic> {
        if let Some(clash) = self.taken.iter().find(|t| t.produced == produced) {
            if clash.service {
                return Err(service_clash(original, produced, loc));
            }
            return Err(name_collision(
                &clash.original,
                original,
                produced,
                "поля структуры модели",
                loc,
            ));
        }
        self.taken.push(Taken {
            original: original.to_string(),
            produced: produced.to_string(),
            service: false,
        });
        Ok(())
    }

    /// Все занятые идентификаторы - для теста, сверяющего набор с выводом.
    #[cfg(test)]
    pub(super) fn produced(&self) -> Vec<&str> {
        self.taken.iter().map(|t| t.produced.as_str()).collect()
    }
}

/// Строит диагностику `RS-026` - имя занято полем, которое печатает цель.
fn service_clash(original: &str, produced: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "имя '{original}' даёт поле '{produced}', которое цель 'rust' печатает \
             сама (состояние автомата, разделяемые переменные, аппаратный слой, \
             механизм времени, аккумулятор 'every', шаг последовательной композиции \
             либо экземпляр под-модели): в структуре модели оказалось бы два поля с \
             одним именем, и 'rustc' отвечает 'E0124: field is already declared'. \
             Это НЕ ограничение языка Takt — имя занято тем, что печатает именно эта \
             цель, и для 'c' и 'sv' модель остаётся валидной. \
             Переименуйте объявление в исходнике .takt"
        ),
    )
    .with_code("RS-026")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Служебное имя занято: авторское объявление отвергается `RS-026`.
    #[test]
    fn service_name_is_refused() {
        let mut fields = Fields {
            taken: vec![Taken {
                original: "state".to_string(),
                produced: "state".to_string(),
                service: true,
            }],
        };
        let err = fields
            .claim("State", "state", Location::Codegen)
            .expect_err("столкновение со служебным полем обязано отказать");
        assert_eq!(err.code.as_deref(), Some("RS-026"));
        assert!(
            err.message.contains("'State'") && err.message.contains("'state'"),
            "диагностика обязана назвать обе стороны: {}",
            err.message
        );
    }

    /// Два авторских имени, слипшихся после приведения регистра, - `RS-005`.
    ///
    /// Отдельный код здесь не выдумка: `RS-026` говорит о поле, которого в исходнике
    /// нет вовсе, а тут обе стороны написал автор - и текст обязан назвать именно
    /// слипание.
    #[test]
    fn two_author_names_give_rs005() {
        let mut fields = Fields { taken: Vec::new() };
        fields
            .claim("fooBar", "foo_bar", Location::Codegen)
            .expect("первое имя занимается");
        let err = fields
            .claim("foo_bar", "foo_bar", Location::Codegen)
            .expect_err("слипание обязано отказать");
        assert_eq!(err.code.as_deref(), Some("RS-005"));
    }

    /// Разные имена занимаются подряд и не мешают друг другу (контроль).
    #[test]
    fn distinct_names_are_accepted() {
        let mut fields = Fields { taken: Vec::new() };
        for (original, produced) in [("level", "level"), ("mode", "mode")] {
            fields
                .claim(original, produced, Location::Codegen)
                .expect("разные имена занимаются");
        }
        assert_eq!(fields.produced(), vec!["level", "mode"]);
    }
}
