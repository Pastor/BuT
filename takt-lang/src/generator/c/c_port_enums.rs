//! Перечисления портов цели `c` - сбор и печать.
//!
//! Выделено из `c_header` по границе **ответственности**: заголовок отвечает за
//! объявления модели, а этот модуль - за один вопрос, "какие порты есть и как
//! называются их перечислители".
//!
//! Двунаправленный порт попадает в оба перечисления, и объявленное направление едет
//! вместе с ним (`PortEntry`): перечислители в C делят одну область видимости, и без
//! сегмента стороны `cc` отвечает `redefinition of enumerator`.

use crate::diagnostics::Diagnostic;
use crate::generator::c::PortClass;
use crate::generator::c::c_map::CMap;
use crate::generator::indent::Printer;
use crate::semantic::minimap::{Element, Name};
use crate::semantic::{PortDirection, VariableNode};

/// Порт в карте: модель, имя и объявленное направление.
pub(in crate::generator::c) type PortEntry = (Name, String, PortDirection);
/// Карта портов по классу и стороне перечисления.
pub(in crate::generator::c) type PortMap =
    std::collections::HashMap<(PortClass, PortDirection), Vec<PortEntry>>;

/// Собирает все порты из всех моделей, сгруппированные по [`PortClass`] и
/// [`PortDirection`].
///
/// Возвращает словарь `(PortClass, PortDirection) -> Vec<(модель, порт, объявленное
/// направление)>`; последнее нужно двунаправленному порту. Порты отсортированы для
/// детерминированной генерации.
pub(in crate::generator::c) fn collect_ports_by_class(map: &CMap) -> Result<PortMap, Diagnostic> {
    use std::collections::HashMap;
    let mut all_models = map.using_models();
    all_models.insert(
        0,
        Element::Model {
            name: map.root_name().clone(),
            states: map.states().clone(),
            start: map.start().clone(),
        },
    );
    let mut result: HashMap<(PortClass, PortDirection), Vec<PortEntry>> = HashMap::new();
    for element in &all_models {
        let model_name = element.name();
        let model = map.raw_model_at(model_name.clone())?;
        let model_borrowed = model.borrow();
        crate::generator::c::c_ports::check_port_types(&model_borrowed, &model_name)?;
        let mut ports: Vec<(Name, String, PortClass, PortDirection)> = model_borrowed
            .variables
            .values()
            .filter_map(|v| {
                if let VariableNode::Port {
                    name,
                    ty,
                    direction,
                    ..
                } = v
                {
                    Some((
                        model_name.clone(),
                        name.clone(),
                        PortClass::from_type(ty),
                        *direction,
                    ))
                } else {
                    None
                }
            })
            .collect();
        ports.sort_by(|(_, a, _, _), (_, b, _, _)| a.cmp(b));
        for (mname, pname, cls, dir) in ports {
            match dir {
                PortDirection::InOut => {
                    // Двунаправленный порт появляется в обоих перечислениях, и
                    // объявленное направление едет вместе с ним: по нему имя получает
                    // сегмент стороны.
                    result.entry((cls, PortDirection::In)).or_default().push((
                        mname.clone(),
                        pname.clone(),
                        dir,
                    ));
                    result
                        .entry((cls, PortDirection::Out))
                        .or_default()
                        .push((mname, pname, dir));
                }
                _ => {
                    result
                        .entry((cls, dir))
                        .or_default()
                        .push((mname, pname, dir));
                }
            }
        }
    }
    Ok(result)
}

/// Генерирует тип-зависимые перечисления портов с разделением по направлению.
///
/// Для каждой комбинации `(PortClass, PortDirection)` генерируется отдельный `typedef
/// enum`: `{Root}_In_BitPort`, `{Root}_Out_BitPort`, `{Root}_In_NumericPort` и т.
/// Варианты именуются `{MODEL_UPPER}_{PORT_UPPER}` с последовательными значениями.
/// Определения помещаются в заголовочный файл до struct-определений.
pub(in crate::generator::c) fn generate_port_enums(
    printer: &mut Printer,
    map: &CMap,
) -> Result<(), Diagnostic> {
    let root_camelcase = map.root_name().unique_camelcase();
    let by_class = collect_ports_by_class(map)?;
    for cls in [PortClass::Bit, PortClass::Rational, PortClass::Numeric] {
        for dir in [PortDirection::In, PortDirection::Out] {
            let Some(ports) = by_class.get(&(cls, dir)) else {
                continue;
            };
            let type_name = cls.qualified_enum_name_with_dir(&root_camelcase, dir);
            printer.print("typedef enum {").nl();
            printer.up();
            for (idx, (model_name, port_name, declared)) in ports.iter().enumerate() {
                let variant = crate::generator::c::c_names::port_enum_variant(
                    model_name, port_name, *declared, dir,
                );
                printer.ident(&format!("{} = {},", variant, idx)).nl();
            }
            printer.down();
            printer.print(&format!("}} {};", type_name)).nl();
            printer.nl();
        }
    }
    Ok(())
}
