//! Печать функций модуля - цель `sv`.
//!
//! Выделено из `sv_fsm` по границе ответственности: тот модуль отвечает за автомат
//! (какие сигналы регистровые, как печатается такт), а здесь - за один вопрос: как
//! функция модели становится `function` в SystemVerilog. Повод к выносу - предел
//! размера модуля (`scripts/check-module-size.sh`).

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::sv::sv_expr::{Scope, sv002};
use crate::generator::sv::sv_fsm::{Block, Fsm};
use crate::generator::sv::sv_map::SvMap;
use crate::generator::sv::sv_module::check_sv_name;
use crate::generator::sv::sv_stmt::{
    emit_hoisted_locals, has_early_return, hoist_locals, print_statement,
};
use crate::generator::sv::sv_type::sv_type;
use crate::semantic::FunctionDefinitionNode;
use std::collections::BTreeSet;

/// Печатает константы модели как `localparam`.
///
/// `localparam`, а не `parameter`: значение задано моделью и переопределению извне не
/// подлежит - `parameter` объявил бы его настройкой модуля, которой автор не давал.
/// Печатает функции модели как `function automatic`.
///
/// **`automatic` обязателен, а не украшение.** У статической функции SV
/// переменные разделяются между вызовами, поэтому два вызова в одном
/// `always_comb` дали бы **гонку** - то есть тихо неверную схему. `function`
/// без `automatic` - скрытый дефект.
///
/// Состояние модели параметрами **не передаётся** - в отличие от цели `rust`
/// (`rust_needs::FnNeeds`): в уплощённом модуле сигналы видны функции напрямую.
pub(crate) fn emit_functions(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    blocks: &[Block],
) -> Result<(), Diagnostic> {
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        for func in model.functions.values() {
            let FunctionDefinitionNode::Local {
                name,
                params,
                ret,
                body,
                loc,
                ..
            } = func
            else {
                // `External` отвергается в месте вызова (`SV-005`): функция, которую
                // никто не зовёт, вывод не ломает и запрета не требует.
                continue;
            };
            if !map.usage().functions.contains(name) || !seen.insert(name.clone()) {
                continue;
            }
            check_sv_name(name, *loc)?;
            // Объявление функции объявляет своё место: отказ "досрочный возврат"
            // рождается вне операторов и печатался без координаты - автор не знал,
            // какую функцию переписывать.
            crate::generator::site::enter_declaration(*loc);
            let ret_ty = sv_type(ret, &format!("возвращаемый тип функции '{}'", name))?;
            let mut sig: Vec<String> = Vec::new();
            // Массив в параметре передаётся плоским вектором: распакованную размерность
            // у порта функции yosys не принимает вовсе ("input/output/inout ports
            // cannot have unpacked dimensions"), тогда как verilator её пропускает -
            // вывод компилировался и не синтезировался при нулевом коде возврата.
            let fields_of = |name: &str| fsm.structs.get(name).cloned();
            let mut unpack: Vec<(
                String,
                crate::semantic::type_node::TypeNode,
                crate::generator::sv::sv_array::FlatParam,
            )> = Vec::new();
            for (param, ty) in params {
                check_sv_name(param, *loc)?;
                // Раскладка считается по листьям: так одна форма обслуживает массив
                // скаляров, структур, перечислений и вложенный массив, а вывод для
                // скаляров остаётся прежним.
                if let Some(flat_param) =
                    crate::generator::sv::sv_array::flat_param(ty, &fields_of, &fsm.enums)
                {
                    let flat = crate::generator::sv::sv_array::flat_param_name(param);
                    sig.push(format!("input logic [{}:0] {}", flat_param.width - 1, flat));
                    unpack.push((param.clone(), ty.clone(), flat_param));
                    continue;
                }
                let decl = sv_type(ty, &format!("параметр '{}' функции '{}'", param, name))?;
                sig.push(format!("input {}", decl.declare(param)));
            }
            // Возврат-Массив объявляется через `typedef`.
            //
            // Прежде печатался один `prefix`, а распакованная размерность терялась:
            // тело присваивало массив скалярному результату, и verilator отвечал
            // "Illegal assignment", yosys - "Insufficient number of array indices", всё
            // при нулевом коде возврата `taktc`. Форма выбрана прогоном обоих
            // инструментов: возврат именованного типа принимают оба, а безымянная
            // размерность в заголовке функции - ни один.
            let ret_text = if ret_ty.suffix.is_empty() {
                ret_ty.prefix.clone()
            } else {
                let alias = format!("{}_ret_t", name.to_lowercase());
                p.ident(&format!(
                    "typedef {} {}{};",
                    ret_ty.prefix, alias, ret_ty.suffix
                ))
                .nl();
                alias
            };
            p.ident(&format!(
                "function automatic {} {}({});",
                ret_text,
                name,
                sig.join(", ")
            ))
            .nl();
            p.up();
            // Объявления - до операторов: этого требует SystemVerilog, а Takt разрешает
            // объявить переменную посреди тела.
            let mut locals = Vec::new();
            hoist_locals(body, &mut locals);
            // Поглотитель для локальной, которую тело только пишет.
            let mut unread = crate::semantic::unused::unread_locals(body);
            // Переменная цикла читается частично: гасим её разряды тем же поглотителем,
            // что и вовсе непрочитанную локальную.
            crate::generator::sv::sv_stmt::loop_variables(body, &mut unread);
            // Локальная, прочитанная только как индекс, читается частично - гасится тем
            // же поглотителем.
            let local_list: Vec<String> = locals.iter().map(|(n, _)| (*n).to_string()).collect();
            crate::generator::sv::sv_stmt::index_only_variables(body, &local_list, &mut unread);
            emit_hoisted_locals(p, &locals, &unread)?;
            // То же для параметров: их объявления в `emit_hoisted_locals` не входят,
            // поэтому поглотитель им печатается здесь - до пролога распаковки, иначе
            // объявление встало бы после операторов.
            let param_list: Vec<String> = params.iter().map(|(n, _)| n.clone()).collect();
            let mut narrow_params = Vec::new();
            crate::generator::sv::sv_stmt::index_only_variables(
                body,
                &param_list,
                &mut narrow_params,
            );
            for param in &narrow_params {
                p.ident(&format!("logic _unused_{param};")).nl();
            }
            // Параметр-Структура, прочитанный только по полям: поля, которых тело не
            // читает, гасит тот же поглотитель. Гранула - поле верхнего уровня:
            // непрочитанный лист глубже `verilator` не считает, а гасить лишнее
            // нельзя.
            let mut unread_fields: Vec<(String, String)> = Vec::new();
            for (param, ty) in params {
                let crate::semantic::type_node::TypeNode::Struct(struct_name) = ty else {
                    continue;
                };
                let Some(fields) = fsm.structs.get(struct_name) else {
                    continue;
                };
                let Some(read) = crate::generator::sv::sv_stmt::field_only_reads(body, param)
                else {
                    continue;
                };
                for (field, _) in fields {
                    if !read.contains(field) {
                        unread_fields.push((param.clone(), field.clone()));
                    }
                }
            }
            for (param, field) in &unread_fields {
                p.ident(&format!("logic _unused_{param}_{field};")).nl();
            }
            // Пролог распаковки - у носителя раскладки.
            for (param, ty, flat_param) in &unpack {
                crate::generator::sv::sv_array::emit_unpack_prologue(
                    p, param, ty, flat_param, name,
                )?;
            }
            // Возврат печатается присваиванием имени функции и исполнения не прерывает,
            // поэтому досрочный возврат сменил бы смысл молча.
            if has_early_return(body) {
                return Err(sv002(&format!(
                    "досрочный возврат из функции '{}': возврат в цели 'sv' \
                     печатается присваиванием имени функции и исполнение не \
                     прерывает, поэтому допустим только последним оператором \
                     тела. Ключевое слово 'return' эту задачу решило бы, но его \
                     не принимает синтезатор yosys. Перепишите функцию так, \
                     чтобы возврат был один и стоял в конце",
                    name
                )));
            }
            // Локальные имена функции - параметры и её `var`: без них локальная
            // переменная, чьё имя совпало с переменной модели, печаталась бы сигналом
            // модели.
            let local_names: BTreeSet<String> = params
                .iter()
                .map(|(param, _)| param.clone())
                .chain(locals.iter().map(|(local, _)| (*local).to_string()))
                .collect();
            let scope = Scope {
                guard_enable: fsm.guard_enable,
                registered: &fsm.registered,
                inouts: &fsm.inouts,
                function: Some(name),
                function_ret: Some(ret),
                locals: &local_names,
                enums: &fsm.enums,
                structs: &fsm.structs,
                warnings: &fsm.warnings,
            };
            // Тело печатается в буфер: параметру, которым тело не пользуется, verilator
            // отвечает `UNUSEDSIGNAL`, а проверка цели считает предупреждение ошибкой.
            // Признак - тот же, что у целей `c` и `rust`: вопрос задаётся напечатанному
            // тексту.
            let mut body_text = String::new();
            {
                let mut buffer = p.fork(&mut body_text);
                print_statement(&mut buffer, body, &scope)?;
            }
            for (param, _) in params {
                if crate::generator::sv::sv_unused::is_unused(&body_text, param) {
                    crate::generator::sv::sv_unused::emit_guard(p, param);
                }
            }
            p.print(&body_text);
            // Присваивание поглотителя частично прочитанного параметра - После тела:
            // чтение до записи verilator встречает `ALWCOMBORDER` (тот же порядок, что
            // у поглотителя локальной, 0387).
            for param in &narrow_params {
                p.ident(&format!("_unused_{param} = &{{1'b0, {param}}};"))
                    .nl();
            }
            for (param, field) in &unread_fields {
                p.ident(&format!(
                    "_unused_{param}_{field} = &{{1'b0, {param}.{field}}};"
                ))
                .nl();
            }
            // Поглотитель локальной, которую тело только пишет - После тела: чтение до
            // записи verilator встречает `ALWCOMBORDER`.
            crate::generator::sv::sv_stmt::emit_local_sinks(p, &locals, &unread);
            p.down();
            p.ident("endfunction").nl().nl();
            // Слой объявления снимается парно входу.
            crate::generator::site::leave_declaration();
        }
    }
    Ok(())
}
