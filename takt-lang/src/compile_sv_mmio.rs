//! Публичная точка входа цели `sv-mmio`.
//!
//! В корне крейта остаётся реэкспорт [`compile_to_sv_mmio`]. Цель соединяет два шага:
//! разрешение адресов (`resolve_addresses` и приоритет источников, как у `c-hal`) и
//! понижение `float -> q`, как у `sv`.

use crate::address_map::{self, AddressEnv, AddressMapEntry};
use crate::apply_float_lowering;
use crate::compile::CompileInput;
use crate::diagnostics::{self, Diagnostic};
use crate::generator::{self, GenerateOptions};

/// Компилирует Takt в синтезируемый SystemVerilog в режиме `sv-mmio`: порт **с**
/// адресом становится битом регистрового файла на шинно-агностичном интерфейсе, порт
/// **без** адреса - портом модуля. Парная цель к
/// [`compile_to_sv`](crate::compile_to_sv), как `c-hal` парная к `c`.
///
/// Адреса разрешаются тем же слоем, что и у `c-hal`
/// ([`resolve_addresses`](address_map::resolve_addresses), приоритет inline < `address`
/// < внешняя карта), поэтому цель принимает `--address-map`. Ошибка разрешения
/// (например `SE-060` - бит вне `[0, 63]`) - немедленный отказ. Возвращает
/// предупреждения наложения/висячих записей карты (как `compile_to_c_hal`).
///
/// В отличие от `c-hal`, флаг [`GenerateOptions::hal`] **не** взводится: режим
/// регистрового файла выбирает вариант
/// [`Language::SvMmio`](generator::Language::SvMmio), а адреса читаются из
/// [`GenerateOptions::address_map`].
#[allow(clippy::too_many_arguments)]
pub fn compile_to_sv_mmio(
    filename: &str,
    source: &str,
    output_path: &str,
    search_paths: &[String],
    external: &[AddressMapEntry],
    env: &AddressEnv,
    options: &GenerateOptions,
) -> Result<Vec<Diagnostic>, Diagnostic> {
    let input = CompileInput {
        filename,
        source,
        search_paths,
        external,
        env: Some(env),
        options,
    };
    crate::compile::compile_files(crate::compile::Target::SvMmio, &input, output_path)
}

/// Та же цель, но вывод - В память: зовётся из
/// [`compile_texts`](crate::compile::compile_texts).
pub(crate) fn compile_sv_mmio_texts(
    input: &CompileInput<'_>,
) -> Result<generator::Output, Diagnostic> {
    let unit = crate::compile::named_unit(input)?;
    let default_env = AddressEnv::default();
    let (external, env, options) = (
        input.external,
        input.env.unwrap_or(&default_env),
        input.options,
    );

    // Порт составного типа разворачивается в скалярные: у регистрового файла поле
    // ложится в своё слово, и `SV-002` о "ширине, не определённой в битах" становится
    // недостижим.
    unit.lower_for_target(
        crate::semantic::condition::port_split::PortSplit::All,
        false,
    )?;

    // Разрешаем адреса (inline < address < внешняя карта) - тот же слой, что у
    // `c-hal`/`st-at`. `SE-060` (бит вне [0, 63]) -> отказ.
    let mut resolution =
        address_map::resolve_addresses(std::rc::Rc::clone(&unit.model), external, env);

    // `SE-052` ("used-порт без адреса") для `sv-mmio` - Не ошибка: порт без адреса
    // штатно становится портом модуля, в отличие от `c-hal`, где адрес обязателен
    // каждому порту. Поэтому `SE-052` снимается - и из фатальной проверки, и из
    // возвращаемых предупреждений. Прочие ошибки (`SE-060` бит вне диапазона,
    // `SE-054`/`SE-055` сломанное выражение адреса) остаются фатальными.
    resolution
        .diagnostics
        .retain(|d| d.code.as_deref() != Some("SE-052"));

    if let Some(err) = resolution
        .diagnostics
        .iter()
        .find(|d| d.level == diagnostics::Level::Error)
    {
        return Err(err.clone());
    }

    let mut mmio_options = options.clone();
    mmio_options.address_map = resolution.map;

    // `float -> q(m, n)` при `--float-as-q` - как у `sv` (без `--float-embedded`,
    // третий аргумент `false`).
    apply_float_lowering(&unit.model, options, false)?;

    // Предупреждения генератора (`SV-009`) присоединяются к адресным.
    let mut output = unit.emit_texts(generator::Language::SvMmio, &mmio_options)?;
    let mut warnings = resolution.diagnostics;
    warnings.append(&mut output.warnings);
    output.warnings = warnings;

    Ok(output)
}
