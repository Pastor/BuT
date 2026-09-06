//! Дефолтный HAL цели `c-hal` (фича 0020-05): таблица адресов портов и
//! реализации `read_*`/`write_*` через `*(volatile T*)addr`.
//!
//! Вынесено из `c_header.rs` (фикс 0020-01 / фича 0098): файл упирался в лимит
//! размера модуля, а HAL — самостоятельная тема. `c_header::generate_header`
//! вызывает [`generate_hal`] в режиме `options.hal`.
//!
//! ## Ширина доступа к бит-порту — по слову, а не по типу (фикс 0020-01)
//!
//! Прежде `read_bit`/`write_bit` читали **один байт** и сдвигали на `b.bit`:
//! бит 8…31 давал **молча ноль** (сдвиг за пределы загруженного байта после
//! продвижения до `int`), бит ≥ 32 — **UB** (`int >> 33`, C11 6.5.7p3). Оба гейта
//! `cc` принимали это молча (`b.bit` — значение времени выполнения). Теперь
//! ширина доступа бит-порта — минимальное слово, содержащее бит
//! ([`word_bytes_for_bit`]), а `read_bit`/`write_bit` выбирают тип по `b.width`
//! (`switch`, как numeric). Бит вне [0, 63] отсекается `SE-060` до кодогенерации.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::c;
use crate::generator::c::c_map::CMap;
use crate::generator::c::c_port_enums::collect_ports_by_class;
use crate::generator::c::{
    FUNCTION_PORT_READ_BIT, FUNCTION_PORT_READ_FLOAT, FUNCTION_PORT_READ_NUMERIC,
    FUNCTION_PORT_WRITE_BIT, FUNCTION_PORT_WRITE_FLOAT, FUNCTION_PORT_WRITE_NUMERIC, PortClass,
};
use crate::generator::indent::Printer;
use crate::semantic::minimap::Name;
use crate::semantic::{PortDirection, VariableNode};

/// Ширина разыменования (в байтах) по C-типу порта из [`get_c_type`](c::get_c_type).
///
/// **Перечисление исчерпывающее** (0029-02, R9). Прежде здесь стояло `_ => 4`,
/// и всё неузнанное читалось четырьмя байтами **молча** — тот же класс дефекта,
/// против которого заведена вся фича 0029. Пробы показали, что ветка достижима
/// и даёт неверный результат: битовый порт читался 4 байтами (через `Bit` →
/// `int`), а порт структурного типа `Point` (2 байта) читается 4 байтами и
/// сегодня. На железе это доступ за пределы регистра.
///
/// `None` — ширина неизвестна: вызывающий обязан дать `CC-016`, а не выбрать
/// число за пользователя.
fn width_from_ctype(ct: &str) -> Option<u8> {
    match ct {
        "uint8_t" | "int8_t" | "bool" => Some(1),
        "uint16_t" | "int16_t" => Some(2),
        "uint32_t" | "int32_t" | "float" => Some(4),
        "uint64_t" | "int64_t" | "double" => Some(8),
        _ => None,
    }
}

/// Ширина доступа (в байтах) к бит-порту — минимальное слово, содержащее бит.
///
/// Фикс 0020-01 / ADR 0098 (правило 2): ширину доступа бит-порта задаёт **не тип
/// порта** (`bool` → 1 байт), а бит адреса. Читать один байт и сдвигать на
/// `b.bit` нельзя: бит 8…31 дал бы **молча ноль** (после продвижения до `int`
/// сдвиг за пределы загруженного байта), бит ≥ 32 — **UB** (`>> ≥ ширины`).
/// Минимальное слово по биту убирает оба класса конструктивно; бит вне [0, 63]
/// уже отсечён `SE-060`, поэтому здесь всегда 1/2/4/8.
fn word_bytes_for_bit(bit: i64) -> u8 {
    match bit {
        b if b < 8 => 1,
        b if b < 16 => 2,
        b if b < 32 => 4,
        _ => 8,
    }
}

/// C-тип порта `(model, name)` через [`get_c_type`](c::get_c_type) (для ширины доступа).
fn port_ctype(map: &CMap, model_name: &Name, port_name: &str) -> Option<String> {
    let model = map.raw_model_at(model_name.clone()).ok()?;
    let borrowed = model.borrow();
    let VariableNode::Port { ty, .. } = borrowed.variables.get(port_name)? else {
        return None;
    };
    // ⚠️ У МАССИВА-порта ширина доступа — ширина ЭЛЕМЕНТА (фича 0533): порт
    // адресуется индексом, и регистр читается по одному элементу. Она же
    // служит шагом: элемент `i` лежит по `addr + i * width`. Прежде массив
    // разворачивался по листам и сюда не доходил; после отмены разворота тип
    // массива в C не представим, и таблица адресов отказывала `CC-015` там,
    // где вывод обязан быть.
    let scalar = match ty {
        crate::semantic::type_node::TypeNode::Array(_, elem)
            if crate::semantic::bit_vector::is_bit_vector(ty).is_none() =>
        {
            elem.as_ref()
        }
        other => other,
    };
    c::get_c_type(scalar, &borrowed, map.float_width())
}

/// Фича 0020-05: эмитит таблицу адресов портов и дефолтную реализацию HAL.
///
/// Для каждой пары `(класс, направление)` генерируется `static const`-таблица
/// `{Enum}__ADDR[]`, индексируемая enum-вариантами порта, и дефолтные
/// `read_*`/`write_*` через `*(volatile T*)addr`. Помощник `bind_default_hal`
/// связывает указатели структуры с этими функциями (только для присутствующих
/// классов/направлений).
pub(super) fn generate_hal(
    printer: &mut Printer,
    map: &CMap,
    options: &crate::generator::GenerateOptions,
) -> Result<(), Diagnostic> {
    let root = map.root_name().unique_camelcase();
    let by_class = collect_ports_by_class(map)?;
    let addr_map = &options.address_map;
    let has = |c: PortClass, d: PortDirection| by_class.contains_key(&(c, d));
    // Нужен ли дефолтный источник времени `now_ms` (профиль «часы», 0134-04b).
    let needs_time = map
        .root_model_node()
        .is_some_and(|m| c::c_time::needs_now_ms(map, &m.borrow()));

    printer.nl();
    printer
        .print(&format!(
            "typedef struct {{ uintptr_t addr; int8_t bit; uint8_t width; }} {}_PortBinding;",
            root
        ))
        .nl();
    printer.nl();

    // Таблицы адресов для всех присутствующих (класс, направление).
    for cls in [PortClass::Bit, PortClass::Rational, PortClass::Numeric] {
        for dir in [PortDirection::In, PortDirection::Out] {
            let Some(ports) = by_class.get(&(cls, dir)) else {
                continue;
            };
            let enum_type = cls.qualified_enum_name_with_dir(&root, dir);
            printer
                .print(&format!(
                    "static const {root}_PortBinding {enum_type}__ADDR[] = {{"
                ))
                .nl();
            printer.up();
            for (model_name, port_name, declared) in ports {
                let variant = crate::generator::c::c_names::port_enum_variant(
                    model_name, port_name, *declared, dir,
                );
                // Фича 0084: карта ключуется квалифицированно (модель+порт) —
                // строим тот же ключ хелвером, что и продюсер `resolve_model`.
                // `model_name.unique()` == `unique_model_name(ModelNode)` (обе
                // из обхода `upper`), поэтому lookup попадает.
                let resolved = addr_map.get(&crate::address_map::qualified_port_key(
                    model_name.unique(),
                    port_name,
                ));
                let addr = resolved.map(|r| r.addr).unwrap_or(0);
                // `-1` означает «позиции бита нет» и остаётся у числовых и
                // вещественных портов: у слова разряда не выбирают, поле в их
                // таблицах не читается.
                //
                // Фича 0176: у **бит-порта** позиция нормирована слоем адресов
                // (`SE-090` подставляет ноль и говорит об этом автору), поэтому в
                // битовых таблицах `-1` появиться не может, и `read_bit`/
                // `write_bit` сдвигают на `b.bit` без проверки знака — прежняя
                // рантайм-заплатка `b.bit < 0 ? 0 : b.bit` снята как мёртвая.
                let bit = resolved.and_then(|r| r.bit).unwrap_or(-1);
                // 0029-02: было `.unwrap_or(4)` поверх `_ => 4` — два молчаливых
                // умолчания подряд. Ширина доступа к MMIO угадыванию не подлежит.
                let ct = port_ctype(map, model_name, port_name).ok_or_else(|| {
                    Diagnostic::error(
                        Location::Codegen,
                        format!(
                            "порт '{}' модели '{}': тип не представим в C — \
                             ширина доступа к регистру неизвестна",
                            port_name, model_name
                        ),
                    )
                    .with_code("CC-015")
                })?;
                let mut width = width_from_ctype(&ct).ok_or_else(|| {
                    Diagnostic::error(
                        Location::Codegen,
                        format!(
                            "порт '{}' модели '{}': ширина доступа к регистру \
                             неизвестна для типа C '{}'",
                            port_name, model_name, ct
                        ),
                    )
                    .with_code("CC-016")
                })?;
                // Бит-порт: ширина доступа — минимальное слово, содержащее бит
                // (ADR 0098 правило 2), а НЕ ширина типа `bool` (1 байт). Иначе
                // `read_bit`/`write_bit` сдвигали бы за пределы одного байта.
                if cls == PortClass::Bit {
                    width = word_bytes_for_bit(bit);
                }
                printer
                    .ident(&format!(
                        "[{variant}] = {{ (uintptr_t)0x{addr:X}u, {bit}, {width} }},",
                        addr = addr as u64,
                    ))
                    .nl();
            }
            printer.down();
            printer.print("};").nl();
        }
    }
    printer.nl();

    // Дефолтные функции чтения/записи (только для присутствующих классов).
    // ⚠️ Бит-порт: тип чтения выбирается по `b.width` (минимальное слово по биту,
    // фикс 0020-01) — иначе `>> b.bit` при бите ≥ 8 читает не тот бит либо даёт UB.
    if has(PortClass::Bit, PortDirection::In) {
        let e = PortClass::Bit.qualified_enum_name_with_dir(&root, PortDirection::In);
        printer
            .print(&format!(
                r#"static bool {root}_default_{f}({e} p, uint8_t bit, void *userdata) {{
    (void)userdata;
    {root}_PortBinding b = {e}__ADDR[p];
    int s = b.bit + (int)bit;
    switch (b.width) {{
        case 2: return ((*(volatile uint16_t*)b.addr) >> s) & 1u;
        case 4: return ((*(volatile uint32_t*)b.addr) >> s) & 1u;
        case 8: return (bool)(((*(volatile uint64_t*)b.addr) >> s) & 1u);
        default: return ((*(volatile uint8_t*)b.addr) >> s) & 1u;
    }}
}}"#,
                f = FUNCTION_PORT_READ_BIT,
            ))
            .nl();
    }
    if has(PortClass::Bit, PortDirection::Out) {
        let e = PortClass::Bit.qualified_enum_name_with_dir(&root, PortDirection::Out);
        printer
            .print(&format!(
                r#"static void {root}_default_{f}({e} p, uint8_t bit, bool val, void *userdata) {{
    (void)userdata;
    {root}_PortBinding b = {e}__ADDR[p];
    int s = b.bit + (int)bit;
    switch (b.width) {{
        case 2: {{
            volatile uint16_t *r = (volatile uint16_t*)b.addr;
            uint16_t m = (uint16_t)((uint16_t)1u << s);
            if (val) *r |= m; else *r &= (uint16_t)~m;
        }} break;
        case 4: {{
            volatile uint32_t *r = (volatile uint32_t*)b.addr;
            uint32_t m = (uint32_t)1u << s;
            if (val) *r |= m; else *r &= ~m;
        }} break;
        case 8: {{
            volatile uint64_t *r = (volatile uint64_t*)b.addr;
            uint64_t m = (uint64_t)1u << s;
            if (val) *r |= m; else *r &= ~m;
        }} break;
        default: {{
            volatile uint8_t *r = (volatile uint8_t*)b.addr;
            uint8_t m = (uint8_t)((uint8_t)1u << s);
            if (val) *r |= m; else *r &= (uint8_t)~m;
        }} break;
    }}
}}"#,
                f = FUNCTION_PORT_WRITE_BIT,
            ))
            .nl();
    }
    if has(PortClass::Rational, PortDirection::In) {
        let e = PortClass::Rational.qualified_enum_name_with_dir(&root, PortDirection::In);
        printer
            .print(&format!(
                r#"static float {root}_default_{f}({e} p, uint8_t index, void *userdata) {{
    (void)userdata;
    {root}_PortBinding b = {e}__ADDR[p];
    return *(volatile float*)(b.addr + (uintptr_t)index * b.width);
}}"#,
                f = FUNCTION_PORT_READ_FLOAT,
            ))
            .nl();
    }
    if has(PortClass::Rational, PortDirection::Out) {
        let e = PortClass::Rational.qualified_enum_name_with_dir(&root, PortDirection::Out);
        printer
            .print(&format!(
                r#"static void {root}_default_{f}({e} p, uint8_t index, float val, void *userdata) {{
    (void)userdata;
    {root}_PortBinding b = {e}__ADDR[p];
    *(volatile float*)(b.addr + (uintptr_t)index * b.width) = val;
}}"#,
                f = FUNCTION_PORT_WRITE_FLOAT,
            ))
            .nl();
    }
    if has(PortClass::Numeric, PortDirection::In) {
        let e = PortClass::Numeric.qualified_enum_name_with_dir(&root, PortDirection::In);
        printer
            .print(&format!(
                r#"static int64_t {root}_default_{f}({e} p, uint8_t index, void *userdata) {{
    (void)userdata;
    {root}_PortBinding b = {e}__ADDR[p];
    uintptr_t at = b.addr + (uintptr_t)index * b.width;
    switch (b.width) {{
        case 1: return (int64_t)*(volatile uint8_t*)at;
        case 2: return (int64_t)*(volatile uint16_t*)at;
        case 8: return (int64_t)*(volatile uint64_t*)at;
        default: return (int64_t)*(volatile uint32_t*)at;
    }}
}}"#,
                f = FUNCTION_PORT_READ_NUMERIC,
            ))
            .nl();
    }
    if has(PortClass::Numeric, PortDirection::Out) {
        let e = PortClass::Numeric.qualified_enum_name_with_dir(&root, PortDirection::Out);
        printer
            .print(&format!(
                r#"static void {root}_default_{f}({e} p, uint8_t index, int64_t val, void *userdata) {{
    (void)userdata;
    {root}_PortBinding b = {e}__ADDR[p];
    uintptr_t at = b.addr + (uintptr_t)index * b.width;
    switch (b.width) {{
        case 1: *(volatile uint8_t*)at = (uint8_t)val; break;
        case 2: *(volatile uint16_t*)at = (uint16_t)val; break;
        case 8: *(volatile uint64_t*)at = (uint64_t)val; break;
        default: *(volatile uint32_t*)at = (uint32_t)val; break;
    }}
}}"#,
                f = FUNCTION_PORT_WRITE_NUMERIC,
            ))
            .nl();
    }
    // Дефолтный источник времени профиля «часы» (фича 0134-04b): библиотечные
    // монотонные часы. ⚠️ Тянет `<time.h>` и `clock_gettime` — на голом железе их
    // нет; там штатный путь — профиль «такты» либо свой `now_ms` (см. книгу).
    // `_POSIX_C_SOURCE` объявлен у самого верха заголовка (0134-04b), иначе на
    // строгом glibc `CLOCK_MONOTONIC` скрыт под `-std=c11`.
    if needs_time {
        printer.print("#include <time.h>").nl();
        printer
            .print(&format!(
                r#"static uint64_t {root}_default_{f}(void *userdata) {{
    (void)userdata;
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000u + (uint64_t)ts.tv_nsec / 1000000u;
}}"#,
                f = c::FUNCTION_TIME_NOW_MS,
            ))
            .nl();
    }
    printer.nl();

    // Помощник связывания дефолтного HAL со структурой модели.
    //
    // ⚠️ `static inline`, а не `static` (фича 0171). Помощник объявлен в
    // ЗАГОЛОВКЕ и вызывается не каждой единицей трансляции — у голого `static`
    // это даёт `-Wunused-function` в любом файле, который заголовок включил, но
    // помощника не позвал. Замер: по одному предупреждению на каждый пример
    // корпуса, то есть у пользователя со своим `-Wall -Werror` порождённый код
    // не собрался бы вовсе. Неиспользованная `static inline` предупреждения не
    // даёт — проверено пробой на `cc`.
    printer
        .print(&format!(
            "static inline void {root}_bind_default_hal({root} *m) {{"
        ))
        .nl();
    printer.up();
    let bindings: [(bool, &str); 6] = [
        (
            has(PortClass::Bit, PortDirection::In),
            FUNCTION_PORT_READ_BIT,
        ),
        (
            has(PortClass::Bit, PortDirection::Out),
            FUNCTION_PORT_WRITE_BIT,
        ),
        (
            has(PortClass::Rational, PortDirection::In),
            FUNCTION_PORT_READ_FLOAT,
        ),
        (
            has(PortClass::Rational, PortDirection::Out),
            FUNCTION_PORT_WRITE_FLOAT,
        ),
        (
            has(PortClass::Numeric, PortDirection::In),
            FUNCTION_PORT_READ_NUMERIC,
        ),
        (
            has(PortClass::Numeric, PortDirection::Out),
            FUNCTION_PORT_WRITE_NUMERIC,
        ),
    ];
    let mut body = String::new();
    {
        let mut buffered = printer.fork(&mut body);
        for (present, field) in bindings {
            if present {
                buffered
                    .ident(&format!("m->{field} = {root}_default_{field};"))
                    .nl();
            }
        }
        if needs_time {
            buffered
                .ident(&format!(
                    "m->{f} = {root}_default_{f};",
                    f = c::FUNCTION_TIME_NOW_MS
                ))
                .nl();
        }
    }
    // У модели без адресованных портов и без источника времени связывать нечего:
    // тело пусто, и параметр остаётся неиспользуемым (фича 0260). Помощник —
    // `static inline` в заголовке, то есть предупреждение уедет к пользователю.
    if c::c_params::is_unused(&body, "m") {
        printer.ident(&c::c_params::unused_guard("m")).nl();
    }
    printer.print(&body);
    printer.down();
    printer.print("}").nl();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generator::c::c_header::generate_header;
    use crate::generator::c::c_map::CMap;
    use crate::{parse, semantic};

    /// Строит `.h` цели `c-hal` с разрешёнными адресами (тем же `resolve_addresses`,
    /// что и `taktc -t c-hal`, — иначе таблица `*__ADDR` не эмитится).
    fn generate_hal_h(src: &str, name: &str, float_width: crate::generator::FloatWidth) -> String {
        let (model_ast, _) = parse(src, 0).unwrap();
        let model = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model.borrow_mut().name = Some(name.to_string());
        let resolution = crate::address_map::resolve_addresses(
            std::rc::Rc::clone(&model),
            &[],
            &crate::address_map::AddressEnv::default(),
        );
        let model = model.borrow();
        let map = CMap::new(model.name(), &*model, true)
            .unwrap()
            .with_float_width(float_width);
        let options = crate::generator::GenerateOptions {
            hal: true,
            address_map: resolution.map,
            float_width,
            ..Default::default()
        };
        generate_header(map.get_filename(), &map, &options).unwrap()
    }

    /// Исходник цели `c-hal` с портами трёх ширин.
    const HAL_SRC: &str = r#"
in temperature: float;
in sensor: bit;
in level: u16;

address temperature = 0x1000;
address sensor = 0x2000;
address level = 0x3000;

var reading: float := 0.0;

start Idle {
    always {
        reading := temperature + 1.0;
    }
}
"#;

    /// **T10 (0029-02/03).** Ширина доступа к MMIO по типу порта.
    ///
    /// Значения **захвачены зондом** (`taktc -t c-hal`), а не угаданы. Ловит два
    /// исправления фичи 0029 сразу:
    /// - битовый порт — **1** байт (было 4: `Bit` → `int` → `_ => 4`); чтение
    ///   4 байтами из однобайтового регистра — доступ за его пределы;
    /// - вещественный порт — **8** байт (было 4: `Rational` → `float`); это
    ///   ожидаемая цена умолчания `--float-width=64`, решение заказчика.
    #[test]
    fn test_hal_port_width_follows_c_type() {
        let header = generate_hal_h(HAL_SRC, "Hal", crate::generator::FloatWidth::W64);
        assert!(
            header.contains("[HAL_PORT_SENSOR] = { (uintptr_t)0x2000u, 0, 1 },"),
            "битовый порт обязан читаться 1 байтом, позиция бита нормирована в 0 (фича 0176):\n{header}"
        );
        assert!(
            header.contains("[HAL_PORT_TEMPERATURE] = { (uintptr_t)0x1000u, -1, 8 },"),
            "вещественный порт при умолчании W64 — 8 байт:\n{header}"
        );
        assert!(
            header.contains("[HAL_PORT_LEVEL] = { (uintptr_t)0x3000u, -1, 2 },"),
            "u16 — 2 байта, без изменений:\n{header}"
        );
    }

    /// **T11 (0029-03).** `--float-width=32` возвращает вещественному порту 4
    /// байта — для платформ, где 8-байтное чтение недопустимо.
    #[test]
    fn test_hal_float_port_width_is_4_with_float_width_32() {
        let header = generate_hal_h(HAL_SRC, "Hal", crate::generator::FloatWidth::W32);
        assert!(
            header.contains("[HAL_PORT_TEMPERATURE] = { (uintptr_t)0x1000u, -1, 4 },"),
            "при W32 вещественный порт — 4 байта:\n{header}"
        );
        // Прочие ширины от флага не зависят.
        assert!(
            header.contains("[HAL_PORT_SENSOR] = { (uintptr_t)0x2000u, 0, 1 },"),
            "битовый порт от --float-width не зависит:\n{header}"
        );
    }

    /// Композиция с **одноимёнными адресованными портами** двух под-моделей.
    /// Сторож фичи 0084: до неё карта ключевалась голым именем `sig`, оба
    /// варианта `COLL_A_PORT_SIG`/`COLL_B_PORT_SIG` брали адрес по `sig` и получали ОДИН
    /// (последний, 0x20) — адрес первого порта терялся. С квалифицированным
    /// ключом каждый порт получает **свой** адрес.
    const COLLISION_SRC: &str = r#"
model A {
    out sig: bit at 0x10:0;
    start S {
        always { sig := true; }
        ref S: 1 = 1;
    }
}
model B {
    out sig: bit at 0x20:0;
    start S {
        always { sig := true; }
        ref S: 1 = 1;
    }
}
start Main = A | B;
"#;

    /// **A1 (0084).** Одноимённые порты под-моделей → каждый свой адрес в c-hal.
    #[test]
    fn address_collision_qualified_key_distinct_addresses() {
        let header = generate_hal_h(COLLISION_SRC, "Coll", crate::generator::FloatWidth::W64);
        assert!(
            header.contains("[COLL_A_PORT_SIG] = { (uintptr_t)0x10u,"),
            "порт sig под-модели A обязан получить СВОЙ адрес 0x10:\n{header}"
        );
        assert!(
            header.contains("[COLL_B_PORT_SIG] = { (uintptr_t)0x20u,"),
            "порт sig под-модели B обязан получить СВОЙ адрес 0x20:\n{header}"
        );
        // До 0084 адрес 0x10 терялся (оба варианта брали 0x20 по голому `sig`).
        assert!(
            header.contains("0x10u") && header.contains("0x20u"),
            "оба адреса обязаны присутствовать (коллизия исправлена):\n{header}"
        );
    }

    /// **R9 (0029-02).** Таблица ширин исчерпывающая: неузнанный тип даёт
    /// `None`, а не молчаливые 4 байта.
    ///
    /// Достижимость проверена зондом: порт структурного типа `Point` (2 байта)
    /// получал `width 4` — доступ за пределы регистра, выданный молча.
    #[test]
    fn test_width_from_ctype_has_no_silent_default() {
        assert_eq!(width_from_ctype("uint8_t"), Some(1));
        assert_eq!(width_from_ctype("bool"), Some(1));
        assert_eq!(width_from_ctype("uint16_t"), Some(2));
        assert_eq!(width_from_ctype("float"), Some(4));
        assert_eq!(width_from_ctype("uint32_t"), Some(4));
        assert_eq!(width_from_ctype("double"), Some(8));
        assert_eq!(width_from_ctype("uint64_t"), Some(8));
        assert_eq!(
            width_from_ctype("Point"),
            None,
            "структурный тип: ширина неизвестна → CC-016, а не 4 байта молча"
        );
        assert_eq!(
            width_from_ctype("int"),
            None,
            "`int` больше не порождается get_c_type; узнавать его нечего"
        );
    }

    /// Фикс 0020-01: ширина доступа бит-порта — минимальное слово, содержащее
    /// бит (границы степеней двойки). Бит 33 → 8 байт (uint64), а не 1 (UB).
    #[test]
    fn bit_word_width_by_bit_index() {
        assert_eq!(word_bytes_for_bit(0), 1, "бит 0 → байт");
        assert_eq!(word_bytes_for_bit(7), 1, "бит 7 — ещё байт");
        assert_eq!(word_bytes_for_bit(8), 2, "бит 8 → 2 байта");
        assert_eq!(word_bytes_for_bit(15), 2);
        assert_eq!(word_bytes_for_bit(16), 4, "бит 16 → 4 байта");
        assert_eq!(word_bytes_for_bit(31), 4);
        assert_eq!(word_bytes_for_bit(32), 8, "бит 32 → 8 байт");
        assert_eq!(word_bytes_for_bit(33), 8, "бит 33 → uint64, не UB");
        assert_eq!(word_bytes_for_bit(63), 8);
        assert_eq!(word_bytes_for_bit(-1), 1, "«нет бита» → байт, сдвиг 0");
    }
}
