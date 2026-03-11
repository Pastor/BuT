pub mod c;
pub mod condition;
pub mod lc3;
pub mod ltl;
pub mod st;
pub mod thumb;
pub mod verilog;

use std::collections::HashMap;

use but_grammar::ast::{SourceUnit, SourceUnitPart, Type, VariableDefinition};

pub use c::{generate_c_all, generate_c_header, generate_c_source};
pub use lc3::{generate_lc3, generate_lc3_all};
pub use st::{generate_st_all, generate_st_decl, generate_st_program};
pub use thumb::{generate_thumb, generate_thumb_all};
pub use verilog::{generate_verilog, generate_verilog_all};

/// Контекст для генераторов кода с глобальными объявлениями.
#[derive(Debug, Default)]
pub struct CodegenContext {
    /// Глобальные определения переменных/портов из исходного файла.
    pub global_vars: Vec<Box<VariableDefinition>>,
    /// Таблица псевдонимов типов: имя псевдонима → определение типа.
    pub type_aliases: HashMap<String, Type>,
}

impl CodegenContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Построить контекст из SourceUnit, собрав глобальные объявления и псевдонимы типов.
    pub fn from_source(source: &SourceUnit) -> Self {
        let mut ctx = Self::new();
        for part in &source.0 {
            match part {
                SourceUnitPart::VariableDefinition(vd) => ctx.global_vars.push(vd.clone()),
                SourceUnitPart::TypeDefinition(td) => {
                    ctx.type_aliases.insert(td.name.name.clone(), td.ty.clone());
                }
                _ => {}
            }
        }
        ctx
    }
}

/// Сгенерировать все выходные данные для всех моделей из SourceUnit.
pub struct AllOutput {
    /// (имя_модели, заголовок, исходник)
    pub c: Vec<(String, String, String)>,
    /// (имя_модели, verilog)
    pub verilog: Vec<(String, String)>,
    /// (имя_модели, объявление, программа)
    pub st: Vec<(String, String, String)>,
    /// (имя_модели, ассемблер)
    pub lc3: Vec<(String, String)>,
    /// (имя_модели, ассемблер)
    pub thumb: Vec<(String, String)>,
}

impl AllOutput {
    pub fn generate(source: &SourceUnit) -> Self {
        let ctx = CodegenContext::from_source(source);
        Self {
            c: generate_c_all(source, &ctx),
            verilog: generate_verilog_all(source, &ctx),
            st: generate_st_all(source, &ctx),
            lc3: generate_lc3_all(source, &ctx),
            thumb: generate_thumb_all(source, &ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// BuT-источник модели Delay для тестов
    const DELAY_SRC: &str = r#"
model Delay {
    state One {
        ref None: input = 0;
        ref One : input = 1;
        enter -> { output = 1; }
    }
    state None {
        ref One : input = 1;
        ref None: input = 0;
        enter -> { output = 0; }
    }
    state End { }
    start -> None;
}
port input : bit = 0x01;
port output: bit = 0x02;
"#;

    /// Вспомогательная функция: разобрать источник и вернуть SourceUnit
    fn parse_delay() -> SourceUnit {
        but_grammar::parse(DELAY_SRC, 0)
            .expect("Исходный код Delay должен разбираться без ошибок")
            .0
    }

    // Источник с псевдонимами типов
    const TYPE_ALIAS_SRC: &str = r#"
type MyByte = u8;
type Counter = u32;
type Flag = bool;
type Nested = MyByte;
model M { start -> M; }
"#;

    fn parse_type_aliases() -> SourceUnit {
        but_grammar::parse(TYPE_ALIAS_SRC, 0)
            .expect("Исходный код с псевдонимами должен разбираться без ошибок")
            .0
    }

    // ===== CodegenContext::from_source =====

    #[test]
    fn from_source_собирает_глобальные_переменные() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        // В источнике объявлены два порта: input и output
        assert_eq!(ctx.global_vars.len(), 2);
    }

    #[test]
    fn from_source_пустой_источник_нет_переменных() {
        // Минимальный источник без глобальных переменных
        let (src, _) = but_grammar::parse("model Empty { start -> Empty; }", 0)
            .expect("Парсинг пустой модели");
        let ctx = CodegenContext::from_source(&src);
        assert_eq!(ctx.global_vars.len(), 0);
    }

    #[test]
    fn from_source_собирает_псевдонимы_типов() {
        let src = parse_type_aliases();
        let ctx = CodegenContext::from_source(&src);
        assert!(ctx.type_aliases.contains_key("MyByte"), "MyByte должен быть в таблице псевдонимов");
        assert!(ctx.type_aliases.contains_key("Counter"), "Counter должен быть в таблице псевдонимов");
        assert!(ctx.type_aliases.contains_key("Flag"), "Flag должен быть в таблице псевдонимов");
        assert!(ctx.type_aliases.contains_key("Nested"), "Nested должен быть в таблице псевдонимов");
    }

    #[test]
    fn from_source_пустой_источник_нет_псевдонимов() {
        let (src, _) = but_grammar::parse("model Empty { start -> Empty; }", 0)
            .expect("Парсинг пустой модели");
        let ctx = CodegenContext::from_source(&src);
        assert!(ctx.type_aliases.is_empty());
    }

    #[test]
    fn from_source_имена_портов_корректны() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let names: Vec<String> = ctx
            .global_vars
            .iter()
            .filter_map(|v| v.name.as_ref().map(|n| n.name.clone()))
            .collect();
        assert!(names.contains(&"input".to_string()));
        assert!(names.contains(&"output".to_string()));
    }

    // ===== AllOutput::generate =====

    #[test]
    fn all_output_generate_возвращает_данные_для_delay() {
        let src = parse_delay();
        let out = AllOutput::generate(&src);
        // Должна быть одна модель Delay во всех выходах
        assert_eq!(out.c.len(), 1);
        assert_eq!(out.verilog.len(), 1);
        assert_eq!(out.st.len(), 1);
        assert_eq!(out.lc3.len(), 1);
        assert_eq!(out.thumb.len(), 1);
    }

    #[test]
    fn all_output_имя_модели_delay() {
        let src = parse_delay();
        let out = AllOutput::generate(&src);
        // C, Verilog, LC-3, Thumb используют to_lowercase() → "delay"
        assert_eq!(out.c[0].0, "delay");
        assert_eq!(out.verilog[0].0, "delay");
        assert_eq!(out.lc3[0].0, "delay");
        assert_eq!(out.thumb[0].0, "delay");
        // ST использует to_uppercase() → "DELAY"
        assert_eq!(out.st[0].0, "DELAY");
    }

    // ===== generate_c_all =====

    #[test]
    fn generate_c_all_создаёт_заголовок_и_исходник() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        assert!(!result.is_empty());
        let (name, header, source) = &result[0];
        // Имя возвращается в нижнем регистре
        assert_eq!(name, "delay");
        // Заголовок должен содержать guard на основе верхнего регистра
        assert!(header.contains("DELAY"), "Заголовок должен содержать DELAY: {}", header);
        // Исходник должен быть непустым
        assert!(!source.is_empty(), "Исходник не должен быть пустым");
    }

    // ===== generate_verilog_all =====

    #[test]
    fn generate_verilog_all_создаёт_модуль() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_verilog_all(&src, &ctx);
        assert!(!result.is_empty());
        let (name, verilog) = &result[0];
        // Имя возвращается в нижнем регистре
        assert_eq!(name, "delay");
        // Verilog должен быть непустым и содержать ключевое слово module
        assert!(!verilog.is_empty(), "Verilog должен быть непустым");
        assert!(verilog.contains("module"), "Verilog должен содержать 'module': {}", verilog);
    }

    // ===== generate_st_all =====

    #[test]
    fn generate_st_all_создаёт_объявление_и_программу() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_st_all(&src, &ctx);
        assert!(!result.is_empty());
        let (name, decl, prog) = &result[0];
        // Имя в ST возвращается в верхнем регистре
        assert_eq!(name, "DELAY");
        // Объявление и программа должны быть непустыми
        assert!(!decl.is_empty() || !prog.is_empty(), "ST должен иметь непустой вывод");
    }

    // ===== generate_lc3_all =====

    #[test]
    fn generate_lc3_all_создаёт_ассемблер() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_lc3_all(&src, &ctx);
        assert!(!result.is_empty());
        let (name, asm) = &result[0];
        // Имя возвращается в нижнем регистре
        assert_eq!(name, "delay");
        assert!(!asm.is_empty(), "LC-3 ассемблер не должен быть пустым");
    }

    // ===== generate_thumb_all =====

    #[test]
    fn generate_thumb_all_создаёт_ассемблер() {
        let src = parse_delay();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_thumb_all(&src, &ctx);
        assert!(!result.is_empty());
        let (name, asm) = &result[0];
        // Имя возвращается в нижнем регистре
        assert_eq!(name, "delay");
        assert!(!asm.is_empty(), "Thumb ассемблер не должен быть пустым");
    }

    // ===== Интеграционные тесты: псевдонимы типов в кодогенерации =====

    /// Источник с псевдонимами типов и портами, использующими эти псевдонимы
    const TYPE_ALIAS_PORTS_SRC: &str = r#"
type Byte    = u8;
type Counter = u32;
type Flag    = bool;
type Speed   = f64;
type NestedAlias = Byte;
port sensor  : Byte        = 0x00;
port count   : Counter     = 0;
port alarm   : Flag        = false;
port rate    : Speed       = 0.0;
port raw     : NestedAlias = 0x00;
model Sensor {
    state Idle { }
    start -> Idle;
}
"#;

    fn parse_type_alias_ports() -> SourceUnit {
        but_grammar::parse(TYPE_ALIAS_PORTS_SRC, 0)
            .expect("Парсинг источника с псевдонимами типов")
            .0
    }

    #[test]
    fn c_header_разворачивает_псевдоним_byte_в_uint8_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint8_t sensor"),
            "Ожидается uint8_t для Byte, заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_разворачивает_псевдоним_counter_в_uint32_t() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint32_t count"),
            "Ожидается uint32_t для Counter, заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_разворачивает_псевдоним_flag_в_int() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("int alarm"),
            "Ожидается int (bool→int) для Flag, заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_разворачивает_псевдоним_speed_в_double() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("double rate"),
            "Ожидается double для Speed (f64), заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_разворачивает_вложенный_псевдоним_в_uint8_t() {
        // NestedAlias = Byte = u8 → uint8_t
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint8_t raw"),
            "Ожидается uint8_t для NestedAlias→Byte→u8, заголовок:\n{}", header
        );
    }

    #[test]
    fn st_decl_разворачивает_псевдоним_counter_в_dword() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_st_all(&src, &ctx);
        let (_, decl, _) = &result[0];
        assert!(
            decl.contains("DWORD"),
            "Ожидается DWORD для Counter→u32, объявление:\n{}", decl
        );
    }

    #[test]
    fn st_decl_разворачивает_псевдоним_flag_в_bool() {
        let src = parse_type_alias_ports();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_st_all(&src, &ctx);
        let (_, decl, _) = &result[0];
        assert!(
            decl.contains("BOOL"),
            "Ожидается BOOL для Flag→bool, объявление:\n{}", decl
        );
    }

    // ===== Интеграционные тесты: система типов (bit и float как базовые) =====

    /// Источник с типами, определёнными через массивы бит (в стиле std.but)
    const STD_TYPES_SRC: &str = r#"
type u8   = [8:   bit];
type u16  = [16:  bit];
type u32  = [32:  bit];
type u64  = [64:  bit];
type u128 = [128: bit];
type bool = bit;
type Counter = u32;
port x8   : u8    = 0;
port x16  : u16   = 0;
port x32  : u32   = 0;
port x64  : u64   = 0;
port x128 : u128  = 0;
port flag : bool  = false;
port cnt  : Counter = 0;
model Test { state A {} start -> A; }
"#;

    fn parse_std_types() -> SourceUnit {
        but_grammar::parse(STD_TYPES_SRC, 0)
            .expect("Исходный код со std-типами должен разбираться без ошибок")
            .0
    }

    #[test]
    fn c_header_u8_через_массив_бит_даёт_uint8_t() {
        // type u8 = [8: bit]; port x8: u8 → uint8_t x8
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint8_t x8"),
            "Ожидается uint8_t для u8=[8:bit], заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_u16_через_массив_бит_даёт_uint16_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint16_t x16"),
            "Ожидается uint16_t для u16=[16:bit], заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_u32_через_массив_бит_даёт_uint32_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint32_t x32"),
            "Ожидается uint32_t для u32=[32:bit], заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_u64_через_массив_бит_даёт_uint64_t() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint64_t x64"),
            "Ожидается uint64_t для u64=[64:bit], заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_u128_через_массив_бит_даёт_uint64_t_массив() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint64_t[2]"),
            "Ожидается uint64_t[2] для u128=[128:bit], заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_bool_через_бит_даёт_uint8_t() {
        // type bool = bit; port flag: bool → uint8_t flag
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint8_t flag"),
            "Ожидается uint8_t для bool=bit, заголовок:\n{}", header
        );
    }

    #[test]
    fn c_header_counter_через_цепочку_псевдонимов() {
        // type u32 = [32: bit]; type Counter = u32; → uint32_t cnt
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_c_all(&src, &ctx);
        let (_, header, _) = &result[0];
        assert!(
            header.contains("uint32_t cnt"),
            "Ожидается uint32_t для Counter→u32→[32:bit], заголовок:\n{}", header
        );
    }

    #[test]
    fn st_decl_u8_через_массив_бит_даёт_byte() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_st_all(&src, &ctx);
        let (_, decl, _) = &result[0];
        assert!(
            decl.contains("BYTE"),
            "Ожидается BYTE для u8=[8:bit], объявление:\n{}", decl
        );
    }

    #[test]
    fn st_decl_u32_через_массив_бит_даёт_dword() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_st_all(&src, &ctx);
        let (_, decl, _) = &result[0];
        assert!(
            decl.contains("DWORD"),
            "Ожидается DWORD для u32=[32:bit], объявление:\n{}", decl
        );
    }

    #[test]
    fn st_decl_u128_через_массив_бит_даёт_массив_lword() {
        let src = parse_std_types();
        let ctx = CodegenContext::from_source(&src);
        let result = generate_st_all(&src, &ctx);
        let (_, decl, _) = &result[0];
        assert!(
            decl.contains("ARRAY [0..1] OF LWORD"),
            "Ожидается ARRAY [0..1] OF LWORD для u128=[128:bit], объявление:\n{}", decl
        );
    }
}
