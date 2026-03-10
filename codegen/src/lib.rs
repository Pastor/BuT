pub mod c;
pub mod condition;
pub mod lc3;
pub mod ltl;
pub mod st;
pub mod thumb;
pub mod verilog;

use but_grammar::ast::{SourceUnit, SourceUnitPart, VariableDefinition};

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
}

impl CodegenContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Построить контекст из SourceUnit, собрав глобальные объявления.
    pub fn from_source(source: &SourceUnit) -> Self {
        let mut ctx = Self::new();
        for part in &source.0 {
            if let SourceUnitPart::VariableDefinition(vd) = part {
                ctx.global_vars.push(vd.clone());
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
}
