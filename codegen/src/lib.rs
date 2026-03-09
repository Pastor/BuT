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
