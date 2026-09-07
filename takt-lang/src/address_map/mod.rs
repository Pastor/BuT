//! Внешняя карта адресов портов - отдельный `.ld`-подобный формат.
//!
//! Карта позволяет держать аппаратные адреса портов отдельно от модели и
//! переопределять их под платформу/ревизию (наложение). Формат намеренно
//! **не** является фрагментом `.takt` - это простой построчный мини-язык в духе
//! линкер-скрипта:
//!
//! ```text
//! // адреса платформы STM32
//! BTN = 0x00200000;
//! LED = 0x00200004:3;   // адрес:бит
//! ```
//!
//! Грамматика (EBNF):
//!
//! ```ebnf
//! AddressMapFile = { Entry } ;
//! Entry          = Name, "=", Address, ";" ;
//! Name           = (letter | "_"), { letter | digit | "_" } ;
//! Address        = HexOrDec, [ ":", digit, { digit } ] ;
//! ```
//!
//! Комментарии - `//` до конца строки. Приоритет источников (внешняя карта главнее
//! inline/`address` в модели) и предупреждения о наложении реализует
//! [`address_map_overlay_warnings`]; вычисление и понижение адреса в целевой код -
//! [`resolve_addresses`].
//!
//! # Раскладка модуля
//!
//! Файл разделён по темам; реэкспорт из `mod.rs` держит контракт - пути импорта у
//! потребителей не меняются:
//!
//! - [`parse`] - парсер `.ld`-формата карты и предупреждения наложения;
//! - [`env`] - среда символов `--define` ([`AddressEnv`]);
//! - [`eval`] - вычислитель выражения адреса (единственная арифметика,
//!   инвариант "`apply_binary` - одна на оба матчера");
//! - [`resolve`] - разрешение адресов портов модели с приоритетом источников.

mod env;
mod eval;
mod export;
mod export_cli;
mod parse;
mod resolve;

pub use env::{AddressEnv, parse_defines};
pub use export::{
    EXPORT_FORMAT_VERSION, export_address_map, export_address_map_json, export_map_entries,
};
pub use export_cli::{
    AddressMapOptions, EmitFormat, parse_address_map_args, run_export_subcommand,
    split_include_dirs,
};
pub use parse::{AddressMapEntry, address_map_overlay_warnings, parse_address_map};
pub use resolve::{
    AddressResolution, AddressSource, PortMeta, ResolvedAddress, address_expr_warnings,
    qualified_port_key, resolve_addresses,
};
