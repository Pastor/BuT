//! Порождённые `taktc -t rust` модули, подключённые так, как их подключает
//! пользователь: через `mod` с указанием пути.
//!
//! Атрибута `#![no_std]` здесь нет намеренно: `main`-ы печатают трассу, то есть
//! требуют std. Совместимость порождённых модулей с `no_std` доказывает не этот
//! крейт, а проверка `scripts/precheck.sh` - она кладёт каждый модуль в корень с
//! `#![no_std]`. Здесь проверяется другое: что автомат **работает** - что его
//! `tick` доводит модель до заявленного поведения на подставном железе.

#[path = "../comprehensive.rs"]
pub mod comprehensive;

#[path = "../elevator.rs"]
pub mod elevator;

#[path = "../elevator_mini.rs"]
pub mod elevator_mini;

#[path = "../extend_complex.rs"]
pub mod extend_complex;

#[path = "../stacker.rs"]
pub mod stacker;
