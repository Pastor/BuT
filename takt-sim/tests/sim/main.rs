//! Тесты симулятора - Одна тестовая цель на все наборы, кроме сверок.
//!
//! Причина объединения и его устройство описаны в `tests/conformance/main.rs`: первый
//! запуск свежесобранного бинарника стоит 0.60 с при загрузке ЦП 2 %, и таких
//! бинарников было 147. Файлы наборов не слиты - каждый остаётся своим модулем, имена
//! тестов различимы (`cargo test --test sim eval_tests::`).

mod aggregate_argument_tests;
mod assignment_place_refusal_tests;
mod bit_range_text_tests;
mod cast_in_initializer_tests;
mod composition_model_always_tests;
mod composition_ports_tests;
mod computed_initializer_norm_tests;
mod const_power_tests;
mod diagnostic_notes_tests;
mod diagnostics_tests;
mod duration_tests;
mod eval_tests;
mod examples_scenario_tests;
mod extern_stub_tests;
mod film_tests;
mod fractional_initializer_tests;
mod implemented_model_sim_tests;
mod import_enum_match_tests;
mod inference_chain_tests;
mod inferred_width_tests;
mod instance_tests;
mod library_entry_tests;
mod named_port_scenario_tests;
mod run_warnings_tests;
mod scenario_run_length_tests;
mod sequential_composition_tests;
mod state_io_tests;
mod state_of_model_tests;
mod struct_types_tests;
