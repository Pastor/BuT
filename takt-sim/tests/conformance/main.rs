//! Сверки эталона с целями — ОДНА тестовая цель на все conformance-наборы
//! (фича 0244, задача 0244-02).
//!
//! # Зачем агрегатор
//!
//! Замер ADR 0244: первый запуск свежесобранного тестового бинарника стоит
//! 0.60 с при загрузке ЦП 2 % (проверка кода ядром, чтение с диска, работа
//! динамического загрузчика), повторный — 0.033 с. Бинарников было 147, и
//! каждый в гейте запускается впервые: ≈ 88 с из 152.9 с стадии `cargo test`
//! уходило только на это. Здесь 24 набора сведены в один бинарник.
//!
//! ⚠️ **Файлы не слиты** — каждый набор остаётся своим файлом и своим модулем:
//! правило размера модуля (`docs/CODE.md`) не задевается, а имя теста получает
//! префикс модуля и остаётся различимым:
//! `cargo test --test conformance conformance_c_tests::`.
//!
//! ⚠️ **Двоеточие из имени потока вычищается** там, где имя идёт в путь
//! временного каталога (инвариант фичи 0190: каталог уникален по тесту). После
//! слияния имя потока — `модуль::тест`, и без замены `:` каталог получил бы
//! двоеточие в пути.

// Общий помощник: текст вывода без комментариев (фича 0535, задача 05).
mod target_code;

mod conformance_anon_tests;
mod conformance_array_assign_tests;
mod conformance_array_param_tests;
mod conformance_array_return_tests;
mod conformance_bit_arith_tests;
mod conformance_bit_write_tests;
mod conformance_block_places_tests;
mod conformance_bounds_guard_tests;
mod conformance_builtin_tests;
mod conformance_c_arrays_tests;
mod conformance_c_bitvec_tests;
mod conformance_c_duration_tests;
mod conformance_c_every_tests;
mod conformance_c_import_tests;
mod conformance_c_shift_width_tests;
mod conformance_c_tests;
mod conformance_c_time_tests;
mod conformance_c_unconditional_edge_tests;
mod conformance_c_variable_shift_tests;
mod conformance_call_coercion_tests;
mod conformance_call_result_field_tests;
mod conformance_collapsible_if_tests;
mod conformance_composite_param_tests;
mod conformance_composition_blocks_tests;
mod conformance_composition_edge_tests;
mod conformance_cond_call_tests;
mod conformance_const_param_tests;
mod conformance_default_init_tests;
mod conformance_default_value_tests;
mod conformance_duplicate_arm_tests;
mod conformance_duration_default_tests;
mod conformance_duration_port_tests;
mod conformance_enum_default_tests;
mod conformance_enum_port_tests;
mod conformance_enum_variant_value_tests;
mod conformance_fixed_cast_const_tests;
mod conformance_fixed_cast_literal_tests;
mod conformance_fixed_literal_body_tests;
mod conformance_fixed_param_tests;
mod conformance_fixed_place_tests;
mod conformance_float_modes_tests;
mod conformance_forward_type_tests;
mod conformance_fsm_table_rust_tests;
mod conformance_fsm_table_st_tests;
mod conformance_fsm_table_sv_tests;
mod conformance_fsm_table_tests;
mod conformance_function_local_shadow_tests;
mod conformance_import_name_clash_tests;
mod conformance_import_specialize_tests;
mod conformance_inline_tests;
mod conformance_inout_port_tests;
mod conformance_invariant_scope_tests;
mod conformance_literal_left_tests;
mod conformance_local_decl_tests;
mod conformance_loop_forms_tests;
mod conformance_match_forms_tests;
mod conformance_mixed_arith_tests;
mod conformance_mixed_sign_tests;
mod conformance_modulo_tests;
mod conformance_named_cond_tests;
mod conformance_nested_chain_targets_tests;
mod conformance_nested_composition_tests;
mod conformance_nested_port_tests;
mod conformance_nested_ready_tests;
mod conformance_param_apply_tests;
mod conformance_param_modes_tests;
mod conformance_port_aggregate_init_tests;
mod conformance_port_array_tests;
mod conformance_port_init_tests;
mod conformance_port_read_in_write_tests;
mod conformance_port_split_tests;
mod conformance_port_subtree_tests;
mod conformance_postfix_index_tests;
mod conformance_power_tests;
mod conformance_rust_duration_tests;
mod conformance_rust_power_tests;
mod conformance_rust_tests;
mod conformance_rust_widen_wrap_tests;
mod conformance_self_transition_time_tests;
mod conformance_shared_const_tests;
mod conformance_shift_tests;
mod conformance_signed_enum_tests;
mod conformance_slice_argument_tests;
mod conformance_slice_tests;
mod conformance_st_duration_tests;
mod conformance_st_every_tests;
mod conformance_st_multidim_tests;
mod conformance_st_per_tick_tests;
mod conformance_st_tests;
mod conformance_st_transitive_tests;
mod conformance_state_observe_tests;
mod conformance_state_of_model_tests;
mod conformance_struct_array_field_tests;
mod conformance_struct_assign_tests;
mod conformance_struct_tests;
mod conformance_sv_array_tests;
mod conformance_sv_cast_tests;
mod conformance_sv_duration_tests;
mod conformance_sv_enum_reset_tests;
mod conformance_sv_for_tests;
mod conformance_sv_match_tests;
mod conformance_sv_mmio_tests;
mod conformance_sv_struct_local_tests;
mod conformance_sv_tests;
mod conformance_sv_time_tests;
mod conformance_target_blocks_tests;
mod conformance_var_shift_tests;
mod conformance_wide_bits_tests;
mod conformance_wider_integer_tests;
mod conformance_write_only_local_tests;

mod simulator_matrix_tests;
