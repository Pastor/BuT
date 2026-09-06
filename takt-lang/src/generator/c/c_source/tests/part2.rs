//! Тесты генерации исходного C-файла, часть 2 (вынос из `c_source.rs`).
//!
//! Хелперы и импорты - из родителя через `use super::*` (приём /08).

use super::*;

#[test]
fn test_generate_source_functions() {
    // Обе функции вызываются в блоке always, чтобы они попали в UsageSet.
    let src = r#"
extern fn log_val(x: bit);
fn double_it(x: bit) -> bit { return x; }
start Main { always { log_val(double_it(0)); } }
        "#;
    let (model_ast, _) = parse(src, 0).unwrap();
    let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Main".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    let source = generate_source(map.get_filename(), &map).unwrap();
    assert!(
        source.contains("extern void log_val"),
        "extern fn отсутствует:\n{source}"
    );
    // (Д2): возвращаемый `bit` -> `uint8_t`, а не `int`.
    assert!(
        source.contains("static uint8_t Main_double_it"),
        "local fn отсутствует:\n{source}"
    );
}

#[test]
fn test_generate_if_no_double_parens() {
    // Проверяет, что условие `if` генерируется без двойных скобок: `if (cond)` а не `if
    // ((cond))`. В Takt условие `if` пишется без скобок (как в Rust): `if cond { ...
    // }`. Генератор добавляет ровно одну пару скобок для C. Функция вызывается в
    // always, чтобы попасть в UsageSet.
    let src = r#"
fn check(value: u8) -> bit {
    if value > 100 {
        return 1;
    }
    return 0;
}
start Main { always { check(0); } }
        "#;
    let (model_ast, _) = parse(src, 0).unwrap();
    let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Main".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    let source = generate_source(map.get_filename(), &map).unwrap();
    // Условие if должно иметь ровно одну пару скобок
    assert!(
        source.contains("if (value > 100)"),
        "ожидается `if (value > 100)`, получено:\n{source}"
    );
    // return должен быть без лишних скобок вокруг значения
    assert!(
        !source.contains("return (1)") && !source.contains("return (0)"),
        "return не должен оборачивать значение в скобки:\n{source}"
    );
}

#[test]
/// Проверяет, что переменная вложенной модели в функции генерируется как
/// `model->state_name.field`, а не `model->model_name.field`.
///
/// Пример: модель `Controller` инстанциируется состоянием `Entry = Controller`. Поле в
/// C-структуре называется `entry` (по имени состояния), поэтому функция `clamp` должна
/// обращаться к переменной как `model->entry.temperature`. Первый параметр функции -
/// `const Root *model` (корневая модель), не `main`.
fn test_submodel_variable_uses_state_field_name() {
    // clamp вызывается в always блоке, чтобы попасть в UsageSet. temperature
    // используется внутри clamp, поэтому поле генерируется в структуре.
    let src = r#"
model Controller {
    var temperature: u8 := 0;
    fn clamp(value: u8) -> u8 {
        if value < temperature { return temperature; }
        return value;
    }
    start Idle { always { clamp(0); } }
}
start Entry = Controller;
        "#;
    let (model_ast, _) = parse(src, 0).unwrap();
    let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Root".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    let source = generate_source(map.get_filename(), &map).unwrap();
    // Поле должно называться по имени состояния (`entry`), а не модели (`controller`).
    // Первый параметр функции - `model` (не `main`).
    assert!(
        source.contains("model->entry.temperature"),
        "ожидается `model->entry.temperature`, получено:\n{source}"
    );
    assert!(
        !source.contains("model->controller.temperature"),
        "не должно быть `model->controller.temperature`:\n{source}"
    );
}

#[test]
fn test_generate_loop_no_double_parens() {
    // Проверяет, что условие `loop` (-> `while` в C) генерируется без двойных скобок. В
    // Takt: `loop cond { ... }` - без скобок вокруг условия. Генератор добавляет ровно
    // одну пару скобок для C: `while (cond)`. Функция вызывается в always, чтобы
    // попасть в UsageSet.
    let src = r#"
fn check(n: u8) -> bit {
    loop n > 0 {
        return 0;
    }
    return 1;
}
start Main { always { check(0); } }
        "#;
    let (model_ast, _) = parse(src, 0).unwrap();
    let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Main".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    let source = generate_source(map.get_filename(), &map).unwrap();
    assert!(
        source.contains("while (n > 0)"),
        "ожидается `while (n > 0)`, получено:\n{source}"
    );
}

// -- Тесты расширенных состояний: Parallel / Concatenation -----------------

/// Вспомогательная функция: генерирует полный `.c`-исходник из Takt-строки.
fn generate_source_str(src: &str) -> String {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model_rc = semantic::tree::construct_model(&ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Root".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    generate_source(map.get_filename(), &map).unwrap()
}

/// INIT-блок для `S = A | B` должен инициализировать оба элемента параллели и выставить
/// `model->s.state = ROOT_S_INIT`.
#[test]
fn test_init_parallel_generates_init_calls() {
    let src = "model A { start Start; } model B { start Start; } start S = A | B { next End; } state End;";
    let code = generate_source_str(src);

    // Оба элемента инициализируются в INIT-блоке
    assert!(
        code.contains("RootA_init(&model->s.a0)"),
        "ожидается RootA_init в INIT:\n{code}"
    );
    assert!(
        code.contains("RootB_init(&model->s.b1)"),
        "ожидается RootB_init в INIT:\n{code}"
    );
    // Состояние параллели выставляется в INIT
    assert!(
        code.contains("model->s.state = ROOT_S_INIT;"),
        "ожидается ROOT_S_INIT:\n{code}"
    );
    // Переход в состояние S
    assert!(
        code.contains("model->state = ROOT_S;"),
        "ожидается model->state = ROOT_S:\n{code}"
    );
}

/// INIT-блок для `S = A + B` должен инициализировать только первый элемент и установить
/// `model->s_state = ROOT_S_A0`. Второй элемент должен инициализироваться только в TICK
/// при завершении первого.
#[test]
fn test_init_concatenation_generates_first_init_only() {
    let src = "model A { start Start; } model B { start Start; } start S = A + B { next End; } state End;";
    let code = generate_source_str(src);
    // Первый элемент инициализируется в INIT-блоке.
    //
    // Указатель на корень печатается по нужде: модель `A` состоянием корня не
    // пользуется, и второго аргумента у вызова больше нет. Предмет теста - **какой**
    // элемент инициализируется, а не состав аргументов.
    assert!(
        code.contains("RootA_init(&model->s_a0)"),
        "ожидается RootA_init в INIT:\n{code}"
    );
    // Указатель конкатенации выставляется на первый элемент
    assert!(
        code.contains("model->s_state = ROOT_S_A0;"),
        "ожидается ROOT_S_A0:\n{code}"
    );
    // Второй элемент инициализируется только в TICK при завершении A
    assert!(
        code.contains("RootB_init(&model->s_b1)"),
        "ожидается RootB_init в TICK (при завершении A):\n{code}"
    );
    // В INIT-блоке B идёт после A (тик A и его is_done)
    let a0_init_pos = code.find("RootA_init(&model->s_a0)").unwrap();
    let b1_init_pos = code.find("RootB_init(&model->s_b1)").unwrap();
    assert!(
        a0_init_pos < b1_init_pos,
        "RootA_init должен быть раньше RootB_init в коде:\n{code}"
    );
}

/// TICK-блок для `S = A | B` должен тикать все элементы и проверять is_done.
#[test]
fn test_tick_parallel_generates_tick_and_done_check() {
    let src = "model A { start Start; } model B { start Start; } start S = A | B { next End; } state End;";
    let code = generate_source_str(src);
    // Тик обоих элементов
    assert!(
        code.contains("RootA_tick(&model->s.a0)"),
        "ожидается RootA_tick:\n{code}"
    );
    assert!(
        code.contains("RootB_tick(&model->s.b1)"),
        "ожидается RootB_tick:\n{code}"
    );
    // Проверка is_done обоих
    assert!(
        code.contains("RootA_is_done(&model->s.a0)"),
        "ожидается RootA_is_done:\n{code}"
    );
    assert!(
        code.contains("RootB_is_done(&model->s.b1)"),
        "ожидается RootB_is_done:\n{code}"
    );
    // Оба условия объединены через &&
    assert!(
        code.contains("&&"),
        "ожидается && для объединения is_done:\n{code}"
    );
}

/// TICK-блок для `S = A + B` должен генерировать if/else if по полю s_state.
#[test]
fn test_tick_concatenation_generates_state_chain() {
    let src = "model A { start Start; } model B { start Start; } start S = A + B { next End; } state End;";
    let code = generate_source_str(src);
    // Проверка по первому элементу
    assert!(
        code.contains("model->s_state == ROOT_S_A0"),
        "ожидается ROOT_S_A0 в условии:\n{code}"
    );
    // Тик A
    assert!(
        code.contains("RootA_tick(&model->s_a0)"),
        "ожидается RootA_tick:\n{code}"
    );
    // При завершении A инициализируется B
    assert!(
        code.contains("RootB_init(&model->s_b1)"),
        "ожидается RootB_init при переходе:\n{code}"
    );
    // Проверка по второму элементу
    assert!(
        code.contains("model->s_state == ROOT_S_B1"),
        "ожидается ROOT_S_B1 в условии:\n{code}"
    );
    // Тик B
    assert!(
        code.contains("RootB_tick(&model->s_b1)"),
        "ожидается RootB_tick:\n{code}"
    );
}

/// TICK-блок для `S = A + (B | C)` должен правильно обрабатывать вложенный параллельный
/// блок внутри конкатенации.
#[test]
fn test_tick_concatenation_nested_parallel() {
    let src = "model A { start Start; } model B { start Start; } model C { start Start; }
start S = A + (B | C) { next End; }
state End;";
    let code = generate_source_str(src);
    // Тик A как первый элемент конкатенации
    assert!(
        code.contains("model->s_state == ROOT_S_A0"),
        "ожидается ROOT_S_A0:\n{code}"
    );
    // Параллельный блок как второй элемент конкатенации
    assert!(
        code.contains("ROOT_S_PARALLEL1"),
        "ожидается ROOT_S_PARALLEL1:\n{code}"
    );
    // Тик B внутри вложенной параллели
    assert!(
        code.contains("RootB_tick(&model->s_parallel1.b0)"),
        "ожидается RootB_tick в параллели:\n{code}"
    );
    assert!(
        code.contains("RootC_tick(&model->s_parallel1.c1)"),
        "ожидается RootC_tick в параллели:\n{code}"
    );
}

/// Генерация extend_complex.takt не должна возвращать ошибку.
#[test]
fn test_extend_complex_generates_without_error() {
    let src = std::fs::read_to_string("../examples/extend_complex.takt")
        .expect("не удалось прочитать extend_complex.takt");
    let (ast, _) = parse(&src, 0).expect("ошибка разбора extend_complex.takt");
    let model_rc =
        semantic::tree::construct_model(&ast, None, &[]).expect("ошибка построения модели");
    model_rc.borrow_mut().name = Some("extend_complex".to_string());
    let model = model_rc.borrow();
    let map = CMap::new("extend_complex", &*model, false).expect("ошибка создания CMap");
    let result = generate_source(map.get_filename(), &map);
    assert!(
        result.is_ok(),
        "ожидается успешная генерация: {:?}",
        result.err()
    );
    let code = result.unwrap();
    // INIT для параллели: оба элемента C1, C2 инициализируются
    assert!(
        code.contains("ExtendComplexCC1_init"),
        "ожидается ExtendComplexCC1_init:\n{code}"
    );
    assert!(
        code.contains("ExtendComplexCC2_init"),
        "ожидается ExtendComplexCC2_init:\n{code}"
    );
    // INIT для конкатенации: только первый элемент A инициализируется
    assert!(
        code.contains("ExtendComplexA_init"),
        "ожидается ExtendComplexA_init:\n{code}"
    );
}

// -- Тесты единственного терминального состояния END -----------------------

/// Терминальное состояние с произвольным именем (не End) должно переходить в MODEL_END.
#[test]
fn test_terminal_state_transitions_to_end() {
    let src = "start S { ref Done: true; } state Done;";
    let code = generate_source_str(src);
    // Done - терминальное состояние, должно переходить в ROOT_END
    assert!(
        code.contains("model->state = ROOT_END;"),
        "ожидается переход Done → ROOT_END:\n{code}"
    );
    // is_done должна проверять ROOT_END
    assert!(
        code.contains("model->state == ROOT_END"),
        "ожидается is_done проверяет ROOT_END:\n{code}"
    );
}

/// Состояние End уже является терминальным - не должно иметь самоперехода.
#[test]
fn test_end_state_no_self_transition() {
    let src = "start S { ref End: true; } state End;";
    let code = generate_source_str(src);
    // End IS ROOT_END, не должно быть model->state = ROOT_END; внутри case End is_done
    // должна проверять ROOT_END
    assert!(
        code.contains("model->state == ROOT_END"),
        "ожидается is_done проверяет ROOT_END:\n{code}"
    );
    // Не должно быть лишнего перехода End->End
    let end_case_start = code.find("case ROOT_END:").unwrap_or(0);
    let _before_end = &code[..end_case_start];
    // До блока ROOT_END: нет model->state = ROOT_END (переход только из S)
    let transition_in_s = code.contains("model->state = ROOT_END;");
    assert!(transition_in_s, "ожидается переход S → ROOT_END:\n{code}");
}

/// is_done всегда проверяет MODEL_END, даже если нет явных терминальных состояний.
#[test]
fn test_is_done_always_checks_model_end() {
    let src = "model A { start Start; } start S = A { next End; } state End;";
    let code = generate_source_str(src);
    // is_done для A: проверяет ROOT_A_END
    assert!(
        code.contains("model->state == ROOT_A_END"),
        "ожидается is_done для A проверяет ROOT_A_END:\n{code}"
    );
    // is_done для Root: проверяет ROOT_END
    assert!(
        code.contains("model->state == ROOT_END"),
        "ожидается is_done для Root проверяет ROOT_END:\n{code}"
    );
}

/// Вложенная модель с нестандартным терминальным состоянием.
#[test]
fn test_submodel_terminal_state_transitions_to_end() {
    let src = "model A { start Run; state Finish; } start S = A { next End; } state End;";
    let code = generate_source_str(src);
    // Finish (терминальное в A) должно переходить в ROOT_A_END
    assert!(
        code.contains("model->state = ROOT_A_END;"),
        "ожидается Finish → ROOT_A_END:\n{code}"
    );
    // is_done для A: ROOT_A_END
    assert!(
        code.contains("model->state == ROOT_A_END"),
        "ожидается is_done для A:\n{code}"
    );
}

// -- Тесты BitAccess --------------------------------------------------------

/// Чтение бита переменной в условии `ref`: `flags.2` -> `((model->flags >> 2) & 1u)`
#[test]
fn test_bit_access_var_read_in_condition() {
    let src = "var flags: u8 := 0; start S { ref Done: flags.2; } state Done;";
    let code = generate_source_str(src);
    assert!(
        code.contains("((model->flags >> 2) & 1u)"),
        "ожидается ((model->flags >> 2) & 1u) в условии:\n{code}"
    );
}

/// Чтение бита порта в условии `ref`: `BTN.0` ->
/// `(((*model->read_numeric)(ROOT_PORT_BTN, ...) >> 0) & 1u)` Корневая модель:
/// используется `model->` (не `main->`).
#[test]
fn test_bit_access_port_read_in_condition() {
    let src = "in BTN: u8 at 0x200000; start S { ref Done: BTN.0; } state Done;";
    let code = generate_source_str(src);
    assert!(
        code.contains("(((*model->read_numeric)(ROOT_PORT_BTN, 0, model->userdata) >> 0) & 1u)"),
        "ожидается (((*model->read_numeric)(ROOT_PORT_BTN, ...) >> 0) & 1u) в условии:\n{code}"
    );
}

/// Чтение бита переменной в блоке `always`: `x = flags.3` -> `((model->flags >> 3) &
/// 1ull)`
///
/// Суфисправление маски - `ull` с: литерал `1u` есть 32-битный `unsigned int`, и та же форма
/// при разряде >= 32 давала `shift count >= width of type` даже у скалярного `u64`.
#[test]
fn test_bit_access_var_read_in_always() {
    let src = "var flags: u8 := 0; var x: u8 := 0; start S { always { x := flags.3; } ref Done: true; } state Done;";
    let code = generate_source_str(src);
    assert!(
        code.contains("((model->flags >> 3) & 1ull)"),
        "ожидается ((model->flags >> 3) & 1ull) при чтении в always:\n{code}"
    );
}

/// Запись бита переменной: `flags.3 = true` -> bit-set идиома C
#[test]
fn test_bit_access_var_write_in_always() {
    let src =
        "var flags: u8 := 0; start S { always { flags.3 := true; } ref Done: true; } state Done;";
    let code = generate_source_str(src);
    assert!(
        code.contains("model->flags = (model->flags & ~(1ull << 3)) | ((true & 1ull) << 3)"),
        "ожидается bit-set идиома для flags.3 = true:\n{code}"
    );
}

/// Чтение бита порта в `always`: `x = BTN.0` ->
/// `(((*model->read_numeric)(ROOT_PORT_BTN, ...) >> 0) & 1u)` Корневая модель: tick
/// получает `model`, поэтому используется `model->`.
#[test]
fn test_bit_access_port_read_in_always() {
    let src = "in BTN: u8 at 0x200000; var x: u8 := 0; start S { always { x := BTN.0; } ref Done: true; } state Done;";
    let code = generate_source_str(src);
    assert!(
        code.contains("(((*model->read_numeric)(ROOT_PORT_BTN, 0, model->userdata) >> 0) & 1u)"),
        "разряд ЧИСЛОВОГО порта читается как часть значения, элемент нулевой (0533):\n{code}"
    );
}

/// Запись бита порта: `LED.7 = true` -> read-modify-write через write_numeric Корневая
/// модель: используется `model->` (не `main->`).
#[test]
fn test_bit_access_port_write_in_always() {
    let src = "out LED: u8 at 0x100000; start S { always { LED.7 := true; } ref Done: true; } state Done;";
    let code = generate_source_str(src);
    assert!(
        code.contains("write_numeric)(ROOT_PORT_LED, 0,")
            && code.contains("read_numeric)(ROOT_PORT_LED, 0, model->userdata) & ~(1LL << 7)")
            && code.contains("(true & 1LL) << 7)"),
        "разряд ЧИСЛОВОГО порта пишется чтением-правкой-записью, элемент нулевой (0533):\n{code}"
    );
}

/// Локальная функция, вызываемая из always-блока корневой модели, должна получать
/// `model` как первый аргумент, а не `main` (в tick корневой модели нет параметра
/// `main`).
#[test]
fn test_sub_model_local_fn_args_use_model_not_main() {
    // В локальной функции Sub_compute (has_model=false), первый параметр - `const Main
    // *model`. При вызове Main_process(root_val), root_val принадлежит Main. Должно
    // генерироваться `model->root_val`, а не несуществующий `main->root_val`.
    let src = r#"
var root_val: bit := 0;
var result: bit := 0;
fn process(x: bit) -> bit { return x; }
model Sub {
    fn compute() -> bit { return process(root_val); }
    start S { always { result := compute(); } }
}
start Main = Sub;
"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model_rc = semantic::tree::construct_model(&ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Main".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    let c_source = generate_source("test", &map).unwrap();
    // Sub_compute вызывает Main_process(model, root_val). root_val в теле Sub_compute
    // должен быть `model->root_val`, а не `main->root_val`
    assert!(
        !c_source.contains("main->root_val"),
        "root_val в аргументе локальной функции Sub не должен использовать `main`:\n{}",
        c_source
    );
    assert!(
        c_source.contains("model->root_val"),
        "root_val в аргументе локальной функции Sub должен использовать `model`:\n{}",
        c_source
    );
}

#[test]
fn test_local_fn_call_in_root_tick_uses_model_not_main() {
    let src = r#"
fn double(x: u8) -> u8 { return x + x; }
var y: u8 := 0;
start Main {
    always { y := double(y); }
}
"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model_rc = semantic::tree::construct_model(&ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Main".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    let c_source = generate_source("test", &map).unwrap();
    // Вызов должен передавать `model` (корневой указатель), а не несуществующий `main`
    assert!(
        c_source.contains("Main_double(model"),
        "Ожидался вызов Main_double(model, ...), но получили:\n{}",
        c_source
    );
    assert!(
        !c_source.contains("Main_double(main"),
        "Недопустимый вызов Main_double(main, ...) в tick корневой модели:\n{}",
        c_source
    );
}

#[test]
fn test_port_read_in_local_fn_uses_model_not_main() {
    // В локальной функции (has_model=false) первый параметр - `const Root *model`.
    // Чтение порта должно генерировать `(*model->read_bit)(...)`, а не
    // `(*main->read_bit)(...)`.
    let src = r#"
in sensor: bit at 0x0:0;
var v: bit := 0;
fn read_port() -> bit { return sensor; }
start Main { always { v := read_port(); } }
"#;
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    let model_rc = semantic::tree::construct_model(&ast, None, &[]).unwrap();
    model_rc.borrow_mut().name = Some("Main".to_string());
    let model = model_rc.borrow();
    let map = CMap::new(model.name(), &*model, true).unwrap();
    let c_source = generate_source("test", &map).unwrap();
    // Функция read_port вызывается в always, поэтому попадёт в UsageSet Порт внутри
    // локальной функции должен использовать `model`, а не `main`
    assert!(
        !c_source.contains("(*main->"),
        "Чтение порта в локальной функции не должно использовать `main`:\n{}",
        c_source
    );
    assert!(
        c_source.contains("(*model->"),
        "Чтение порта в локальной функции должно использовать `model`:\n{}",
        c_source
    );
}
