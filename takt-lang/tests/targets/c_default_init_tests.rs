//! Цель `c`: переменная без инициализатора обнуляется в `_init`.

use takt_lang::generator::GenerateOptions;

fn generate(tag: &str, source: &str) -> String {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0353_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    takt_lang::compile_to_c(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|e| panic!("порождение C для '{tag}': {e:?}"));
    std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение порождённого файла")
}

/// Скаляр обнуляется.
#[test]
fn scalar_without_initializer_is_zeroed() {
    let text = generate(
        "scalar",
        "var n: u8; out o: u8 at 0x100; start Run { always { o := n; } next Done; } state Done { }",
    );
    assert!(
        text.contains("model->n = 0;"),
        "переменная без инициализатора обязана получить ноль в `_init`.\n{text}"
    );
}

/// Массив обнуляется **поэлементно**: в C он не присваивается.
#[test]
fn array_without_initializer_is_zeroed_elementwise() {
    let text = generate(
        "arr",
        "var a: [u8; 3]; out o: u8 at 0x100; start Run { always { o := a[0]; } next Done; } state Done { }",
    );
    for i in 0..3 {
        assert!(
            text.contains(&format!("model->a[{i}] = 0;")),
            "элемент {i} обязан быть обнулён: массив в C не присваивается.\n{text}"
        );
    }
}

/// Бит-вектор шире 64 бит - массив слов, и обнуляется по словам.
///
/// Проверка на порядок ветвей: `[bit; 128]` есть `TypeNode::Array(128, Bit)`, и общая
/// ветвь массива напечатала бы **128** присваиваний вместо двух - по несуществующим
/// индексам.
#[test]
fn wide_bit_vector_is_zeroed_by_words() {
    let text = generate(
        "wide",
        "var w: [bit; 128]; out o: bit at 0x100; start Run { always { o := w.0; } next Done; } state Done { }",
    );
    assert!(
        text.contains("model->w[0] = 0;") && text.contains("model->w[1] = 0;"),
        "бит-вектор шире 64 бит обнуляется по СЛОВАМ (⌈128/64⌉ = 2).\n{text}"
    );
    assert!(
        !text.contains("model->w[2] = 0;"),
        "слов ровно два: счёт по разрядам дал бы 128 присваиваний.\n{text}"
    );
}

/// Структура обнуляется по полям, рекурсивно.
#[test]
fn struct_without_initializer_is_zeroed_field_by_field() {
    let text = generate(
        "nested",
        "struct In { v: u8 } struct Out { i: In, n: u8 } var s: Out; \
         out o: u8 at 0x100; start Run { always { o := s.n; } next Done; } state Done { }",
    );
    assert!(
        text.contains("model->s.i.v = 0;") && text.contains("model->s.n = 0;"),
        "структура в C не присваивается — обнуляется по полям, с рекурсией.\n{text}"
    );
}

/// **Контрпример:** авторский инициализатор нулём не подменяется.
#[test]
fn author_initializer_is_kept() {
    let text = generate(
        "kept",
        "var n: u8 := 7; out o: u8 at 0x100; start Run { always { o := n; } next Done; } state Done { }",
    );
    assert!(
        text.contains("model->n = 7;"),
        "написанное автором значение остаётся.\n{text}"
    );
    assert!(
        !text.contains("model->n = 0;"),
        "умолчание применяется только там, где инициализатора нет.\n{text}"
    );
}

/// **Контрпример:** неиспользуемая переменная в структуру не попадает - и в
/// `_init` тоже.
///
/// Штатный фильтр цели (`c_header::generate_model_header`) выбрасывает её из структуры;
/// обнуление несуществующего поля не скомпилировалось бы.
#[test]
fn unused_variable_is_not_zeroed() {
    let text = generate(
        "unused",
        "var used: u8; var spare: u8; out o: u8 at 0x100; \
         start Run { always { o := used; } next Done; } state Done { }",
    );
    assert!(
        text.contains("model->used = 0;"),
        "используемая переменная обнуляется.\n{text}"
    );
    assert!(
        !text.contains("model->spare"),
        "неиспользуемой переменной в структуре нет — обнулять нечего.\n{text}"
    );
}
