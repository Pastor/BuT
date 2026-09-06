//! Присваивание среза массива переводят четыре цели.

use takt_lang::generator::GenerateOptions;

/// Границы смещены намеренно: при нулевом начале `start + k` и `k` совпадают, и ошибка
/// в начале среза стала бы невидимой.
const SOURCE: &str = "var src: [u8; 4] := {1, 2, 3, 4}; var part: [u8; 2] := {0, 0}; \
                      out o: u8 at 0x100; \
                      start Run { always { part := src[1:3]; o := part[1]; } next Done; } \
                      state Done { }";

fn dir_for(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0355_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

fn generate_c() -> String {
    let dir = dir_for("c");
    takt_lang::compile_to_c(
        "slice",
        SOURCE,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join("slice.c")).expect("чтение")
}

fn generate(target: &str) -> String {
    let dir = dir_for(target);
    let path = dir.to_str().expect("путь");
    let opts = GenerateOptions::default();
    match target {
        "st" => takt_lang::compile_to_st("slice", SOURCE, path, &[], &opts).map(|_| ()),
        "rust" => takt_lang::compile_to_rust("slice", SOURCE, path, &[], &opts).map(|_| ()),
        "sv" => takt_lang::compile_to_sv("slice", SOURCE, path, &[], &opts).map(|_| ()),
        other => panic!("неизвестная цель '{other}'"),
    }
    .unwrap_or_else(|e| panic!("порождение для '{target}': {e:?}"));
    let ext = match target {
        "st" => "st",
        "rust" => "rs",
        _ => "sv",
    };
    std::fs::read_to_string(dir.join(format!("slice.{ext}"))).expect("чтение")
}

/// Цель `c`: массив не присваивается - только по элементам.
#[test]
fn c_prints_slice_elementwise() {
    let text = generate_c();
    assert!(
        text.contains("model->part[0] = model->src[1];")
            && text.contains("model->part[1] = model->src[2];"),
        "срез копируется по элементам со СМЕЩЕНИЕМ начала.\n{text}"
    );
}

/// Цель `st`: формы "взять кусок" в IEC 61131-3 нет.
#[test]
fn st_prints_slice_elementwise() {
    let text = generate("st");
    assert!(
        text.contains("part[0] := src[1];") && text.contains("part[1] := src[2];"),
        "срез копируется по элементам.\n{text}"
    );
}

/// Цель `rust`: `copy_from_slice` - оператор, а печатник здесь выражение.
#[test]
fn rust_prints_slice_elementwise() {
    let text = generate("rust");
    assert!(
        text.contains("self.part[0] = self.src[1];")
            && text.contains("self.part[1] = self.src[2];"),
        "срез копируется по элементам.\n{text}"
    );
}

/// Цель `sv`: массив распакованный, `{...}` для него - склейка разрядов.
#[test]
fn sv_prints_slice_elementwise() {
    let text = generate("sv");
    assert!(
        text.contains("[0] = ") && text.contains("[1] = "),
        "срез копируется по элементам.\n{text}"
    );
    assert!(
        !text.contains("[1:3]"),
        "запись Takt в вывод не переносится.\n{text}"
    );
}

/// Пропущенные границы означают края: `src[:2]`, `src[2:]`, `src[:]`.
///
/// Проверка на носителе границ, а не на печати: правило одно на четыре цели, и
/// проверять его четырежды значило бы завести четыре знания об одном.
#[test]
fn omitted_bounds_are_edges_in_c() {
    let dir = dir_for("edges");
    let source = "var src: [u8; 4] := {1, 2, 3, 4}; var head: [u8; 2] := {0, 0}; \
                  var tail: [u8; 2] := {0, 0}; var all: [u8; 4] := {0, 0, 0, 0}; \
                  out o: u8 at 0x100; \
                  start Run { always { head := src[:2]; tail := src[2:]; all := src[:]; \
                  o := head[0] + tail[0] + all[3]; } next Done; } state Done { }";
    takt_lang::compile_to_c(
        "edges",
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join("edges.c")).expect("чтение");
    assert!(
        text.contains("model->head[0] = model->src[0];"),
        "`src[:2]` начинается с нуля.\n{text}"
    );
    assert!(
        text.contains("model->tail[0] = model->src[2];"),
        "`src[2:]` идёт до конца.\n{text}"
    );
    assert!(
        text.contains("model->all[3] = model->src[3];"),
        "`src[:]` берёт весь массив.\n{text}"
    );
}

/// **Контрпример:** срез над бит-вектором отвергают все четыре цели.
///
/// `[bit;N]` при `N <= 64` - упакованный **скаляр**, и `res[0] = mem[1];` над `uint8_t`
/// не собрался бы вовсе; при `N > 64` элемент там слово, а не разряд. Эталон такой срез
/// тоже не исполняет (`SIM-010` "не является массивом"), поэтому цели обязаны
/// отказывать.
///
/// Без этой проверки правка порождала бы невалидный вывод при нулевом коде возврата
/// `taktc` - и это не гипотеза: первая редакция фичи так и делала, поймал её чужой тест
/// (`c_diagnostic_code_tests`).
#[test]
fn bit_vector_slice_is_refused_by_all_targets() {
    let source = "var mem: [bit;8] := 0; var res: [bit;8] := 0; \
                  start Main { always { res := mem[1:2]; } }";
    let opts = GenerateOptions::default();
    for target in ["c", "st", "rust", "sv"] {
        let dir = dir_for(&format!("bv_{target}"));
        let path = dir.to_str().expect("путь");
        let result = match target {
            "c" => takt_lang::compile_to_c("bv", source, path, &[], &opts).map(|_| ()),
            "st" => takt_lang::compile_to_st("bv", source, path, &[], &opts).map(|_| ()),
            "rust" => takt_lang::compile_to_rust("bv", source, path, &[], &opts).map(|_| ()),
            _ => takt_lang::compile_to_sv("bv", source, path, &[], &opts).map(|_| ()),
        };
        assert!(
            result.is_err(),
            "цель '{target}' обязана отвергнуть срез бит-вектора: поэлементной формы \
             у него нет, а эталон отвечает SIM-010"
        );
    }
}
