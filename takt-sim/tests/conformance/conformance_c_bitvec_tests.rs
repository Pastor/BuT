//! Сверка симулятора с порождённым `taktc -t c` по **бит-векторам** `[bit;N]`.
//!
//! `[bit;N]` - упакованный N-битный вектор: N <= 64 - скаляр `uint{round_up(N)}_t`
//! (`[bit;12]`->`uint16_t`), N > 64 - массив слов `uint64_t[⌈N/64⌉]`. Симулятор обязан
//! хранить и читать биты так же, как порождённый C, - иначе он врёт про то, что
//! синтезируется. Здесь проверяется значение бит-вектора и бит-доступ `x.k` на
//! скалярном представлении (N <= 64).

use std::path::PathBuf;
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const MAX_TICKS: usize = 8;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// **.** `[bit;12]` - упакованный скаляр (хранится как `uint16_t`),
/// бит-доступ `v.k` совпадает с C. Значение `4095` (12 единичных бит) и биты 3/10.
#[test]
fn bit_vector_scalar_matches_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] bit_vector_scalar_matches_generated_c: `cc` не найден");
        return;
    }

    // `v` - [bit;12]; b3/b10 - извлечённые биты (проверяют бит-доступ на скаляре).
    let source = "\
model BitVec {
    var v: [bit;12] := 0;
    var b3: bit := 0;
    var b10: bit := 0;
    var b11: bit := 0;
    start S {
        always {
            v := 4095;
            b3 := v.3;
            b10 := v.10;
            b11 := v.11;
        }
    }
}
start Entry = BitVec;
";
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let _ = unit.tick();

    let num = |name: &str| -> i128 {
        match unit.variable(name) {
            Some(Value::Number(n)) => n,
            Some(Value::Boolean(b)) => i128::from(b),
            other => panic!("{name}: неожиданное значение {other:?}"),
        }
    };
    // Симулятор: v = 4095 (упакованный скаляр), биты 3/10/11 = 1.
    assert_eq!(num("v"), 4095, "v — упакованный скаляр [bit;12]");
    assert_eq!(num("b3"), 1, "бит 3 из 4095");
    assert_eq!(num("b10"), 1, "бит 10 из 4095");
    assert_eq!(num("b11"), 1, "бит 11 (старший из 12) из 4095");

    // Порождённый C: тип `v` - uint16_t (12 -> округление вверх до 16).
    let dir: PathBuf = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_0078_bitvec");
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    takt_lang::compile_to_c(
        "bitvec",
        source,
        dir.to_str().expect("UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    // Тип поля v обязан быть uint16_t (округление вверх 12 -> 16).
    let header = std::fs::read_to_string(dir.join("bitvec.h")).expect("заголовок");
    assert!(
        header.contains("uint16_t v"),
        "поле v должно быть uint16_t (12→16), заголовок:\n{header}"
    );

    let harness = format!(
        r#"#include <stdio.h>
#include "bitvec.h"

int main(void) {{
    Bitvec m;
    Bitvec_init(&m);
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        Bitvec_tick(&m);
        if (Bitvec_is_done(&m)) break;
    }}
    printf("v=%d\n", (int)m.entry.v);
    printf("b3=%d\n", (int)m.entry.b3);
    printf("b10=%d\n", (int)m.entry.b10);
    printf("b11=%d\n", (int)m.entry.b11);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, harness).expect("харнесс");
    let bin = dir.join("bitvec_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(&dir)
        .arg(dir.join("bitvec.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск");
    assert!(run.status.success(), "C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let c_val = |key: &str| -> i128 {
        out.lines()
            .find_map(|l| l.strip_prefix(&format!("{key}="))?.trim().parse().ok())
            .unwrap_or_else(|| panic!("C не напечатал '{key}': {out}"))
    };
    // Потактовая сверка значения и битов.
    assert_eq!(
        c_val("v"),
        num("v"),
        "v: симулятор={} C={}",
        num("v"),
        c_val("v")
    );
    assert_eq!(c_val("b3"), num("b3"), "бит 3");
    assert_eq!(c_val("b10"), num("b10"), "бит 10");
    assert_eq!(c_val("b11"), num("b11"), "бит 11");
}
