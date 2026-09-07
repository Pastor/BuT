#!/usr/bin/env bash
# Тест проверки модуля WebAssembly.
#
# Проверка, который никогда не падал, неотличим от неработающего. Здесь мутациями
# доказывается, что `check-wasm-identity.mjs` ловит ровно то, ради чего заведён:
#
# W1 - расхождение файла цели (подменённый вывод инструмента);
# W2 - расхождение трассы прогона (подменённая строка шага);
# W3 - расхождение кода отказа (инструмент отверг, "модуль" принял);
# W4 - согласованный вход принимается (иначе проверка красен всегда и бесполезен);
# W5 - отсутствие `node` даёт мягкий пропуск, а под `PRECHECK_STRICT=1` -
# ошибку (политика внешних инструментов).
#
# Мутируются подставные инструменты, а не модуль: собрать испорченный `.wasm`
# нельзя без пересборки крейта (30 с на каждую мутацию), а предмет проверки -
# способность проверки заметить расхождение, откуда бы оно ни пришло.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "Сторож гейта модуля WebAssembly (фича 0531)..."

if ! command -v node >/dev/null 2>&1; then
  if [[ "${PRECHECK_STRICT:-0}" == "1" ]]; then
    echo "  ОШИБКА: не найден node (PRECHECK_STRICT=1)"
    exit 1
  fi
  echo "  пропуск: не найден node"
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# -- Подставные инструменты ---------------------------------------------------
# `taktc`: печатает в каталог вывода один файл с известным текстом.
cat > "$WORK/taktc" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out=""
prev=""
for arg in "$@"; do
  if [[ "$prev" == "-o" ]]; then out="$arg"; fi
  prev="$arg"
done
mkdir -p "$out"
printf 'ОБРАЗЕЦ\n' > "$out/probe.c"
EOF
chmod +x "$WORK/taktc"

# `takt-sim`: печатает одну строку трассы.
cat > "$WORK/takt-sim" <<'EOF'
#!/usr/bin/env bash
printf 'Шаг   1:  [S]  vars:n=1\n'
EOF
chmod +x "$WORK/takt-sim"

# Подставной "модуль": перехватывает `WebAssembly.instantiate` и отвечает вместо
# настоящего wasm. Так проверяется сам проверка - его сверка, а не мост.
make_module() {  # $1 = файл ответа модуля, $2 = режим
  cat > "$1" <<EOF
const MODE = "$2";
const encoder = new TextEncoder();
const memory = new WebAssembly.Memory({ initial: 4 });
let last = "";
function reply(text) {
  last = text;
  const bytes = encoder.encode(text);
  new Uint8Array(memory.buffer, 0, bytes.length).set(bytes);
  return bytes.length;
}
function request(len) {
  return JSON.parse(new TextDecoder().decode(new Uint8Array(memory.buffer, 0, len)));
}
const exports = {
  memory,
  takt_io_ptr: () => 0,
  takt_io_cap: () => 65536,
  takt_io_reserve: () => 65536,
  takt_version: () => reply(JSON.stringify({ ok: true, language: "0.0.0" })),
  takt_compile: (len) => {
    request(len);
    if (MODE === "file") {
      return reply(JSON.stringify({ ok: true, files: [{ name: "probe.c", text: "ДРУГОЕ\n" }], warnings: [] }));
    }
    if (MODE === "code") {
      return reply(JSON.stringify({ ok: true, files: [{ name: "probe.c", text: "ОБРАЗЕЦ\n" }], warnings: [] }));
    }
    return reply(JSON.stringify({ ok: true, files: [{ name: "probe.c", text: "ОБРАЗЕЦ\n" }], warnings: [] }));
  },
  takt_sim_open: () => reply(JSON.stringify({ ok: true, id: 1, warnings: [] })),
  takt_sim_tick: () => {
    const line = MODE === "trace" ? "Шаг   1:  [S]  vars:n=999" : "Шаг   1:  [S]  vars:n=1";
    return reply(JSON.stringify({ ok: true, lines: [line], done: true, info: [], errors: [] }));
  },
  takt_sim_close: () => reply(JSON.stringify({ ok: true, closed: true })),
};
for (const name of ["takt_diagnostics", "takt_tokens", "takt_symbols", "takt_completion",
                    "takt_hover", "takt_goto", "takt_references", "takt_format"]) {
  exports[name] = () => reply(JSON.stringify({ ok: true }));
}
const original = WebAssembly.instantiate;
WebAssembly.instantiate = async () => ({ instance: { exports } });
EOF
}

# Дерево-двойник: один пример и один сценарий, чтобы прогон был быстрым.
setup_tree() {  # $1 = корень
  mkdir -p "$1/examples/simulations" "$1/scripts"
  printf 'var n: u8 := 0;\n\nstart S {\n    always {\n        n := n + 1;\n    }\n}\n' \
    > "$1/examples/probe.takt"
  printf '[{}]\n' > "$1/examples/simulations/probe_run.json"
  cp "$ROOT/scripts/check-wasm-identity.mjs" "$ROOT/scripts/gatelib.mjs" "$1/scripts/"
}

run_gate() {  # $1 = корень дерева, $2 = режим модуля
  make_module "$1/module.mjs" "$2"
  ( cd "$1" && node --import "./module.mjs" scripts/check-wasm-identity.mjs \
      "$1/fake.wasm" "$WORK/taktc" "$WORK/takt-sim" 2>&1 )
}

TREE="$WORK/tree"
setup_tree "$TREE"
printf 'fake' > "$TREE/fake.wasm"

# -- W4: согласованный вход принимается ---------------------------------------
if out="$(run_gate "$TREE" "ok")"; then
  echo "  OK: W4 согласованный вход принимается"
else
  echo "  ПРОВАЛ: W4 согласованный вход отвергнут:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi

# -- W1: расхождение файла ловится --------------------------------------------
if out="$(run_gate "$TREE" "file")"; then
  echo "  ПРОВАЛ: W1 подменённый файл цели не пойман"
  exit 1
fi
if grep -q "расходится" <<< "$out"; then
  echo "  OK: W1 расхождение файла ловится и названо"
else
  echo "  ПРОВАЛ: W1 отказ не называет расхождение:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi

# -- W2: расхождение трассы ловится -------------------------------------------
if out="$(run_gate "$TREE" "trace")"; then
  echo "  ПРОВАЛ: W2 подменённая строка трассы не поймана"
  exit 1
fi
if grep -q "шаг 1" <<< "$out"; then
  echo "  OK: W2 расхождение трассы ловится и названо"
else
  echo "  ПРОВАЛ: W2 отказ не называет шаг:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi

# -- W3: расхождение исхода ловится -------------------------------------------
# Инструмент отвергает вход, а "модуль" рапортует об успехе - самый дорогой
# класс: браузер показал бы файл там, где компилятор отказал.
cat > "$WORK/taktc" <<'EOF'
#!/usr/bin/env bash
echo "examples/probe.takt:2:5: Ошибка компиляции [CC-023]: подставной отказ" >&2
exit 1
EOF
chmod +x "$WORK/taktc"
if out="$(run_gate "$TREE" "ok")"; then
  echo "  ПРОВАЛ: W3 рапорт об успехе на отвергнутом входе не пойман"
  exit 1
fi
if grep -q "отверг" <<< "$out"; then
  echo "  OK: W3 расхождение исхода ловится"
else
  echo "  ПРОВАЛ: W3 отказ не называет расхождение исхода:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi

# -- W5: политика внешнего инструмента ----------------------------------------
# `node` подменяется несуществующим путём (`TAKT_NODE`): проверка обязан пропустить
# шаг словом, а под `PRECHECK_STRICT=1` - отказать. Чистить PATH нельзя -
# вместе с `node` исчезают `bash` и `cargo`, и проверялась бы не политика, а
# отсутствие оболочки.
if out="$(TAKT_NODE="$WORK/нет-такого-node" bash "$ROOT/scripts/check-wasm.sh" 2>&1)"; then
  if grep -q "пропуск" <<< "$out"; then
    echo "  OK: W5 без node — мягкий пропуск"
  else
    echo "  ПРОВАЛ: W5 без node гейт не сказал о пропуске:"
    echo "$out" | sed 's/^/    /'
    exit 1
  fi
else
  echo "  ПРОВАЛ: W5 без node гейт уронил прогон:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
if out="$(PRECHECK_STRICT=1 TAKT_NODE="$WORK/нет-такого-node" bash "$ROOT/scripts/check-wasm.sh" 2>&1)"; then
  echo "  ПРОВАЛ: W5 под PRECHECK_STRICT=1 отсутствие node принято"
  exit 1
fi
echo "  OK: W5 под PRECHECK_STRICT=1 отсутствие node — ошибка"

echo "  Сторож гейта модуля: все проверки пройдены (W1…W5)."
