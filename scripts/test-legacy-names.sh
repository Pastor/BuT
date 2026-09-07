#!/bin/sh
# Тест проверки старых имён: проверяет, что
# scripts/check-legacy-names.sh ловит регресс, молчит на исключениях и не врёт
# на чистом файле. Шаг precheck.sh.
#
# Повод: проверка, который никогда не падал, неотличим от проверки, который не смотрит.
# Проверка A4 тест-плана 0161 - мутация: вносим старое имя, ждём падения.
#
# POSIX sh (образец - scripts/test-precheck-hygiene.sh, scripts/test-new-feature.sh).
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-legacy-names.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта старых имён (фича 0161)..."

# --- 1. Чистый файл - проверка зелен -------------------------------------------
printf '// Модель на языке Takt, собирается taktc.\n' > "$TMP/clean.takt"
if "$GATE" "$TMP/clean.takt" >/dev/null 2>&1; then
    ok "чистый файл принимается"
else
    fail "чистый файл отвергнут — гейт даёт ложное срабатывание"
fi

# --- 2. Мутация: каждое запрещённое имя обязано ловиться ---------------------
# Проверяются все шаблоны поимённо: шаблон, выпавший из ERE, иначе молчит.
for BAD in 'язык BuT' 'язык Lam' 'сборка butc file' 'сборка lamc file' \
           'редактор lam-lsp' 'файл model.but' 'файл model.lam' \
           'путь grammar/tests/data' 'путь simulation/src/eval' \
           'cargo run -p simulation'; do
    printf '// %s\n' "$BAD" > "$TMP/bad.takt"
    OUT="$("$GATE" "$TMP/bad.takt" 2>&1 || true)"
    if printf '%s' "$OUT" | grep -q 'ОШИБКА'; then
        if "$GATE" "$TMP/bad.takt" >/dev/null 2>&1; then
            fail "«$BAD» назван, но код возврата нулевой"
        else
            ok "ловится: $BAD"
        fi
    else
        fail "«$BAD» НЕ пойман — шаблон выпал из гейта"
    fi
done

# --- 3. Падение списком, а не первым местом ---------------------------------
printf '// язык Lam\n// сборка lamc\n// файл model.but\n' > "$TMP/many.takt"
HITS="$("$GATE" "$TMP/many.takt" 2>&1 | grep -c 'ОШИБКА:' || true)"
if [ "$HITS" -ge 3 ]; then
    ok "падает списком (названо мест: $HITS)"
else
    fail "названо мест: $HITS, ожидалось не менее 3 — гейт молчит о хвосте"
fi

# --- 4. Исключения работают на настоящих данных -----------------------------
# `docs/` цитирует старое имя и обязан проходить. Файл берётся
# Настоящий ( самой фичи): синтетический не доказал бы, что префикс совпадает
# так, как совпадает в дереве.
# Раздел "Архитектура" живёт в карточке фичи.
CARD="docs/features/0161-fixture-comments-rename.md"
if [ ! -f "$ROOT/$CARD" ]; then
    fail "нет $CARD — проверка исключений не проведена"
elif ! grep -qE '(^|[^A-Za-z0-9_])Lam($|[^A-Za-z0-9_-])' "$ROOT/$CARD"; then
    fail "$CARD не содержит старого имени — проверка исключений вырождена"
elif (cd "$ROOT" && "$GATE" "$CARD" >/dev/null 2>&1); then
    ok "исключение docs/ действует на настоящем файле"
else
    fail "docs/ проверяется, хотя обязан быть исключён"
fi

# --- 5. Имя в порождаемом коде ловится --------------------------
# Прежде этот класс был из проверки исключён, и проверка 5 требовала лишь, чтобы
# исключение было названо словами. переименовала хелперы
# (`lam_q_*` -> `takt_q_*`, `LAM_Q_*` -> `TAKT_Q_*`) и сняла исключение, поэтому
# проверяется теперь поимка, а не объяснение молчания.
#
# Формы взяты те, в которых старое имя и жило: определение хелпера цели `c`
# (единственная форма, доезжавшая до прошивки) и вызов POU цели `st`. Проверка
# идёт на файле с расширением порождаемого кода, а не на `.takt`: проверка смотрит
# рабочие файлы дерева, и снапшоты `examples/generated/` - тоже они.
for BAD in 'static int64_t lam_q_floordiv(int64_t x, int64_t d) {' \
           'v := LAM_Q_SAT(x, lo, hi);' \
           'return lam_q_mul(a, b, n);'; do
    printf '%s\n' "$BAD" > "$TMP/generated.c"
    OUT="$("$GATE" "$TMP/generated.c" 2>&1 || true)"
    if printf '%s' "$OUT" | grep -q 'ОШИБКА' && ! "$GATE" "$TMP/generated.c" >/dev/null 2>&1; then
        ok "ловится в порождаемом коде: $BAD"
    else
        fail "«$BAD» НЕ пойман — исключение для порождаемого кода вернулось"
    fi
done

# --- 6. Новое имя хелпера проверка не трогает -----------------------------------
# Контроль к проверке 5: без него "ловится" означало бы лишь, что проверка ругается
# на любую строку с `q_`. Форма - ровно та, которую печатает цель после 0253.
printf 'static int64_t takt_q_floordiv(int64_t x, int64_t d) {\n' > "$TMP/new.c"
printf 'v := TAKT_Q_SAT(x, lo, hi);\n' >> "$TMP/new.c"
if "$GATE" "$TMP/new.c" >/dev/null 2>&1; then
    ok "новое имя хелпера принимается"
else
    fail "новое имя `takt_q_*`/`TAKT_Q_*` отвергнуто — шаблон слишком широк"
fi

# --- 7. Служебные идентификаторы ловятся ------------------------
# Прежде класс был исключён как "видит только разработчик инструмента", и в
# дереве накопилось 114 вхождений: переменные скриптов, префикс временных
# каталогов сорока тестов, константы LSP, крейт порождённых примеров, ключи
# цветов плагина. Исключение и было причиной живучести - новое имя заводили по
# образцу соседа. Формы ниже взяты из тех самых мест.
for BAD in 'LAMC="$PRECHECK_TARGET_DIR/debug/taktc"' \
           'for lam_file in examples/*.takt; do' \
           'let dir = std::env::temp_dir().join("lam_conformance_0033");' \
           'pub(super) const BUT_KEYWORDS: &[(&str, &str)] = &[' \
           'use lam_generated::elevator::Elevator;' \
           '@JvmField val KEYWORD = key("LAM_KEYWORD", Colors.KEYWORD)'; do
    printf '%s\n' "$BAD" > "$TMP/internal.rs"
    if ! "$GATE" "$TMP/internal.rs" >/dev/null 2>&1; then
        ok "ловится служебное имя: $BAD"
    else
        fail "«$BAD» НЕ пойман — исключение для служебных имён вернулось"
    fi
done

# --- 8. Действующие служебные имена проверка не трогает --------------------------
# Контроль к проверке 7: без него "ловится" означало бы, что проверка ругается на
# любое имя с подчёркиванием. Формы - ровно те, что стоят в дереве после 0254.
printf 'TAKTC="$PRECHECK_TARGET_DIR/debug/taktc"\n' > "$TMP/new_internal.rs"
printf 'for takt_file in examples/*.takt; do\n' >> "$TMP/new_internal.rs"
printf 'let dir = std::env::temp_dir().join("takt_conformance_0033");\n' >> "$TMP/new_internal.rs"
printf 'pub(super) const TAKT_KEYWORDS: &[(&str, &str)] = &[\n' >> "$TMP/new_internal.rs"
printf 'use takt_generated::elevator::Elevator;\n' >> "$TMP/new_internal.rs"
if "$GATE" "$TMP/new_internal.rs" >/dev/null 2>&1; then
    ok "действующие служебные имена принимаются"
else
    fail "действующее имя отвергнуто — шаблон служебных имён слишком широк"
fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта старых имён провален." >&2; exit 1; }
echo "  Сторож гейта старых имён пройден."
