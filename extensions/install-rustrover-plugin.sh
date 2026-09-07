#!/bin/sh
# install-rustrover-plugin.sh - сборка плагина Takt для IntelliJ Platform и его
# установка/обновление в найденные инсталляции JetBrains RustRover.
#
# Что делает:
#   1. Ставит (переустанавливает) инструменты, на которые настроен плагин:
#      `takt-lsp`, `taktc`, `takt-sim` - через `cargo install --force` в
#      `~/.cargo/bin`.
#   2. Собирает плагин `extensions/intellij-takt` (его же gradlew -> buildPlugin),
#      получая build/distributions/intellij-takt-<версия>.zip.
#   3. Находит каталоги плагинов RustRover в стандартном месте JetBrains
#      (macOS/Linux; в т.ч. установки через Toolbox - конфиг там стандартный).
#   4. Ставит плагин, а при наличии прежней версии - обновляет (удаляет старую
#      папку плагина и распаковывает свежую).
#
# Использование:
#   extensions/install-rustrover-plugin.sh [--skip-build] [--skip-tools]
#                                          [--jdk путь] [-h|--help]
#
#   --skip-build   не пересобирать плагин - взять готовый zip из distributions.
#   --skip-tools   не переустанавливать takt-lsp/taktc/takt-sim.
#   --jdk путь     пусковой JDK для Gradle (иначе берётся текущий, а при его
#                  непригодности ищется подходящий автоматически).
#
# зачем шаг 1. Плагин и сервер `takt-lsp` - разные артефакты из одного репозитория,
# и устаревает именно сервер: плагин переустанавливают, а бинарник в `~/.cargo/bin`
# остаётся прежним. Разбор языка живёт в сервере, поэтому свежий плагин со старым
# сервером даёт ошибку на новом синтаксисе - например `SY-002: нераспознанный
# токен 'at'` на разборе `out ready: bit at 0x600:0;` (замер 2026-08-02: сервер
# отстал на 13 минорных версий крейта). Чинить это переустановкой плагина
# бесполезно - отсюда шаг.
#
# Пусковой JDK. Сборка идёт под тем
# JDK, на котором работает демон Gradle, и это не тот JDK, которым компилируется
# код (`jvmToolchain(21)` - его Gradle скачивает сам). Измерено прогоном
# `./gradlew test`: работают 17 и 26; 27+ не проверялся (такого JDK нет). Прежняя
# верхняя граница 21 снята вместе с причиной - подъёмом сборочной связки.
# Поэтому скрипт больше не ищет именно 17, а лишь следит за диапазоном:
#   * `--jdk ПУТЬ` - берётся как есть (проверяется только наличие bin/javac);
#   * текущий JDK в диапазоне - не трогается вовсе;
#   * иначе ищется подходящий (macOS - `/usr/libexec/java_home`, Linux -
#     `/usr/lib/jvm/*`, SDKMAN, `/opt/java/*`).
# Подходящий не найден - предупреждение, а не отказ: сборка идёт на текущем JDK
# (запрещать неизмеренное - та же догадка, только с другим знаком).
#
# После установки RustRover нужно перезапустить (плагины подхватываются при
# старте). Если IDE запущена, изменения применятся после перезапуска.
set -eu

SKIP_BUILD=0
SKIP_TOOLS=0
JDK_OPT=""
while [ $# -gt 0 ]; do
  case "$1" in
    --skip-build) SKIP_BUILD=1; shift ;;
    --skip-tools) SKIP_TOOLS=1; shift ;;
    --jdk)
      [ $# -ge 2 ] || { echo "--jdk требует путь к каталогу JDK" >&2; exit 2; }
      JDK_OPT="$2"; shift 2 ;;
    -h|--help)
      # Печатается вся шапка: со второй строки до первой НЕкомментарной.
      # Диапазон не зашивается числом - он разъезжается при каждой правке
      # шапки (уже разъезжался дважды, обрезая справку на полуслове).
      sed -n '2,${/^#/!q;s/^# \{0,1\}//p;}' "$0"
      exit 0 ;;
    *) echo "Неизвестный аргумент: $1" >&2; exit 2 ;;
  esac
done

# Корень скрипта = extensions/; проект плагина рядом, репозиторий - уровнем выше.
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
PLUGIN_DIR="$SCRIPT_DIR/intellij-takt"
REPO_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

[ -d "$PLUGIN_DIR" ] || { echo "Не найден проект плагина: $PLUGIN_DIR" >&2; exit 1; }
[ -x "$PLUGIN_DIR/gradlew" ] || { echo "Не найден $PLUGIN_DIR/gradlew" >&2; exit 1; }

# --- 0. Пусковой JDK -------------------------------------------------------
# Мажорная версия JDK по каталогу установки: "17.0.12" -> "17", "1.8.0_402" -> "1".
# `java -version` печатает в stderr - отсюда перенаправление.
jdk_major() {
  [ -x "$1/bin/java" ] || return 1
  "$1/bin/java" -version 2>&1 | sed -n '1s/.*version "\([0-9][0-9]*\).*/\1/p'
}

# Измеренный диапазон пускового JDK. Границы - в переменных, чтобы
# проза сообщений и сама проверка брались из одного значения: зашитое числом
# "17" в тексте разъехалось бы с проверкой при следующем перезамере.
JDK_MIN=17
JDK_MAX=26

# Версия того java, что сейчас в PATH (для сообщения, когда подходящий не нашли).
current_major() {
  command -v java >/dev/null 2>&1 || return 1
  java -version 2>&1 | sed -n '1s/.*version "\([0-9][0-9]*\).*/\1/p'
}

# Годится ли мажорная версия: пусто/нечисло - нет.
jdk_ok() {
  case "${1:-}" in
    ''|*[!0-9]*) return 1 ;;
  esac
  [ "$1" -ge "$JDK_MIN" ] && [ "$1" -le "$JDK_MAX" ]
}

# Первый JDK измеренного диапазона в типовых местах ос. Каталог обязан быть JDK,
# а не JRE (`bin/javac`), и обязан сам отчитаться версией: маска имени лжёт -
# `/usr/lib/jvm/java-17-openjdk` бывает симлинком на другую версию.
find_jdk_in_range() {
  if [ "$(uname -s)" = "Darwin" ] && [ -x /usr/libexec/java_home ]; then
    _jh=$(/usr/libexec/java_home -v "$JDK_MIN" 2>/dev/null || true)
    if [ -n "$_jh" ] && [ -x "$_jh/bin/javac" ] && jdk_ok "$(jdk_major "$_jh" 2>/dev/null || true)"; then
      echo "$_jh"
      return 0
    fi
  fi
  for _c in \
    /usr/lib/jvm/* \
    "$HOME"/.sdkman/candidates/java/* \
    /opt/java/* \
    /Library/Java/JavaVirtualMachines/*/Contents/Home \
    "$HOME"/Library/Java/JavaVirtualMachines/*/Contents/Home
  do
    [ -x "$_c/bin/javac" ] || continue
    jdk_ok "$(jdk_major "$_c" 2>/dev/null || true)" || continue
    echo "$_c"
    return 0
  done
  return 1
}

if [ "$SKIP_BUILD" -eq 0 ]; then
  if [ -n "$JDK_OPT" ]; then
    [ -x "$JDK_OPT/bin/javac" ] || {
      echo "В каталоге --jdk '$JDK_OPT' нет bin/javac — это не JDK." >&2
      exit 2
    }
    JAVA_HOME="$JDK_OPT"
    echo "==> Пусковой JDK: $JAVA_HOME (задан --jdk, версия $(jdk_major "$JAVA_HOME" 2>/dev/null || echo '?'))"
  elif jdk_ok "$(jdk_major "${JAVA_HOME:-/nonexistent}" 2>/dev/null || true)"; then
    echo "==> Пусковой JDK: $JAVA_HOME (JAVA_HOME годится, версия $(jdk_major "$JAVA_HOME"))"
  elif jdk_ok "$(current_major 2>/dev/null || true)"; then
    echo "==> Пусковой JDK: текущий из PATH (версия $(current_major)) — годится, не трогаю"
  elif JDK_FOUND=$(find_jdk_in_range); then
    JAVA_HOME="$JDK_FOUND"
    echo "==> Пусковой JDK: $JAVA_HOME (найден автоматически, версия $(jdk_major "$JAVA_HOME"))"
  else
    echo "!!  JDK из диапазона $JDK_MIN…$JDK_MAX не найден; сборка пойдёт на текущем JDK (версия $(current_major || echo 'неизвестна'))." >&2
    echo "    Измерено фичей 0224: работают $JDK_MIN и $JDK_MAX; выше не проверялось, ниже требует Gradle." >&2
    echo "    Свой JDK можно указать явно: --jdk /путь/к/jdk" >&2
  fi
  if [ -n "${JAVA_HOME:-}" ]; then
    export JAVA_HOME
    PATH="$JAVA_HOME/bin:$PATH"
    export PATH
  fi
fi

# --- 1. Инструменты: takt-lsp, taktc, takt-sim -----------------------------
# Ставятся `cargo install --force` в `~/.cargo/bin` - туда же смотрят настройки
# плагина (`serverPath`/`compilerPath`/`simulatorPath`). Сервер обязателен с
# фичей `lsp`: без флага бинарник `takt-lsp` не собирается вовсе.
if [ "$SKIP_TOOLS" -eq 0 ]; then
  if ! command -v cargo >/dev/null 2>&1; then
    echo "!!  cargo не найден — инструменты не переустановлены." >&2
    echo "    Плагин будет работать со старым takt-lsp; ошибки на новом синтаксисе — отсюда." >&2
    echo "    Пропустить шаг осознанно: --skip-tools" >&2
  else
    echo "==> Установка инструментов (takt-lsp, taktc, takt-sim)…"
    cargo install --path "$REPO_DIR/takt-lang" --bin takt-lsp --features lsp --force
    cargo install --path "$REPO_DIR/takt-lang" --bin taktc --force
    cargo install --path "$REPO_DIR/takt-sim" --bin takt-sim --force
    for T in takt-lsp taktc takt-sim; do
      BIN=$(command -v "$T" 2>/dev/null || true)
      echo "    $T → ${BIN:-НЕ НАЙДЕН в PATH}"
    done
    # Бинарник может лежать в PATH раньше `~/.cargo/bin` - тогда плагин с
    # настройкой по умолчанию возьмёт старую копию, и шаг окажется бесполезным.
    CARGO_BIN="${CARGO_HOME:-$HOME/.cargo}/bin"
    RESOLVED=$(command -v takt-lsp 2>/dev/null || true)
    if [ -n "$RESOLVED" ] && [ "$RESOLVED" != "$CARGO_BIN/takt-lsp" ]; then
      echo "!!  В PATH первым идёт $RESOLVED, а установлено в $CARGO_BIN." >&2
      echo "    Проверьте, на какой путь настроен плагин (Settings → Takt)." >&2
    fi
  fi
else
  echo "==> Инструменты не тронуты (--skip-tools)."
fi

# --- 2. Сборка плагина -----------------------------------------------------
if [ "$SKIP_BUILD" -eq 0 ]; then
  echo "==> Сборка плагина (buildPlugin)…"
  "$PLUGIN_DIR/gradlew" -p "$PLUGIN_DIR" --console=plain buildPlugin
else
  echo "==> Сборка пропущена (--skip-build)."
fi

# --- 2. Поиск собранного zip ----------------------------------------------
DIST_DIR="$PLUGIN_DIR/build/distributions"
VERSION=$(sed -n 's/^pluginVersion[[:space:]]*=[[:space:]]*//p' "$PLUGIN_DIR/gradle.properties" | tr -d '[:space:]')
ZIP="$DIST_DIR/intellij-takt-$VERSION.zip"
if [ ! -f "$ZIP" ]; then
  # Резерв: самый свежий zip в distributions.
  ZIP=$(ls -t "$DIST_DIR"/*.zip 2>/dev/null | head -1 || true)
fi
[ -n "${ZIP:-}" ] && [ -f "$ZIP" ] || {
  echo "Не найден собранный плагин в $DIST_DIR (запустите без --skip-build)." >&2
  exit 1
}

# Имя папки плагина внутри zip (верхний компонент пути первого элемента архива).
PLUGIN_NAME=$(unzip -Z1 "$ZIP" 2>/dev/null | sed -n '1p' | cut -d/ -f1)
[ -n "$PLUGIN_NAME" ] || { echo "Не удалось определить имя плагина из $ZIP" >&2; exit 1; }
echo "==> Плагин: $PLUGIN_NAME (из $(basename "$ZIP"))"

# --- 3. Каталоги плагинов RustRover ---------------------------------------
# На macOS плагины лежат в <config>/RustRover<ver>/plugins; на Linux - прямо в
# <data>/RustRover<ver>. Пробегаем все найденные версии.
OS=$(uname -s)
case "$OS" in
  Darwin) JB_BASE="$HOME/Library/Application Support/JetBrains"; MAC=1 ;;
  Linux)  JB_BASE="$HOME/.local/share/JetBrains";                MAC=0 ;;
  *) echo "ОС '$OS' не поддерживается этим скриптом (macOS/Linux)." >&2; exit 1 ;;
esac

[ -d "$JB_BASE" ] || { echo "Каталог JetBrains не найден: $JB_BASE" >&2; exit 1; }

INSTALLED=0
RUNNING=0
for RR in "$JB_BASE"/RustRover*; do
  [ -d "$RR" ] || continue   # шаблон без совпадений → пропускаем литерал
  if [ "$MAC" -eq 1 ]; then
    PLUGINS_DIR="$RR/plugins"
    LOCK="$RR/.lock"
  else
    PLUGINS_DIR="$RR"
    LOCK="$HOME/.config/JetBrains/$(basename "$RR")/.lock"
  fi

  mkdir -p "$PLUGINS_DIR"
  TARGET="$PLUGINS_DIR/$PLUGIN_NAME"
  ACTION="установлен"
  [ -d "$TARGET" ] && ACTION="обновлён"
  rm -rf "$TARGET"
  unzip -q -o "$ZIP" -d "$PLUGINS_DIR"

  echo "==> [$( basename "$RR")] плагин $ACTION → $TARGET"
  INSTALLED=$((INSTALLED + 1))
  [ -f "$LOCK" ] && RUNNING=1
done

if [ "$INSTALLED" -eq 0 ]; then
  echo "RustRover не найден в $JB_BASE (нет каталогов RustRover*)." >&2
  echo "Установите RustRover и запустите его хотя бы раз, затем повторите." >&2
  exit 1
fi

echo "==> Готово: установок обновлено — $INSTALLED."
if [ "$RUNNING" -eq 1 ]; then
  echo "!!  RustRover, похоже, запущен — перезапустите IDE, чтобы применить плагин."
else
  echo "    Запустите/перезапустите RustRover, чтобы плагин загрузился."
fi
