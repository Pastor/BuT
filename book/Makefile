# Сборка документа "Язык Takt - описание" (Typst -> PDF).
#
# Внешний инструмент один - `typst`. Прежняя связка (mdbook + mdbook-pandoc +
# pandoc + latexmk + xelatex + rsvg-convert) снята переходом на Typst: вёрстка,
# метаданные, подсветка и вставка примеров живут в самом документе
# (src/template.typ, src/main.typ, ../takt.sublime-syntax, ../takt.tmTheme).
#
# Зависимости:
#   - typst >= 0.13     - https://typst.app/ (brew install typst / cargo install typst-cli)
#                         проверенная версия - 0.15.1
#   - шрифт Fira Code   - установить в систему (https://github.com/tonsky/FiraCode)
#   - для авто-регенерации графов верификации (цель `graphs`, зависимость `build`):
#     cargo (сборка taktc), dot (Graphviz), python3 + fontTools, шрифт ГОСТ 2.304-81
#     тип А (наклонный, ставит цель `fonts`). Все - best-effort: нет инструмента ->
#     регенерация пропускается, берутся закоммиченные (самодостаточные) SVG.
#     SVG вставляет сам Typst, поэтому rsvg-convert больше не нужен.
#
# Кросс-ссылки (оглавление, ссылки на разделы и приложения) сходятся за один
# прогон: Typst разрешает метки сам, а неизвестная метка у него ошибка сборки, а
# не "??" в готовом PDF. Повторные проходы (latexmk) не нужны.
#
# Результат: book/book/pdf/takt-language.pdf (каталог сборки игнорируется git).
#
# Цели:
#   make build   - регенерировать графы + собрать PDF
#   make pdf     - собрать PDF без регенерации графов (шаг предкоммита: проверка не
#                  вправе менять файлы репозитория - перегенерация SVG оставляла
#                  за собой изменённую строку версии graphviz)
#   make check   - то же "на прочность" (для CI)
#   make graphs  - регенерировать SVG-графы верификации (taktc -> dot -> флаттеринг)
#   make fonts   - установить шрифт ГОСТ тип А, если его нет
#   make clean   - удалить каталог сборки

TYPST ?= typst

# Корень репозитория (Makefile лежит в book/). Нужен для сборки taktc и запуска
# скрипта регенерации графов.
ROOT := $(abspath $(CURDIR)/..)

# Корень проекта Typst - каталог book/: относительно него разрешаются пути,
# начинающиеся со `/` (например `/takt.sublime-syntax` в шаблоне). Выше корня
# Typst не читает - это его песочница.
TYPST_ROOT := $(CURDIR)
MAIN       := src/main.typ
OUT        := book/pdf/takt-language.pdf

# Шрифт подписей графов верификации - ГОСТ тип А, наклонный (ГОСТ 2.304-81,
# стандартное чертёжное начертание). Диаграммы Крипке/Бюхи (`taktc verify
# --emit-graph` -> SVG) подписаны им; без шрифта rsvg-convert подставит запасной.
# Свободный шрифт из релиза проекта Metrolog/Font.GOST2.304-81.
GOST_FONT_URL  := https://github.com/Metrolog/Font.GOST2.304-81/releases/download/v0.7.6/GOST2.304-81TypeA-Slanted.ttf
GOST_FONT_FILE := GOST2.304-81TypeA-Slanted.ttf
# Каталог пользовательских шрифтов по ос (macOS / прочее).
FONT_DIR := $(if $(filter Darwin,$(shell uname)),$(HOME)/Library/Fonts,$(HOME)/.local/share/fonts)

.PHONY: build check clean deps fonts graphs pdf

# Регенерирует SVG-диаграммы верификации перед сборкой - устраняет ручной шаг.
# Конвейер: taktc verify --emit-graph -> dot -Tsvg -> svg_flatten_text.py (см.
# book/scripts/render_verify_graphs.sh). Требует cargo (taktc), dot (Graphviz),
# python3 + fontTools и шрифт ГОСТ (цель fonts). BEST-EFFORT: если инструмента или
# шрифта нет, оставляет уже закоммиченные (самодостаточные) SVG и не валит сборку -
# так минимальная PDF-сборка без Rust/Graphviz продолжает работать.
graphs: fonts
	@ok=1; \
	for t in cargo dot python3; do command -v $$t >/dev/null 2>&1 || ok=0; done; \
	python3 -c "import fontTools" >/dev/null 2>&1 || ok=0; \
	fc-list 2>/dev/null | grep -qi "GOST 2.304" || ok=0; \
	if [ "$$ok" = "1" ]; then \
	  echo "Регенерация графов верификации…"; \
	  ( cd "$(ROOT)" && cargo build --quiet --bin taktc ) \
	    && sh "$(ROOT)/book/scripts/render_verify_graphs.sh" \
	    || echo "  ⚠ регенерация не удалась — использую закоммиченные SVG."; \
	else \
	  echo "  ⚠ нет cargo/dot/python3+fontTools/шрифта — пропускаю регенерацию (использую закоммиченные SVG)."; \
	fi

# Устанавливает шрифт ГОСТ тип А (наклонный), если его ещё нет в системе.
# Идемпотентно (при наличии семейства ничего не качает) и BEST-EFFORT: сбой
# загрузки (офлайн, нет curl/wget) не валит сборку - подписи графов уже впечатаны
# в SVG контурами (см. book/scripts/svg_flatten_text.py), поэтому для сборки PDF
# шрифт не требуется. Он нужен лишь для регенерации графов
# (book/scripts/render_verify_graphs.sh).
fonts:
	@if fc-list 2>/dev/null | grep -qi "GOST 2.304"; then \
	  echo "Шрифт ГОСТ 2.304-81 уже установлен."; \
	else \
	  echo "Ставлю шрифт ГОСТ тип А (наклонный) в $(FONT_DIR) (best-effort)…"; \
	  mkdir -p "$(FONT_DIR)"; \
	  if command -v curl >/dev/null 2>&1; then \
	    curl -fsSL "$(GOST_FONT_URL)" -o "$(FONT_DIR)/$(GOST_FONT_FILE)" \
	      || echo "  ⚠ загрузка шрифта не удалась — не критично (SVG самодостаточны)."; \
	  elif command -v wget >/dev/null 2>&1; then \
	    wget -qO "$(FONT_DIR)/$(GOST_FONT_FILE)" "$(GOST_FONT_URL)" \
	      || echo "  ⚠ загрузка шрифта не удалась — не критично (SVG самодостаточны)."; \
	  else \
	    echo "  ⚠ нет ни curl, ни wget — пропускаю (SVG самодостаточны)."; \
	  fi; \
	  command -v fc-cache >/dev/null 2>&1 && fc-cache -f "$(FONT_DIR)" >/dev/null 2>&1 || true; \
	fi

# Проверка окружения. Версия проверяется намеренно: Typst до 1.0 меняет язык
# между минорными выпусками (на `skew`, которым набирается синтетический курсив
# отсутствующего в Fira Code начертания, документ опирается с 0.13).
deps:
	@command -v $(TYPST) >/dev/null 2>&1 || { \
	  echo "ОШИБКА: не найден '$(TYPST)' в PATH."; \
	  echo "Установите Typst: brew install typst  (или cargo install typst-cli)"; \
	  exit 1; }
	@v=$$($(TYPST) --version 2>/dev/null | awk '{print $$2}'); \
	major=$$(echo "$$v" | cut -d. -f1); minor=$$(echo "$$v" | cut -d. -f2); \
	if [ "$$major" -eq 0 ] && [ "$$minor" -lt 13 ]; then \
	  echo "ОШИБКА: typst $$v старше требуемой 0.13 (проверенная версия — 0.15.1)."; \
	  exit 1; \
	fi

# Флаги сборки PDF. `--no-pdf-tags` отключает дерево тегов доступности
# (PDF/UA): замер показал, что именно оно даёт основной
# вес документа - 14 208 объектов `StructElem` против 51 кб шрифтов (1.7 %) и
# 192 кб SVG (6 %). Без тегов PDF весит 947 кб вместо 3.08 мб, а оглавление и
# ссылки сохраняются полностью (проверено: Outlines и 54 объекта `/Link` на
# месте). Решение проекта; цена названа - структурной разметки для
# программ чтения с экрана в документе нет.
TYPST_PDF_FLAGS ?= --no-pdf-tags

pdf: deps
	@mkdir -p $(dir $(OUT))
	$(TYPST) compile --root $(TYPST_ROOT) $(TYPST_PDF_FLAGS) $(MAIN) $(OUT)
	@echo "Готово: book/$(OUT)"

build: graphs pdf

check: build

clean:
	rm -rf book
