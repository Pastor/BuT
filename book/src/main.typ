// Документ "Язык Takt - описание": состав и порядок разделов.
//
// Этот файл заменил `book/src/SUMMARY.md`: порядок частей и разделов, а также
// автонумерация глав задаются здесь. Каждый новый раздел -
// отдельная фича, и его строка добавляется сюда.
//
// Сборка: `make -C book build` -> `book/book/pdf/takt-language.pdf`.

#import "template.typ": book, part

#show: book

// ── Передний блок: главы без номера ────────────────────────────────────────
#set heading(numbering: none)

#include "title.typ"
#include "introduction.typ"
#include "purpose.typ"

// ── Части и нумерованные главы ─────────────────────────────────────────────
#set heading(numbering: "1.1.1")

#part("Описание языка Takt")

#include "01-overview/index.typ"
#include "02-lexical/index.typ"
#include "03-types/index.typ"
#include "04-models-states/index.typ"
#include "05-expressions/index.typ"
#include "06-control-flow/index.typ"
#include "07-functions/index.typ"
#include "08-ports-addresses/index.typ"
#include "09-imports/index.typ"
#include "10-named/index.typ"
#include "11-execution/index.typ"
#include "12-time/index.typ"
#include "13-verification/index.typ"
#include "14-targets/index.typ"
#include "15-simulation/index.typ"
#include "16-diagnostics/index.typ"

#part("Применение")

#include "17-tools/index.typ"
#include "18-showcase/index.typ"
#include "19-library/index.typ"
#include "20-processor/index.typ"
#include "21-code-style/index.typ"

#part("Приложения")

#include "appendix-errors/index.typ"
#include "appendix-generated/index.typ"
#include "appendix-grammar/index.typ"
#include "appendix-references/index.typ"
#include "appendix-glossary/index.typ"
