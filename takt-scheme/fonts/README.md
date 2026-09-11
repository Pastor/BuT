# Шрифты чертежа

Чертёж набирается теми же шрифтами, что лист схемы на странице, и несёт их с
собой: SVG — в `@font-face`, растр — вшитыми байтами. Системные шрифты не
ищутся: иначе картинка зависела бы от машины.

Файлы — те же начертания, что `web/static/font/*.woff2`, развёрнутые обратно в
TTF (`fontTools`: `TTFont(woff2).flavor = None`) — растеризатор формата WOFF2 не
читает. Глифы и метрики у страницы и у чертежа одни; сверку держит
`scripts/check-scheme-fonts.py`.

- `GOST2.304-81TypeA-Slanted.ttf` — ГОСТ 2.304-81 тип А, наклонный, `Version 0.7.6`,
  лицензия SIL OFL 1.1 — [`OFL-GOST.txt`](OFL-GOST.txt); семейство внутри файла —
  «ГОСТ 2.304-81».
- `FiraCode-Regular.ttf` — Fira Code Regular `Version 6.002`, лицензия SIL OFL 1.1 —
  [`OFL-FiraCode.txt`](OFL-FiraCode.txt); семейство — «Fira Code».
