#!/usr/bin/env python3
# Переводит <text> в SVG-выводе Graphviz в векторные контуры (<path>) по заданному
# шрифту - так подписи графов верификации становятся самодостаточными: наклонный
# ГОСТ тип А "впечатан" в SVG и рендерится одинаково в любом просмотрщике и в PDF,
# без зависимости от установленного шрифта (фикс 0124-01).
#
# Использование: dot -Tsvg ... | svg_flatten_text.py <font.ttf> > out.svg
#
# Поддерживает text-anchor start|middle|end и типичные подписи (латиница, цифры,
# ',', '{', '}', '!', '_'). Глиф, которого нет в шрифте, пропускается с продвижением
# на ширину пробела - "тихой потери" символа быть не должно (подписи ASCII).
import re
import sys
import xml.sax.saxutils as sx

from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont

TEXT_RE = re.compile(
    r'<text\b([^>]*?)\btext-anchor="(?P<anchor>[^"]*)"'
    r'[^>]*?\bx="(?P<x>[-0-9.]+)"[^>]*?\by="(?P<y>[-0-9.]+)"'
    r'[^>]*?\bfont-size="(?P<fs>[-0-9.]+)"[^>]*?>(?P<txt>[^<]*)</text>',
    re.DOTALL,
)


def build(font_path):
    font = TTFont(font_path)
    upm = font["head"].unitsPerEm
    cmap = font.getBestCmap()
    gset = font.getGlyphSet()
    hmtx = font["hmtx"]
    space = hmtx["space"][0] if "space" in hmtx.metrics else upm // 2

    def glyph_name(ch):
        return cmap.get(ord(ch))

    def advance(ch):
        g = glyph_name(ch)
        return hmtx[g][0] if g else space

    def replace(m):
        x, y, fs = float(m["x"]), float(m["y"]), float(m["fs"])
        txt = sx.unescape(m["txt"])
        s = fs / upm
        total = sum(advance(c) for c in txt) * s
        anchor = m["anchor"]
        penx = x - total / 2 if anchor == "middle" else (x - total if anchor == "end" else x)
        d = []
        for ch in txt:
            g = glyph_name(ch)
            if g is not None:
                pen = SVGPathPen(gset)
                # Аффинное: масштаб s, y-flip (шрифт y-вверх, SVG y-вниз), сдвиг к (penx, y).
                gset[g].draw(TransformPen(pen, (s, 0, 0, -s, penx, y)))
                seg = pen.getCommands()
                if seg:
                    d.append(seg)
            penx += advance(ch) * s
        if not d:
            return ""
        return f'<path d="{" ".join(d)}"/>'

    data = sys.stdin.read()
    sys.stdout.write(TEXT_RE.sub(replace, data))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("использование: svg_flatten_text.py <font.ttf> < in.svg > out.svg")
    build(sys.argv[1])
