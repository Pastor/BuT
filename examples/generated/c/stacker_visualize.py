#!/usr/bin/env python3
"""
Stacker simulation visualizer.

Reads simulation log from stdin or a file, renders warehouse movement
as animated GIF — one file per simulation:
    stacker_sim1.gif, stacker_sim2.gif, stacker_sim3.gif, stacker_sim4.gif

Dependencies:
    pip install matplotlib Pillow

Usage:
    ./cmake-build-debug/stacker | python3 stacker_visualize.py
    python3 stacker_visualize.py simulation.log
    ./cmake-build-debug/stacker > sim.log && python3 stacker_visualize.py sim.log
"""

import re
import sys
import io
from pathlib import Path

# ── Log format (from print_tick in stacker_main.c) ───────────────────────────
#
# T%3d | pos(%2u,%2u,%2u)->(%2u,%2u,%2u)
#  | task[t=%d s=%u r=%u y=%u v=%d]
#  | fork=%d load=%d chrg=%d batt=%d
#  | ack=%d done=%d eta=%u
#  | CR=%-10s MV=%-12s LF=%-10s

SIM_HEADER_RE = re.compile(r'^(SIM\d+)\s+(\[[\w]+\])\s+(.+)$')

TICK_RE = re.compile(
    r'T\s*(\d+)'
    r'\s*\|\s*pos\(\s*(\d+),\s*(\d+),\s*(\d+)\)'
    r'\s*→\s*\(\s*(\d+),\s*(\d+),\s*(\d+)\)'
    r'\s*\|\s*task\[t=(-?\d+)\s+s=(\d+)\s+r=(\d+)\s+y=(\d+)\s+v=(-?\d+)\]'
    r'\s*\|\s*fork=(\d+)\s+load=(\d+)\s+chrg=(\d+)\s+batt=(\d+)'
    r'\s*\|\s*ack=(\d+)\s+done=(\d+)\s+eta=(\d+)'
    r'\s*\|\s*CR=(\S+)\s+MV=(\S+)\s+LF=(\S+)'
)

# Warehouse geometry
STACKS   = 12   # X axis: 0 … 11
SECTIONS = 21   # Y axis: 0 … 20
ROW_MAX  = 10   # Z axis: 0 … 10


def parse_log(text):
    """Return ordered dict: sim_name -> {'label': str, 'ticks': [dict, ...]}."""
    result = {}
    current = None

    for raw in text.splitlines():
        line = raw.strip()
        m = SIM_HEADER_RE.match(line)
        if m:
            current = m.group(1)
            result[current] = {
                'label': f"{m.group(1)} {m.group(2)} {m.group(3)}",
                'ticks': [],
            }
            continue

        if current is None:
            continue

        m = TICK_RE.search(raw)
        if not m:
            continue

        g = m.groups()
        result[current]['ticks'].append({
            'tick':         int(g[0]),
            'pos_stack':    int(g[1]),
            'pos_row':      int(g[2]),
            'pos_section':  int(g[3]),
            'tgt_stack':    int(g[4]),
            'tgt_row':      int(g[5]),
            'tgt_section':  int(g[6]),
            'task_type':    int(g[7]),
            'task_stack':   int(g[8]),
            'task_row':     int(g[9]),
            'task_section': int(g[10]),
            'task_valid':   int(g[11]),
            'fork':         int(g[12]),
            'loaded':       int(g[13]),
            'at_charge':    int(g[14]),
            'batt_low':     int(g[15]),
            'ack':          int(g[16]),
            'done':         int(g[17]),
            'eta':          int(g[18]),
            'cr_state':     g[19],
            'mv_state':     g[20],
            'lf_state':     g[21],
        })

    return result


def _draw_frame(tick, idx, all_ticks, sim_name, label):
    """Create and return one matplotlib Figure for the given tick."""
    import matplotlib
    import matplotlib.pyplot as plt
    import matplotlib.patches as mpatches
    import matplotlib.cm as mcm
    from matplotlib.colors import Normalize

    cmap = plt.cm.plasma
    norm = Normalize(vmin=0, vmax=ROW_MAX)

    fig, ax = plt.subplots(figsize=(12, 9))
    fig.patch.set_facecolor('#0d1b2a')
    ax.set_facecolor('#0d1b2a')

    # ── Warehouse grid ────────────────────────────────────────────────────────
    for sx in range(STACKS):
        for sec in range(SECTIONS):
            ax.add_patch(mpatches.Rectangle(
                (sx - 0.48, sec - 0.48), 0.96, 0.96,
                linewidth=0, facecolor='#0f2540'
            ))
    for sx in range(STACKS):
        ax.axvline(sx - 0.5, color='#1c3a5e', linewidth=0.35, zorder=1)
    for sec in range(SECTIONS):
        ax.axhline(sec - 0.5, color='#1c3a5e', linewidth=0.35, zorder=1)

    # ── Special zones ─────────────────────────────────────────────────────────
    def zone(sx, sec, facecolor, edgecolor, txt):
        ax.add_patch(mpatches.FancyBboxPatch(
            (sx - 0.46, sec - 0.46), 0.92, 0.92,
            boxstyle='round,pad=0.04',
            linewidth=1.8, edgecolor=edgecolor,
            facecolor=facecolor, alpha=0.35, zorder=2
        ))
        ax.text(sx, sec, txt, ha='center', va='center',
                fontsize=5.5, color=edgecolor, fontweight='bold', zorder=3)

    zone(0, 0,   '#005030', '#00e676', 'CHG')   # charge station
    zone(0, 1,   '#002a50', '#40c4ff', 'PICK')  # inbound pickup
    zone(11, 1,  '#502000', '#ff6d00', 'DROP')  # outbound dropoff

    # ── Movement trail ────────────────────────────────────────────────────────
    if idx > 0:
        xs = [t['pos_stack']   for t in all_ticks[:idx + 1]]
        ys = [t['pos_section'] for t in all_ticks[:idx + 1]]
        ax.plot(xs, ys, '-', color='#5577ff', alpha=0.45, linewidth=1.4, zorder=4)
        # start dot
        ax.plot(xs[0], ys[0], 'o', color='#5577ff', markersize=5,
                alpha=0.6, zorder=4)

    # ── Target cell ───────────────────────────────────────────────────────────
    ts, tr, ty = tick['tgt_stack'], tick['tgt_row'], tick['tgt_section']
    ax.add_patch(mpatches.Rectangle(
        (ts - 0.46, ty - 0.46), 0.92, 0.92,
        linewidth=2, edgecolor='#ff1744', facecolor='#ff1744',
        alpha=0.22, zorder=5
    ))
    ax.text(ts, ty, f'R{tr}', ha='center', va='center',
            fontsize=6, color='#ff5566', fontweight='bold', zorder=6)

    # ── Stacker marker (filled rectangle, color = row height) ────────────────
    ps, pr, py = tick['pos_stack'], tick['pos_row'], tick['pos_section']
    row_color = cmap(norm(pr))
    ax.add_patch(mpatches.FancyBboxPatch(
        (ps - 0.40, py - 0.40), 0.80, 0.80,
        boxstyle='square,pad=0',
        facecolor=row_color, edgecolor='white', linewidth=1.8, zorder=7
    ))
    ax.text(ps, py, str(pr),
            ha='center', va='center',
            fontsize=8, color='white', fontweight='bold', zorder=8)

    # Fork frame - yellow border when active
    if tick['fork']:
        ax.add_patch(mpatches.FancyBboxPatch(
            (ps - 0.48, py - 0.48), 0.96, 0.96,
            boxstyle='square,pad=0',
            facecolor='none', edgecolor='#ffd740', linewidth=2.5, zorder=9
        ))

    # Loaded cargo - orange rectangle below the stacker body
    if tick['loaded']:
        ax.add_patch(mpatches.FancyBboxPatch(
            (ps - 0.22, py - 0.62), 0.44, 0.18,
            boxstyle='square,pad=0',
            facecolor='#ff9100', edgecolor='#ffc400',
            linewidth=1, zorder=9
        ))

    # Battery low warning - dashed red border
    if tick['batt_low']:
        ax.add_patch(mpatches.FancyBboxPatch(
            (ps - 0.56, py - 0.56), 1.12, 1.12,
            boxstyle='square,pad=0',
            facecolor='none', edgecolor='#ff1744',
            linewidth=2, linestyle='--', zorder=9
        ))

    # ── Info panel ────────────────────────────────────────────────────────────
    task_dir = 'INBOUND' if tick['task_type'] == 0 else 'OUTBOUND'
    batt_tag = '  !! LOW BATTERY' if tick['batt_low'] else ''
    info = (
        f"{sim_name}   T = {tick['tick']}\n"
        f"Pos   stack={ps:2d}  row={pr:2d}  sec={py:2d}\n"
        f"Tgt   stack={ts:2d}  row={tr:2d}  sec={ty:2d}\n"
        f"Task  {task_dir}  ETA={tick['eta']:2d}{batt_tag}\n"
        f"CR = {tick['cr_state']}\n"
        f"MV = {tick['mv_state']}\n"
        f"LF = {tick['lf_state']}\n"
        f"fork={tick['fork']} load={tick['loaded']} "
        f"ack={tick['ack']} done={tick['done']}"
    )
    ax.text(0.01, 0.99, info, transform=ax.transAxes,
            fontsize=8, color='#d0e8ff', va='top', ha='left',
            bbox=dict(boxstyle='round,pad=0.5', facecolor='#080f1a',
                      alpha=0.88, edgecolor='#2d4a7a', linewidth=1),
            fontfamily='Fira Code', zorder=10)

    # ── Axes ──────────────────────────────────────────────────────────────────
    ax.set_xlim(-0.75, STACKS - 0.25)
    ax.set_ylim(-0.75, SECTIONS - 0.25)
    ax.set_xlabel('Stack  (X axis)', color='#6080a0', fontsize=9)
    ax.set_ylabel('Section  (Y axis)', color='#6080a0', fontsize=9)
    ax.set_xticks(range(STACKS))
    ax.set_yticks(range(0, SECTIONS, 2))
    ax.tick_params(colors='#506070', labelsize=7)
    for sp in ax.spines.values():
        sp.set_edgecolor('#1c3a5e')

    # Row-height colorbar
    sm = mcm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = fig.colorbar(sm, ax=ax, orientation='vertical',
                        fraction=0.022, pad=0.008)
    cbar.set_label('Row  (Z height)', color='#6080a0', fontsize=8)
    cbar.ax.tick_params(colors='#6080a0', labelsize=7)

    ax.set_title(label, color='#90b8d8', fontsize=10, pad=7, fontweight='bold')

    fig.tight_layout()
    return fig


def render_gif(sim_name, sim_data, output_dir):
    """Render one animated GIF for a simulation."""
    from PIL import Image
    import matplotlib.pyplot as plt
    plt.rcParams.update({
        'font.family':   'Fira Code',
        'font.monospace': 'Fira Code',
    })

    label  = sim_data['label']
    ticks  = sim_data['ticks']
    if not ticks:
        print(f"  {sim_name}: нет тиков — пропускаем")
        return

    print(f"  {sim_name}: {len(ticks)} кадров …", end='', flush=True)
    frames = []

    for idx, tick in enumerate(ticks):
        fig = _draw_frame(tick, idx, ticks, sim_name, label)
        buf = io.BytesIO()
        fig.savefig(buf, format='png', dpi=90,
                    facecolor=fig.get_facecolor())
        buf.seek(0)
        frames.append(Image.open(buf).copy())
        buf.close()

        import matplotlib.pyplot as plt
        plt.close(fig)

    out = Path(output_dir) / f"stacker_{sim_name.lower()}.gif"
    frames[0].save(
        out,
        save_all=True,
        append_images=frames[1:],
        loop=0,
        duration=1000,
        optimize=False,
    )
    print(f" → {out}")


def main():
    # ── Check dependencies early ──────────────────────────────────────────────
    missing = []
    for pkg in ('matplotlib', 'PIL'):
        try:
            __import__(pkg)
        except ImportError:
            missing.append(pkg.replace('PIL', 'Pillow'))
    if missing:
        print("Отсутствуют зависимости:", ', '.join(missing))
        print("Установите командой:  pip install matplotlib Pillow")
        sys.exit(1)

    import matplotlib
    matplotlib.use('Agg')

    # ── Read input ────────────────────────────────────────────────────────────
    if len(sys.argv) > 1:
        text = Path(sys.argv[1]).read_text(encoding='utf-8')
    else:
        text = sys.stdin.read()

    simulations = parse_log(text)
    if not simulations:
        print("Симуляции не найдены. Убедитесь что лог содержит строки вида:")
        print("  SIM1 [POSITIVE] ...")
        print("  T  4 | pos( 0, 0, 0)→( 0, 0, 0) | ...")
        sys.exit(1)

    output_dir = Path(__file__).parent
    print(f"Найдено симуляций: {sorted(simulations.keys())}")
    print(f"Вывод в каталог:   {output_dir}\n")

    for name in sorted(simulations):
        render_gif(name, simulations[name], output_dir)

    print("\nГотово.")


if __name__ == '__main__':
    main()
