"""Generate assets/pixel-fade.svg, a pixel-dissolve gradient.

Usage:
  python3 assets/generate-pixel-fade.py [--flip] [--seed N]

Default: dense at top, dissolving toward the bottom.
--flip: dense at bottom, dissolving toward the top.
"""

import argparse
import random

CELL_W = 20
CELL_H = 10
COLS = 80
ROWS = 9
OVERLAP = 0.6
COLOR = "#419CF5"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--flip", action="store_true")
    parser.add_argument("--seed", type=int, default=7)
    args = parser.parse_args()

    random.seed(args.seed)
    rects = []
    for row in range(ROWS):
        t = row / (ROWS - 1)
        density = t**2.5 if args.flip else (1 - t) ** 2.5
        for col in range(COLS):
            if random.random() < density:
                x = col * CELL_W - OVERLAP / 2
                y = row * CELL_H - OVERLAP / 2
                rects.append(
                    f'<rect x="{x}" y="{y}" width="{CELL_W + OVERLAP}" height="{CELL_H + OVERLAP}"/>'
                )

    svg = (
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {COLS * CELL_W} {ROWS * CELL_H}" '
        f'preserveAspectRatio="none" shape-rendering="crispEdges">'
        f'<g fill="{COLOR}">{"".join(rects)}</g></svg>'
    )
    with open("assets/pixel-fade.svg", "w") as f:
        f.write(svg)


if __name__ == "__main__":
    main()
