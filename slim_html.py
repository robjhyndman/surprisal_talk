"""Post-render step: make the self-contained slide HTML smaller.

Run by Quarto after each render (see _quarto.yml) on the HTML files it
produced, or by hand: python3 slim_html.py file.html ...
"""

import base64
import os
import re
import shutil
import subprocess
import sys
import urllib.parse
from concurrent.futures import ThreadPoolExecutor


# Quarto's revealjs theme always imports Source Sans Pro (3 MB once embedded),
# but the slides use Fira Sans; only the slide menu names it, and falls back
# to Helvetica.
def strip_unused_fonts(html):
    def strip(m):
        css = urllib.parse.unquote(m.group(1))
        slim = re.sub(
            r"@font-face\s*\{[^{}]*font-family:\s*'Source Sans Pro'[^{}]*\}", "", css
        )
        if slim == css:
            return m.group(0)
        return "data:text/css," + urllib.parse.quote(slim, safe="")

    return re.sub(r"data:text/css,([^\"']*)", strip, html)


# Plots and photos reduced to a palette of at most 256 colours by pngquant:
# about a third of the size, and visually the same (PSNR 37-55 dB). The
# source PNGs in figs/ are left lossless.
def quantise_png(b64):
    result = subprocess.run(
        ["pngquant", "--quality=80-98", "--speed=1", "--strip", "--skip-if-larger", "-"],
        input=base64.b64decode(b64),
        capture_output=True,
    )
    # Non-zero when the quality can't be met or the PNG would grow: keep it as is
    if result.returncode != 0 or not result.stdout:
        return b64
    return base64.b64encode(result.stdout).decode("ascii")


def quantise_pngs(html):
    if shutil.which("pngquant") is None:
        print("slim_html: pngquant not found, PNGs left as they are")
        return html
    pattern = r"data:image/png;base64,([A-Za-z0-9+/=]+)"
    pngs = set(re.findall(pattern, html))
    with ThreadPoolExecutor() as pool:
        smaller = dict(zip(pngs, pool.map(quantise_png, pngs)))
    return re.sub(pattern, lambda m: "data:image/png;base64," + smaller[m.group(1)], html)


def slim(path):
    with open(path, encoding="utf-8") as f:
        html = f.read()
    before = len(html)
    html = strip_unused_fonts(html)
    html = quantise_pngs(html)
    with open(path, "w", encoding="utf-8") as f:
        f.write(html)
    print(f"slim_html: {path} {before / 1e6:.1f} MB -> {len(html) / 1e6:.1f} MB")


if __name__ == "__main__":
    files = sys.argv[1:] or os.environ.get("QUARTO_PROJECT_OUTPUT_FILES", "").split("\n")
    for path in files:
        if path.endswith(".html"):
            slim(path)
