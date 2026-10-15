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
# about a third of the size, and visually the same (PSNR 37-55 dB). Then
# compressed losslessly by oxipng (as ImgBot does), saving about a fifth more.
# The source PNGs in figs/ are left as they are.
PNGQUANT = shutil.which("pngquant")
OXIPNG = shutil.which("oxipng") or shutil.which("oxipng", path=os.path.expanduser("~/.cargo/bin"))


def run(command, png):
    result = subprocess.run(command, input=png, capture_output=True)
    # Non-zero when pngquant can't meet the quality or the PNG would grow: keep it
    if result.returncode != 0 or not result.stdout:
        return png
    return result.stdout


def compress_png(b64):
    png = base64.b64decode(b64)
    if PNGQUANT:
        png = run([PNGQUANT, "--quality=80-98", "--speed=1", "--strip", "--skip-if-larger", "-"], png)
    if OXIPNG:
        png = run([OXIPNG, "--opt", "4", "--strip", "safe", "--stdout", "-"], png)
    return base64.b64encode(png).decode("ascii")


def compress_pngs(html):
    for name, path in [("pngquant", PNGQUANT), ("oxipng", OXIPNG)]:
        if path is None:
            print(f"slim_html: {name} not found, skipped")
    pattern = r"data:image/png;base64,([A-Za-z0-9+/=]+)"
    pngs = set(re.findall(pattern, html))
    with ThreadPoolExecutor() as pool:
        smaller = dict(zip(pngs, pool.map(compress_png, pngs)))
    return re.sub(pattern, lambda m: "data:image/png;base64," + smaller[m.group(1)], html)


def slim(path):
    with open(path, encoding="utf-8") as f:
        html = f.read()
    before = len(html)
    html = strip_unused_fonts(html)
    html = compress_pngs(html)
    with open(path, "w", encoding="utf-8") as f:
        f.write(html)
    print(f"slim_html: {path} {before / 1e6:.1f} MB -> {len(html) / 1e6:.1f} MB")


if __name__ == "__main__":
    files = sys.argv[1:] or os.environ.get("QUARTO_PROJECT_OUTPUT_FILES", "").split("\n")
    for path in files:
        if path.endswith(".html"):
            slim(path)
