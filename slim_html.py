"""Post-render step: make the self-contained slide HTML smaller.

Run by Quarto after each render (see _quarto.yml) on the HTML files it
produced, or by hand: python3 slim_html.py file.html ...
"""

import os
import re
import sys
import urllib.parse


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


def slim(path):
    with open(path, encoding="utf-8") as f:
        html = f.read()
    before = len(html)
    html = strip_unused_fonts(html)
    with open(path, "w", encoding="utf-8") as f:
        f.write(html)
    print(f"slim_html: {path} {before / 1e6:.1f} MB -> {len(html) / 1e6:.1f} MB")


if __name__ == "__main__":
    files = sys.argv[1:] or os.environ.get("QUARTO_PROJECT_OUTPUT_FILES", "").split("\n")
    for path in files:
        if path.endswith(".html"):
            slim(path)
