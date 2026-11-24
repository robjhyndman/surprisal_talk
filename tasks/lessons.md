# Lessons

- Before re-running an expensive computation copied from the book (e.g. rolling-origin model fits),
  look for the book's cached result (`~/git/OTexts/weird/rds/*.rds`, via its `cache()` helper) and reuse it.
  Tell the user up front if something will take more than a few minutes.
- When restyling fonts (or any CSS property), verify the computed style on every element type it should
  apply to (code blocks, syntax spans, output, inline code) in the rendered page, not just one of them:
  theme rules with higher specificity can silently win.
- Never load fonts (or other assets) from a CDN. Serve them locally. Fira Sans and Hack woff2 files already
  exist in `~/git/robjhyndman.com/fonts` (with `fonts.css` and licences); reuse that set (weights 400/700 only).
- Quarto loads KaTeX JS/CSS from a CDN even with `embed-resources`; bundle it locally and test with the
  network blocked.
- The editor formats on save with one sentence per line, which splits `. . .` pauses into three lines.
  Use `::: {.fragment}` divs for incremental reveals instead.
- Overflow checks on letterbox slides: content may extend into the 40px letterbox border below the
  720px slide area and is still visible. Only flag content that goes beyond that border.
- Staged figures overlaid as fragments: build every stage with the same structure (e.g. all patchwork,
  hidden parts as transparent placeholders), then check both that stages are pixel-aligned and that each
  stage looks right on its own (placeholders must not hide anything).
- Incremental code reveals (`data-steps` line numbers): screenshot every step in a headless browser
  (inject `Reveal.slide(h, 0, f)` into a scratch copy) and check that each step shows exactly the lines
  meant for it, every line is in some step, and no visible line ends in a dangling `|>`.
- The editor's panache formatter also reformats R code chunks (one pipe step per line), which can undo
  hand-tuned line joins and push slides into overflow. Protect such chunks with
  `<!-- panache-ignore-format-start -->` / `<!-- panache-ignore-format-end -->` around the chunk.
  It also realigns multi-line display maths (e.g. `cases` environments) into one long, deeply indented
  line; protect hand-laid-out `$$` blocks with the same markers.
- Don't stage the rendered slide HTML (e.g. `uconn_2026.html`, ~40 MB) in ordinary commits. Commit it once
  at the end of the session (usually end of day).
- Words like "log" in running text (e.g. "$-$log likelihood") are deliberately kept out of maths so they
  stay in Fira Sans. Don't move them into KaTeX to fix a line break; use
  `[...]{style="white-space: nowrap"}` instead.
- After optimising SVGs (svgo), check them in place on the rendered slide in Chrome, not just with
  `rsvg-convert`: the two render differently, and svgo's convertShapeToPath turned svglite's
  transparent clip rect into a black box that only showed in the browser.
- When fitting a figure to a slide, measure where its visible content (axis title) ends in a screenshot,
  not the image's bounding box, which includes the PNG's blank margin. Grow figures until the axis title
  sits just above the progress bar.
  This applies to every figure, including ones below code or text (not just figure-only slides): never
  stop at the default slide bottom. A figure in an `.absolute` box can take a negative `bottom` to reach
  into the letterbox border; also check its blank top margin doesn't cover content above it.
- Code shown on a slide must match what the figure actually does (e.g. the colour used), even when the
  real figure code adds styling. When consecutive slides build on the same plot, write their code in the
  same form so only the changed line differs; if a new line won't fit, reshape the earlier slide's code
  too rather than only the new one.
