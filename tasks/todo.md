# UConn talk: Surprises in time series analysis (26 Oct 2026)

- [x] Read Auckland talk, FSI talk, book chapter (`~/git/OTexts/weird/timeseries.qmd`) and abstract
- [x] Add fable/feasts/tsibble/tsibbledata/ggtime and dev weird (for `us_mortality`) to renv
- [x] `R/timeseries.R`: functions for book examples (US mortality STL, PCs, PBS features, PBS surveillance loop)
- [x] `_targets.R`: targets for slow computations + `tar_quarto` for `uconn_2026.qmd`
- [x] Title graphic from a time series (`figs/title_ts.pdf`)
- [x] Make URL on title page a YAML variable (was hard-coded to ihaka2026)
- [x] Write `uconn_2026.qmd`: surprisal theory (from FSI talk), weird times, weird series, surveillance
- [x] Render and check PDF page by page

## Review

- Draft rendered: `uconn_2026.pdf` (87 pages incl. overlays), every page checked for overflow.
- Examples and results follow the current book chapter: French mortality (Hampel), US weekly mortality (STL + PCs),
  PBS weird series (features + LOO KDE), PBS surveillance (ETS one-step surprisals + Binomial across series).
- Theory from FSI talk: surprisal distribution, three-type theorem, GPD corollary, univariate experiment, LOO KDE.
- `pbs_scores` reads the book's cached `rds/pbs_scores.rds` (copied to `data/`); recomputing takes ~1 hour.
- Fixed `R/expt1.R`: used `surprisals()` (now returns surprisals, not probabilities) and old `"empirical"` option.
- Open: `attach_weird.txt` still shows weird 3.0.0; `us_mortality` is only in dev weird (3.1.0.9000);
  `/uconn2026` alias not yet added to robjhyndman.com/seminars/ts_surprises.md.

# Convert UConn talk from beamer to revealjs+letterbox (12 Oct 2026)

- [x] Switch YAML to `presentation-revealjs+letterbox`; svglite figures; drop LaTeX header/partials
- [x] Local CSS (`uconn_2026.scss`) for beamer-like blocks, alerts, outline slides, font sizes
- [x] Local `title-slide.html` partial that also shows the URL; PNG title graphic
- [x] Convert every slide: `\only`/`\pause` → fragments, textblock/placefig → `.absolute`, blocks → divs,
      LaTeX macros → KaTeX, kable → markdown table, outline slides per section
- [x] Update `tar_quarto` extra files for `slides_uconn`
- [x] Render, print to PDF via headless Chrome, check every slide for overflow/placement

## Review

- `uconn_2026.qmd` now renders to `uconn_2026.html` (64 slides) with `presentation-revealjs+letterbox`; no warnings.
- Checked every slide by driving headless Chrome over CDP with all fragments shown: screenshots plus a
  bounding-box overflow test. Only flags left are the letterbox title background (deliberately oversized)
  and a 5px rounding on the Commune de Paris photo.
- Block headings are spans (`[...]{.block-title}`), not `###`: headings inside divs become nested
  `<section>`s, which reveal.js treats as vertical slides.
- Beamer-only files (`header.tex`, `before-title.tex`) are still used by `auckland_2026.qmd`, so kept.
  Stale `uconn_2026.pdf`/`uconn_2026.tex` from the beamer render are untracked leftovers.

# Redesign UConn slides to look HTML-native (12 Oct 2026)

- [x] New palette (navy / warm paper / orange accent), Fira Sans text, Hack code
- [x] Dark section-divider slides with numbered agenda; dark title slide with light series graphic
- [x] Blocks as cards; dark code blocks with own syntax colours; light output panels; styled tables
- [x] Full-bleed photo slides; ggplot theme matching the slides; ragg PNG figures sized to fill slides
- [x] Bundle KaTeX CSS + fonts so maths works offline (quarto loads them from a CDN otherwise)
- [x] Re-render and screenshot every slide (all fragments shown); no overflow

## Review

- Plots are PNG (ragg, Fira Sans) rather than SVG: SVG in `<img>` cannot use web fonts.
- HTML is now ~19.5 MB because of the embedded PNGs (200 dpi).

# Move uconn_2026.qmd R code into targets (13 Oct 2026)

- [x] `R/uconn.R`: deck theme/options (`uconn_style()`, `with_uconn_style()`), outline/section helpers, video embed helper
- [x] New targets: fr anomaly counts, hampel fig, fr girafe + hover card, US mortality (data, STL scores, episodes, figs, girafe, video file, hover card),
      PBS features/PCA/KDE probs/figs, S01 first iteration, PBS surveillance anomalies/figs, joint Binomial table/fig, tscv fig, sp KaTeX figs
- [x] Rendered/styled widgets and video built inside `with_uconn_style()` so they match the qmd theme
- [x] qmd: displayed code kept with `eval: false`; hidden chunks only `tar_read()`/`tar_load()`
- [x] `tar_make()`, render, and compare against the previous HTML (text output + figures)

## Review

- All computation for uconn_2026.qmd now lives in targets; chunks only `tar_read()`/`tar_load()` (plus print/kable and the
  fig_sp overlay layout). Code shown on slides kept as `eval: false` chunks.
- Before/after HTML: all code output, tables, girafe widgets and 25/26 figures identical; zoom video frames identical.
- Difference: `pbs_features` was recomputed (now depends on `pbs_raw`). feasts 0.5.0 gives 45 features (old cache: 39),
  so the PCA scatter, LOO KDE table (R06, D11 replace A15, D04) and "features each" count changed. Old object not recoverable.
- Gotcha: unserialised ggtext/ggiraph objects need those packages attached in the qmd for their S3 methods.

# Faster rendering: figure PNGs as targets (13 Oct 2026)

- [x] `save_png()` / `include_png()` in `R/uconn.R`; `tar_png()` factory in `_targets.R` (16 figures + aligned fig_sp steps)
- [x] qmd chunks include the PNGs instead of drawing ggplots
- [x] Verify: rendered HTML identical to previous render (all 23 images pixel-identical)

## Review

- Render of slides_uconn: 15.3s -> 9.3s when no figure changed.
- PNGs must be 400 dpi (dpi 200 x Quarto fig-retina 2) and included with `dpi = 192` to get knitr's `width=1800`.
- ggtext measures text on the open device when grobs are built: build aligned grobs with a matching ragg device open.

# Staged Hampel figure (13 Oct 2026)

- [x] `create_hampel_fig()`: `stage` and `centre` arguments, fixed extents across stages
- [x] `hampel_window_video()`: window sliding 1828 -> 1849 tracing the median
- [x] Targets: `fig_hampel_stages`, `fig_hampel_png` (4 PNGs), `hampel_video`
- [x] qmd: r-stack of stages with fragment indices synced to the bullets; video plays on fragment
- [x] Verify: panels align, last video frame = window PNG, screenshots per step

## Review

- Stages share axes (geom_blank over the final extent, fixed xlim): PNG diffs confined to the plotted content.
- Video's last frame matches the "window" PNG (differences only from 200 vs 400 dpi and compression).
- Dropped the "y_t - m_t" text label (no clear space: overlaps the series); segment marked with points at m_t and y_t.
- Headless check: fragment shown -> video plays to its end (t = 3.00 s).
- Reveal renumbers fragment indices from 0 at runtime; data-fragment-index in the qmd only sets the order.

# Graphical build-up of feature space for "Weird series" (13 Oct 2026)

Replace the bullet-point "Feature-based anomaly detection" slide with a build-up modelled on
fbtsa (github.com/robjhyndman/fbtsa), using the PBS series that the section already uses.

- [x] "How do we compare 336 series?": PBS series rescaled to [0,1], overlaid 1, 2, 5, 20, 100, all (fragments)
- [x] "Strength of trend": histogram over PBS series, then lowest and highest example series (fragments)
- [x] "Strength of seasonality": same
- [x] "Feature space": series grid → pairs plot of a few features → PC scatter, with arrows (fragments);
      closing statement that a weird series is an anomaly in feature space
- [x] Targets for all staged PNGs; render and check every slide/fragment

## Review

- Five slides replace the bullet slide: PBS data, spaghetti build-up (1→all series), trend and seasonality
  histograms with low/high examples, and the series → pairs plot → PC space schematic, whose last step
  colours the three anomalous series (prob < 0.001) and their PC points orange.
- PBS data slide moved ahead of the build-up so "336 series" has context.
- Figures: `fig_pbs_build_png` target (functions at the end of `R/timeseries.R`); stacks via `cat_png_stack()`.
- Rendered with tar_make and checked in headless Chrome with all fragments shown.
- The caveat "a series unusual in a way no feature captures will not be found" was dropped from the slides.

# Slide 27 spaghetti build: similar series, then an odd one, then an animation (13 Oct 2026)

- [x] Stages 1–3: three similar series (Concessional Co-payments C08, H02, N02; near the PCA centre)
- [x] Stage 4: a very different series (General Co-payments C05, one of the weirdest)
- [x] Video adding the remaining series at an accelerating rate, last frame as poster
- [x] Size videos in `.r-stack.r-stretch` like images; check alignment in headless Chrome

## Review

- Video box matches the PNG stages exactly (156.7,143.1, 1286.7×629.1); its first frame equals stage 4.
- Played by the existing fragment-video script on the Hampel slide. The poster shows the full plot when
  returning to the slide.

# Slide 8: GPD surprisal probabilities as a staged figure (14 Oct 2026)

- [x] `create_fig_gpd_stages()` in `R/surprisals.R`: N(0,1) data, t(4) surprisals; stages histogram →
      threshold u (top β highlighted) → fitted β·ĝ → one s_t with shaded p̂_t
- [x] Targets `fig_gpd_stages` and `fig_gpd_png` (aligned PNGs)
- [x] Replace the bullet slide with the PNG stack plus a short closing fragment
- [x] tar_make, render, check every stage in headless Chrome (alignment and fit)

## Review

- Stages: histogram of surprisals → threshold u with the top 10% in orange → zoom to the tail with the
  fitted GPD density ĝ(s) → s_t with the shaded tail area and p̂_t = 0.1 × 0.205 ≈ 0.0205.
- Data: 1000 N(0,1) observations, surprisals under a t(4) density (as in the univariate experiment).
- Stages 1–2 and 3–4 have different axes (the zoom); panels are aligned by `save_aligned_pngs()`.
- Checked every step in headless Chrome; the slide ends at 708px.

## Zoom between stages 2 and 3 (14 Oct 2026)

- [x] Density scale throughout (curve is β·ĝ, shaded area is p̂_t), so the zoom is continuous
- [x] `gpd_stage_files()`: 4 PNGs and a 1.5 s zoom MP4 from grobs aligned together; frames drawn at the
      PNG resolution (400 dpi) and scaled to 1800 px by ffmpeg, as text layout differs at 200 dpi
- [x] `cat_png_stack(video_after =)` to place the video between stages
- [x] First video frame matches stage 2 (RMSE 1.3%); video and PNG boxes identical in Chrome

# Slide 37: surveillance with forecast distributions as a staged figure (14 Oct 2026)

- [x] `pbs_roll_data()`: rolling one-step ETS forecasts for S01 (mean, sd, observation, p)
- [x] `pbs_roll_plot()` / `pbs_roll_files()`: 4 stages (data → density → surprisal → p) + rolling video
- [x] Targets `pbs_roll`, `fig_pbs_roll_files`; remove `fig_tscv`, `fig_s01_forecast`
- [x] qmd: replace slides 37–38 with the stack, drop slide 42, retitle slide 39
- [x] tar_make, render, check every stage and the video in headless Chrome

## Review

- Slides 37–38 (bullets + rolling-origin dot diagram) are now one staged figure on S01: data to t →
  sideways ETS forecast density f_{t+1}(y) → observation with s_{t+1} = −log f_{t+1}(y_{t+1}) → tails
  (traced along the curve, as they are too thin to see) with p_{t+1} = 0.0029 → 6.2 s video rolling the
  origin to May 1999, past observations coloured by p (Mar 1999: p = 0.000245).
- Slide 42 (S01 fan chart) dropped; old slide 39 retitled "Why surprisal probabilities?".
  Removed `tscv_plot()`, `pbs_forecast_plot()` and targets `fig_tscv`, `fig_s01_forecast`.
- p values from the per-series refit match `pbs_fc_probs` and `pbs_scores` exactly.
- Headless Chrome: all PNGs and the video share one box (116,167, 1047×489); slide ends 11px inside;
  stepping with Reveal.next() the video restarts at 0 on its fragment and plays to the end.
- Every video in the deck already starts playing (hidden) on slide entry; harmless, since the fragment
  handler rewinds it. Not changed.
- Follow-up: density labelled f_{t+1|t}; tails evaluated at the exact boundaries (fill edges horizontal),
  traced only when thin (p < 0.05, curve near vertical), dashed boundary lines dropped.

# Slide 41: heat map of surprisal probabilities rolled forward (14 Oct 2026)

- [x] `pbs_heatmap_plot()` / `pbs_heatmap_files()`: series × month tiles coloured by p (as slide 37),
      empty start PNG, video filling in month by month, poster = final frame
- [x] Target `fig_pbs_heatmap_files`; qmd: replace the loop code on slide 41
- [x] tar_make, render, check alignment and playback in headless Chrome

## Review

- Slide 41's loop code replaced by a heat map (84 ATC groups × 168 months, coloured by p as on slide 37):
  empty map, then an 8 s video filling it in month by month behind a moving line; count line as a fragment.
  Months affecting many series (Dec 1997, Mar 1999, Dec 2001, early 2008) show as vertical stripes.
- Gotcha: Quarto's auto-stretch flattens a slide whose only image is in an r-stack (dropping the stack
  and the video). Slides with a single-PNG stack need `{.nostretch}`, as on the Hampel slide.
- Headless Chrome: PNG and video share one box; video plays to its end (8.0 s); slide ends 4px inside.

# Deck review: accuracy, font sizes, design, text (14 Oct 2026)

- [x] Screenshot every slide (all fragments) in headless Chrome; measure font sizes and content extent
- [x] Slide 9: fix the description of the experiment (it didn't match the plot on slide 10); 3 short cards
- [x] Slide 19: `a` is one constant per age group, not `a_t`; shorter lead-in
- [x] Slides 32, 43: drop `.small` (room to spare); slide 28: lone bullet → plain line
- [x] Title slide: URL pinned bottom left, below the background series
- [x] Plot theme: base_size 18; warm grey panels (#ebe6dc), grid in slide colour, navy strips; rebuilt all targets

## Review

- Re-rendered and re-screenshotted: only slides 1, 9, 19, 28, 32, 43 changed; slide 32 ends at 736px
  (within the letterbox border), slide 43 at 710px.
- Theme rebuild: 25 targets in ~3 min. Follow-up fixes: slides 20/21 y breaks (us_mortality_plot, so the static,
  zoom video and girafe versions match), slide 27 pairs-plot strips back to plain text, slide 10 legend label
  "f itself" (legend clipped at base_size 18). Contact sheets of all 22 figure slides checked.
- Slides 20/21 at 9 x 4.65in like slide 18 (PNG, girafe and zoom video). Slide 21's girafe container
  reaches 768px, but its SVG ends at 756px; the extra is empty widget space, as before.
- Slide 2: sketches revealed one at a time with their cards (fig_tsparadigms_stage_pngs, hidden sketches
  drawn transparent; stage 3 pixel-identical to the full figure); cards moved down 15px to centre on panels.
- Alert cards: fill #f6e9e0 → #fff1e6 (light peach), which no longer clashes with the warm grey panels.

# Smaller HTML and Monash logo (15 Oct 2026)

- [x] girafe(): no embedded Liberation fonts (plots use Fira Sans, loaded by the slides)
- [x] Post-render step: strip the unused Source Sans Pro / Source Code Pro fonts from the HTML
- [x] Post-render step: quantise embedded PNGs with pngquant
- [x] Monash logo (reversed white) at the top right of the title slide
- [x] Render; check size, computed styles, screenshots of the title slide and figure slides

## Review

- HTML 41.1 MB -> 16.9 MB: Liberation fonts 5.4 MB (girafe font set), Source Sans Pro 3.0 MB and
  PNG quantisation 15.7 MB (post-render slim_html.py, via a minimal _quarto.yml; runs in ~2 s).
- tar_quarto runs the post-render step and stays up to date afterwards (tar_outdated() empty).
- All 48 slides screenshotted fully revealed, old vs new: identical or >= 36 dB PSNR; slide 0 differs
  only by the logo. Girafe SVG text is all Fira Sans, as before.
- The ImgBot merge had re-compressed generated PNGs in figs/uconn, so targets rebuilt them; left uncommitted.

# WOMBAT talk: data science audience (20 Nov 2026)

New deck `wombat_2026.qmd`: uconn design and animations, Auckland content where it fits.

- [x] Outline helpers: section list per deck (option), ids safe for "?" etc.
- [x] Targets: uconn-styled PNGs for chi-square strips, spot-the-anomaly (with circled reveals),
      Old Faithful scatter/KDE/HDR boxplot, wine scatter/log/model plots, cricket plots + GAM,
      surprisal-probability stages with y_i, N(0,1) example figure; QR code; title graphic
- [x] Sections: What is an anomaly? / Wine / Approximate extreme surprisals (+ GPD animation) /
      Cricket / Old Faithful / French mortality (uconn) / Surveillance (uconn PBS) / Summary
- [x] `tar_quarto` target for the new deck; tar_make; render
- [x] Screenshot every slide (all fragments) in headless Chrome; check overflow and alignment

## Review

- `wombat_2026.qmd` (58 slides, 13.2 MB) renders via `tar_make()` (target `slides_wombat`); uconn deck
  and its figures unchanged (shared helpers got a `sub`/sections option with the old defaults).
- New: `R/wombat.R` (figures/results), `wombat_2026.css` (quote cards, photos, legend cards),
  `figs/title_of.png` (Old Faithful title graphic, added to `title_page.R`), PNGs in `figs/wombat/`.
- Screenshotted every slide (all fragments) plus each step of the staged slides; only the slides
  that are the same as in uconn reach into the letterbox border.
- Both cricket and PBS surveillance sections included; delete whichever isn't wanted (and its
  entry in `slide_sections` in the setup chunk). US weekly mortality dropped.
- QR code `figs/qr_wombat2026.svg` (made with segno) on the closing slide.
- Open: robjhyndman.com/wombat2026 alias not yet added.

# Trim the repo to the current talk (24 Nov 2026)

Past talks are frozen as tags/releases (Toptime2025, FSI2025, Auckland2026, UConn2026).

- [x] Baseline: copy freshly built `wombat_2026.html` and `figs/` to scratchpad
- [x] `_targets.R`: keep only the 67 targets upstream of `slides_wombat`; drop `_i` suffixes
      (`fig_sp_i_png` → `fig_sp_png`, `fig_gpd_i_files` → `fig_gpd_files`); all figures in `figs/`
- [x] `R/` by topic, only functions Wombat uses: `theme.R` (was `uconn.R`), `functions.R`, `katex.R`,
      `anomaly.R`, `surprisals.R`, `wine.R`, `old_faithful.R`, `fr_mortality.R`, `pbs.R`
- [x] Rename UConn-named shared things: `uconn_2026.css` → `slides.css`, `uconn_theme` → `slide_theme`,
      `with_uconn_style` → `with_slide_style`, `uconn_girafe_fonts` → `girafe_fonts`,
      `wombat_colours` → `slide_colours`, `wombat_of_labels` → `of_labels`
- [x] Delete old talks' sources/PDFs and support files only they use (keep `uconn_2026.html`: linked from
      robjhyndman.com via GitHub Pages)
- [x] `tar_make()`, `tar_prune()`, remove stale `figs/uconn`, `figs/wombat`
- [x] Compare rendered `wombat_2026.html` (and figures) with the baseline

## Review

- `_targets.R` has 65 targets (was ~300 incl. 198 animation frames); `R/` has 9 topic files (was 21);
  old talks' sources, PDFs and 10 images removed. `uconn_2026.html` kept at the root (GitHub Pages link).
- `of_labels` now uses `ggplot2::labs()`: it only worked before because a stray top-level
  `library(weird)` in the old `R/fr_mortality.R` happened to be sourced first.
- Verified: old code (worktree at 4a0ea62) and new code, both built cleanly with the same packages, give
  pixel-identical PNGs (37/37) and frame-identical videos (4/4); the slide HTML is identical apart from
  embedded media.
- Found a pre-existing order dependence: after `hampel_window_video()` draws text at 200 dpi, later
  400-dpi figures in the same R session are shifted by a fraction of a pixel (and vice versa), so figure
  pixels depend on which targets ran first. The figures before this change were the "contaminated" ones;
  the current `figs/` are clean renders. Not fixed here.

# Fix order-dependent figure rendering (24 Nov 2026)

- [x] `hampel_window_video()` draws frames at 400 dpi (like every PNG) and scales the video to 1800 px,
      as the other videos do; it was the only device opened at 200 dpi
- [x] Check: drawing the video, or the girafe widgets, no longer changes later figures; the video is the
      same whether drawn first or after other figures
- [x] `make clean && make`: all 37 PNGs pixel-identical to the clean reference, other 3 videos
      frame-identical; Hampel video still 1800x520, 91 frames, and its first frame now matches
      `fig_hampel_window.png` 4x more closely (RMSE 0.011 vs 0.043)

# The boxplot rule slide (review suggestion #3, 26 Nov 2026)

- [x] `boxplot_rule_normal()` in `R/anomaly.R` and target `fig_boxplot_normal_png`: N(0,1) density drawn like
      slide 9 (fig_sp2), ticks at Q1, Q3 and the fences, shared density level dashed, tails shaded (0.35% each)
- [x] Slide "The boxplot rule" at the end of section 1, after the N(0,1) slide: the rule, the figure, then two corner cards
      (symmetric unimodal density; Normal false alarm rate α ≈ 0.007)
- [x] Rendered; screenshotted each step

## Review

- A first version (χ² strips with fences, then χ²₅ against the implied Normal) was dropped as adding too little;
  its code was reverted and its targets pruned.

# Wine anomalies with GPD probabilities (27 Nov 2026)

- [x] Targets `wine_gpd_girafe` and `wine_gpd_card_html`: slide 17's original-scale plot and hover card, from `wine_p_gpd`
- [x] New slide after the GPD code slide: plot, hover card, header cards "9 anomalies (4.6 expected)" and $p_i < 0.001$
- [x] Rendered; screenshot checked; hover card fills in with the GPD p-value (checked in the DOM)
- [x] The 8 wines flagged only under the Normal model shown in amber (`former` argument in `R/wine.R`),
      also hoverable (card shows their GPD p); legend card "$p_i < 0.001$ / Normal only", header cards closer (`.close-cards`)

# Merge "What if f is wrong?" with the GPD figure (27 Nov 2026)

- [x] First two bullets moved to the Surprisals slide; there, all three lines after the card are bullets at
      body-text size with tighter spacing (`.body-size-list`), so the slide still fits
- [x] GPD figure stack (with zoom video) below the remaining three bullets; old slide 20 removed
- [x] `cat_png_stack()` gains `class`; `.gpd-stack` fixed at 340px, left-aligned; ordering card to its right
- [x] EVT bullet shortened to two lines to make room
- [x] Figure hidden until the three bullets are shown (`cat_png_stack(hide_first = TRUE)`)
- [x] Bullets closer together (`.tight-list`, shared with slide 8); GPD PNGs and video redrawn at
      7.8 x 3.4 in (was 9 x 3.8), so text is ~20% larger; stack 385px tall, axis title just above the
      progress bar; ordering card moved right
- [x] Dropped the β ĝ(s) label (notation not used elsewhere); ordering card top-aligned with the figure
- [x] Screenshotted each step; axis title clears the progress bar

## Review

- The GPD figure is ~880px wide (was full width on its own slide); text enlarged to compensate.

# PBS heatmap count card; French anomalies in one colour (27 Nov 2026)

- [x] Heatmap slide: the count under the plot is now a title-row card, "267 anomalies (14 expected)",
      revealed as before; the figure grows into the freed space
- [x] French anomalies plot: both sexes in the anomaly orange (`create_fr_anomaly_girafe(colour = )`)
- [x] Rendered; screenshotted both slides

# Teaser order, PBS first-iteration title (27 Nov 2026)

- [x] Teaser slides swapped: wine, then Old Faithful (the order of the sections)
- [x] "A first iteration: t = 36", with "First 3 years, to June 1994" in a title-row card
- [x] Checked: the HDR boxplot's 19 outliers are exactly the 19 eruptions with p < 0.005
- [x] Slide 38: the two longest code lines wrapped (reveal steps renumbered), `.mean` dropped from the
      output, code and output 0.7em (was 0.6em), columns 42/58 so the last table doesn't wrap;
      every step screenshotted

# PBS code without t (27 Nov 2026)

- [x] `t` dropped from `pbs` and the data slide; computed only in `pbs_roll_data()` (rolling figures unchanged)
- [x] Slide 38: `filter_index(~ "1994 Jun")`; no `group_by_key()`/`filter(cumsum(...))`/`ungroup()` (only drops L03
      at this origin, other forecasts identical; speaker note added); `left_join(pbs)`; 83 series shown
- [x] Slide 38 at the deck's normal code size (`.compact-code` removed), columns 43/57
- [x] Every step of slides 36 and 38 screenshotted; `fig_pbs_anomalies_png` hash unchanged
- [x] Slide 38: `mutate()` spread over six lines, one argument per line (steps renumbered); screenshotted

# Uniform text size (27 Nov 2026)

- [x] Slide 42: "Upper tail probability: P(N_t ≥ n_t)" (binomial tail, not a surprisal probability;
      the two agree for every flagged month)
- [x] Paragraphs now 1.15em like bullet lists (same exclusions: title/section slides, outlines,
      columns, legend cards; plus paragraphs inside list items)
- [x] `.body-size-list` → `.body-size` (covers text and lists; also usable on the slide heading);
      on Surprisals and Surprisal probabilities, which overflow at the larger size
- [x] `.find-out-more` container no longer 1.15em (its paragraphs were enlarged twice)
- [x] Measured computed sizes and content bottoms on every slide, all fragments shown; screenshotted
      the slides that grew

- [x] Surprisals and Surprisal probabilities back at the larger size by trimming words instead:
      "The surprisal of observation $y_i$ under density $f$: …"; "$s_i$ is one number, …, image, …";
      "Surprisals are already familiar:" dropped; "… under $f$:"; definition figure 466px (`.sp-def-stack`)
- [x] Shiraz caption: "*Wine Enthusiast* reviews: week of 15 June 2017" (one line)
- [x] Screenshotted all three slides at their last fragment
- [x] Boxplot rule: figure 585px (`.boxplot-stack`, was 484px) so the cards clear the curve and the
      0.35% labels; axis title just above the progress bar; screenshotted with both cards
- [x] Boxplot rule: left card 265px wide (was 330); right card "Normal density false alarm rate", moved right (left 920, was 880)
- [x] Shiraz model card: formula spilled out (inline 0.85em in a `.small` card, enlarged by the
      paragraph rule); both shrinks removed, card sized to its content (no width: at 2560x1440
      KaTeX renders relatively wider, and a fixed 660px still spilled); no card spills at 1280x800,
      1920x1080 or 2560x1440 (checked all)
- [x] Shiraz model card fits the empty top-left of the plot (no points under it, checked at 1280x800
      and 2560x1440): title dropped, formula on two lines ("log Price ~ N(mu, sigma^2)",
      "mu = a + b Points"), card at top 115, left 130
- [x] PBS data slide: "Monthly data" card lifted (top 645, was 664) off the progress bar
- [x] PBS rolling-forecast slide: the text under the figure is now in a card

## Review

- Text and bullets are 39.1px on every slide outside columns (34px in columns, as before).
  `.body-size` is still defined but no slide uses it.

# Simplify the CSS: one stylesheet (27 Nov 2026)

- [x] Style baseline (computed styles + boxes, every element, all fragments) at 1280x800 and 2560x1440;
      re-dump of the unchanged page identical (so the check is deterministic)
- [x] Merged wombat_2026.css into slides.css; dropped unused rules (image cards, step outputs, agenda,
      US zoom, .small, .body-size, duplicate list spacing); folded overrides (pre size, section h2)
- [x] wombat_2026.css dropped from the YAML and `_targets.R` extra_files; `git rm`
- [x] Re-rendered: computed styles identical at both sizes (first try caught Spot the anomaly's image
      margin, from merging its rule with the Old Faithful one: kept separate); sample slides
      pixel-identical

## Review

- 995 lines in two files -> 880 in one; no visible change.
