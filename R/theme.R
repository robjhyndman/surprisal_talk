# Slide theme, colours and helpers for drawing and placing figures on the slides

slide_colours <- c(
  ink = "#1e293b",
  faint = "#94a3b8",
  blue = "#1d6fa5",
  amber = "#F39B00",
  orange = "#c14b14"
)

# ggplot2 theme for the slides
slide_theme <- function() {
  theme_minimal(base_size = 18, base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(fill = "#fbf9f4", colour = NA),
      # Warm grey panels with grid lines in the slide colour, so the grid fades
      # into the background; strips in the navy softened into the panel grey (70%)
      panel.background = element_rect(fill = "#ebe6dc", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#fbf9f4", linewidth = 0.5),
      text = element_text(colour = "#1e293b"),
      axis.text = element_text(colour = "#64748b"),
      strip.background = element_rect(fill = "#516272", colour = NA),
      strip.text = element_text(
        face = "bold", hjust = 0, colour = "white",
        margin = margin(4, 6, 4, 6)
      ),
      plot.title = element_text(face = "bold", colour = "#0f2a44"),
      plot.title.position = "plot"
    )
}

# Evaluate `expr` with the slides' ggplot2 theme and colours, restoring the
# previous ones afterwards. ggplots are styled when drawn, so this is needed
# for targets that draw plots (widgets, videos) rather than return them.
with_slide_style <- function(expr) {
  opts <- c("ggplot2.discrete.colour", "ggplot2.discrete.fill")
  old_opts <- lapply(stats::setNames(nm = opts), getOption)
  old_theme <- theme_set(slide_theme())
  on.exit({
    options(old_opts)
    theme_set(old_theme)
  })
  set_ggplot_options()
  force(expr)
}

# Font set for girafe() that embeds no font files. By default girafe() embeds
# all twelve Liberation fonts (5 MB); the plots use Fira Sans, which the slides
# already load.
girafe_fonts <- function() {
  fonts <- gdtools::font_set(sans = "Fira Sans")
  fonts$dependencies <- list()
  fonts
}

# (another deck can set its own with options(slide_sections = ...))
outline <- function(current = NULL,
                    sections = getOption("slide_sections")) {
  ids <- section_id(sections)
  status <- if (is.null(current)) {
    ""
  } else {
    ifelse(sections == current, " .current", " .shaded")
  }
  items <- paste0("[", sections, "](#", ids, "){.outline-link", status, "}")
  paste0(
    "\n\n::: {.outline}\n\n",
    paste0(seq_along(sections), ". ", items, collapse = "\n"),
    "\n\n:::\n\n"
  )
}

# Slide id of a section divider: "part-" and the name in lower case, with
# punctuation dropped and spaces as hyphens ("What is an anomaly?" ->
# "part-what-is-an-anomaly")
section_id <- function(sections) {
  paste0("part-", gsub("[^a-z0-9]+", "-", gsub("[?!.,]", "", tolower(sections))))
}

# Body of a section divider slide: section number, outline and, if given, a
# credit for the background photo (markdown, e.g. "Photo: [Name](url), CC BY")
section <- function(current,
                    sections = getOption("slide_sections"),
                    credit = NULL) {
  paste0(
    "\n\n::: {.section-number}\n",
    sprintf("%02d", match(current, sections)),
    "\n:::\n",
    outline(current, sections),
    if (!is.null(credit)) paste0("::: {.photo-credit}\n", credit, "\n:::\n\n")
  )
}

# Video element with an MP4 file embedded, so the slides are self-contained
video_tag <- function(file) {
  htmltools::tags$video(
    src = paste0("data:video/mp4;base64,", base64enc::base64encode(file)),
    muted = NA,
    playsinline = NA,
    preload = "auto"
  )
}

# Save a ggplot (or a grob) as a PNG styled as on the slides, matching the
# knitr options in the talk (dpi 200, with Quarto's fig-retina 2), so the
# slides needn't redraw it each render
save_png <- function(plot, file, width = 9, height = 4.4, res = 400) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  register_katex_font()
  with_slide_style({
    ragg::agg_png(file, width = width, height = height, units = "in",
      res = res, background = "#fbf9f4")
    if (ggplot2::is_ggplot(plot)) {
      print(plot)
    } else {
      grid::grid.newpage()
      grid::grid.draw(plot)
    }
    grDevices::dev.off()
  })
  optimise_png(file)
}

# Compress PNGs losslessly with oxipng (as ImgBot does on GitHub), so the files
# targets records are already compressed. Returns `files`.
optimise_png <- function(files) {
  oxipng <- Sys.which("oxipng")
  if (oxipng == "") {
    oxipng <- path.expand("~/.cargo/bin/oxipng")
  }
  if (!file.exists(oxipng)) {
    warning("oxipng not found; PNGs left uncompressed")
    return(files)
  }
  status <- system2(oxipng, c("--quiet", "--strip", "safe", shQuote(files)))
  if (status != 0) {
    warning("oxipng failed on ", paste(files, collapse = ", "))
  }
  files
}

# Save ggplots as PNGs with identical panel positions, so they overlay exactly.
# The grobs are built with a matching PNG device open, as ggtext measures text
# on the current device.
save_aligned_pngs <- function(plots, files, width = 9, height = 4.4, res = 400) {
  register_katex_font()
  tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, width = width, height = height, units = "in", res = res)
  grobs <- with_slide_style(align_ggplots(plots))
  grDevices::dev.off()
  unlink(tmp)
  mapply(save_png, grobs, files, MoreArgs = list(width = width, height = height, res = res))
}

# Include a PNG from save_png() in the slides, sized as knitr sizes its own
# plots: width in pixels / fig-retina
include_png <- function(file, retina = 2) {
  knitr::include_graphics(file, dpi = 96 * retina)
}

# Write markdown for printed outputs stacked on top of each other, one per
# step of a code reveal (call from a chunk with `results: asis`). The first is
# shown with the slide; output k replaces output k - 1 at fragment index k - 1,
# to match a `.code-steps` div whose steps have indices 1, 2, ...
cat_output_steps <- function(tables, n = 5, width = 80) {
  cat("\n::: {.r-stack .output-steps}\n\n")
  for (k in seq_along(tables)) {
    cls <- if (k == 1) {
      ".fragment .fade-out"
    } else if (k < length(tables)) {
      ".fragment .current-visible"
    } else {
      ".fragment"
    }
    cat(
      "::: {", cls, " fragment-index=\"", max(k - 1, 1), "\"}\n",
      "::::: {.cell-output .cell-output-stdout}\n````\n",
      sep = ""
    )
    cat(capture.output(print(tables[[k]], n = n, width = width)), sep = "\n")
    cat("\n````\n:::::\n:::\n\n")
  }
  cat(":::\n")
}

# Write markdown for PNGs stacked on top of each other, each after the first
# revealed as a fragment (call from a chunk with `results: asis`). An optional
# video (MP4 file and poster PNG of its last frame) is added as a fragment after
# PNG number `video_after` (by default the last), played when shown by the
# script on the Hampel slide. `indices`, if given, sets the fragment index of
# each PNG after the first, so other fragments on the slide can come between.
cat_png_stack <- function(files, video = NULL, video_after = length(files), indices = NULL) {
  video_md <- function() {
    tag <- htmltools::tagAppendAttributes(
      video_tag(video[1]),
      poster = paste0("data:image/png;base64,", base64enc::base64encode(video[2])),
      width = 1800
    )
    cat("\n::: {.fragment}\n```{=html}\n", as.character(tag), "\n```\n:::\n", sep = "")
  }
  cat("\n::: {.r-stack .r-stretch}\n")
  for (step in seq_along(files)) {
    cat(
      if (step == 1) {
        "\n::: {.step}\n"
      } else if (is.null(indices)) {
        "\n::: {.fragment}\n"
      } else {
        paste0("\n::: {.fragment fragment-index=\"", indices[step - 1], "\"}\n")
      }
    )
    cat("![](", files[step], "){width=1800}\n", sep = "")
    cat("\n:::\n")
    if (!is.null(video) && step == video_after) {
      video_md()
    }
  }
  cat("\n:::\n")
}
