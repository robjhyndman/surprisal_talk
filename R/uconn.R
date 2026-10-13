# Styling and slide helpers for uconn_2026.qmd

# ggplot2 theme for the slides
uconn_theme <- function() {
  theme_minimal(base_size = 15, base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(fill = "#fbf9f4", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e4dfd4", linewidth = 0.4),
      text = element_text(colour = "#1e293b"),
      axis.text = element_text(colour = "#64748b"),
      strip.text = element_text(face = "bold", hjust = 0, colour = "#0f2a44"),
      plot.title = element_text(face = "bold", colour = "#0f2a44"),
      plot.title.position = "plot"
    )
}

# Evaluate `expr` with the slides' ggplot2 theme and colours, restoring the
# previous ones afterwards. ggplots are styled when drawn, so this is needed
# for targets that draw plots (widgets, videos) rather than return them.
with_uconn_style <- function(expr) {
  opts <- c("ggplot2.discrete.colour", "ggplot2.discrete.fill")
  old_opts <- lapply(stats::setNames(nm = opts), getOption)
  old_theme <- theme_set(uconn_theme())
  on.exit({
    options(old_opts)
    theme_set(old_theme)
  })
  set_ggplot_options()
  force(expr)
}

# Outline listing all sections, with the current one highlighted
uconn_sections <- c(
  "Surprisals",
  "Weird times",
  "Weird series",
  "Surveillance",
  "Summary"
)
outline <- function(current = NULL, sections = uconn_sections) {
  ids <- paste0("part-", gsub(" ", "-", tolower(sections)))
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
# Body of a section divider slide: section number and outline
section <- function(current, sections = uconn_sections) {
  paste0(
    "\n\n::: {.section-number}\n",
    sprintf("%02d", match(current, sections)),
    "\n:::\n",
    outline(current, sections)
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
# knitr options in uconn_2026.qmd (dpi 200, with Quarto's fig-retina 2), so the
# slides needn't redraw it each render
save_png <- function(plot, file, width = 9, height = 4.4, res = 400) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  register_katex_font()
  with_uconn_style({
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
  file
}

# Save ggplots as PNGs with identical panel positions, so they overlay exactly.
# The grobs are built with a matching PNG device open, as ggtext measures text
# on the current device.
save_aligned_pngs <- function(plots, files, width = 9, height = 4.4, res = 400) {
  register_katex_font()
  tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, width = width, height = height, units = "in", res = res)
  grobs <- with_uconn_style(align_ggplots(plots))
  grDevices::dev.off()
  unlink(tmp)
  mapply(save_png, grobs, files, MoreArgs = list(width = width, height = height, res = res))
}

# Include a PNG from save_png() in the slides, sized as knitr sizes its own
# plots: width in pixels / fig-retina
include_png <- function(file, retina = 2) {
  knitr::include_graphics(file, dpi = 96 * retina)
}

# Write markdown for PNGs stacked on top of each other, each after the first
# revealed as a fragment (call from a chunk with `results: asis`)
cat_png_stack <- function(files) {
  cat("\n::: {.r-stack .r-stretch}\n")
  for (step in seq_along(files)) {
    cat(if (step == 1) "\n::: {.step}\n" else "\n::: {.fragment}\n")
    cat("![](", files[step], "){width=1800}\n", sep = "")
    cat("\n:::\n")
  }
  cat("\n:::\n")
}
