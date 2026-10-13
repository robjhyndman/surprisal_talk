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
