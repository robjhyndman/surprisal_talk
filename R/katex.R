# Helpers for setting maths in R graphics in the same fonts that KaTeX uses on
# the slides: italic letters in KaTeX_Math, everything else in KaTeX_Main

# Register KaTeX's fonts as one family ("KaTeX") for plotmath labels
register_katex_font <- function() {
  if ("KaTeX" %in% systemfonts::registry_fonts()$family) {
    return(invisible())
  }
  font <- function(family, italic = FALSE, bold = FALSE) {
    systemfonts::match_fonts(
      family,
      italic = italic,
      weight = if (bold) "bold" else "normal"
    )$path
  }
  systemfonts::register_font(
    name = "KaTeX",
    plain = font("KaTeX_Main"),
    bold = font("KaTeX_Main", bold = TRUE),
    italic = font("KaTeX_Math", italic = TRUE),
    bolditalic = font("KaTeX_Math", italic = TRUE, bold = TRUE)
  )
}

# Markup for ggtext labels (element_markdown(), geom_richtext()), so maths can
# be mixed with text in the slide font
katex_it <- function(x) {
  sprintf("<span style='font-family:KaTeX_Math'><i>%s</i></span>", x)
}
katex_up <- function(x) {
  sprintf("<span style='font-family:KaTeX_Main'>%s</span>", x)
}
katex_sub <- function(x, sub) paste0(x, "<sub>", sub, "</sub>")

# Restyle the surprisal probability figures (fig_sp0, fig_sp1, fig_sp2): axis
# titles and the observation's tick labels (y_i, f(y_i) in the figures) in KaTeX
# fonts, with subscript `sub`. The observation's tick labels are set in
# `obs_size` points (the axis title size), as they are smaller than the others.
katex_fig_sp <- function(p, sub = "t", obs_size = 15) {
  y <- katex_it("y")
  f_of <- function(x) paste0(katex_it("f"), katex_up("("), x, katex_up(")"))
  y_obs <- katex_sub(y, katex_it(sub))
  obs_label <- function(x) sprintf("<span style='font-size:%spt'>%s</span>", obs_size, x)
  p <- p +
    labs(x = y, y = f_of(y)) +
    # Position-specific elements, as complete themes such as theme_minimal()
    # set these and they would not inherit from axis.text.x etc.
    theme(
      axis.title.x.bottom = ggtext::element_markdown(),
      axis.title.y.left = ggtext::element_markdown(angle = 90, hjust = 0.5),
      axis.text.x.bottom = ggtext::element_markdown(),
      axis.text.y.left = ggtext::element_markdown()
    )
  # Replace the tick labels that are maths (named "$...$" by latex2exp)
  relabel <- function(scale, math) {
    if (is.null(scale) || !is.expression(scale$labels)) {
      return(scale)
    }
    scale <- scale$clone()
    labels <- names(scale$labels)
    scale$labels <- ifelse(grepl("^\\$", labels), math, labels)
    scale
  }
  p$scales$scales <- lapply(p$scales$scales, function(scale) {
    if ("x" %in% scale$aesthetics) {
      relabel(scale, obs_label(y_obs))
    } else if ("y" %in% scale$aesthetics) {
      relabel(scale, obs_label(f_of(y_obs)))
    } else {
      scale
    }
  })
  p
}
