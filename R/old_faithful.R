# Old Faithful eruptions

of_labels <- ggplot2::labs(
  x = "Duration (seconds)",
  y = "Waiting time (seconds)"
)

# Common axis limits, so the points stay put between the KDE and HDR box plot
# slides (the 99% HDR extends beyond the data)
of_coord <- ggplot2::coord_cartesian(xlim = c(0, 312), ylim = c(3000, 7315))

old_faithful_scatter <- function(oldfaithful) {
  oldfaithful |>
    ggplot(aes(x = duration, y = waiting)) +
    geom_point(alpha = 0.3, colour = slide_colours[["ink"]]) +
    of_labels +
    labs(title = "Eruptions from 14 January 2017 to 29 December 2023")
}

# Kernel density estimate of the eruptions, with the points
old_faithful_kde <- function(oldfaithful) {
  oldfaithful |>
    select(duration, waiting) |>
    dist_kde() |>
    gg_density(show_points = TRUE, alpha = 0.15, colors = slide_colours[["blue"]]) +
    of_coord +
    of_labels
}

old_faithful_hdrboxplot <- function(oldfaithful) {
  oldfaithful |>
    gg_hdrboxplot(duration, waiting, color = slide_colours[["blue"]]) +
    of_coord +
    of_labels
}

# Surprisal probabilities of the eruptions, as on the slide
old_faithful_probs <- function(oldfaithful) {
  oldfaithful$p <- surprisals_prob(
    oldfaithful[, c("duration", "waiting")],
    loo = TRUE,
    approximation = "gpd"
  )
  oldfaithful
}

# The KDE plot with the eruptions of surprisal probability < `alpha` in orange,
# as an interactive plot: hovering over one shows old_faithful_card()
old_faithful_girafe <- function(oldfaithful, of_probs, alpha = 0.005, width = 10, height = 3.95) {
  anomalies <- of_probs |>
    filter(p < alpha) |>
    mutate(id = of_id(time))
  p <- old_faithful_kde(oldfaithful) +
    ggiraph::geom_point_interactive(
      data = anomalies,
      aes(x = duration, y = waiting, data_id = id),
      colour = slide_colours[["orange"]],
      size = 2.5
    )
  ggiraph::girafe(
    ggobj = p,
    font_set = girafe_fonts(),
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_hover(css = "stroke:#0f2a44; stroke-width:2px; r:6px;"),
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc"))
    )
  )
}

of_id <- function(time) format(time, "%d %b %Y %H:%M")

# Card for old_faithful_girafe(), in the title row: the duration as recorded on
# geysertimes.org, the parsed duration and waiting time, and the surprisal
# probability
old_faithful_card <- function(of_probs, alpha = 0.005) {
  anomalies <- of_probs |>
    filter(p < alpha) |>
    mutate(
      prob = if_else(p < 1e-4, "&lt; 0.0001", sprintf("= %.4f", p)),
      label = paste0(
        "recorded as &ldquo;", htmltools::htmlEscape(recorded_duration), "&rdquo;<br>",
        "Duration ", duration, " s, waiting ", waiting, " s &middot; ",
        "<span style='font-style:italic'>p</span> ", prob
      )
    )
  hover_card(
    stats::setNames(anomalies$label, of_id(anomalies$time)),
    sep = ",",
    class = "of-card"
  )
}
