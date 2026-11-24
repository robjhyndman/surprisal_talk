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

# The same KDE plot, with the eruptions of surprisal probability < 0.005
# highlighted
old_faithful_kde_anomalies <- function(oldfaithful, of_probs) {
  old_faithful_kde(oldfaithful) +
    geom_point(
      data = filter(of_probs, p < 0.005),
      aes(x = duration, y = waiting),
      colour = slide_colours[["orange"]]
    )
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
  oldfaithful |> arrange(p)
}
