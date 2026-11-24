# Old Faithful eruptions

of_labels <- ggplot2::labs(
  x = "Duration (seconds)",
  y = "Waiting time (seconds)"
)

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
    of_labels
}

old_faithful_hdrboxplot <- function(oldfaithful) {
  oldfaithful |>
    gg_hdrboxplot(duration, waiting, color = slide_colours[["blue"]]) +
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
