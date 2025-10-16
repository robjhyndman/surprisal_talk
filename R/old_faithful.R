of_labels <- labs(
  x = "Eruption duration (seconds)",
  y = "Waiting time to next eruption (seconds)",
  title = "Old Faithful eruptions from 14 January 2017 to 29 December 2023"
)

create_fig_of <- function(oldfaithful, scale = FALSE, contour = TRUE) {
  df <- oldfaithful |>
    select(duration, waiting)
  if (scale) {
    df <- mvscale(df)
    colnames(df) <- c("duration", "waiting")
  }
  if (contour) {
    p <- dist_kde(df) |>
      gg_density(show_points = TRUE)
  } else {
    p <- ggplot(df, aes(x = duration, y = waiting)) +
      geom_point(alpha = 500 / NROW(df), col = "#0072b2")
  }
  if (scale) {
    p +
      labs(
        x = "Z1",
        y = "Z2",
        title = "Standardized Old Faithful eruptions from 14 January 2017 to 29 December 2023"
      )
  } else {
    p + of_labels
  }
}
analyse_old_faithful <- function(oldfaithful, scale, alpha, beta, gamma) {
  lookobjNew <- lookout::lookout(
    oldfaithful[, c("duration", "waiting")],
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )
  as.data.frame.lookoutliers(lookobjNew) |>
    mutate(method = "New Lookout")
}

create_old_faithful_figure <- function(results, show_anomalies = TRUE) {
  set_ggplot_options()
  alpha <- 500 / NROW(results)
  p <- results |>
    ggplot(aes(x = duration, y = waiting))
  if (show_anomalies) {
    p <- p +
      geom_point(
        aes(color = outliers),
        alpha = alpha + (1 - alpha) * results$outliers
      ) +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red")
      ) +
      guides(color = "none")
  } else {
    p <- p + geom_point(alpha = alpha, col = "#0072b2")
  }
  p + of_labels
}

get_of_surprisals <- function(oldfaithful) {
  of <- oldfaithful |>
    select(duration, waiting)
  of |>
    mutate(
      loo_kde_surprisal = surprisals(of, loo = TRUE, probability = FALSE),
      prob = surprisals(of, loo = TRUE, approximation = "gpd")
    ) |>
    filter(prob < 0.005) |>
    arrange(prob, duration)
}
