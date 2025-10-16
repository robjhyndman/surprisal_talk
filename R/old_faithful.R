of_labels <- labs(
  x = "Eruption duration (seconds)",
  y = "Waiting time to next eruption (seconds)",
  title = "Old Faithful eruptions from 14 January 2017 to 29 December 2023"
)

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

create_old_faithful_figure <- function(
  results,
  scale = FALSE,
  contour = FALSE,
  show_anomalies = FALSE
) {
  set_ggplot_options()
  alpha <- 500 / NROW(results)
  df <- results |>
    select(duration, waiting, outliers)
  if (scale) {
    df <- mvscale(df[, 1:2])
    colnames(df) <- c("duration", "waiting")
    df$outliers <- results$outliers
  }
  if (contour) {
    p <- dist_kde(df[, 1:2]) |>
      gg_density(show_points = !show_anomalies, colors = "black")
  } else {
    p <- df |>
      ggplot(aes(x = duration, y = waiting))
    if (!show_anomalies) {
      p <- p +
        geom_point(alpha = alpha, color = "black")
    }
  }
  if (show_anomalies) {
    p <- p +
      geom_point(
        data = df,
        aes(x = duration, y = waiting, color = outliers),
        alpha = alpha + (1 - alpha) * results$outliers
      ) +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red")
      ) +
      guides(color = "none")
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
